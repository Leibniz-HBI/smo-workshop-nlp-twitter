options(stringsAsFactors = F)

# load required packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(dplyr)
pacman::p_load(tidytext)
pacman::p_load(slam)
pacman::p_load(igraph)
pacman::p_load(quanteda)
pacman::p_load(quanteda.textplots)
pacman::p_load(ggplot2)
pacman::p_load(ggwordcloud)

# now load the preprocessed data
load("annotated_text.RData")

# The CSU is just a bavarian CDU ;)
btw_tweets$Partei[btw_tweets$Partei == "CSU"] <- "CDU"

# plan:
# - roter faden / story: klima begriff / debatte verfolgen
# - quali-rückbindung: validate^3
# - story
#   - which topics are most prominent per party: keyterms per party
#   - did klima became more important during the campaign: klimaterms over time
#   - how does climate framing differ: klima coocs per party
#   - skip topic modeling
#   - leave time for playing around


# Key terms
# ===========================================

# We extract important vocabulary for each party based on log-likelihood statistics
# load a prepared function
source("src/calculateLogLikelihood.R")

selected_party <- "GRÜNE"

dtm <- annotated_text %>%
  filter(upos %in% c("NOUN", "ADV", "ADJ")) %>%
  mutate(count = 1) %>%
  cast_dtm(doc_id, lemma, count)

all_parties <- btw_tweets$Partei
names(all_parties) <- btw_tweets$doc_id
all_parties <- all_parties[rownames(dtm)]

# First, we count all terms in one party and only keep those occurring > 2
word_counts_target <- col_sums(dtm[all_parties == selected_party, ])
word_counts_target <- word_counts_target[word_counts_target > 2]

word_counts_comparison <- col_sums(dtm[all_parties != selected_party, ])
word_counts_comparison <- word_counts_comparison[word_counts_comparison > 2]

# ... to use it in the function to determine the ll-significance of Abe's vocabulary
llSignificance <- calculateLogLikelihood(word_counts_target, word_counts_comparison)

# We extract the top 25 terms most distinct for the SPD
top25 <- sort(llSignificance, decreasing = T)[1:25]

top25_df <- data.frame(
  word = names(top25),
  frq = top25
)
ggwordcloud2(top25_df)

# Check for certain keywords
View(btw_tweets[btw_tweets$Partei == selected_party & grepl("originalbrecht", btw_tweets$text, fixed = T), ])


# all parties in one plot
all_keyterms <- NULL
for (selected_party in unique(all_parties)) {
  # First, we count all terms in one party and only keep those occurring > 2
  word_counts_target <- col_sums(dtm[all_parties == selected_party, ])
  word_counts_target <- word_counts_target[word_counts_target > 2]
  
  word_counts_comparison <- col_sums(dtm[all_parties != selected_party, ])
  word_counts_comparison <- word_counts_comparison[word_counts_comparison > 2]
  
  # ... to use it in the function to determine the ll-significance of Abe's vocabulary
  llSignificance <- calculateLogLikelihood(word_counts_target, word_counts_comparison)
  
  # We extract the top 25 terms most distinct for the SPD
  top25 <- sort(llSignificance, decreasing = T)[1:25]
  
  top25_df <- data.frame(
    party = selected_party,
    word = names(top25),
    ll = top25
  )
  all_keyterms <- rbind(all_keyterms, top25_df)
}

ggplot(all_keyterms, aes(label = word, size = ll, color = party)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  facet_wrap(~party) +
  theme_minimal()




# Frequency over time
# ======================================

# We choose som terms for a time series plot
selected_terms <- c("Impfplicht", "Sicherheit", "sozial", "Freiheit", "Klimaschutz", "Mindestlohn")
# "Digitalisierung", "Mietendeckel", "sozial", ...

# This block counts our selected terms per decade by combining metadata
counts_per_day <- annotated_text %>%
  filter(lemma %in% selected_terms) %>%
  group_by(created_at) %>%
  count(lemma)

# We plot the time series
ggplot(counts_per_day, aes(x = created_at, y = n, group = lemma, color = lemma)) +
  geom_line(size = 1) + theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Cooccurrence term networks
# ==================================

selected_party <- "AfD"

dtm <- annotated_text %>%
  filter(Partei == selected_party) %>%
  filter(upos %in% c("NOUN", "ADV", "ADJ")) %>%
  mutate(count = 1) %>%
  tidytext::cast_dfm(doc_id, lemma, count)

# Read in the source code for the co-occurrence calculation
source("src/calculateCoocStatistics.R")

# term to start the network
cooc_term <- "Klima"

significant_coocs <- get_cooc_significances(cooc_term, dtm, numberOfCoocs = 8)
significant_coocs[significant_coocs < 3.84] <- 0

matrix2fcm <- getFromNamespace("matrix2fcm", "quanteda")
plot_matrix <- matrix2fcm(significant_coocs)
quanteda.textplots::textplot_network(plot_matrix, min_freq = 0.0)

# validate by reading!
matched_tweets <- btw_tweets[btw_tweets$Partei == selected_party & grepl(cooc_term, btw_tweets$text), ]
View(matched_tweets)

# Topic models
# ==================================


library(topicmodels)
library(tidytext)

# First, we need to create a document-term-matrix as input for the LDA process
dtm <- annotated_text %>%
  filter(upos == "NOUN") %>%
  mutate(count = 1) %>%
  cast_dtm(doc_id, lemma, count)

dtm <- dtm[, col_sums(dtm) > 2]
dtm <- dtm[row_sums(dtm) > 0, ]

dim(dtm)  
  
# Compute the LDA model
topicmodel <- LDA(dtm, k = 10, method = "Gibbs", control = list(alpha = 0.05, iter = 500, seed = 1234, verbose = 1))

topicmodel

# Extract the topic-term-distributions (beta) and bring them into a tidy format
tweet_topics <- tidy(topicmodel, matrix = "beta")
tweet_topics

# Now extract the top 10 terms for each topic
top_terms <- tweet_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

# ... and plot them as bar plot.
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

