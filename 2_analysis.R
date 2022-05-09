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

# Let's just treat the CSU as a very special CDU ;)
btw_tweets$Partei[btw_tweets$Partei == "CSU"] <- "CDU"

# Key terms
# =========

# We extract important vocabulary for each party based on log-likelihood statistics
# For this, we load a prepared function
source("src/calculateLogLikelihood.R")

# We set a variable to select tweets from a single party
selected_party <- "GRÜNE"

# We create a document-term matrix containing only nouns, adverbs and adjectives
dtm <- annotated_text %>%
  filter(upos %in% c("NOUN", "ADV", "ADJ")) %>%
  mutate(count = 1) %>%
  cast_dtm(doc_id, lemma, count)

# We create a selection vector that ensures the same order for the party 
# metadata as in the just created dtm
all_parties <- btw_tweets$Partei
names(all_parties) <- btw_tweets$doc_id
all_parties <- all_parties[rownames(dtm)]

# Then, we count all terms in one party and only keep those occurring > 2
word_counts_target <- col_sums(dtm[all_parties == selected_party, ])
word_counts_target <- word_counts_target[word_counts_target > 2]

# For comparison, we count all terms in all other parties
word_counts_comparison <- col_sums(dtm[all_parties != selected_party, ])
word_counts_comparison <- word_counts_comparison[word_counts_comparison > 2]

# We compute the ll-significance of the vocabulary of the selected party
llSignificance <- calculateLogLikelihood(word_counts_target, word_counts_comparison)

# We extract the top 25 terms most distinct for the party
top25 <- sort(llSignificance, decreasing = T)[1:25]
top25_df <- data.frame(
  word = names(top25),
  frq = top25
)
# and plot them as a word cloud
ggwordcloud2(top25_df)

# Read examples for tweets of that party containing certain keywords
keyterm <- "Scheuer"
View(btw_tweets[btw_tweets$Partei == selected_party & grepl(keyterm, btw_tweets$text, fixed = T), ])

# Finally, we put word clouds for all parties in one plot
all_keyterms <- NULL
for (selected_party in unique(all_parties)) {
  word_counts_target <- col_sums(dtm[all_parties == selected_party, ])
  word_counts_target <- word_counts_target[word_counts_target > 2]
  word_counts_comparison <- col_sums(dtm[all_parties != selected_party, ])
  word_counts_comparison <- word_counts_comparison[word_counts_comparison > 2]
  llSignificance <- calculateLogLikelihood(word_counts_target, word_counts_comparison)
  top25 <- sort(llSignificance, decreasing = T)[1:25]
  top25_df <- data.frame(
    party = selected_party,
    word = names(top25),
    ll = top25
  )
  all_keyterms <- rbind(all_keyterms, top25_df)
}

# Here's the code for plotting word clouds as a faceted plot
ggplot(all_keyterms, aes(label = word, size = ll, color = party)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 26) +
  facet_wrap(~party) +
  theme_minimal()


# Frequency over time
# ===================

# We choose some key terms for a time series plot
selected_terms <- c("Impfplicht", "Sicherheit", "sozial", "Freiheit", "Klimaschutz", "Mindestlohn")
# What about "Digitalisierung", "Mietendeckel", "sozial", ...

# This block counts our selected terms per decade by combining metadata
counts_per_day <- annotated_text %>%
  filter(lemma %in% selected_terms) %>%
  group_by(created_at) %>%
  count(lemma)

# We plot the time series
ggplot(counts_per_day, aes(x = created_at, y = n, group = lemma, color = lemma)) +
  geom_line(size = 1) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Open question: did other parties take over successful keyterms of a individual party?


# Cooccurrence term networks
# ==========================

# Again, we start with a single party
selected_party <- "AfD"

# compute a document-term matrix (this time: document-feature matrix of the quanteda package!)
dtm <- annotated_text %>%
  filter(Partei == selected_party) %>%
  filter(upos %in% c("NOUN", "ADV", "ADJ")) %>%
  mutate(count = 1) %>%
  tidytext::cast_dfm(doc_id, lemma, count)

# Read in the source code for the co-occurrence calculation
source("src/calculateCoocStatistics.R")

# Choose a term to start the network
cooc_term <- "Klima"

# Use a prepared function to compute the LL significance and get the n cooccurring terms
significant_coocs <- get_cooc_significances(cooc_term, dtm, numberOfCoocs = 8, min_count = 3)
significant_coocs[significant_coocs < 3.84] <- 0

matrix2fcm <- getFromNamespace("matrix2fcm", "quanteda")
plot_matrix <- matrix2fcm(significant_coocs)
quanteda.textplots::textplot_network(plot_matrix, min_freq = 0.0)

# validate by reading!
matched_tweets <- btw_tweets[btw_tweets$Partei == selected_party & grepl(cooc_term, btw_tweets$text), ]
View(matched_tweets)


# Topic modeling
# ==================================

library(topicmodels)
library(tidytext)

# First, we need to create a document-term-matrix as input for the LDA process
dtm <- annotated_text %>%
  filter(upos == "NOUN") %>%
  mutate(count = 1) %>%
  cast_dtm(doc_id, lemma, count)

# we remove words occurring less than twice
dtm <- dtm[, col_sums(dtm) > 2]
# we remove empty documents
dtm <- dtm[row_sums(dtm) > 0, ]

# what are the resulting dimensions?
dim(dtm)  
  
# Compute the LDA model
topicmodel <- LDA(dtm, k = 10, method = "Gibbs", control = list(alpha = 0.05, iter = 500, seed = 1234, verbose = 1))

# inspect
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

# look at the top terms
top_terms

# ... and plot them as bar plot.
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

