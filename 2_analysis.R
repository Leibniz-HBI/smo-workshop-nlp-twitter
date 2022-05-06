options(stringsAsFactors = F)

# load required packages
library(dplyr)

# now load the preprocessed data
load("annotated_text.RData")
btw_tweets <- read.csv("data/btw-candidates_2021_tweets_dboes.csv", encoding = "UTF-8", colClasses = c("doc_id" = "character", "author_id" = "character"))

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

library(tidytext)
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
require(ggplot2)
ggplot(counts_per_day, aes(x = created_at, y = n, group = lemma, color = lemma)) +
  geom_line(size = 1) + theme(axis.text.x = element_text(angle = 90, hjust = 1))







# Cooccurrence analysis
# ================================================

# Statistical significance of co-ocurring terms can be determined by 
# functions provided in the widyr package
library(widyr)

# We extract nouns occurring > 10 times and compute their pearson's
# correlation of occurring together in tweets
word_cooccurrences <- annotated_text %>%
  filter(Partei == selected_party) %>%
  group_by(lemma) %>%
  filter(upos %in% c("NOUN", "ADV", "ADJ")) %>%
  filter(n() > 10) %>%
  pairwise_cor(lemma, doc_id, sort = TRUE, method = "pearson")

# What are the most significant co-occurrences?
word_cooccurrences

# What are the most significant co-occurrences for the term "change"?
word_cooccurrences %>%
  filter(item1 == "Klima")

# Let's display co-occurrence as a graph network
require(igraph)
require(ggraph)
word_cooccurrences %>%
  top_n(150, correlation) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


# Cooccurrence term networks
# ==================================

selected_party <- "SPD"

dtm <- annotated_text %>%
  filter(Partei == selected_party) %>%
  filter(upos %in% c("NOUN", "ADV", "ADJ")) %>%
  mutate(count = 1) %>%
  cast_dtm(doc_id, lemma, count)

# Read in the source code for the co-occurrence calculation
source("src/calculateCoocStatistics.R")
# Definition of a parameter for the representation of the co-occurrences of a concept
numberOfCoocs <- 15
# Determination of the term of which co-competitors are to be measured.
coocTerm <- "Klima"

coocs <- calculateCoocStatistics(coocTerm, dtm, measure="LOGLIK")
# Display the numberOfCoocs main terms
print(coocs[1:numberOfCoocs])


resultGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
# The structure of the temporary graph object is equal to that of the resultGraph
tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))

# Fill the data.frame to produce the correct number of lines
tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs]
# Entry of the search word into the first column in all lines
tmpGraph[, 1] <- coocTerm
# Entry of the co-occurrences into the second column of the respective line
tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
# Set the significances
tmpGraph[, 3] <- coocs[1:numberOfCoocs]

# Attach the triples to resultGraph
resultGraph <- rbind(resultGraph, tmpGraph)

# Iteration over the most significant numberOfCoocs co-occurrences of the search term
for (i in 1:numberOfCoocs){
  
  # Calling up the co-occurrence calculation for term i from the search words co-occurrences
  newCoocTerm <- names(coocs)[i]
  coocs2 <- calculateCoocStatistics(newCoocTerm, dtm, measure="LOGLIK")
  
  #print the co-occurrences
  coocs2[1:10]
  
  # Structure of the temporary graph object
  tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
  tmpGraph[, 1] <- newCoocTerm
  tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
  tmpGraph[, 3] <- coocs2[1:numberOfCoocs]
  
  #Append the result to the result graph
  resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
}
# Sample of some examples from resultGraph
resultGraph[sample(nrow(resultGraph), 6), ]

require(igraph)

# set seed for graph plot
set.seed(1)

# Create the graph object as undirected graph
graphNetwork <- graph.data.frame(resultGraph, directed = F)

# Identification of all nodes with less than 2 edges
verticesToRemove <- V(graphNetwork)[degree(graphNetwork) < 2]
# These edges are removed from the graph
graphNetwork <- delete.vertices(graphNetwork, verticesToRemove) 

# Assign colors to nodes (search term blue, others orange)
V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, 'cornflowerblue', 'orange') 

# Set edge colors
E(graphNetwork)$color <- adjustcolor("DarkGray", alpha.f = .5)
# scale significance between 1 and 10 for edge width
E(graphNetwork)$width <- scales::rescale(E(graphNetwork)$sig, to = c(1, 10))

# Set edges with radius
E(graphNetwork)$curved <- 0.15 
# Size the nodes by their degree of networking (scaled between 5 and 15)
V(graphNetwork)$size <- scales::rescale(log(degree(graphNetwork)), to = c(5, 15))

# Define the frame and spacing for the plot
par(mai=c(0,0,1,0)) 

# Final Plot
plot(
  graphNetwork,             
  layout = layout.fruchterman.reingold, # Force Directed Layout 
  main = paste(coocTerm, ' Graph'),
  vertex.label.family = "sans",
  vertex.label.cex = 0.8,
  vertex.shape = "circle",
  vertex.label.dist = 0.5,          # Labels of the nodes moved slightly
  vertex.frame.color = adjustcolor("darkgray", alpha.f = .5),
  vertex.label.color = 'black',     # Color of node names
  vertex.label.font = 2,            # Font of node names
  vertex.label = V(graphNetwork)$name,      # node names
  vertex.label.cex = 1 # font size of node names
)

matched_tweets <- btw_tweets[btw_tweets$Partei == selected_party & grepl(coocTerm, btw_tweets$text), ]
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
chapter_topics <- tidy(topicmodel, matrix = "beta")
chapter_topics

# Now extract the top 10 terms for each topic
top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

# ... and plot them as bar plot.
require(ggplot2)
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

