# SMO Starter Academy: Twitter
# -----------------------------------------------------
# Workshop: Intro to NLP
# Author: Gregor Wiedemann, g.wiedemann@leibniz-hbi.de
#
# This script preprocesses and analyzes a corpus of Tweets from candidates of
# the larger German parties for the election to the German Bundestag.
#
# We strive to answer the following questions:
# - Which topics are most prominent for each party?
# - How do central keyterms for each party evolve throughout the campaign?
# - How does climate framing differ between parties?
# =====================================================

# set option to not convert text into R factors
options(stringsAsFactors = F)

# load required packages via pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr)
pacman::p_load(udpipe)
pacman::p_load(ggwordcloud)

# LOADING DATA
# ============

# load prepared Twitter data from zipped CSV
zip_connection <- unz("data/btw-candidates_2021_tweets_dboes.zip", "btw-candidates_2021_tweets_dboes.csv")
btw_tweets <- read.csv(zip_connection, encoding = "UTF-8", colClasses = c("doc_id" = "character", "author_id" = "character"))

# if this code runs on RStudio Cloud, we reduce the data to every 3rd item to fit into RAM
if (getwd() == "/cloud/project") {
  btw_tweets <- btw_tweets[rep(c(T, F, F), length.out = nrow(btw_tweets)), ]
}

# convert to tibble
# all data:
text_df <- tibble(btw_tweets[, c("doc_id", "text")])

# LINGUISTIC PREPROCESSING
# ========================
#
# we use a statistical model for German text trained on the Hamburg Dependency
# Treebank provided by the UDPipe NLP toolkit
 
# remove # and @
text_df$text <- gsub("#", "", text_df$text)
text_df$text <- gsub("@", "", text_df$text)

# download german udpipe model 
udpipe_download_model("german-hdt")

# load model
udmodel_german <- udpipe_load_model(file = "german-hdt-ud-2.5-191206.udpipe")

# annotate tweets
annotated_text <- udpipe_annotate(
  udmodel_german, 
  x = text_df$text,
  doc_id = text_df$doc_id,
  parser = "none",
  trace = 1000
)
annotated_text <- as.data.frame(annotated_text)

# free some memory
rm(text_df, udmodel_german)
gc()

# View the annotated tidy text format
View(annotated_text)

# join with metadata
annotated_text <- annotated_text %>%
  left_join(btw_tweets[, -which(colnames(btw_tweets) == "text")], by = "doc_id")
View(annotated_text)

# save for later use
save(annotated_text, btw_tweets, file = "annotated_text.RData")


# BASIC INSPECTION OF PREPROCESSING RESULTS
# =========================================

# frequency counts
freq <- annotated_text %>%
  group_by(lemma) %>%
  count(lemma, sort = TRUE)
View(freq)
# ... punctuation marks, and too many stopwords


# count only nouns
nouns <- annotated_text %>%
  filter(upos == "NOUN") %>%
  group_by(lemma) %>%
  count(lemma, sort = TRUE)
View(nouns)
# ... wait, why 'unknown' dominates the list?


# correct 'unknown' lemmas
lidx <- annotated_text$lemma == 'unknown'
lidx[is.na(lidx)] <- F
annotated_text$lemma[lidx] <- annotated_text$token[lidx]
# count nouns again
nouns <- annotated_text %>%
  filter(upos == "NOUN") %>%
  group_by(lemma) %>%
  count(lemma, sort = TRUE)
View(nouns)


# count named entities
proper_nouns <- annotated_text %>%
  filter(upos == "PROPN") %>%
  group_by(lemma) %>%
  count(lemma, sort = TRUE)
View(proper_nouns)


# by joining a metadata table to the token table, we can obtain counts grouped by party
annotated_text %>%
  filter(upos == "NOUN") %>%
  group_by(Partei) %>%
  count(lemma) %>%
  top_n(15) %>%
  arrange(Partei, desc(n)) -> nouns_per_party
View(nouns_per_party)


# plot as word cloud
ggplot(nouns_per_party, aes(label = lemma, size = n, color = Partei)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  facet_wrap(~Partei) +
  theme_minimal()

