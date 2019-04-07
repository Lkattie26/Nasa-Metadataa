#title: Text Analysis in R: Metadata Analysis
#Authors: Louis Kattie
#Purpose: To conduct a text analysis on the Nasa dataset description to uncover...

#set my working directory
setwd("~/Desktop")

#Install Packages
install.packages("readtext")    # data preparation
install.packages("stringi")     # data preparation
install.packages("quanteda")    # data preparation and analysis
install.packages("topicmodels") # analysis
install.packages("spacyr")      # advanced topics
install.packages("corpustools") # advanced topics
install.packages("dplyr")
install.packages("tidyr")
install.packages("widyr")
install.packages("ggraph")
install.packages("ggplot2")
install.packages("farver")

#download Libraries
library(readtext)
library(stringi)
library(quanteda)
library(topicmodels)
library(spacyr)
library(corpustools)
library(jsonlite)
library(tibble)
library(dplyr)
library(tidyr)
library(tidytext)
library(widyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(farver)

#Data Preparation
metadata <- fromJSON("https://data.nasa.gov/data.json")
names(metadata$dataset)
class(metadata$dataset$title)
class(metadata$dataset$description)
class(metadata$dataset$keyword)

#set up separate tidy data frames for title, description, and keyword
nasa_title <- tibble(id = metadata$dataset$identifier, title = metadata$dataset$title)
nasa_title
write.csv(nasa_title,'nasa_title.csv')

nasa_desc <- tibble(id = metadata$dataset$identifier, desc = metadata$dataset$description)
nasa_desc
write.csv(nasa_desc,'nasa_desc.csv')

nasa_publisher <- metadata$dataset%>%
  select(id = identifier, publisher = publisher)
nasa_publisher

#Dataframe for the keywords  
nasa_keyword <- tibble(id = metadata$dataset$identifier, keyword = metadata$dataset$keyword) %>% 
  
  unnest(keyword)

nasa_keyword
write.csv(nasa_keyword,'nasa_keyword.csv')

#remove stop words from the titles and descriptions
nasa_title <- nasa_title %>% 
  unnest_tokens(word, title) %>% 
  anti_join(stop_words)

nasa_title

nasa_desc <- nasa_desc %>% 
  unnest_tokens(word, desc) %>% 
  anti_join(stop_words)

nasa_desc

#exploratory analysis
#most common words in the NASA dataset titles
nasa_title %>%
  count(word, sort = TRUE)

#most common words in the NASA dataset descriptions
nasa_desc %>% 
  count(word, sort = TRUE)

#remove digits and some “words” like “v1.0” using anti_join()
my_stopwords <- tibble(word = c(as.character(1:10), 
                                "v1.0", "v03", "l2", "l3", "l4", "v5.2.0", 
                                "v003", "v004", "v005", "v006", "v7", "ii", "v001", "0.5", "1", "2"))
nasa_title <- nasa_title %>% 
  anti_join(my_stopwords)
nasa_desc <- nasa_desc %>% 
  anti_join(my_stopwords)

#most common keywords in the NASA dataset
nasa_keyword %>% 
  group_by(keyword) %>% 
  count(sort = TRUE)

#change all of the keywords to either lower case to get rid of duplicates
nasa_keyword <- nasa_keyword %>% 
  mutate(keyword = toupper(keyword))

#examine which words commonly occur together in the titles, descriptions, and keywords of NASA datasets
title_word_pairs <- nasa_title %>% 
  pairwise_count(word, id, sort = TRUE, upper = FALSE)

title_word_pairs 

desc_word_pairs <- nasa_desc %>% 
  pairwise_count(word, id, sort = TRUE, upper = FALSE)

desc_word_pairs

#plot networks of these co-occurring words in the titles
set.seed(1234)
title_word_pairs %>%
  filter(n >= 250) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()

#plot networks of these co-occurring words in the description
set.seed(1234)
desc_word_pairs %>%
  filter(n >= 1750) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "darkred") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()


keyword_pairs <- nasa_keyword %>% 
  pairwise_count(keyword, id, sort = TRUE, upper = FALSE)

keyword_pairs

#the most commonly co-occurring words, but also just the most common keywords in general.
set.seed(1234)
keyword_pairs %>%
  filter(n >= 700) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

#the relationships among keywords 
keyword_cors <- nasa_keyword %>% 
  group_by(keyword) %>%
  filter(n() >= 50) %>%
  pairwise_cor(keyword, id, sort = TRUE, upper = FALSE)

keyword_cors

#the network of keyword correlations
set.seed(1234)
keyword_cors %>%
  filter(correlation > .8) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

#Calculating tf-idf(term frequency times inverse document frequency) for the description fields
#to identify words that are especially important to the document within a collection of documents.
desc_tf_idf <- nasa_desc %>% 
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, id, n)

#the highest tf-idf words in the NASA description fields
desc_tf_idf %>% 
  arrange(-tf_idf)

#full join of the keyword data frame and the data frame of description words with tf-idf
desc_tf_idf <- full_join(desc_tf_idf, nasa_keyword, by = "id")

#plot some of the most important words, as measured by tf-idf
desc_tf_idf %>% 
  filter(!near(tf, 1)) %>%
  filter(keyword %in% c("SOILS", "EARTH SCIENCE", "WEATHER", "CLOUDS",
                        "ATMOSPHERE", "LAND SURFACE", "SOLAR ACTIVITY",
                        "AIR QUALITY", "SEISMOLOGY")) %>%
  arrange(desc(tf_idf)) %>%
  group_by(keyword) %>%
  distinct(word, keyword, .keep_all = TRUE) %>%
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ggplot(aes(word, tf_idf, fill = keyword)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~keyword, ncol = 3, scales = "free") +
  coord_flip() +
  labs(title = "Highest tf-idf words in NASA metadata description fields",
       caption = "NASA metadata from https://data.nasa.gov/data.json",
       x = NULL, y = "tf-idf")

# Casting to a document-term matrix
my_stop_words <- bind_rows(stop_words, 
                           tibble(word = c("nbsp", "amp", "gt", "lt",
                                           "timesnewromanpsmt", "font",
                                           "td", "li", "br", "tr", "quot",
                                           "st", "img", "src", "strong",
                                           "http", "file", "files",
                                           as.character(1:12)), 
                                  lexicon = rep("custom", 30)))

word_counts <- nasa_desc %>%
  anti_join(my_stop_words) %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

word_counts

# make a DocumentTermMatrix
desc_dtm <- word_counts %>%
  cast_dtm(id, word, n)

desc_dtm

# be aware that running this model is time intensive
desc_lda <- LDA(desc_dtm, k = 24, control = list(seed = 1234))
desc_lda

#construct a tidy data frame that summarizes the results of the model.
tidy_lda <- tidy(desc_lda)
tidy_lda

#examine the top 10 terms for each topic.
top_terms <- tidy_lda %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

#plot the top 10 terms for each topic.
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"), 
                       levels = rev(paste(term, topic, sep = "__")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top 10 terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 4, scales = "free")

#the probability that each document belongs in each topic
lda_gamma <- tidy(desc_lda, matrix = "gamma")
lda_gamma

#visualize the probability
ggplot(lda_gamma, aes(gamma)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       y = "Number of documents", x = expression(gamma))

#how the probabilities are distributed within each topic
ggplot(lda_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))

#connect these topic models with the keywords and see what relationships
lda_gamma <- full_join(lda_gamma, nasa_keyword, by = c("document" = "id"))
lda_gamma

#the document-topic entries that have probabilities greater than...
top_keywords <- lda_gamma %>% 
  filter(gamma > 0.9) %>% 
  count(topic, keyword, sort = TRUE)

top_keywords

#the top keywords for each topic
top_keywords %>%
  group_by(topic) %>%
  top_n(5, n) %>%
  group_by(topic, keyword) %>%
  arrange(desc(n)) %>%  
  ungroup() %>%
  mutate(keyword = factor(paste(keyword, topic, sep = "__"), 
                          levels = rev(paste(keyword, topic, sep = "__")))) %>%
  ggplot(aes(keyword, n, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(title = "Top keywords for each LDA topic",
       x = NULL, y = "Number of documents") +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap(~ topic, ncol = 4, scales = "free")
