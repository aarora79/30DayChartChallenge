library(glue)
library(tidyr)
library(httr)
library(ggraph)
library(igraph)
library(ggplot2)
library(jsonlite)
library(tidyverse)

SHOW_NAME <- "just-add-magic"
# API_END_POINT <- glue("http://api.tvmaze.com/singlesearch/shows?q={SHOW_NAME}&embed=episodes")

resp <- httr::GET(API_END_POINT)
print(glue("got a response for API={API_END_POINT}, statuscode={resp$status_code}"))

# Convert to df 
resp_json <- jsonlite::fromJSON(rawToChar(resp$content))
df <- resp_json$`_embedded`$episodes
head(df)

# clean the data to remove <p> and </p> (from HTML formatting of original text)
summary <- df %>% 
  mutate(summary = str_replace_all(summary, "<p>|</p>", "")) %>%
  select(season, summary) %>%
  mutate(season = glue("season {season}"))


# remove stop words by doing an "anti join" i.e. remove the words that match
data(stop_words)
summary_tidy <- summary %>%
  unnest_tokens(word, summary) %>%
  anti_join(stop_words)

# generate the cleaned summary column
summary_clean <- summary_tidy %>%
  group_by(season) %>%
  summarize(text=paste(word, collapse = " "))

# bigrams in the summary
summary_bigrams <- summary_clean %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# per season bigram counts
summary_bigrams_counts <- summary_bigrams %>%
  count(season, bigram, sort = TRUE) %>%
  mutate(word1 = str_split(bigram, " ", simplify=TRUE)[,1],
         word2 = str_split(bigram, " ", simplify=TRUE)[,2]) 
summary_bigrams_counts

# find tf-idf for bigrams to figure out which ones are important
summary_bigrams_tf_idf <- summary_bigrams %>%
  count(season, bigram) %>%
  bind_tf_idf(bigram, season, n) %>%
  arrange(desc(tf_idf))

summary_bigrams_tf_idf
summary_bigrams_tf_idf %>%
  group_by(season) %>%
  slice_max(order_by = tf_idf, n=5) %>%
  ggplot(aes(x=reorder(bigram, -tf_idf), y=tf_idf, col=season, fill=season)) +
  geom_bar(stat='identity') +
  facet_wrap(~season, scales="free") +
  coord_flip()

# network graph of the bigrams, exclude bigrams which only occur once
bigram_graph <- summary_bigrams_counts %>%
  ungroup() %>%
  filter(n > 1) %>%
  graph_from_data_frame()

# plot the network graph
set.seed(2020)
title <- glue("Just Add Magic season {min(df$season)} through {max(df$season)} visualized from episode summaries")
subtitle <- "Season 2 and 3 had a lot in common with Kelly, Hannah and Darbie all cooking spells. Season 1 and 3 just have the town \"Saffron Falls\" in common."
caption <- glue("Source: {API_END_POINT}")
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_fivethirtyeight() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.line=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank()) +
  labs(title=title,
       subtitle=subtitle,
       caption=caption)

# wordcloud. We have to use this hack to put a title there because wordcloud
# does not provide a functionality to provide a title
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "What is \"Just Add Magic\" all about?\"\nHannah, Kelley and Darbie cooking spells from grandma's cookbook")

word_freq <- summary_tidy %>%
  count(word) 
wordcloud(words = word_freq$word, freq = word_freq$n, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Set2")) + labs(title="this")

