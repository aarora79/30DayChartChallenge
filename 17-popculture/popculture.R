library(glue)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(ggpmisc)
library(forcats)
library(ggthemes)
library(janitor)
library(lubridate)
library(tidytext)
library(tidyverse)
library(ggwordcloud)
library(patchwork)

netflix_titles <- read_csv("17-popculture/netflix_titles.csv")
glimpse(netflix_titles)
head(netflix_titles)

netflix_tv_shows_count <- netflix_titles %>%
  filter(type == "TV Show" & !is.na(country)) %>%
  count(country, sort=TRUE) 


description_for_tv_shows <- netflix_titles %>%
  select(country, description)

description_for_tv_shows

data(stop_words)
description_tidy <- description_for_tv_shows %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)

description_words_with_counts <- description_tidy %>%
  count(country, word)

set.seed(42)
WORD_COUNT_THRESHOLD <- 10
p1 <- description_words_with_counts %>%
  filter(country == "India") %>%
  filter(n >= WORD_COUNT_THRESHOLD) %>%
  ggplot(aes(label = word, size=n)) +
  geom_text_wordcloud_area( color=I("#F28E2B")) +
  scale_size_area(max_size = 20) +
  theme_fivethirtyeight() +
  ggtitle(glue("India, shows={netflix_tv_shows_count %>% filter(country == 'India') %>% pull(n)}")) +
  theme(plot.title = element_text(colour = "#F28E2B"))
p1

# United States
p2 <- description_words_with_counts %>%
  filter(country == "United States") %>%
  filter(n >= WORD_COUNT_THRESHOLD) %>%
  ggplot(aes(label = word, size=n)) +
  geom_text_wordcloud_area(color="#4E79A7") +
  scale_size_area(max_size = 20) +
  theme_fivethirtyeight() + 
  ggtitle(glue("United States, shows={netflix_tv_shows_count %>% filter(country == 'United States') %>% pull(n)}")) +
  theme(plot.title = element_text(colour = "#4E79A7"))
p2

p1+p2 + plot_annotation(
  title = 'Comparing wordclouds from description of Netflix TV Shows in India and the United States',
  subtitle = glue('Only including words that occured at least {WORD_COUNT_THRESHOLD} times'),
  caption = 'Pop Culture | Source: https://www.kaggle.com/shivamb/netflix-shows'
)
