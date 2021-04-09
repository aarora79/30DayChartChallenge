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
library(tidyverse)

# dataset from https://www.kaggle.com/kumazaki98/dinosaur-list
dino <- read_csv("08-animals/dinosaur.csv") %>%
  janitor::clean_names()
head(dino)
print(glue("there are {nrow(dino)} dinosaurs in this dataset"))

N <- 6
dino_name_suffixes <- dino %>%
  mutate(suffix = str_sub(name, -N,-1))%>%
  group_by(suffix) %>%
  summarize(n = n()) %>%
  mutate(pct = round((100*n)/sum(n), 2)) %>%
  arrange(desc(n)) %>%
  head(20)

top_5_suffixes <- paste(dino_name_suffixes %>% 
  head(N) %>%
  pull(suffix), collapse=", ")
dino_name_suffixes %>%
  ggplot(aes(x=reorder(suffix, -pct), y=pct, col=pct, fill=pct)) +
  geom_bar(stat="identity") + 
  coord_flip() +
  labs(title=glue("Dinosaur name suffixes"),
       subtitle = glue("Pick your favorite: {top_5_suffixes}...")) +
  theme_fivethirtyeight() +
  theme(legend.position = "none")

TOP_N <- 10
top_n_suffixes <- dino %>%
  mutate(suffix = str_sub(name,-N,-1)) %>%
  count(suffix, sort=TRUE) %>%
  top_n(TOP_N)
top_n_suffixes

dino %>%
  mutate(suffix = str_sub(name,-N,-1)) %>%
  filter(suffix %in% top_n_suffixes$suffix) %>%
  group_by(suffix, diet) %>%
  summarize(n = n()) %>%
  mutate(suffix = factor(suffix, levels=top_n_suffixes$suffix)) %>%
  arrange(desc(suffix)) %>%
  ggplot(aes(x=reorder(diet, -n), y=n, col=diet, fill=diet)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~suffix, scales="free") + 
  scale_y_continuous(breaks = pretty_breaks()) + 
  coord_flip() +
  scale_color_tableau() + 
  scale_fill_tableau() +
  labs(title="Does dinosaur name end with a suffix giving a clue about their diet?",
       subtitle = glue("Seems like the <span style='color:#0000FF'>**\"saurus\"es**</span>, the <span style='color:#00DDAF'>**\"raptor\"s**</span> and the <span style='color:#0FAF00'>**\"pteryx\"es**</span> had the most diverse diet!")) +
  theme_fivethirtyeight() +
  theme(legend.position = "none", plot.subtitle = element_markdown()) +
  theme(axis.title = element_text()) +
  ylab('Number of Dinosaur Species') +
  xlab("")

