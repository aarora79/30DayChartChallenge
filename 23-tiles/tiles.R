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
library(viridis)
library(gridExtra)

NO_TAG_PROVIDED <- "no-tag-provided"
so_tags <- read_csv("23-tiles/so_tags.csv") %>%
  replace_na(list(t=NO_TAG_PROVIDED)) %>%
  group_by(year) %>%
  mutate(pct = round((100*count)/sum(count), 5))

head(so_tags)
glimpse(so_tags)

#View(so_tags)

so_tags %>%
  #group_by(year) %>%
  summarize(total=sum(pct))
N <- 25
top_N_tags <- so_tags %>%
  group_by(t) %>%
  summarize(count=sum(count)) %>%
  slice_max(order_by=count, n=N)

top_N_tags
#View(top_N_tags)

top_N_tags_timeseries <- so_tags %>%
  filter(t %in% top_N_tags$t & t != NO_TAG_PROVIDED) %>%
  mutate(t = fct_reorder(t, pct, .fun=sum, na.rm=TRUE))


p <- top_N_tags_timeseries %>%
  ggplot(aes(year, t, fill = pct)) + 
  geom_tile(colour="gray20", size=1.5, stat="identity") + 
  geom_text(aes(label=glue("{round(pct, 2)}%"))) +
  scale_fill_viridis(option="C", name="%") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(title="Percentage share of tags in Stackoverflow Posts over the years",
       subtitle="Javacript is growing and hasn't peaked yet! Python is the same. c#'s best days are long gone.",
       caption="Source: bigquery-public-data.stackoverflow.stackoverflow_posts | Graphic: Amt Arora") +
  theme_fivethirtyeight() + 
  theme(legend.position = "none",
        text = element_text(size=15))
p


