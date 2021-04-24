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
#install.packages("remotes")
#remotes::install_github("davidsjoberg/ggstream")
library(ggstream)

daily_show_guests <- read_csv("24-monochrome/daily_show_guests.csv") %>%
  janitor::clean_names()
head(daily_show_guests)
glimpse(daily_show_guests)

N <- 5
top_N_guest_groups <- daily_show_guests %>%
  count(group, sort=TRUE) %>%
  slice_max(order_by=n, n=N)
daily_show_guest_groups_timeseries <- daily_show_guests %>%
  group_by(year, group) %>%
  summarize(n=n()) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  mutate(rn = row_number()) %>%
  mutate(group = ifelse(rn > N | is.na(group), "Other", group)) %>%
  select(-rn) %>%
  group_by(year, group) %>%
  summarize(n = sum(n)) %>%
  complete(year, group, fill=list(n=0))

top_N_guest_groups

daily_show_guest_groups_timeseries <- daily_show_guests %>%
  mutate(group = ifelse(group %in% top_N_guest_groups$group, group, "Other")) %>%
  group_by(year, group) %>%
  summarize(n = n())


daily_show_guest_groups_timeseries


View(daily_show_guest_groups_timeseries)
daily_show_guest_groups_timeseries %>%
  ggplot(aes(x = year, y = n, fill = group)) +
  geom_stream() +
  geom_stream_label(aes(label = group)) +
  scale_fill_grey() +
  scale_color_grey() +
  theme_fivethirtyeight() +
  labs(title="Which professions did guests in \"The Daily Show\" come from?",
       subtitle="Not much change after 2005, Acting and Media remain the main source",
       caption="Source: https://www.kaggle.com/fivethirtyeight/fivethirtyeight | Graphic: Amit Arora") +
  theme(legend.title = element_blank(), axis.title = element_text()) +
  ylab("Number of guests") +
  xlab("")


