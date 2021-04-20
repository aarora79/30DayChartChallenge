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

internet_usage <- read_csv("19-globalchange/API_IT.NET.USER.ZS_DS2_en_csv_v2_2164146.csv", skip=4) %>%
  janitor::clean_names()

head(internet_usage)
glimpse(internet_usage)


internet_usage_tidy <- internet_usage %>%
  select(-indicator_code, -indicator_name) %>%
  gather(year, value, -country_name, -country_code) %>%
  mutate(year = as.integer(str_replace(year, "x", "")))

head(internet_usage_tidy)
glimpse(internet_usage_tidy)


most_populated_countries <- c("China", "India", "United States", "Indonesia", 
                              "Pakistan", "Brazil", "Nigeria", "Bangladesh",
                              "Russia", "Mexico", "World")

tableau_colors <- c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2", "#59a14f",
                    "#edc948", "#b07aa1", "#ff9da7", "#9c755f", "#bab0ac", "black")

names(tableau_colors) <- most_populated_countries

internet_usage_tidy_top10 <- internet_usage_tidy %>%
  filter(country_code=="WLD" | country_name %in% most_populated_countries) %>%
  drop_na()

year_of_interest <- 2017
pretty_br <- c(seq(min(internet_usage_tidy_top10$year), max(internet_usage_tidy_top10$year)+1, 5), year_of_interest)

#View(internet_usage_tidy_top10)
internet_usage_tidy_top10 %>%
  group_by(country_name) %>%
  mutate(label = if_else(year == max(year), as.character(country_name), NA_character_)) %>%
  ungroup() %>%
  ggplot(aes(x=year, y=value, col=country_name)) +
  geom_line() +
  geom_point() +
  geom_label_repel(aes(label = label),
                   nudge_x = .5,
                   na.rm = TRUE, size=2.5) +
  geom_vline(xintercept = 2017, linetype="dotted") +
  scale_x_continuous(breaks = pretty_br) +
  scale_colour_manual(name="country_name", values = tableau_colors) +
  labs(title="Internet usage growth in the top 10 most populous countries of the world",
       subtitle="Brazil & China have remarkable growth, things seemed to have slowed down in the Indian subcontinent after 2017!",
       caption="Source: https://data.worldbank.org/indicator/IT.NET.USER.ZS | Graphic: Amit Arora") +
  theme_fivethirtyeight() +
  theme(legend.position = "none") +
  theme(axis.title = element_text()) +
  ylab("Individuals using the Internet (% of population)") +
  xlab("Year") +
  theme(legend.position = "none") 

