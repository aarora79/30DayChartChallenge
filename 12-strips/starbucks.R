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

# starbucks stores dataset, last updated 2017
sb_stores <- read_csv("https://raw.githubusercontent.com/chrismeller/StarbucksLocations/master/stores.csv")
sb_stores <- sb_stores %>%
  janitor::clean_names() 
  
head(sb_stores)

# list of country codes
country_codes <- read_csv("https://gist.githubusercontent.com/radcliff/f09c0f88344a7fcef373/raw/2753c482ad091c54b1822288ad2e4811c021d8ec/wikipedia-iso-country-codes.csv")
country_codes <- country_code %>%
  janitor::clean_names()
head(country_codes)

# replace the country code in Starbucks dataset with country name
sb_stores <- sb_stores %>%
  left_join(country_codes, by=c("country_code"="alpha_2_code")) %>%
  rename(country = english_short_name_lower_case)

stores_per_country <- sb_stores %>%
  count(country, sort=TRUE)
head(stores_per_country, 10)

stores_per_city <- sb_stores %>%
  count(country, city, sort=TRUE)

head(stores_per_city)

top_n <- 10
stores_per_city %>%
  mutate(country = fct_reorder(country, n, .fun=sum)) %>%
  arrange(desc(country)) %>%
  filter(country %in% stores_per_country$country[1:top_n]) %>%
  filter(n <= 100) %>%
  ggplot() +
  geom_point(aes(x = n, y  = country, col=country),
             size = 0.75, position = "jitter", alpha = 0.4) +
  scale_color_brewer(palette="Paired") +
  labs(title="Number of <span style='color:#00704A'>**Starbucks**</span> stores per city across different countries",
       subtitle = "Each point represents a city. Cities with more than 100 stores not included.",
       caption="Source: https://raw.githubusercontent.com/chrismeller/StarbucksLocations/master/stores.csv") +
  theme_fivethirtyeight() +
  theme(legend.position = "none", legend.title = element_blank(), plot.title = element_markdown()) +
  theme(axis.title = element_text()) +
  xlab("Stores per city") +
  ylab("")
  