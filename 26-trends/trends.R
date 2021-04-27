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

life_expectancy_vs_gdp_per_capita <- read_csv("26-trends/life-expectancy-vs-gdp-per-capita.csv") %>%
  janitor::clean_names()
head(life_expectancy_vs_gdp_per_capita)
glimpse(life_expectancy_vs_gdp_per_capita)
#View(life_expectancy_vs_gdp_per_capita)

country_continent_mapping <- life_expectancy_vs_gdp_per_capita %>%
  filter(!is.na(continent)) %>%
  filter(entity != continent) %>%
  count(entity, continent) %>%
  select(-n)


YEAR_OF_INTREST <- 2018
life_expectancy_vs_gdp_per_capita %>%
  filter(year == YEAR_OF_INTREST) %>%
  select(-continent) %>%
  left_join(country_continent_mapping, by="entity") %>%
  filter(!is.na(continent)) %>%
  ggplot(aes(x=gdp_per_capita, y=life_expectancy)) +
  geom_point(aes(col=continent, size=total_population_gapminder_hyde_un)) + 
  geom_text(aes(label=ifelse(total_population_gapminder_hyde_un>50000000, entity,''), size=total_population_gapminder_hyde_un), hjust=0, vjust=0) +
  geom_smooth(method = "gam", show.legend = FALSE) +
  guides(size=FALSE) +
  scale_color_tableau() +
  #scale_color_discrete("Set3") +
  scale_y_continuous(breaks=seq(10, 100, 5)) +
  scale_x_continuous(breaks=seq(0, max(life_expectancy_vs_gdp_per_capita$gdp_per_capita, na.rm=TRUE), 20000), 
                     labels=scales::dollar_format()) +
  theme_fivethirtyeight() +
  labs(title="Life Expectancy Vs GDP per capita, 2018",
       subtitle="Trend shows a steep increase in life expectancy with GDP upto $40,000, tapers off afterwards. Lots of uncertainity in the relationship after $70,000.",
       caption="Source: https://ourworldindata.org/grapher/life-expectancy-vs-gdp-per-capita | Graphic: Amit Arora") +
  theme(legend.title = element_blank(), axis.title = element_text()) +
  ylab("Life Expectancy (years)") +
  xlab("GDP per capita (measured in 2011 international dollars)")
