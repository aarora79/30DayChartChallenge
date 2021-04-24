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

aaqol <- read_csv("24-monochrome/Final_Report_of_the_Asian_American_Quality_of_Life__AAQoL_.csv") %>%
  janitor::clean_names()
head(aaqol)
glimpse(aaqol)


aaqol_english_speaking <- aaqol %>%
  filter(!is.na(ethnicity)) %>%
  mutate(english_speaking = factor(english_speaking, c("Did not answer", "Not at all", "Not well", "Well", "Very well"))) %>%
  count(ethnicity, english_speaking) %>%
  replace_na(list(english_speaking="Did not answer")) %>%
  group_by(ethnicity) %>%
  mutate(pct = (100*n)/sum(n)) %>%
  complete(ethnicity, english_speaking, fill=list(pct=0, n=0))
aaqol_english_speaking

aaqol_english_speaking %>%
  ggplot(aes(x=ethnicity, y=n, col=english_speaking, fill=english_speaking, group=english_speaking)) +
  geom_bar(stat='identity', position='dodge') +
  geom_text(aes(label=glue("{round(pct, 1)}%")), position=position_dodge(width=0.9), vjust=-0.25, color=I("black")) +
  scale_fill_grey() +
  scale_color_grey() +
  theme_fivethirtyeight() +
  labs(title="How well do you speak English?",
       subtitle="Survey of people of various Asian American ethnicites in Austin, TX",
       caption="Source: https://data.austintexas.gov/City-Government/Final-Report-of-the-Asian-American-Quality-of-Life/hc5t-p62z | Graphic: Amit Arora") +
  theme(legend.title = element_blank(), axis.title = element_text()) +
  ylab("Number of participants") +
  xlab("")
