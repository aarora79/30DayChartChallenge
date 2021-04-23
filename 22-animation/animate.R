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
library(gganimate)
library(gifski)

springer <- read_csv("22-animation/springer.csv") %>%
  janitor::clean_names()
glimpse(springer)
head(springer)


yearly <- springer %>%
  filter(year(date) != year(max(springer$date))) %>% 
  mutate(year=floor_date(date, unit = "year")) %>%
  group_by(year, type) %>%
  summarize(count=sum(count)) 

head(yearly)
p <- yearly %>%
  ggplot(aes(x=year, y=count, color=type)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label=type, group=type)) +
  scale_x_date(date_breaks="5 years", date_labels="%Y") +
  scale_color_tableau() +
  theme_fivethirtyeight() +
  labs(title="Books and journals articles published by Springer",
       subtitle="Not unexpectedly the number of articles far exceed number of books",
       caption="Source: bigquery-public-data:breathe.springer | Graphic: Amit Arora") +
  theme(axis.title = element_text()) +
  ylab("Number of publications") +
  xlab("Year") +
  theme(legend.position = "none") 
  
p

anim <- p + 
  transition_reveal(year) +
  ease_aes('linear')

animate(anim, duration = 15, end_pause = 10, fps = 20, width = 900, height = 500, renderer = gifski_renderer())
anim_save(file.path("22-animation", "springer.gif"))
# delete all the intermediate files
unlink("gganim_plot*")

