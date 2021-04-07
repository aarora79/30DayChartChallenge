library(glue)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(lubridate)
library(tidyverse)

# read data
DATA_URL <- "https://raw.githubusercontent.com/aarora79/biomettracker/master/raw_data/Amit_deadlifts.csv"
deadlifts <- read_csv(DATA_URL) %>%
  rename(exercise = excercise)
head(deadlifts)

# for now we are considering just the deadlift but the code
# also handles deadlift & carry, will add more groups later (when?)
deadlifts <- deadlifts %>%
  filter(exercise == "deadlift") %>%
  group_by(exercise) %>%
  summarize(max_weight = max(weight)) %>%
  mutate(starting_up = case_when(
    exercise == "deadlift" ~ 135,
    exercise == "deadlift & carry" ~ 100,
    TRUE ~ 0)) %>%
  mutate(this_aint_light = case_when(
    exercise == "deadlift" ~ 185,
    exercise == "deadlift & carry" ~ 155,
    TRUE ~ 0)) %>%
  mutate(this_is_serious = case_when(
    exercise == "deadlift" ~ 285,
    exercise == "deadlift & carry" ~ 195,
    TRUE ~ 0)) %>%
  mutate(did_i_really_lift_that = case_when(
    exercise == "deadlift" ~ 315,
    exercise == "deadlift & carry" ~ 260,
    TRUE ~ 0)) %>%
  mutate(my_target = case_when(
    exercise == "deadlift" ~ 400,
    exercise == "deadlift & carry" ~ 295,
    TRUE ~ 0)) %>% 
  mutate(ok_that_is_beast_mode = case_when(
      exercise == "deadlift" ~ 420,
      exercise == "deadlift & carry" ~ 315,
      TRUE ~ 0)) %>%
  mutate(exercise = stringr::str_to_title(exercise))

deadlifts

# here is how this chart is made, execute individual steps one by one to see
# the results of the intermediate steps...
p <- deadlifts %>%
  ggplot() +
  geom_col(
    aes(x = exercise, y = ok_that_is_beast_mode),
    fill = "#A9A9A9",
    width = 0.6,
    alpha = 0.9
  ) +
  geom_col(
    aes(x = exercise, y = this_is_serious),
    fill = "#808080",
    width = 0.6,
    alpha = 0.9
  ) +
  geom_col(
    aes(x = exercise, y = starting_up),
    fill = "#696969",
    width = 0.6,
    alpha = 0.9
  ) +
  geom_col(
    aes(x = exercise, y = did_i_really_lift_that),
    fill = "black",
    color = NA,
    width = 0.2
  ) +
  geom_errorbar(
    aes(x = exercise, ymin = my_target, ymax = my_target),
    color = "red",
    width = 0.45,
    size = 2
  ) +
  coord_flip() +
  theme_fivethirtyeight() +
  labs(
    x = "",
    y = "\nWeight lifted (lb)",
    title = "How far along am I on my <span style='color:#0000FF'>**deadlift**</span> journey?",
    subtitle = "Made my own milestones and set my own targets."
  ) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_markdown(),
    axis.text = element_text(face = "bold"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  NULL
p + 
  annotate(
    "text",
    x = c(rep((1.35), 4)),
    y = c(
      85,
      215,
      335,
      400
    ),
    label = c("Starting up...", "This is serious!", "Wait, did I really lift that?", "The Target."),
    color = c(rep("black", 3), "red")
  ) +
  scale_y_continuous(breaks = seq(0, 450, 50)) +
  NULL
