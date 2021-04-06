library(glue)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(lubridate)
library(tidyverse)

# read data
DATA_URL <- "https://raw.githubusercontent.com/aarora79/biomettracker/master/data/Amit.csv"
daily_measurements <- read_csv(DATA_URL)
head(daily_measurements)

# we want to draw a slope chart inspired from: https://ibecav.github.io/slopegraph/, 
# please see it for more details

# we want to visualize the measurements once every 3 months
start_date <- min(daily_measurements$Date)
three_months_date <- min(daily_measurements$Date)+months(3)
six_months_date <- min(daily_measurements$Date)+months(6)
nine_months_date <- min(daily_measurements$Date)+months(9)
twelve_months_date <- min(daily_measurements$Date)+months(12)
latest_date <- max(daily_measurements$Date)
total_interval_in_months <- interval(start_date, latest_date) %/% months(1)
latest_label <- as.character(glue("Current ({total_interval_in_months} months)"))

every_3_months_measurements <- daily_measurements %>%
  filter(Date %in% c(start_date,
                     three_months_date, six_months_date,
                     nine_months_date, twelve_months_date, latest_date)) %>%
  mutate(duration=case_when(
    Date == start_date ~ "Start",
    Date == three_months_date ~ "3 months",
    Date == six_months_date ~ "6 months",
    Date == nine_months_date ~ "9 months",
    Date == twelve_months_date ~ "12 months",
    Date == latest_date ~ latest_label
  )) %>%
  select(duration, Weight, `Lean Mass`, BMI) %>%
  mutate(duration = factor(duration, levels=c("Start", "3 months", "6 months", "9 months", "12 months", latest_label)))

# convert to tidy format
every_3_months_measurements_tidy <- every_3_months_measurements %>%
  gather(measure, value, -duration) %>%
  mutate(value = round(value, 1))

# copy pasted from https://ibecav.github.io/slopegraph with minor changes
slope_theme <- list(  
  # move the x axis labels up top
  scale_x_discrete(position = "top"),
  scale_color_tableau(),
  theme_fivethirtyeight(),
  # Format tweaks
  # Remove the legend
  theme(legend.position = "none"),
  # Remove the panel border
  theme(panel.border     = element_blank()),
  # Remove just about everything from the y axis
  theme(axis.title.y     = element_blank()),
  theme(axis.text.y      = element_blank()),
  theme(panel.grid.major.y = element_blank()),
  theme(panel.grid.minor.y = element_blank()),
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x     = element_blank()),
  theme(panel.grid.major.x = element_blank()),
  theme(axis.text.x.top      = element_text(size=12)),
  # Remove x & y tick marks
  theme(axis.ticks       = element_blank()),
  # Format title & subtitle
  theme(plot.title       = element_text(size=14, face = "bold", hjust = 0.5)),
  theme(plot.subtitle    = element_text(hjust = 0.5))
)

every_3_months_measurements_tidy %>%
  ggplot(aes(x = duration, y = value, group = measure)) +
  geom_line(aes(color = measure, alpha = 1), size = 1) +
  #  geom_point(aes(color = Type, alpha = .1), size = 4) +
  geom_text_repel(data = every_3_months_measurements_tidy %>% filter(duration == "Start"), 
                  aes(label = measure) , 
                  hjust = "left", 
                  fontface = "bold", 
                  size = 3, 
                  nudge_x = -.70, 
                  direction = "y") +
  geom_text_repel(data = every_3_months_measurements_tidy %>% filter(duration == latest_label), 
                  aes(label = measure) , 
                  hjust = "right", 
                  fontface = "bold", 
                  size = 3, 
                  nudge_x = .75, 
                  direction = "y") +
  geom_label(aes(label = value), 
             size = 2.5, 
             label.padding = unit(0.05, "lines"), 
             label.size = 0.0) +
  slope_theme +
  labs(
    title = "Improvements in body composition with clean eating and exercise",
    subtitle = "More such charts in my book \"Blueberries in my salad: my journey towards fitness & strength\"",
    caption = glue("Data: {DATA_URL}")
  )

