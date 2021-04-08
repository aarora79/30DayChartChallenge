library(glue)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(ggpmisc)
library(ggridges)
library(ggthemes)
library(lubridate)
library(tidyverse)

# read data
DATA_URL <- "https://raw.githubusercontent.com/aarora79/biomettracker/master/raw_data/Amit_deadlifts.csv"
deadlifts <- read_csv(DATA_URL) %>%
  rename(exercise = excercise) %>%
  mutate(date = ymd(date)) %>%
  filter(exercise %in% c("deadlift", "deadlift & carry")) 
head(deadlifts)

# add a summary table to the plot
table <- deadlifts %>%
  group_by(exercise) %>%
  summarize(max_lift = max(weight)) %>%
  mutate(max_lift = glue("{max_lift} lb")) %>%
  rename(Exercise=exercise, `Max Lift`=max_lift)

# handy labeler function
add_lb <- function(x) { glue("{x} lb")}

# calc the duration to include it in the title
duration_in_months <- round(as.numeric(difftime(max(deadlifts$date),
                                                min(deadlifts$date),
                                                units = "days"))/30)

# now for the plot
p <- deadlifts %>%
       mutate(date_month = floor_date(date, unit = "month")) %>%
       mutate(month_str = glue("{month.abb[month(date)]} {year(date)}")) %>%
       mutate(month_str = fct_reorder(month_str, date_month, .fun=min)) %>%
       ggplot(aes(x = weight, y = month_str,
                  color = exercise, point_color = exercise, fill = exercise)) +
       geom_density_ridges(
         jittered_points = TRUE, scale = .95, rel_min_height = .01,
         point_shape = "|", point_size = 3, size = 0.25,
         position = position_points_jitter(height = 0)
       ) + 
       annotate(geom = "table", x = 15, y = 14, label = list(table), 
                vjust = 1, hjust = 0, color="slategrey", size=4) +
       scale_y_discrete(expand = c(0, 0)) +
       scale_x_continuous(expand = c(0, 0), name = "Weight (lb)", labels = add_lb) +
       scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("deadlift", "deadlift & carry")) +
       scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
       scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
       coord_cartesian(clip = "off") +
       guides(fill = guide_legend(
         override.aes = list(
           fill = c("#D55E00A0", "#0072B2A0"),
           color = NA, point_color = NA))) +
      labs(title=glue("My deadlift progress over the past {duration_in_months} months"),
           subtitle = "The deadlift and the deadlift & carry sound similar but the shoulders tell you the difference the morning after!") +
      theme_fivethirtyeight() +
      theme(legend.title = element_blank())

p
