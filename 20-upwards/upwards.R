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

P2_DEADLIFT_TARGET_WEIGHT <- 300
CHART_ELEMENT_TEXT_SIZE <- 15

DATA_URL <- "https://raw.githubusercontent.com/aarora79/biomettracker/master/raw_data/Amit_deadlifts.csv"
df_deadlifts <- read_csv(DATA_URL)
df_dl <- df_deadlifts %>%
  filter(excercise == "deadlift") %>%
  select(-set)
df_dl <- as.data.frame(lapply(df_dl, rep, df_dl$reps)) %>%
  mutate(date=as.Date(date)) %>%
  group_by(date) %>%
  mutate(median_deadlift_wt = median(weight))
df_dl_median <- as.data.frame(lapply(df_dl, rep, df_dl$reps)) %>%
  mutate(date=as.Date(date)) %>%
  group_by(date) %>%
  summarize(median_deadlift_wt = median(weight))
time_duration_in_weeks <- ceiling(as.numeric(difftime(max(df_dl$date), min(df_dl$date), units="weeks")))

# function to generate custom x-axis labels for the date, we want to put the year
# alongside the first and the last entry only (otherwise it gets too crowded on the x-axis)
# also for some reason the first and last entries of the data vector input to this function
# are NA so put the year label on the 2nd and last but one entries (the NAs dont get displayed on the chart)
date_labeller <- function(d) {
  date_list <- month.abb[month(d)]
  date_list[2] <- glue("{date_list[2]} {year(min(df_dl$date))}")
  date_list[length(date_list)-1] <- glue("{date_list[length(date_list)-1]} {year(max(df_dl$date))}")
  date_list
}

y_labeller <- function(y) { glue("{y} lb")}

p <- df_dl %>%
  ggplot(aes(x=date, group=date, weight, fill=median_deadlift_wt)) +
  geom_boxplot() +
  geom_hline(yintercept = P2_DEADLIFT_TARGET_WEIGHT, colour="#990000", linetype="dashed") +
  geom_label(aes(min(df_dl$date) + 25, P2_DEADLIFT_TARGET_WEIGHT-2,
                 label = glue("{P2_DEADLIFT_TARGET_WEIGHT} lb, deadlift target for 2020"),
                 vjust = -0.75), size=4) +
  
  geom_hline(yintercept = P2_DEADLIFT_TARGET_WEIGHT+100, colour="#990000", linetype="dashed") +
  geom_label(aes(min(df_dl$date) + 25, P2_DEADLIFT_TARGET_WEIGHT+100-2,
                 label = glue("{P2_DEADLIFT_TARGET_WEIGHT+100} lb, deadlift target for 2021"),
                 vjust = -0.75), size=4) +
  scale_y_continuous(breaks = seq(0, P2_DEADLIFT_TARGET_WEIGHT+25, by = 50), labels=y_labeller) + 
  scale_x_date(date_breaks = "1 month",  labels=date_labeller) +
  labs(title="That which does not kill us makes us stronger!",
       subtitle=glue("Boxplot shows distribution of weight of deadlifts done on a given day. My journey from deadlifting {min(df_dl$weight)} lb to {max(df_dl$weight)} lb in {time_duration_in_weeks} weeks."),
       caption=glue("Source: https://leanpub.com/blueberries-in-my-salad | Graphic: Amit Arora")) +
  theme_fivethirtyeight() + 
  scale_fill_gradient_tableau(palette="Blue") + 
  theme(legend.title = element_blank(), legend.position = "none",
        text = element_text(size=CHART_ELEMENT_TEXT_SIZE))
p

