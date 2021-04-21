library(gt)
library(xts)
library(zoo)
library(glue)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(ggthemes)
#library(dygraphs)
library(lubridate)
library(tidyverse)
#library(flexdashboard)
library(futile.logger)

df <- read_csv("https://raw.githubusercontent.com/aarora79/biomettracker/a44ffdbe9730ea0f1a9f4d175049b18bedf32b89/data/Amit.csv") %>%
  mutate(Date=ymd(Date)) %>%
  mutate(Weight=as.numeric(Weight)) %>%
  arrange(Date)

head(df)

df %>%
  filter(Weight > 250)

starting_weight <- df$Weight[1]
current_weight <- df$Weight[nrow(df)]
subtitle=glue("From {starting_weight}lb to {current_weight}lb in {ceiling(difftime(df$Date[nrow(df)], df$Date[1], units='week'))} weeks.")

df_imp_dates <- data.frame(date=c("2020-01-17", "2020-09-1", "2020-12-31"),
                           label=c("smooth ride", "things got tough", "back on track"))

df_imp_dates <- data.frame(start=c("2020-01-15", "2020-09-1", "2021-01-01"),
                           end=c("2020-08-31", "2020-12-31", "2021-3-31"),
                           label=c("A smooth slide...", "~ Holiday season ~", "Back on track!"))
df_imp_dates <- df_imp_dates %>%
  mutate(start=as.Date(start), end=as.Date(end)) 

ggplot(df) +
  geom_point(aes(x=Date, y=Weight)) + 
  geom_smooth(aes(x=Date, y=Weight), method="loess") +
  geom_rect(
    aes(xmin = start, xmax = end, fill=label), 
    ymin = -Inf, ymax = Inf, alpha = 0.2, 
    data = df_imp_dates
  ) +
  geom_text(
    aes(x = start, y = 190, label = label), 
    data = df_imp_dates, 
    size = 6, vjust = 0, hjust = 0, nudge_x = 35
  ) +
  #geom_hline(yintercept=190) +  
  annotate("text", x=min(df$Date), y=max(df$Weight)+3, label=glue("{max(df$Weight)} lb")) +  
  scale_color_tableau() +
  scale_x_date(date_breaks = "1 month", date_labels="%b-%Y") +
  theme_fivethirtyeight() + 
  labs(title="My Weight Loss Journey: Eat Clean, Workout, Sleep, Repeat!",
       subtitle=subtitle) +
  theme(legend.position="none") +
  theme(axis.title = element_text(), axis.title.x = element_blank()) + 
  ylab('Weight (lb)')

