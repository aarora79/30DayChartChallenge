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

TARGET_WEIGHT <- 190
NUDGE_X <- 45
DATA_URL <- "https://raw.githubusercontent.com/aarora79/biomettracker/master/data/Amit.csv"
df <- read_csv(DATA_URL) %>%
  mutate(Date=ymd(Date)) %>%
  filter(!is.na(Date)) %>%
  mutate(Weight=as.numeric(Weight)) %>%
  arrange(Date) %>%
  mutate(wt_lte_target = ifelse(Weight <= TARGET_WEIGHT, "in-progress", "lte"))

tail(df)



starting_weight <- df$Weight[1]
current_weight <- df$Weight[nrow(df)]
subtitle=glue("From {starting_weight}lb to {current_weight}lb in {ceiling(difftime(df$Date[nrow(df)], df$Date[1], units='week'))} weeks.")

df_imp_dates <- data.frame(start=c("2020-01-15", "2020-08-31", "2021-1-1"),
                           end=c("2020-08-31", "2020-12-31", "2021-3-31"),
                           start_text=c("2020-02-15", "2020-08-10", "2020-12-10"),
                           label=c("A smooth slide...", "~ Holiday season ~", "Back on track!"))
df_imp_dates <- df_imp_dates %>%
  mutate(start=as.Date(start), start_text=as.Date(start_text), end=as.Date(end)) 

df_arrow <- data.frame(x1 = min(df$Date), x2 = min(df$Date), y1 = max(df$Weight), y2 = df$Weight[nrow(df)])

ggplot(df) +
  geom_point(aes(x=Date, y=Weight, col=wt_lte_target)) + 
  geom_smooth(aes(x=Date, y=Weight), method="loess") +
  geom_segment(data=df_arrow, arrow = arrow(length=unit(0.30,"cm"),
                                         ends="first",
                                         type = "closed"),
            aes(x=x2, xend=x1, y=y2, yend=y1 )) +
  geom_rect(
    aes(xmin = start, xmax = end, fill=label), 
    ymin = -Inf, ymax = Inf, alpha = 0.2, 
    data = df_imp_dates
  ) +
  geom_text(
    aes(x = start_text, y = 240, label = label), 
    data = df_imp_dates, 
    size = 6, vjust = 0, hjust = 0, nudge_x = 35
  ) +
  geom_hline(yintercept=TARGET_WEIGHT, linetype="dashed") +  
  geom_label(aes(min(df$Date) + NUDGE_X, TARGET_WEIGHT,
                 label = glue("Weight target: {TARGET_WEIGHT} lb"),
                 vjust = -0.75), size=4) +
  annotate(geom = "text", x = df$Date[1]+5, y = 220, label = glue("Total weight loss: {max(df$Weight)-df$Weight[nrow(df)]}lb"), color = "red",
           angle = 90)+
  #annotate("text", x=min(df$Date), y=max(df$Weight)+3, label=glue("{max(df$Weight)} lb")) +  
  scale_color_tableau() +
  scale_y_continuous(breaks=scales::pretty_breaks(10)) +
  scale_x_date(date_breaks = "1 month", date_labels="%b-%Y") +
  theme_fivethirtyeight() + 
  labs(title="My Weight Loss Journey: Eat Clean, Workout, Sleep, Repeat!",
       subtitle=subtitle) +
  theme(legend.position="none") +
  theme(axis.title = element_text(), axis.title.x = element_blank()) + 
  ylab('Weight (lb)')

