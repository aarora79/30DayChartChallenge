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


df_amit <- read_csv("https://raw.githubusercontent.com/aarora79/biomettracker/master/data/Amit.csv") %>%
  janitor::clean_names() %>%
  filter(!is.na(date)) %>%
  mutate(date = ymd(date), weight=as.numeric(weight)) %>%
  select(date, weight) %>%
  mutate(weight_change = weight-lead(weight, 1)) %>%
  mutate(name="Amit")

glimpse(df_amit)

df_nidhi <- read_csv("https://raw.githubusercontent.com/aarora79/biomettracker/master/data/Nidhi.csv") %>%
  janitor::clean_names() %>%
  filter(!is.na(date)) %>%
  mutate(date = ymd(date), weight=as.numeric(weight)) %>%
  select(date, weight)%>%
  mutate(weight_change = weight-lead(weight, 1)) %>%
  mutate(name="Nidhi")


glimpse(df_nidhi)


df_wt_change <- bind_rows(df_amit, df_nidhi)  %>%
  group_by(name) %>%
  mutate(mean_daily_wt_change = mean(weight_change, na.rm=TRUE))
glimpse(df_wt_change)


df1 <- df_wt_change %>%
  ungroup() %>%
  mutate(m = floor_date(date, unit="1 month")) %>%
  filter(year(m) >= 2020) %>%
  mutate(covid = ifelse(m==ymd("2021-02-01"), "yes", "no"))

 View(df1)
labeller_y <- function(y) {
  glue("{round(y, 1)} lb")
}

get_mean_daily_wt_change  <- function(n) {
  df_wt_change %>%
    filter(name==n) %>%
    head(1) %>%
    mutate(mean_daily_wt_change=round(mean_daily_wt_change, 2)) %>%
    pull(mean_daily_wt_change)
}

NAMES <- c("Amit", "Nidhi")
ann_text <- data.frame(m = c(ymd("2021-04-01"), ymd("2021-02-01")),
                       weight_change = c(3.5, -4.6),
                       lab = c("3 day\nwater fast", "Got Covid!"), 
                       covid=c("no", "yes"),
                       name = factor( 
                         c("Amit", "Nidhi"), levels = NAMES))

df_wt_change %>%
  ungroup() %>%
  mutate(name=factor(name, NAMES)) %>%
  mutate(m = floor_date(date, unit="1 month")) %>%
  filter(year(m) >= 2020) %>%
  mutate(covid = ifelse(m==ymd("2021-02-01") & name == "Nidhi", "yes", "no")) %>%
  ggplot(aes(x=m, y=weight_change, group=m, col=name, fill=covid)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_text(data = ann_text, aes(label = lab)) +
  geom_hline(aes(yintercept=mean_daily_wt_change, color=name), linetype="dashed") +
  facet_wrap(~name) +
  scale_color_tableau() +
  scale_fill_manual(values=list(yes="pink", no="#ffffff")) +
  scale_x_date(breaks="3 months", date_labels = "%b %Y") + 
  scale_y_continuous(breaks=seq(as.integer(min(df_wt_change$weight_change, na.rm=TRUE)),
                                as.integer(max(df_wt_change$weight_change, na.rm=TRUE)),
                                0.5), labels = labeller_y) +
  theme_fivethirtyeight() +
  labs(title="How much did the daily bodyweight change deviate from the overall average every month?",
       subtitle=glue("Dashed line represents average daily weight change over the entire duration, Amit {get_mean_daily_wt_change('Amit')}lb, Nidhi {get_mean_daily_wt_change('Nidhi')}lb"),
       caption="Source: https://github.com/aarora79/biomettracker | Graphic: Amit Arora") +
  theme(legend.position = "none", axis.title = element_text()) +
  ylab("Daily Change in Bodyweight") +
  xlab("")

