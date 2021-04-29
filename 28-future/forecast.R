
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
library(prophet)
library(tidymodels)
library(modeltime)

df <- read_csv("https://raw.githubusercontent.com/aarora79/biomettracker/master/data/Amit.csv") %>%
  janitor::clean_names() %>%
  filter(!is.na(date)) %>%
  mutate(date = ymd(date), weight=as.numeric(weight)) %>%
  select(date, weight) %>%
  rename(ds=date, y=weight)

glimpse(df)

CAP <- 260
FLOOR <- 160
df$cap <- CAP
df$floor <- FLOOR

m <- prophet(df, growth = "logistic")

# R
future <- make_future_dataframe(m, periods = 365)
tail(future)

future$cap <- CAP
future$floor <- FLOOR

forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])


# R
plot(m, forecast)

head(forecast)

df2 <- forecast %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  left_join(df[, c("ds", "y")], by="ds") %>%
  gather(k, v, -ds)

training <- df2 %>%
  filter(k == "y")

yhat <- df2 %>%
  filter(k == "yhat")

yhat_bounds <- forecast %>%
  select(ds, yhat_lower, yhat_upper)


labeller_y <- function(y) {
  glue("{y} lb")
}

p <- training %>%
  left_join(yhat_bounds, by="ds") %>%
  mutate(ds=as.Date(ds)) %>%
  ggplot(aes(x=ds, y=v, col=k)) +
  geom_point(color="black", alpha = 1) +
  geom_line(data=yhat, aes(x=as.Date(ds), y=v), color="blue") +
  geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), color="#999999", fill="#1a1a1a", alpha=0.1) +
  geom_hline(yintercept=c(CAP, FLOOR), linetype="dashed") + 
  annotate("text", x = as.Date(min(training$ds)), y = CAP+2, label = glue("Cap = {CAP} lb")) +
  annotate("text", x = as.Date(min(training$ds)), y = FLOOR+2, label = glue("Floor = {FLOOR} lb")) +
  geom_text(aes(label=ifelse(v == min(df$y, na.rm=TRUE), glue("{v} lb"), ""), x= ds, y=v),hjust=0,vjust=1, color="red") +
  geom_text(aes(label=ifelse(v == max(df$y, na.rm=TRUE), glue("{v} lb"), ""), x= ds, y=v),hjust=0,vjust=0, color="red") +
  scale_x_date(breaks="3 months", date_labels = "%b %Y") + 
  scale_y_continuous(breaks=seq(FLOOR, CAP, 10), labels = labeller_y) +
  theme_fivethirtyeight() +
  labs(title="My body weight forecast, gazing into the future 365 days",
       subtitle="Forecasting engine: Prophet (https://facebook.github.io/prophet/).",
       caption="Source: https://raw.githubusercontent.com/aarora79/biomettracker/master/data/Amit.csv | Graphic: Amit Arora") +
  theme(legend.title = element_blank(), axis.title = element_text()) +
  ylab("Body Weight") +
  xlab("")

p
