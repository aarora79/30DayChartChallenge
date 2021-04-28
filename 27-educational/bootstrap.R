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

#install.packages("boot",dep=TRUE)
library(boot)
scores <- read_csv("https://stats.idre.ucla.edu/stat/data/hsb2.csv") %>%
  janitor::clean_names()
head(scores)
glimpse(scores)

df <- scores %>%
  select(math, read) %>%
  rename(y=math, x=read)


set.seed(1)

lm_res <- function(df) {
  model <- lm(y ~ x, df %>% sample_frac(replace=T))
  return (model$coefficients)
}

model_base <- lm(y ~ x, df)
summary(model_base)

NUM_REPLICATES <- 10000
df_boot <- replicate(NUM_REPLICATES, lm_res(df))
df_boot <- tibble::as_tibble(data.frame(t(df_boot)) %>% rename(intercept=`X.Intercept.`, slope=x))

df_boot_tidy <- df_boot %>%
  gather(var, value)

df_boot_agg <- df_boot_tidy %>%
  group_by(var) %>%
  summarize(
    average = mean(value),
    low_ci = quantile(value, 0.025),
    high_ci = quantile(value, 0.975)
  ) %>%
  mutate(value=0)

df_coefs <- data.frame(var=c("intercept", "slope"), val=c(as.numeric(model_base$coefficients[1]), as.numeric(model_base$coefficients[2])))

df_boot_tidy %>%
ggplot(aes(x=value, fill=var, col=var)) +
  geom_rect(data=df_boot_agg,
            aes(xmin=low_ci, xmax=high_ci, ymin=0, ymax=Inf),
            color="#999999", fill="#1a1a1a", alpha=0.10) +
  geom_text(data=df_boot_agg, aes(label=sprintf("%0.2f",low_ci), x=low_ci, y=Inf), vjust=2, hjust=1.5, color="#999999", size=2.5, family="Open Sans Condensed Bold") +
  geom_text(data=df_boot_agg, aes(label=sprintf("%0.2f",high_ci), x=high_ci, y=Inf), vjust=2, hjust=-0.5, color="#999999", size=2.5, family="Open Sans Condensed Bold") +
  geom_histogram(binwidth=0.1, fill="#2980b9") +
  geom_vline(data=df_coefs, aes(xintercept = val), color="#2c3e50", size=0.5, linetype="dashed") +
  facet_wrap(~var, scales="free") +
  theme_fivethirtyeight() +
  labs(title=glue("Bootstrap Resampling of Math ~ Reading scores (n = {nrow(df_boot)})"),
       caption="Source: https://stats.idre.ucla.edu/stat/data/hsb2.csv | Graphic: Amit Arora") +
  theme(legend.position = "none", axis.title = element_text()) +
  xlab("") + ylab("# of Resampled Regressions w/ Coefficient")
