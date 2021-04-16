library(glue)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(ggpmisc)
library(ggforce)
library(forcats)
library(ggthemes)
library(janitor)
library(lubridate)
library(tidyverse)


census_adult_income <- read_csv("15-multivariate/census_adult_income.csv")
head(census_adult_income)

census_adult_income_counts <- df %>%
  count(workclass, education, marital_status, occupation, race, sex, income_bracket) %>%
  filter(n >= 10) %>%
  rename_with(str_to_title) %>%
  rename(`Income Bracket`=Income_bracket, `Marital Status`=Marital_status)

data <- gather_set_data(census_adult_income_counts, 1:7)
  

ggplot(data, aes(fct_inorder(x), id = id, split = y, value = N)) +
  geom_parallel_sets(aes(fill = `Income Bracket`), alpha = 0.3, axis.width = 0.2) +
  geom_parallel_sets_axes(axis.width = 0.2, fill="lightgrey") +
  geom_parallel_sets_labels(colour = "black", angle = 360, size = 3) +
  scale_fill_tableau() +
  theme_fivethirtyeight() +
  labs(title="How different factors such as Workclass, Education, Race etc. define the income bracket",
       subtitle="Many inferences evident from this chart, the first one being train a classifier!",
       caption="Source: bigquery-public-data:ml_datasets.census_adult_income")
