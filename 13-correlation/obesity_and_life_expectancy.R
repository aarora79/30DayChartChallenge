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
library(readxl)

life_expectancy <- read_excel("13-correlation/IHME_county_data_LifeExpectancy_Obesity_PhysicalActivity_NATIONAL.xlsx", sheet="Life Expectancy") %>%
  janitor::clean_names()
head(life_expectancy)

physical_activity <- read_excel("13-correlation/IHME_county_data_LifeExpectancy_Obesity_PhysicalActivity_NATIONAL.xlsx", sheet="Physical Activity") %>%
  janitor::clean_names()
head(physical_activity)

obesity <- read_excel("13-correlation/IHME_county_data_LifeExpectancy_Obesity_PhysicalActivity_NATIONAL.xlsx", sheet="Obesity") %>%
  janitor::clean_names()
head(obesity)

# join latest data for each state and county
life_expectancy_and_obesity_males <- life_expectancy %>%
  select(state, county, male_life_expectancy_2010_years) %>%
  left_join(obesity %>%
              select(state, county, male_obesity_prevalence_2011_percent), 
            by=c("state", "county")) %>%
  rename(life_expectancy=male_life_expectancy_2010_years, obesity_prevalence=male_obesity_prevalence_2011_percent) %>%
  mutate(sex="Male")
head(life_expectancy_and_obesity_males)

corr_males <- cor(life_expectancy_and_obesity_males$life_expectancy, life_expectancy_and_obesity_males$obesity_prevalence)
corr_males 

life_expectancy_and_obesity_females <- life_expectancy %>%
  select(state, county, female_life_expectancy_2010_years) %>%
  left_join(obesity %>%
              select(state, county, female_obesity_prevalence_2011_percent), 
            by=c("state", "county")) %>%
  rename(life_expectancy=female_life_expectancy_2010_years, obesity_prevalence=female_obesity_prevalence_2011_percent) %>%
  mutate(sex="Female")
head(life_expectancy_and_obesity_females)
corr_females <- cor(life_expectancy_and_obesity_females$life_expectancy, life_expectancy_and_obesity_males$obesity_prevalence)
corr_females

life_expectancy_and_obesity_combined <- life_expectancy_and_obesity_females %>%
  bind_rows(life_expectancy_and_obesity_males)

corr_text <- data.frame(sex=c("Female", "Male"), 
                        life_expectancy=c(70, 70), 
                        obesity_prevalence=c(40,40),
                        label=c(glue("correlation: {round(corr_females, 2)}"), glue("correlation: {round(corr_males, 2)}")))
life_expectancy_and_obesity_combined %>%
  filter(!is.na(county)) %>%
  ggplot(aes(x=life_expectancy, y=obesity_prevalence, col=sex)) +
  geom_point(size=0.2) +
  geom_smooth(aes(col=sex)) + 
  facet_wrap(~sex) +
  #geom_text(data=corr_text, aes(x = life_expectancy, y = obesity_prevalence, label = label, col=sex), size = 4, stat = "identity") +
  scale_color_tableau() + 
  scale_fill_tableau() +
  labs(title="Higher the obesity prevalence in a population, lower the life expectancy!",
       subtitle="Correlation between life expectancy and prevalence of obesity in U.S. counties",
       caption="Source: Institute of Health Metrics and Evaluation, http://www.healthdata.org/us-health/data-download")+
  theme_fivethirtyeight() +
  theme(legend.position = "none", plot.subtitle = element_markdown()) +
  theme(axis.title = element_text()) +
  ylab("Prevalence of obesity (%) as of 2011") +
  xlab("Life expectancy (years) as of 2010")

