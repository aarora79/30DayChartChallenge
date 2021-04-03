library(glue)
library(tidyr)
library(tidyverse)
library(highcharter)


# set to "./" if running from the pictogram directory
# need to do this because knitr::image_uri does not support web links
BASE_DIR <- "pictogram"

# read the data
df <- read_csv(file.path(BASE_DIR, "workout.csv"))
head(df)


png_to_datauri <- function(name, ...) {
  knitr::image_uri(file.path(BASE_DIR, glue("{name}.png")))
}

# Setting icon to data mapping
icon_map <- data.frame(exercise = c("burpees", "pushpress", "sprints", "bodyrows"),
                       label =  c("Burpees", "Push Press", "Sprints", "Body Rows"),
                       faico =  c("burpees", "pushpress", "sprints", "bodyrows"),
                       col = c("#d62828", "#1d3557", "#2a9d8f", "#2b2d42"),
                       stringsAsFactors = FALSE) 


# Aggregate the data by energy source and add the icons data
df <- df %>%
  group_by(exercise) %>%
  summarize(reps=sum(reps)) %>%
  ungroup() %>%
  dplyr::left_join(icon_map, by = "exercise") %>%
  dplyr::mutate(
    uri = map2_chr(faico, col, ~png_to_datauri(.x, fill = .y)),
    marker = map(uri, ~ list(symbol = str_glue("url({data_uri})", data_uri = .x)))
  ) 
    
df

# Plot
df_labels <- df %>%
  mutate(label = glue("{exercise}: {reps}")) %>%
  select(label)
title <- glue("My Weekend Workout<br>{paste(df_labels$label, collapse=', ')}")

hchart(
  df,
  "item",
  hcaes(name = exercise, y = reps),
  name = "Workout",
  showInLegend = FALSE
) %>% 
  hc_plotOptions(
    # avoid hide series due bug
    series = list(point = list(events = list(legendItemClick = JS("function(e) {e.preventDefault() }"))))
  ) %>% 
  hc_title(text = title)


