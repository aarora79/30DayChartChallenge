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
library(circlize)
DATA_URL <- "https://raw.githubusercontent.com/jeffreylancaster/game-of-thrones/master/data/episodes.json"
resp <- httr::GET(DATA_URL)
print(glue("got a response for API={DATA_URL}, statuscode={resp$status_code}"))

# Convert to df 
resp_json <- jsonlite::fromJSON(rawToChar(resp$content))
episodes <- resp_json$episodes$scenes

# just for understanding the structure of the json and debugging
for (epi_id in seq(1, length(episodes))) {
  print(glue("episode={epi_id}, number of scenes={length(episodes[[epi_id]])}"))
  for (scene_id in seq(1, length(episodes[[epi_id]]))) {
    characters <- as.data.frame(episodes[[epi_id]]$characters[scene_id])
  }
}

# it is best to View(episodes) to understand but basically
# episodes is a list of scenes and each scenes has some scales and list/dataframe
# of shape number_of_scenesXcenes, so there are 4 scalars and the 5th element is another
# dataframe of characters and some attributes (of variable number) that are in that scene
# so we loop through episodes and within each episodes each scene and extract
# the characters dataframe and append episode number and scene number columns
# and return this dataframe. At the end of this two level map, all the dataframes
# are concatenated to get an overall characters dataframe
characters <- map(seq(1, length(episodes)), function(epi_id) {
  map(seq(1, length(episodes[[epi_id]]$characters)), function(scene_id) {
    print(glue("episode={epi_id}, scene_id={scene_id}"))
    episodes[[epi_id]]$characters[[scene_id]] %>% mutate(episode=epi_id, scene=scene_id)
  })
}) %>% reduce(bind_rows)

dim(characters)

scenes_per_character <- characters %>%
  count(name, sort=TRUE)

# we want to generate a dataframe which has one row for each pair of characters
# that occur together in a scene. We do this by:
# 1. first getting all the unique combinations of episodes and scenes in an episode,
# 2. then for each such combination pull the name of the characters
# 3. then use the "combn" function to generate all groups of 2 possible from this list of characters
#    and then loop through that matrix columns to conver into a dataframe of n rows and 2 columns
#    where n being the number of combinations (of 2 characters) generated.
# 4. then row bind all such combination dataframe, this is all combinations in an episode
# 5. repeat and row bind to get all combinations across all episodes

episodes_and_scenes <- characters %>%
  count(episode, scene)
characters_occuring_together <- map2(episodes_and_scenes$episode, episodes_and_scenes$scene, function(e, s) {
  print(glue("e={e}, s={s}"))
  character_names <- characters %>%
    filter(episode==e & scene==s) %>%
    pull(name)
  
  if(length(character_names) > 1) {
    x <- combn(character_names, 2)
    # print(x)
    num_of_combinations <- dim(x)[2]
    # print(glue("num_of_combinations={num_of_combinations}"))
    map(seq(1, num_of_combinations), function(i) {
      print(x[i])
      # sort in (any) order so that "jon", "arya" is same as "arya", "jon" (they occur together, 
      # doesnt matter in which order)
      sorted_x <- sort(c(x[1, i], x[2, i]))
      name1 <- sorted_x[1]
      name2 <- sorted_x[2]
      data.frame(char1=name1, char2=name2)
    }) %>%
      reduce(bind_rows)
  } else {
    NULL
  }
  
}) %>% reduce(bind_rows) %>%
  count(char1, char2, sort=TRUE)

head(characters_occuring_together)


N <- 10
tableau_colors <- c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2", "#59a14f",
                    "#edc948", "#b07aa1", "#ff9da7", "#9c755f", "#bab0ac")
important_charachers_occuring_together <- characters_occuring_together %>%
  #head(20) %>%
  filter(char1 %in% scenes_per_character$name[1:N] & char2 %in% scenes_per_character$name[1:N]) %>%
  rename(from=char1, to=char2, value=n)

df <- characters_occuring_together %>%
  #head(20) %>%
  filter(char1 %in% scenes_per_character$name[1:N] & char2 %in% scenes_per_character$name[1:N])
View(df)
important_charachers_occuring_together

chordDiagram(important_charachers_occuring_together,
             grid.col = tableau_colors)
#c("red", "blue", "purple","pink", "orange"))
title(main=glue("Number of times the {N} most frequently appearing characters in GoT were in a scene together"))

# also see 
# https://jokergoo.github.io/circlize_book/book/
# http://mr.schochastics.net/netVizR.html
