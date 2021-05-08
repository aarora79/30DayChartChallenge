library(glue)
library(dplyr)
library(tidyr)
library(ggmap)
library(rtweet)
library(ggplot2)
library(forcats)
library(leaflet)
library(ggthemes)
library(tidyverse)
library(htmltools)
library(hrbrthemes)
library(htmlwidgets)


# good reference to consult for twitter data analysis via rtweet
# https://rud.is/books/21-recipes/


# read the twitter credentials from env vars, these need to be set separately
key <- Sys.getenv("TWITTER_KEY")
secret <- Sys.getenv("TWITTER_SECRET")
access_token <- Sys.getenv("TWITTER_ACCESS_TOKEN")
access_secret <- Sys.getenv("TWITTER_ACCESS_SECRET")
google_maps_key <- Sys.getenv("GOOGLE_MAPS_KEY")

# create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)


#######################################################
# Liked & Retweets
#######################################################

# read tweets from me
MY_TWITTER_SCREEN_NAME <- "aarora79"
tweets <- get_timeline(user=MY_TWITTER_SCREEN_NAME)
glimpse(tweets)
print(glue("shape of the tweets data from \"@{MY_TWITTER_SCREEN_NAME}\" is {nrow(tweets)}x{ncol(tweets)}"))
View(tweets)

# filter tweets containing #30DayChartChallenge
HASHTAG_OF_INTEREST <- "#30DayChartChallenge"
tweets_of_interest <- tweets %>%
  filter(str_detect(text, HASHTAG_OF_INTEREST))
print(glue("shape of the tweets data containing \"{HASHTAG_OF_INTEREST}\" is {nrow(tweets_of_interest)}x{ncol(tweets_of_interest)}"))


# get the labels for tweets done on each day of the 30 day challenge
# all tweets had the format "Day 25 of #30DayChartChallenge! #Demographic. How well do people of ....."
# so we can extract the label for the tweet done on a day by first splitting
# on ! and then splitting on ".". This could be done better with a regex (next time..).
chart_labels <- str_split(str_trim(str_split(str_split(tweets_of_interest$text, "!", simplify = TRUE)[,2],
                                             "\\.",
                                             simplify = TRUE)[,1]),
                          " ", simplify=TRUE)[,1]

# manually fix some labels for tweets that did not follow this format
chart_labels[1] <- "Collage"
chart_labels[25] <- "Physical"
chart_labels[27] <- "Slope Chart"
chart_labels[32] <- "Part-to-whole"
chart_labels

tweets_of_interest$chart_label <- chart_labels

# plot a bar plot with the likes and retweets for each tweet
q50_likes <- quantile(tweets_of_interest$favorite_count, 0.5)
q50_retweets <- quantile(tweets_of_interest$retweet_count, 0.5)

p <- tweets_of_interest %>%
  arrange(created_at) %>%
  select(chart_label, favorite_count, retweet_count) %>%
  mutate(rn = row_number()) %>%
  mutate(chart_label = fct_reorder(chart_label, rn, .fun=mean)) %>%
  select(-rn) %>%
  gather(k, v, -chart_label) %>%
  mutate(k = case_when( k == "favorite_count" ~ "Likes",
                        k == "retweet_count" ~ "Retweets")) %>%
  ggplot(aes(x=chart_label, y=v, col=k, fill=k)) +
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) +
  geom_hline(yintercept = q50_likes, linetype="dashed", color="blue") +
  geom_hline(yintercept = q50_retweets, linetype="dashed", color="orange") +
  scale_y_continuous(breaks=seq(0, max(tweets_of_interest$favorite_count), 5)) +
  scale_color_tableau() +
  scale_fill_tableau() +
  theme_fivethirtyeight() +
  labs(title=glue("Likes and Retweets for each tweet from \"@{MY_TWITTER_SCREEN_NAME}\" with \"{HASHTAG_OF_INTEREST}\""),
       subtitle=glue("Total retweets={sum(tweets_of_interest$retweet_count)} (Median {q50_retweets} retweets/tweet), Total likes={sum(tweets_of_interest$favorite_count)} (Median {q50_likes} likes/tweet)"),
       caption="Source: Twitter data extracted using the rtweet package | Graphic: Amit Arora") +
  theme(legend.position = "bottom", legend.title = element_blank(), axis.title = element_text(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("Count") +
  xlab("")
p

#######################################################
# Understanding who and from where liked the tweets?
#######################################################

# read the files containing the users who liked the tweets
# this file is hand created by copy pasting from the twitter gui
lines <- readLines("tweets/likes.txt")
lines

# lines starting with @ contain the screen name (of who liked a particular tweet)
unique(lines[startsWith(lines, "@")])

# covnert to a dataframe
users <- as_tibble(data.frame(user=lines[startsWith(lines, "@")])) %>%
  mutate(user = str_remove(user, '@'))
users

# liked per user
user_likes <- users %>%
  count(user, sort=TRUE)

#View(user_likes)
head(user_likes, 10)

## get metadata about the users who liked the tweets
users <- lookup_users(user_likes$user)
# we are only interested in location and description
users <- users %>%
  select(screen_name, location, description)
print(glue("there are {nrow(users)} unique users who liked the tweets"))

# get geo location using google maps API, we exclude rows where location
# not specified and also only keep unique locations
coded <- discard(users$location, `==`, "") %>% 
  unique() %>%
  ggmap::geocode()
  
# add the location name to the geo coded information
coded$location <- discard(users$location, `==`, "") %>% 
  unique()

# now join it back with original dataframe
users <- users %>%
  left_join(coded, by="location")

users

# plot the user locations, whatever is available on a map
rr <- tags$div(
  HTML('<a href="https://github.com/aarora79/30DayChartChallenge">Data visualization enthusiasts from all over liked my charts :)</a>')
)  

my_location <- ggmap::geocode("Clarksburg, MD")
my_status <- "me: Data Scientist, Fitness @40, self published author."

map_leaflet <- leaflet(data = users) %>% 
  addTiles() %>%
  addMarkers(lng = my_location$lon, lat = my_location$lat, popup = my_status, label = my_status) %>%
  addMarkers(
    clusterOptions = markerClusterOptions(),
    popup = ~as.character(glue("{screen_name}: {description}")), 
    label = ~as.character(glue("{screen_name}: {description}"))
  )  %>%
  addControl(rr, position = "topleft")
map_leaflet
saveWidget(map_leaflet, file="likes.html")


# anything interesting in the status messages of these users
library(tidytext)
library(ggwordcloud)
data(stop_words)
description_tidy <- users %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words)
TWITTER__STOP_WORDS <- c("t.co", "https", "de", "en")
description_words_w_counts <- description_tidy %>%
  filter(!(word %in% TWITTER__STOP_WORDS)) %>%
  mutate(word = ifelse(word=="ph.d", "phd", word)) %>%
  count(word, sort=TRUE)
set.seed(42)
p <- description_words_w_counts %>%
  filter(n > 1) %>%
  ggplot(aes(label = word, size=n)) +
  geom_text_wordcloud_area( color=I("#4E79A7")) +
  scale_size_area(max_size = 20) +
  theme_fivethirtyeight() +
  labs(title = glue("What can we learn about the interests of those who liked my charts?"),
       subtitle = "Data extracted from the twitter status of individual users",
       caption="Source: Twitter data extracted using the rtweet package | Graphic: Amit Arora") +
  theme(plot.title = element_text(colour = "#4E79A7"),
        plot.subtitle = element_text(colour = "#4E79A7"),
        plot.caption = element_text(colour = "darkblue"))
p
# View(description_words_w_counts)


###########################################################################
# What is the influence or reach of the tweets? C
# influence = count of followers + sum (count of followers of first level followers)
# code taken as is from https://rud.is/books/21-recipes/
###########################################################################
influence_snapshot <- function(user, all = FALSE, trans=c("log10", "identity")) {
  user <- user[1]
  trans <- match.arg(tolower(trimws(trans[1])), c("log10", "identity"))
  
  scale_lab <- ""
  if (trans == "log10") sclae_lab <- " (log scale)"
  
  user_info <- lookup_users(user)
  
  n <- if (all[1]) user_info$followers_count else 5000
  
  user_followers <- get_followers(user_info$user_id)
  uf_details <- lookup_users(user_followers$user_id)
  
  primary_influence <- sum(c(uf_details$followers_count, user_info$followers_count))
  
  filter(uf_details, followers_count > 0) %>% 
    ggplot(aes(followers_count)) +
    geom_density(aes(y=..count..), color="lightslategray", fill="lightslategray",
                 alpha=2/3, size=1) +
    scale_x_continuous(expand=c(0,0), trans=trans, labels=scales::comma) +
    scale_y_comma() +
    labs(
      x=sprintf("Number of Followers of Followers%s", scale_lab), 
      y="Number of Followers",
      title=sprintf("Follower chain distribution of %s (@%s)", user_info$name, user_info$screen_name),
      subtitle=sprintf("Follower count: %s; Primary influence/reach: %s", 
                       scales::comma(user_info$followers_count),
                       scales::comma(primary_influence))
    ) +
    theme_ipsum_rc(grid="XY") -> gg
  
  print(gg)
  
  return(invisible(list(user_info=user_info, follower_details=uf_details)))
  
}

influence <- influence_snapshot("aarora79")

