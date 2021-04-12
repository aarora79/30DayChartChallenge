library(glue)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(forcats)
library(ggthemes)
library(janitor)
library(lubridate)
library(tidyverse)

licenses_and_languages <- read_csv("11-circular/github.csv")
head(licenses_and_languages)

# most commonly used licenses
repos_per_license <- licenses_and_languages %>%
  group_by(license) %>%
  summarize(count = sum(count)) %>%
  arrange(desc(count))

head(repos_per_license)

N <- 5
top_n_licenses_and_languages <- licenses_and_languages %>%
  filter(license %in% repos_per_license$license[1:N]) %>%
  group_by(license) %>%
  slice_head(n=N*2) %>%
  ungroup()
head(top_n_licenses_and_languages)
dim(top_n_licenses_and_languages)

data <- top_n_licenses_and_languages %>%
  mutate(id=row_number(), 
         individual = glue("{license} {name}"),
         value = count) %>%
  select(id, individual, value) 
head(data)
data

# library
library(tidyverse)

# Create dataset
data <- data.frame(
  individual=top_n_licenses_and_languages$name,
  group=top_n_licenses_and_languages$license ,
  value=top_n_licenses_and_languages$count/10000
)

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]


head(data)


# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  ylim(-80,80) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0,0, 1), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE) +
  scale_fill_tableau() + 
  theme_fivethirtyeight() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  ) +
  labs(title = "Top 10 languages in GitHub Repos for Top 5 Licenses",
       subtitle = "Number of repos in tens of thousands",
       caption = "Source: BigQuery Public Data: bigquery-public-data.github_repos")

p

