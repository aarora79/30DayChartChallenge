library(glue)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(mlbench)
library(rpart)
library(rpart.plot)


# read the wheat dataset.
DATASET_URL <- "https://raw.githubusercontent.com/jbrownlee/Datasets/master/wheat-seeds.csv"
wheat <- read_csv(DATASET_URL, col_names = FALSE)
names(wheat) <- c("area", "perimeter", "compactness", "kernel_length", "kernel_width", "asymmetry_coefficient",
                  "length_of_kernel_groove", "wheat_class")
wheat <- wheat %>%
  mutate(wheat_class=as.factor(wheat_class))
head(wheat)

# make a decision tree using parsnip package and parttree package
# remotes::install_github("grantmcdermott/parttree")
library(parttree)
# start with a simple scatter plot
p <- ggplot(data = wheat, aes(x = length_of_kernel_groove, y = area)) +
  geom_point(aes(col = wheat_class)) +
  theme_fivethirtyeight() +
  labs(title="Treemap showing decision boundaries for classifying wheat kernels",
       subtitle="Two features i.e. area, length of kernel groove identified by first making a decision tree",
       caption=glue("Source: {DATASET_URL}"))
p

## Fit a decision tree using the same variables as the above plot
tree = rpart(wheat_class ~ length_of_kernel_groove + area, data = wheat)

## Visualise the tree partitions by adding it via geom_parttree()
p <- p +  
  geom_parttree(data = tree, aes(fill=wheat_class), alpha = 0.1) +
  labs(caption = "Note: Points denote observed data. Shaded regions denote tree predictions.")

p

fit <- rpart(wheat_class ~ ., data = wheat, method = 'class')
rpart.plot(fit, box.palette = "RdBu",
           shadow.col="gray",
           nn=TRUE, 
           main="Decision tree for wheat kernels")
