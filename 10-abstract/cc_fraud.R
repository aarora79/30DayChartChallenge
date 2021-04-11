library(glue)
library(dplyr)
library(tidyr)
library(Rtsne)
library(ggplot2)
library(ggrepel)
library(ggpmisc)
library(forcats)
library(ggthemes)
library(janitor)
library(lubridate)
library(tidyverse)


# read the data, transactions taken from the 
# BigQuery public datasets: bigquery-public-data:ml_datasets.ulb_fraud_detection
# Andrea Dal Pozzolo, Olivier Caelen, Reid A. Johnson and Gianluca Bontempi. Calibrating Probability with Undersampling for Unbalanced Classification. In Symposium on Computational Intelligence and Data Mining (CIDM), IEEE, 2015

fraud <- read_csv("10-abstract/cc_fraud.csv")
head(fraud)

# print the class counts
class_counts <- fraud %>%
  count(Class)
head(class_counts)

# convert the data to a matrix for the Rtsne package
fraud_matrix <- as.matrix(fraud %>%
                            select(contains('V')))
# fix a seed for repeatable results
set.seed(123)

# since the data features are already PCA components so we dont need to do PCA again
# there are some duplicates so set check_duplicates to FALSE
# we are OK with keeping just 2 dimensions with t-SNE, verified that it gives
# good results clearly distinguishing the two classes
tsne_out <- Rtsne(fraud_matrix,
                  pca=FALSE,
                  check_duplicates = FALSE,
                  dims = 2,
                  verbose = TRUE,
                  max_iter = 500) # Run TSNE

tsne_plot_data <- data.frame(x = tsne_out$Y[,1],
                             y = tsne_out$Y[,2], 
                             class = case_when(
                               fraud$Class == 0 ~ "Genuine",
                               fraud$Class == 1 ~ "Fraud",
                               TRUE ~ "unknown")) %>%
                  mutate(class = factor(class, levels = c("Genuine", "Fraud")) )  

tsne_plot_data %>%
  ggplot() +
  geom_point(aes(x=x, y=y, color=class)) + 
  scale_color_tableau() +
  #scale_color_manual(values = c(GENUINE_COLOR_VAL, FRAUD_COLOR_VAL)) + 
  labs(title="Using t-SNE to separate out <span style='color:#F28E2B'>**\"Fraud\"**</span> Vs <span style='color:#4E79E7'>**\"Genuine\"**</span> credit card transactions",
       subtitle = glue("The closeness of the points representing the fraud transactions show how t-SNE can reveal structures in high dimensional data!"),
       caption="Source: bigquery-public-data:ml_datasets.ulb_fraud_detection") +
  theme_fivethirtyeight() +
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_markdown()) +
  theme(axis.title = element_text()) +
  ylab('') +
  xlab("")


