library(glue)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(plotly)
library(viridis)
library(factoextra)


# read the wheat dataset.
DATASET_URL <- "https://raw.githubusercontent.com/jbrownlee/Datasets/master/wheat-seeds.csv"
wheat <- read_csv(DATASET_URL, col_names = FALSE)
names(wheat) <- c("area", "perimeter", "compactness", "kernel_length", "kernel_width", "asymmetry_coefficient",
                  "length_of_kernel_groove", "wheat_class")
wheat <- wheat %>%
  mutate(wheat_class=as.factor(wheat_class))
head(wheat)



# without PCA
kd <- with(wheat, MASS::kde2d(kernel_length, kernel_width, n = 100))
fig1 <- plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% add_surface()

fig1


#install.packages("factoextra")

# with PCA
res.pca <- prcomp(wheat %>% select(-wheat_class), scale = TRUE)
res.pca$x

pca_df <- data.frame(PC1=res.pca$x[,1], PC2=res.pca$x[,2])
kd <- with(pca_df, MASS::kde2d(PC1, PC2, n = 50))
fig2 <- plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% 
  add_surface() %>%
  layout(title = '\n<b>Wheat Seed Classification Using First Two Principal Components<br>Density plot identifies the three classes present in the data.',
         scene = list(xaxis = list(title = "PC1"),
                      yaxis = list(title = "PC2"),
                      zaxis = list(title = "Density")))


fig2

