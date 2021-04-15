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


exoplanets <- read_csv("https://raw.githubusercontent.com/OpenExoplanetCatalogue/oec_tables/master/comma_separated/open_exoplanet_catalogue_kepler.txt") %>%
  janitor::clean_names()

head(exoplanets)

exoplanets <- exoplanets %>%
  arrange(system_distance) %>%
  select(name,
         system_distance,
         radius,
         temperature,
         period,
         hoststar_temperature)
         #inclination , hoststar_radius , hoststar_mass, hoststar_age )

exoplanets <- exoplanets %>%
  mutate(radius_categorized = case_when(
    radius < 2.5 ~"small",
    radius >= 2.5 & radius < 5 ~ "medium",
    radius >= 5 & radius < 7.5 ~ "large",
    radius >= 7.5 ~ "very large"
  ))

exoplanets_largest_and_smallest <- exoplanets %>%
  filter(radius==max(radius, na.rm=TRUE) | radius==min(radius, na.rm=TRUE))

max_radius_distance_from_sun <- exoplanets_largest_and_smallest %>%
  filter(radius==max(radius, na.rm=TRUE)) %>%
  pull(system_distance)

max_radius_temperature <- exoplanets_largest_and_smallest %>%
  filter(radius==max(radius, na.rm=TRUE)) %>%
  pull(temperature)

max_radius_exoplanet_name <- exoplanets_largest_and_smallest %>%
  filter(radius==max(radius, na.rm=TRUE)) %>%
  pull(name)


min_radius_distance_from_sun <- exoplanets_largest_and_smallest %>%
  filter(radius==min(radius, na.rm=TRUE)) %>%
  pull(system_distance)

min_radius_temperature <- exoplanets_largest_and_smallest %>%
  filter(radius==min(radius, na.rm=TRUE)) %>%
  pull(temperature)

min_radius_exoplanet_name <- exoplanets_largest_and_smallest %>%
  filter(radius==min(radius, na.rm=TRUE)) %>%
  pull(name)

exoplanets %>%
  ggplot(aes(x=system_distance, y=temperature, col=temperature, size=radius)) +
  geom_point(alpha=0.2) +
  geom_point(data=exoplanets_largest_and_smallest, 
             aes(x=system_distance, y=temperature, size=radius), col="black", show.legend = FALSE) +
  scale_color_gradient_tableau() +
  labs(title="Distance of known \"Exoplanets\" from the sun and their surface temperatures",
       subtitle="The largest exoplanet is far and hot! The smallest one is near(er) and cool(er)!",
       caption="Source: https://github.com/OpenExoplanetCatalogue/oec_tables")+
  theme_fivethirtyeight() +
  theme(legend.position = "none", plot.subtitle = element_markdown()) +
  theme(axis.title = element_text()) +
  xlab("Distance from Sun [parsec]") +
  ylab("Surface or equilibrium temperature [K]") +
  annotate(geom = "text", x = max_radius_distance_from_sun+200, y = max_radius_temperature+100, label = max_radius_exoplanet_name, hjust = "left") +
  annotate(geom = "text", x = min_radius_distance_from_sun+75, y = min_radius_temperature+100, label = min_radius_exoplanet_name, hjust = "left")



