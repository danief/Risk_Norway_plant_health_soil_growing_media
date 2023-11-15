# Importing necessary libraries
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(tidyverse)

# Retrieving world data with medium scale using "rnaturalearth" package
world <- ne_countries(scale = "medium", returnclass = "sf")

# Extracting the "sovereignt" column of the world data
world$sovereignt

# Defining the area of interest
area_interest <- c("Albania", "Andorra", "Austria", 
                   "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", 
                   "Croatia", "Cyprus", "Czech Republic", 
                   "Denmark", 
                   "Estonia", 
                   "Finland", "France", 
                   "Germany", "Greece", 
                   "Hungary", 
                   "Iceland", "Ireland", "Italy", 
                   "Kosovo", 
                   "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", 
                   "Malta", "Moldova", "Monaco", "Montenegro", 
                   "Netherlands", "Macedonia", "Norway", 
                   "Poland", "Portugal", "Romania", 
                   "Russia", 
                   "San Marino", "Republic of Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", 
                   "Ukraine", "United Kingdom", 
                   "Vatican City")

# Modifying the world data by adding a new column "area" indicating whether a country is in the area of interest or not
world_modified <- world %>% 
  mutate(area = ifelse(admin %in% area_interest, "Yes", "No"))

# Creating a ggplot object and adding a geom_sf layer to display the modified world data
ggplot() +
  geom_sf(data = world_modified, aes(fill=area)) +
  scale_fill_manual(values = c("#f98e09", "#5ec962")) +
  theme_bw() +
  coord_sf(xlim = c(-10, 60), ylim = c(35, 70)) +
  theme(legend.position = "none") +
  labs(title = "Map of Europe showing our area of interest", 
       subtitle = "Russia is included to 60 degrees east", 
       caption = "VKM - Source: Natural Earth")


# Saving the plot as a PNG file
ggsave("./Data/5_outputs/map_europe.png", width = 20, height = 20, units = "cm", dpi = 300)

# end