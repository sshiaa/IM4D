#####################################
# Dummy for Dummies---Lecture 3 #
# Voronoi Diagram #
# Lecturer: SHI Shuang #
#####################################
#install.packages("ggvoronoi")
#https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/JIISNB

library(tidyverse) 
library(readxl) 
library(scales) 
library(quantmod) 

library(sf) 
library(ggspatial)
library(rnaturalearth) 
library(rnaturalearthdata)
library(foreign) 
library(lubridate) 

library(ggmap)
library(maps) #https://www.rdocumentation.org/packages/maps/versions/3.3.0
library(mapdata)
library(readr)
library(ggvoronoi)

setwd("D:/Dropbox/SOE0330/LaTex/R course/spatial analysis/China_HSR_2016_stations")

mainstation <- read_excel("China_HSR_2016_stations.xlsx")

#directly plot the dots
mainstation_sf <- mainstation %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
ggplot() + geom_sf(data = mainstation_sf)


chn <- ggplot2::map_data('world2', region='china')
names(chn) <- c("lon","lat", "group","order","region","subregion")
chn
#write_excel_csv(chn, "chn.csv")
chn_outline <- chn %>% filter(group %in% c("13"))
chn_outline_detailed <- map_data("china")

mainstation %>%
  ggplot(aes(x=lon, y=lat)) +
  theme_void(base_family="Roboto Condensed") +
  geom_polygon(data=chn_outline,fill="#ffffff", color="dimgrey") +
  geom_point(aes(color=speed_kph),size=0.1, alpha=0.8) +
  scale_color_viridis_c(end=0.5, guide="none") +
  labs(title="Each HIgh-Speed Railway Station as a Point in Year 2016") +
  coord_quickmap()

mainstation %>%
  ggplot(aes(x=lon, y=lat)) +
  theme_void(base_family="Roboto Condensed") +
  geom_polygon(data=chn_outline,fill="#ffffff", color="dimgrey") +
  geom_path(stat="voronoi", size=0.1, aes(color=speed_kph)) +
  coord_quickmap() +
  scale_color_viridis_c(end=0.5, guide="none") +
  labs(title="Voronoi Diagram with station as a seed")

mainstation %>% 
  ggplot(aes(x=lon, y=lat)) +
  theme_void(base_family="Hiragino Sans W5") +
  geom_voronoi(aes(fill=lon), size=0.05, color="#ffffff", 
               outline=chn_outline) + 
  coord_quickmap() +
  scale_fill_viridis_c(end=0.8, option="magma", guide="none") 
