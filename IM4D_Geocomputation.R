###################################
# Dummy for Dummies---Lecture 3 #
# Transportation #
# Lecturer: SHI Shuang #
###################################

#source: https://github.com/robinlovelace/geocompr


#install.packages("devtools")
#install.packages("XML", type = "binary")
#devtools::install_github("robinlovelace/geocompr")
#devtools::install_github("geocompr/geocompkg", dependencies = TRUE)
#install.packages("odbc")

library(sf)
library(dplyr)
library(spDataLarge)  # load larger geographic data: TTWA(Travel to Work Areas)
library(stplanr)      # geographic transport data package
library(tmap)         # visualization package
library(osrm)
library(mapview)
library(tidyverse)


# view input data
summary(bristol_ways)
summary(bristol_ttwa)
summary(bristol_region)


ggplot( )+
  geom_sf(data=bristol_ttwa, fill="white") +
  geom_sf(data=bristol_region, fill="yellow") +
  coord_sf(crs = st_crs(3035))

#A case study of Bristol
region_all = rbind(bristol_region, bristol_ttwa)


tmap_mode("view") #Draw a thematic map quickly
qtm(bristol_ways, lines.col = "highway", lines.lwd = 3, lines.palette = c("green", "black", "red")) + 
  tm_scale_bar() +
  tm_shape(region_all) +
  tm_borders(lwd = c(5, 7), col = "darkblue") 

#area level analysis: administrative polygons
names(bristol_zones)
head(bristol_zones)

names(bristol_od) #check variable names, origin and destination
head(bristol_od)

nrow(bristol_od) #we have 2910 pairs between districts
nrow(bristol_zones) 

zones_attr = bristol_od %>% 
  group_by(o) %>% 
  summarize_if(is.numeric, sum) %>% 
  dplyr::rename(geo_code = o)

zones_attr$geo_code %in% bristol_zones$geo_code #are geo_code included in administrative dataset?
#summary(zones_attr$geo_code %in% bristol_zones$geo_code)

zones_joined = left_join(bristol_zones, zones_attr, by = "geo_code") #https://dplyr.tidyverse.org/reference/join.html
sum(zones_joined$all)
names(zones_joined)

zones_od = bristol_od %>% 
  group_by(d) %>% 
  summarize_if(is.numeric, sum) %>% 
  dplyr::select(geo_code = d, all_dest = all) %>% #keep and rename 
  inner_join(zones_joined, ., by = "geo_code")

library(tmap)
tmap_mode("plot")
tm_shape(zones_od) + tm_fill(c("all", "all_dest"), 
                             palette = viridis::plasma(4),
                             breaks = c(0, 2000, 4000, 10000, 50000),
                             title = "Trips")  +
  tm_borders(col = "black", lwd = 0.5) + 
  tm_facets(free.scales = FALSE, ncol = 2) +
  tm_layout(panel.labels = c("Zone of origin", "Zone of destination"))

#shortest path
od_top5 = bristol_od %>% 
  arrange(desc(all)) %>%  #sort decending order
  top_n(5, wt = all) #to arrange the OD data by all trips and then filter-out only the top 5

bristol_od$Active = (bristol_od$bicycle + bristol_od$foot) /
  bristol_od$all * 100 #the percentage of each desire line that is made by these active modes

od_intra = filter(bristol_od, o == d) #intra district
od_inter = filter(bristol_od, o != d) #inter district

desire_lines = od2line(od_inter, zones_od)

tmap_mode("plot")
desire_lines_top5 = od2line(od_top5, zones_od)
# tmaptools::palette_explorer()
tm_shape(desire_lines) +
  tm_lines(palette = "plasma", breaks = c(0, 5, 10, 20, 40, 100),
           lwd = "all",
           scale = 9,
           title.lwd = "Number of trips",
           alpha = 0.6,
           col = "Active",
           title = "Active travel (%)"
  ) +
  tm_shape(desire_lines_top5) +
  tm_lines(lwd = 5, col = "black", alpha = 0.7) +
  tm_scale_bar()


#identify a real route
desire_lines$distance = as.numeric(st_length(desire_lines)) #st_length function computes the length of a LINESTRING or MULTILINESTRING geometry.

desire_carshort = dplyr::filter(desire_lines, car_driver > 300 & distance < 5000) #car trips that can be replaced by cycling

route_carshort = route(
  l = desire_carshort,
  route_fun = osrmRoute,
  returnclass = "sf" # argument passed to route_fun
)

desire_carshort$geom_car = st_geometry(route_carshort) #plot only the geometry
mapview::mapview(desire_carshort$geom_car)


desire_rail = top_n(desire_lines, n = 3, wt = train)
ncol(desire_rail)
#> [1] 10
desire_rail = line_via(desire_rail, bristol_stations)
ncol(desire_rail)
#> [1] 13


route_rail = desire_rail %>%
  st_set_geometry("leg_orig") %>% 
  route(l = ., route_fun = osrmRoute, returnclass = "sf") %>% 
  select(names(route_carshort))

route_cycleway = rbind(route_rail, route_carshort)
route_cycleway$all = c(desire_rail$all, desire_carshort$all)

qtm(route_cycleway, lines.lwd = "all")


tmap_mode("plot")
bristol_stations_top = bristol_stations[desire_rail, , op = st_is_within_distance, dist = 500] #[] extract a list
m_leaflet = tm_shape(bristol_ttwa) +
  tm_borders(col = "darkblue") +
  tm_shape(bristol_ways) +
  tm_lines(col = "highway", lwd = 3, palette = c("lightgreen", "grey", "pink")) +
  tm_scale_bar() +
  tm_shape(route_cycleway) +
  tm_lines(col = "blue", lwd = "all", scale = 20, alpha = 0.6) +
  tm_shape(bristol_stations_top) +
  tm_dots(size = 0.3, col = "red") +
  tm_layout(legend.position = c("LEFT", "TOP"))

m_leaflet 



