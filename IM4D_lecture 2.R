###################################
# Dummy for Dummies---Lecture 2 #
# Draw Maps #
# Lecturer: Shawn SHI #
###################################

library(tidyverse) #include ggplot2, dplyr, forcats etc
library(readxl) #read excel
library(scales) #change breaks in x and y axis
library(ggspatial) #create a scale bar on a map
library(foreign) #reading and wirting data from other source. e.g dta file
library(classInt) #making intervals for choropleth map


library(sf) #simple features, a standardized way to encode spatial vector data
library(ggmap) #load in basemap from other source
library(tmap) #Choropleth map making
library(leaflet) #interactive maps(panning/zooming)

library(rnaturalearth) #https://www.naturalearthdata.com/
library(rnaturalearthdata)
library(tigris) #TIGER/Line shapefiles from the United States Census Bureau (<https://www.census.gov/geo/maps-data/data/tiger-line.html>) 
library(osmdata)


#set working directory
setwd("D:/Dropbox/SOE0330/LaTex/R course/ECON4016-7930/Lecture5_2020Spring")
getwd()


##########################
# Draw map with shapefile #
##########################

# Download the shapefile. (note that I store it in a folder called DATA. You have to change that if needed.)
# download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="shapefile/world_shape_file.zip")
# Unzip this file. You can do it with R (as below), or clicking on the object you downloaded.

# Read this shape file with the rgdal library. 
library(rgdal)
library(sp)
my_spdf <- readOGR("shapefile/world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp") 
plot(my_spdf)

summary(my_spdf) #tells you the max and min coordinates, the kind of projection in use
length(my_spdf) #how many regions you have
head(my_spdf@data) #the firs few rows of the data slot associated with the regions


#########################
# Draw map with polygon #
#########################

rm(list = ls(all.names = TRUE))

# Load data from library 
world <- ne_countries(scale = "medium", returnclass = "sf") 
world

# Data and basic plot (ggplot and geom_sf)
ggplot(data = world) +
  geom_sf()

# Add Title, subtitle, and axis labels (ggtitle, xlab, ylab)
ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)")) #append function using separation factor

# Map color (geom_sf)
ggplot(data = world) + 
  geom_sf(color = "black", fill = "lightgreen")

ggplot(data = world) +
  geom_sf(aes(fill = pop_est), color="white", size=0.5) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") #square root transformation

#https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html

# Projection and extent (coord_sf)
ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ") #EPSG:3035

#where to check the projection: https://spatialreference.org/ref/

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "+init=epsg:3035")

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = st_crs(3035))

# Restrict to China
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(73, 136), ylim = c(10, 54), expand = FALSE)

# Scale bar and North arrow (package ggspatial)
ggplot(data = world) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", #or you can use tl
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(73, 136), ylim = c(10, 54))

# Country names and other names (geom_text and annotate)
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry))) 

#retrieve coordinates in matrix form
#

ggplot(data = world) +
  geom_sf() +
  geom_text(data= world_points,aes(x=X, y=Y, label=name),
            color = "darkblue", check_overlap = T, fontface = "bold", size=3) +
  annotate(geom = "text", x = 114, y = 16, label = "South China Sea", 
           fontface = "italic", color = "grey22", size = 4) +
  coord_sf(xlim = c(73, 136), ylim = c(10, 54), expand = FALSE)


# Final map
ggplot(data = world) +
  geom_sf(fill= "antiquewhite") + 
  geom_text(data= world_points,aes(x=X, y=Y, label=name),
            color = "darkblue", check_overlap = FALSE, fontface = "bold", size=3) +
  annotate(geom = "text", x = 114, y = 16, label = "South China Sea", 
           fontface = "italic", color = "grey22", size = 4) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(73, 136), ylim = c(10, 54), expand = FALSE)+ 
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle("Map of China and South China Sea") + 
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))

# Saving the map with ggsave
ggsave("china_map.pdf")
ggsave("china_map.png", width = 6, height = 6, dpi = "screen")


##############
# Add layers #
##############

# import the location data

# coronavirus  <- read.dta("coronavirus_china_data.dta")
coronavirus <- read_excel("coronavirus_china.xls") # a trick to transform everything tp UTF-8
sites <- subset(coronavirus, date=="2020-01-23" & !is.na(coronavirus$x)) #check for NA

# add location layers
ggplot(data = world) +
  geom_sf() +
  geom_text(data= world_points,aes(x=X, y=Y, label=name),
            color = "darkblue", check_overlap = FALSE, fontface = "bold", size=3) +
  geom_point(data=sites, aes(x=x, y=y), size=2) +
  coord_sf(xlim = c(73, 136), ylim = c(10, 54), expand = FALSE)


ggplot(data = world) +
  geom_sf() +
  geom_text(data= world_points,aes(x=X, y=Y, label=name),
            color = "darkblue", check_overlap = FALSE, fontface = "bold", size=3) +
  geom_point(data=sites, aes(x=x, y=y, size=confirmed), color="red", alpha=0.3) +
  coord_sf(xlim = c(73, 136), ylim = c(10, 54), expand = FALSE)


#######################################
# Import a shapefile + Attribute Join #
#######################################

# Import a shapefiles
unzip("city2007.zip")

chinacity <- st_read("city2007/city_region.shp")
chinacity

ggplot() +
  geom_sf(data=chinacity) +
  ggtitle("China Prefecture Map") +
  coord_sf()

# suppose we want to plot the cumulative confirmed cases from 2019-12-01 to 2020-02-25, then first we have to caclulate the cumulated value by aggregating the new confirmed cases each day
unique(coronavirus$date) #show the time span

coronavirus_city <- subset(coronavirus, !is.na(coronavirus$cityid)) # drop if cityid==.

# aggregate confirmed cases by city id
coronavirus_cumu <-
  coronavirus_city %>%
  group_by(cityid) %>%
  summarise(confirmed = sum(confirmed, na.rm = T)) #sm only non missing and non string

# join a data frame to a spatial data frame by an attribute 
chinacity2 <- geo_join(chinacity, coronavirus_cumu, 'CNTY_CODE', 'cityid')

# make a map!
ggplot() +
  geom_sf(data=subset(chinacity2, !is.na(confirmed) & confirmed!=0), aes(fill=confirmed), color="white", size=0.1) +
  geom_sf(data=subset(chinacity2, is.na(confirmed) | confirmed==0), fill="gainsboro", color="white", size=0.1) +
  scale_fill_gradient(trans = "log2") + #why log2?
  ggtitle("China Cumulative Confirmed Cases, by 25 Feb 2020") +
  coord_sf()

# If we wanted to plot our map as a choropleth map we need to convert our continouse variable into a categoriacal one

# This requires two steps:
## Determine the quantile breaks;
## Add a categorical variable to the object which assigns each continious vaule to a bracket
breaks_qt <- classIntervals(chinacity2$confirmed, n = 7, style = "quantile") #or we can use equal/kmeans(clustering)
breaks_qt

# We use cut to confirmed case variable into intervals and code them according to which interval they are in
# Lastly, we can use scale_fill_brewer() and add our color palette
chinacity2 <- 
  mutate(chinacity2, confirmed_cat = cut(confirmed, breaks_qt$brks)) 

ggplot(chinacity2) + 
  geom_sf(aes(fill=confirmed_cat)) +
  scale_fill_brewer(palette = "OrRd") +
  ggtitle("China Cumulative Confirmed Cases, by 25 Feb 2020") +
  coord_sf()


##############################
# Adding basemaps with ggmap #
##############################

# An simple example! Let us find hkbu first! 
hkbu <- data.frame(longitude = c(114.18, 114.20), latitude = c(22.35, 22.33))
hkbu

hkbubox <-make_bbox(lon=hkbu$longitude, lat=hkbu$latitude, f =0.1) #how much fraction should I extend out
hkbubox

# get a google api to download the basemap from google
register_google(key = "your key")

hkbu_map <- get_map(location = hkbubox, maptype = "terrain", source = "google")
ggmap(hkbu_map)

hk <- data.frame(longitude = c(113.8, 114.5), latitude = c(22.1, 22.5))
hkbox <-make_bbox(lon=hk$longitude, lat=hk$latitude, f = .10)
hk_map <- get_map(location = hkbox, maptype = "terrain", source = "osm")
ggmap(hk_map)

gz <- data.frame(longitude = c(109.6, 117.4), latitude = c(20.1, 25.5))
gzbox <-make_bbox(lon=gz$longitude, lat=gz$latitude, f = .10)
gz_map <- get_map(location = gzbox, maptype = "terrain", source = "osm")
ggmap(gz_map)


china <- data.frame(longitude = c(75, 135), latitude = c(20, 54))
chinabox <-make_bbox(lon=china$longitude, lat=china$latitude, f = .10)
china_map <- get_map(location = chinabox, maptype = "terrain", source = "osm")
ggmap(china_map)

# get road map from open street project  https://www.openstreetmap.org/
kowloon <- opq(bbox =  c(114.16, 22.35, 114.22, 22.30)) %>% #internal API
  add_osm_feature(key = 'highway') %>% #add feature (https://wiki.openstreetmap.org/wiki/Map_Features)
  osmdata_sf() %>% 
  osm_poly2line() #Convert osmdata polygons into lines

# "highway" %in% names(kowloon$osm_line)

head(kowloon$osm_line)

kowloon_center <- kowloon$osm_lines %>% 
  select(highway)

ggplot(data = kowloon_center) + geom_sf()


# plot a route
ride <- read.csv("bike-ride.csv")
head(ride)

bike <- data.frame(longitude = c(-122.10, -122.06), latitude = c(36.95, 36.99))
bikebox <-make_bbox(lon=bike$longitude, lat=bike$latitude, f =0.1) #how much fraction should I extend out
bike_map <- get_map(location = bikebox, maptype = "terrain", source = "google")
ggmap(bike_map)

ggmap(bike_map) + 
  geom_path(data = ride, aes(color = elevation), size = 3, lineend = "round") + 
  scale_color_gradientn(colours = rainbow(7), breaks = seq(25, 200, by = 25))


##################################################
# Creating a spatial object from a lat/lon table #
##################################################

# download and prepare data
download.file("http://bit.ly/R-spatial-data", "R-spatial-data.zip")
unzip("R-spatial-data.zip", exdir = "data")

# read in the csv file
philly_homicides_df <- read.csv("data/philly_homicides.csv")
str(philly_homicides_df) # we have longitude/latitude information inside

# create an sf object from a data frame using the st_as_sf() function
philly_homicides_sf <- st_as_sf(philly_homicides_df, coords = c("POINT_X", "POINT_Y"))
str(philly_homicides_sf) 

# To make it a complete geographical object we assign the WGS84 projection, which has the EPSG code 4326:

st_crs(philly_homicides_sf) <- 4326 # we can use EPSG as numeric here
st_crs(philly_homicides_sf) #Retrieve coordinate reference system from object

# save the object as shapefile on our hard drive for later use
st_write(philly_homicides_sf, "data/PhillyHomicides", driver = "ESRI Shapefile")

# to force the save: 
st_write(philly_homicides_sf, "data/PhillyHomicides", driver = "ESRI Shapefile", delete_layer = TRUE)


#########################
# Choropleth with tmap #
########################

# load in the crime rate data
philly_crimes_sf <- st_read("data/PhillyCrimerate/PhillyCrimerate.shp")
philly_crimes_sf

# get a base map
ph_basemap <- get_stamenmap(location=c(lon = -75.16522, lat = 39.95258), zoom=11, maptype = 'terrain-background') #http://maps.stamen.com/#toner/12/37.7706/-122.3782
ggmap(ph_basemap)

tm_shape(philly_crimes_sf) +
  tm_polygons("homic_rate", 
              style="quantile", 
              title="Philadelphia \nhomicide density \nper sqKm")

tmap_mode("view") #interactive viewing
tmap_last() #Retrieve the last map to be modified or created

# The tmap library also includes functions for simple spatial operations, geocoding and reverse geocoding using OSM. For more check vignette("tmap-getstarted")

############################
# Web mapping with leaflet #
############################

# http://leaflet-extras.github.io/leaflet-providers/preview/

#discover New England
new_england %>% 
  leaflet() %>% 
  addProviderTiles(providers$MtbMap) %>%
  setView(lng = -71.05, lat = 42.36, 
          zoom = 12) %>% 
  addProviderTiles(providers$Stamen.TonerLines,
                   options = providerTileOptions(opacity = 0.35)) %>%
  addProviderTiles(providers$Stamen.TonerLabels)



#discover Providence
leaflet() %>% # Leaflet map widget
  addTiles() %>% # OpenStreetMap default
  setView(lat = 41.82, 
          lng = -71.40, 
          zoom = 15) %>% 
  addMarkers(lat = 41.82,
             lng = -71.40, 
             popup = "Brown Univeristy") %>% 
  addProviderTiles(provider =
                     providers$OpenTopoMap) %>%
  addCircles(lat = 41.82,
             lng = -71.40,
             radius = 500,
             color = "orange",
             fillColor = "red") #%>% 
  setView(lat = 41.82,
          lng = -71.40,
          zoom = 2)  


# reproject
philly_WGS84 <- st_transform(philly_crimes_sf, 4326)

leaflet(philly_WGS84) %>%
  addPolygons() #dd uniform polygons with default styling

# set a fillColor for each polygon based on homic_rate and make it look nice by adjusting fillOpacity and smoothFactor (how much to simplify the polyline on each zoom level)
pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)

# add a popup with the homic_rate values. We will create as a vector of strings, that we then supply to addPolygons()
p_popup <- paste0("<strong>Homicide Density: </strong>", philly_WGS84$homic_rate)

leaflet(philly_WGS84) %>%
  addPolygons(
    stroke = FALSE, # remove polygon borders
    fillColor = ~pal_fun(homic_rate), # set fill color with function from above and value
    fillOpacity = 0.8, smoothFactor = 0.5, # make it nicer
    popup = p_popup)  # add popup


# Add a basemap, which defaults to OSM, with addTiles()
leaflet(philly_WGS84) %>%
  addPolygons(
    stroke = FALSE, 
    fillColor = ~pal_fun(homic_rate),
    fillOpacity = 0.8, smoothFactor = 0.5,
    popup = p_popup) %>%
  addTiles()

# Add a legend using addLegend()

leaflet(philly_WGS84) %>%
  addPolygons(
    stroke = FALSE, 
    fillColor = ~pal_fun(homic_rate),
    fillOpacity = 0.8, smoothFactor = 0.5,
    popup = p_popup) %>%
  addTiles() %>%
  addLegend("bottomright",  # location
            pal=pal_fun,    # palette function
            values=~homic_rate,  # value to be passed to palette function
            title = 'Philadelphia homicide density per sqkm') # legend title

# Set the labels for our breaks manually 
## get quantile breaks. Add .00001 offset to catch the lowest value
breaks_qt <- classIntervals(philly_crimes_sf$homic_rate, n = 5, style = "quantile")
breaks_qt

library(RColorBrewer)

leaflet(philly_WGS84) %>%
  addPolygons(
    stroke = FALSE, 
    fillColor = ~pal_fun(homic_rate),
    fillOpacity = 0.8, smoothFactor = 0.5,
    popup = p_popup) %>%
  addTiles() %>%
  addLegend("bottomright", 
            colors = brewer.pal(5, "YlOrRd"), 
            labels = paste0("up to ", format(breaks_qt$brks[-1], digits = 2)),
            title =  'Philadelphia homicide density per sqkm')


# Add a control to switch to another basemap from "Carto", and option to turn the polygon off and on

leaflet(philly_WGS84) %>%
  addPolygons(
    stroke = FALSE, 
    fillColor = ~pal_fun(homic_rate),
    fillOpacity = 0.8, smoothFactor = 0.5,
    popup = p_popup,
    group = "philly") %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "Carto") %>%
  addLegend("bottomright", 
            colors = brewer.pal(5, "YlOrRd"), 
            labels = paste0("up to ", format(breaks_qt$brks[-1], digits = 2)),
            title = 'Philadelphia homicide density per sqkm') %>%
  addLayersControl(baseGroups = c("OSM", "Carto"), 
                   overlayGroups = c("philly"))  



