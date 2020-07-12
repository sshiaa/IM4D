#####################################
# Dummy for Dummies---Lecture 1 #
    # Lecturer: SHI Shuang #
#####################################

#install.packages(c("tidyverse", "readxl", "ggrepel", "scales", "quantmod"))
#install.packages(c("googleway", "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

library(tidyverse) #include ggplot2, dplyr, forcats etc
library(readxl) #read excel
library(scales) #change breaks in x and y axis
library(quantmod) #Quantitative Financial Modelling Framework

library(sf) #simple features, a standardized way to encode spatial vector data
library(ggspatial)
library(rnaturalearth) #https://www.naturalearthdata.com/
library(rnaturalearthdata)
library(foreign) #reading and wirting data from other source. e.g dta file
library(lubridate) #work with dates and times


#set working directory
setwd("D:/Dropbox/SOE0330/LaTex/R course/ECON4016-7930/Lecture4_2020Spring")

#import dataset
df <- data.frame(region=c("CA", "MA", "DC"),len=c(4.2, 10, 29.5))
head(df) #have a look at the data

excel_df <- read_excel("df.xlsx")
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

read_csv()


######################
# ggplot---bar plot #
######################

df <- read_excel("df.xlsx")

# Simple barplot
bargraph <-ggplot(data=df, aes(x=dose, y=len)) +geom_bar(stat="identity") 
bargraph 

# Horizontal bar plot
bargraph + coord_flip()

# Change the width of bars
ggplot(data=df, aes(x=dose, y=len)) + geom_bar(stat="identity", width=0.5)

# Change colors
ggplot(data=df, aes(x=dose, y=len)) +geom_bar(stat="identity", color="blue", fill="white") 

# Minimal theme + blue fill color
basic <-ggplot(data=df, aes(x=dose, y=len)) + geom_bar(stat="identity", fill="steelblue")+ theme_minimal()
basic

# Choose which ones to display
basic + scale_x_discrete(limits=c("D0.5", "D2")) 

# Labels: outside bars
ggplot(data=df, aes(x=dose, y=len)) + geom_bar(stat="identity", fill="steelblue")+ geom_text(aes(label=len), vjust=-0.3, size=3.5)

# Inside bars
ggplot(data=df, aes(x=dose, y=len)) + geom_bar(stat="identity", fill="steelblue")+ geom_text(aes(label=len), vjust=1.6, color="white", size=3.5)


# Stacked barplot with multiple groups
insurance <- read_excel("insurance.xlsx")
ggplot(data=insurance, aes(x=insurance, y=rate, fill=age)) + geom_bar(stat="identity")
basic <-ggplot(data=insurance, aes(x=reorder(insurance, order), y=rate, fill=age)) + geom_bar(stat="identity", position="stack") 
basic

# Use custom colors
basic + scale_fill_manual(values=c('#76232f','#ff9e1b', '#cf4520'))

# Use brewer color palettes
basic + scale_fill_brewer(palette="Blues")

# Modify legend titles
basic + scale_fill_manual(values=c('#76232f','#ff9e1b', '#cf4520')) + labs(fill = "Age Groups")

# Edit legend title and labels
advance <- basic + scale_fill_manual(values=c('#76232f','#ff9e1b', '#cf4520'), labels = c("65+", "35-64", "19-35")) + labs(fill = "Age Groups") 
advance

# Edit x and y label
advance + xlab("Insurance Type") + ylab("Percentage of Population") + ggtitle("HEALTHCARE FACTS")

rm(list = ls(all.names = TRUE))



######################
# ggplot---line plot #
######################

# Simple line plot 
head(economics) #https://fred.stlouisfed.org/
basic<- ggplot(economics, aes(x = date, y = psavert)) + geom_line(color = "indianred3", size=1) 
basic
basic + labs(title = "Personal Savings Rate", x = "Date", y = "Personal Savings Rate")
basic + labs(title = "Personal Savings Rate", x = "Date", y = "Personal Savings Rate") + geom_smooth() 

advance <- basic + labs(title = "Personal Savings Rate", x = "Date", y = "Personal Savings Rate") + geom_smooth() 

# Add x-axis sclae 
advance + scale_x_date(date_breaks = '5 years', labels = date_format("%b-%y")) + 
  labs(title = "Personal Savings Rate", subtitle = "1967 to 2015", x = "", y = "Personal Savings Rate") +
  theme_minimal()

############################
# multivariate time series #
############################
## get apple (AAPL) closing prices
getSymbols("AAPL", return.class = "data.frame", from="2019-07-01")


apple <- AAPL %>% 
  mutate(Date = as.Date(row.names(.))) %>%
  select(Date, AAPL.Close) %>%
  rename(Close = AAPL.Close) %>%
  mutate(Company = "Apple")

#have a look at the data
head(AAPL)
head(apple)

## get facebook (FB) closing prices
getSymbols("FB", return.class = "data.frame", from="2019-07-01")

facebook <- FB %>% 
  mutate(Date = as.Date(row.names(.))) %>%
  select(Date, FB.Close) %>%
  rename(Close = FB.Close) %>%
  mutate(Company = "Facebook")

## Append data for both companies
stockprice <- rbind(apple, facebook) 

## plot data
ggplot(stockprice,  aes(x=Date, y= Close, color=Company)) + geom_line(size=1) + 
  scale_x_date(date_breaks = '1 month', labels = scales::date_format("%b")) +
  scale_y_continuous(limits = c(100, 350), 
                     breaks = seq(100, 300 , 50),
                     labels = scales::dollar) + labs(title = "NASDAQ Closing Prices",
                                                     subtitle = "July 2019 - July 2020",
                                                     caption = "source: Yahoo Finance",
                                                     y = "Closing Price") +
  theme_minimal() + 
  scale_color_brewer(palette = "Dark2")


####################
# Plot area charts #
####################
# basic area chart
ggplot(economics, aes(x = date, y = psavert)) + geom_area(fill="lightblue", color="black") + labs(title = "Personal Savings Rate",
       x = "Date",
       y = "Personal Savings Rate")

#######################
# Stacked area charts #
#######################
#install.packages("gcookbook") #Data sets used in the book "R Graphics Cookbook" by Winston Chang, published by O'Reilly Media.
data(uspopage, package = "gcookbook") #Age distribution of population in the United States, 1900-2002
ggplot(uspopage, aes(x = Year, y = Thousands, fill = AgeGroup)) + geom_area() + labs(title = "US Population by age",
       x = "Year",
       y = "Population in Thousands")

# Make it prettier 
ggplot(uspopage, aes(x = Year, y = Thousands/1000,  fill = forcats::fct_rev(AgeGroup))) + geom_area() +
  labs(title = "US Population by age",
       subtitle = "1900 to 2002",
       caption = "source: U.S. Census Bureau, 2003, HS-3",
       x = "Year",
       y = "Population in Millions",
       fill = "Age Group") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()


#scatter plot
library(gapminder) #cross country level social-economic factors
library(ggrepel) #repel overlapping text labels

head(gapminder)

basic <- ggplot(data = subset(gapminder, continent=="Asia" & year==2007),aes(x=log(gdpPercap), y=lifeExp))+ geom_point()
basic + geom_text(mapping = aes(label=country))
basic + geom_point() + geom_text_repel(mapping = aes(label=country))


#set working directory
setwd("D:/Dropbox/SOE0330/LaTex/R course/ECON4016-7930/Lecture5_2020Spring")

#########################
# Draw map with polygon #
#########################

# Load data from sf library 
world <- ne_countries(scale = "medium", returnclass = "sf") 
class(world)

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
coronavirus  <- read.dta("coronavirus_china_data.dta")
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
unique(coronavirus$date) #duplicates drop

coronavirus_city <- subset(coronavirus, !is.na(coronavirus$cityid))

# aggregate confirmed cases by city id
coronavirus_cumu <-
  coronavirus_city %>%
  group_by(cityid) %>%
  summarise(confirmed = sum(confirmed, na.rm = T)) #sm only non missing and non string

# another way: generate a cumulative sum
coronavirus_city$date <- ymd(coronavirus_city$date)

coronavirus_city <-
  coronavirus_city %>%
  arrange(cityid, date)

coronavirus_city <-
  coronavirus_city %>%
  arrange(date, cityid)

coronavirus_city <-
  coronavirus_city %>%
  group_by(cityid) %>%
  mutate(cumu_confimred=cumsum(confirmed))

# Can we then subset the data at date=2020-02-25 (the last day) to get the cumulateive aggregate?

# May not! If the data only have observation when there is updated number for one particular date ---> An issue of unbalanced panel 

# To get a balanced panel data

#install.packages("plm")
library(plm)
coronavirus_city <- pdata.frame(coronavirus_city, index = c("cityid", "date"), drop.index = F)
pdim(coronavirus_city)$balanced
coronavirus_bal <- make.pbalanced(coronavirus_city)

# it does not work! another way?
library(tidyr)
w_coronavirus_city <- 
  coronavirus_city %>% 
  spread(key=date, value=confirmed)

coronavirus_city <- 
  w_coronavirus_city %>% 
  gather(`2019-12-01`: `2020-02-25`, key = "date", value = "confirmed")

# remove duplicates cases
coronavirus_city <- distinct(coronavirus_city, cityid, date, .keep_all = T)

# generate the cumulative aggregate again!
coronavirus_city <-
  coronavirus_city %>%
  group_by(cityid) %>%
  mutate(cumu_confimred=cumsum(confirmed))

# join a data frame to a spatial data frame by an attribute 
#install.packages("tigris")
library(tigris)
chinacity2 <- geo_join(chinacity, coronavirus_cumu, 'CNTY_CODE', 'cityid')

# make a map!
ggplot() +
  geom_sf(data=subset(chinacity2, !is.na(confirmed) & confirmed!=0), aes(fill=confirmed), color="white", size=0.1) +
  geom_sf(data=subset(chinacity2, is.na(confirmed) | confirmed==0), fill="gainsboro", color="white", size=0.1) +
  scale_fill_gradient(trans = "log10") + 
  ggtitle("China Cumulative Confirmed Cases, by 25 Feb 2020") +
  coord_sf()

# If we wanted to plot our map as a 鈥榯rue�?? choropleth map we need to convert our continouse variable into a categoriacal one, according to whichever brackets we want to use
# This requires two steps:
## Determine the quantile breaks;
## Add a categorical variable to the object which assigns each continious vaule to a bracket
library(dplyr)
library(classInt)
breaks_qt <- classIntervals(c(min(chinacity2$confirmed) - .00001, chinacity2$confirmed), n = 7, style = "quantile")
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

install.packages("ggmap")
library(ggmap)

# An simple example! Let us find hkbu first! 
hkbu <- data.frame(longitude = c(114.18, 114.20), latitude = c(22.35, 22.33))
hkbu

hkbubox <-make_bbox(lon=hkbu$longitude, lat=hkbu$latitude, f = .10)

# get a google api to download the basemap from google
register_google(key = "AIzaSyDsh0yKm-aUByFWfSdEgGGkqIUrZMBL7BU")

hkbu_map <- get_map(location = hkbubox, maptype = "terrain", source = "google")
ggmap(hkbu_map)


china <- data.frame(longitude = c(75, 135), latitude = c(20, 54))
chinabox <-make_bbox(lon=china$longitude, lat=hkbu$latitude, f = .10)
china_map <- get_map(location = chinabox, maptype = "terrain", source = "osm")

ggplot(china) + 
  geom_sf(data=chinacity2, aes(fill=confirmed_cat)) +
  scale_fill_brewer(palette = "OrRd") +
  ggtitle("China Cumulative Confirmed Cases, by 25 Feb 2020") +
  coord_sf()

# get road map from open street project 
install.packages("osmdata")
library(osmdata)

kowloon <- opq(bbox =  c(114.16, 22.35, 114.22, 22.30)) %>% 
  add_osm_feature(key = 'highway') %>% 
  osmdata_sf() %>% 
  osm_poly2line()

kowloon_center <- kowloon$osm_lines %>% 
  select(highway)

kowloon_center

ggplot(data = kowloon_center) + geom_sf()


# plot a route
ride <- read.csv("bike-ride.csv")

bikemap <- get_map(location = c(-122.080954, 36.971709), maptype = "terrain", source = "google", zoom = 14)

ggmap(bikemap) + 
  geom_path(data = ride, aes(color = elevation), size = 3, lineend = "round") + 
  scale_color_gradientn(colours = rainbow(7), breaks = seq(25, 200, by = 25))




##################################################
# Creating a spatial object from a lat/lon table #
##################################################
library(ggplot2)
library(sf)

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
st_crs(philly_homicides_sf)

st_crs(philly_homicides_sf) <- 4326 # we can use EPSG as numeric here
st_crs(philly_homicides_sf)

# save the object as shapefile on our hard drive for later use
st_write(philly_homicides_sf, "data/PhillyHomicides", driver = "ESRI Shapefile")
# to force the save: 
st_write(philly_homicides_sf, "data/PhillyHomicides", driver = "ESRI Shapefile", delete_layer = TRUE)

#########################
# Choropleth with tmap #
########################
library(tmap)

# load in the crime rate data
philly_crimes_sf <- st_read("data/PhillyCrimerate/PhillyCrimerate.shp")
philly_crimes_sf

# get a base map
ph_basemap <- get_map(location=c(lon = -75.16522, lat = 39.95258), zoom=11, maptype = 'terrain-background', source = 'stamen')
ggmap(ph_basemap)

tm_shape(philly_crimes_sf) +
  tm_polygons("homic_rate", 
              style="quantile", 
              title="Philadelphia \nhomicide density \nper sqKm")

tmap_mode("view")
tmap_last()

# The tmap library also includes functions for simple spatial operations, geocoding and reverse geocoding using OSM. For more check vignette("tmap-getstarted")

############################
# Web mapping with leaflet #
############################

# leaflet provides bindings to the 鈥楲eaflet�?? JavaScript library, 鈥渢he leading open-source JavaScript library for mobile-friendly interactive maps�??

library(leaflet) 

# reproject
philly_WGS84 <- st_transform(philly_crimes_sf, 4326)

leaflet(philly_WGS84) %>%
  addPolygons()

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
breaks_qt <- classIntervals(c(min(philly_crimes_sf$homic_rate) - .00001, philly_crimes_sf$homic_rate), n = 5, style = "quantile")
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



