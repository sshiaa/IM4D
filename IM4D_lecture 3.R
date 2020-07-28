#####################################
# Dummy for Dummies---Lecture 3 #
# Raster and Geocoding #
# Lecturer: SHI Shuang #
#####################################


# General R setup ----
rm(list = ls())
getwd() 
setwd("D:/Dropbox/SOE0330/LaTex/R course/ECON4016-7930/Lecture6_2020Spring")

# Load new packages
#install.packages("pacman")
library(pacman) #Package Management Tool: It checks to see if a package is installed, if not it attempts to install the package.
p_load(rgeos, GISTools) #Load One or More Packages
p_load(lubridate, rgdal, raster, broom, rgeos, GISTools)
p_load(dplyr, sp, ggplot2, ggthemes, fields, magrittr, viridis, ggmap, readxl)

library(rnaturalearth) 
library(rnaturalearthdata)


# My ggplot2 theme
theme_ed <- theme(
  legend.position = "bottom",
  panel.background = element_rect(fill = NA), #rect: all rectangular elements
  # panel.border = element_rect(fill = NA, color = "grey75"),
  axis.ticks = element_line(color = "grey95", size = 0.3), #tick marks along axes
  panel.grid.major = element_line(color = "grey95", size = 0.3), #back ground
  panel.grid.minor = element_line(color = "grey95", size = 0.3),
  legend.key = element_blank()) #feature of legend


# The directory stored the nightlight data
# https://ngdc.noaa.gov/eog/dmsp/downloadV4composites.html
# https://ngdc.noaa.gov/eog/viirs/download_dnb_composites.html

#packageurl <- "https://cran.r-project.org/src/contrib/Archive/Rnightlights/Rnightlights_0.2.4.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")

# 1.1 read raster data
nl_world  <- raster("F101992.v4b_web.stable_lights.avg_vis.tif")
gz_t <- readOGR("gz.shp")
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf()

plot(gz_t)

# 1.2 plot and summarize a raster
plot(nl_world)

# change a color
library(viridis) #color plette pacakge
plot(nl_world, col = viridis(1e3), axes = F, box = F)


#clip out the region we want
nl_gz <- crop(nl_world, gz_t)
plot(nl_gz, col = viridis(1e3), axes = F, box = F)
lines(gz_t, col = "white", lwd = 0.8)

# R's description of the raster
nl_gz
# R's summary of the raster
nl_gz %>% summary()
# The slot names
nl_gz %>% slotNames()
# The first 6 values
nl_gz %>% getValues() %>% head()
# The first six coordinates
nl_gz %>% coordinates() %>% head()

# 1.3 reclassify value in a raster
## A histogram of the raster values:
ggplot(data = data.frame(light = getValues(nl_gz)),
       aes(x = light)) +
  geom_histogram(bins = 20, fill = viridis(20)) +
  ylab("Count") +
  xlab("radiance") +
  ggtitle("Distribution of light radiance near Guangzhou, China",
          subtitle = "1992") +
  theme_bw()

ggplot(data = data.frame(light = getValues(nl_gz)),
       aes(x = log(light))) +
  geom_histogram(bins = 20, fill = viridis(20)) +
  ylab("Count") +
  xlab("radiance") +
  ggtitle("Distribution of log(light) radiance near Guangzhou, China",
          subtitle = "1992") +
  theme_bw()

## reclassify value in a raster 
summary(nl_gz)
reclass_df <- c(-1, 9, 1,
                9, 24, 2,
                24, 50, 3,
                50, Inf, 4)
reclass_df

reclass_m <- matrix(reclass_df,
                    ncol = 3,
                    byrow = TRUE)
reclass_m


nl_gz_re <- reclassify(nl_gz, reclass_m)

nl_gz_re %>% summary()

plot(nl_gz_re, col = viridis(1e3), axes = F, box = F)
lines(gz_t, col = "white", lwd = 0.8)

plot(nl_gz, col = viridis(1e3), axes = F, box = F)
lines(gz_t, col = "white", lwd = 0.8)

## Convert raster to matrix and then to data frame
nl_gz_df <- nl_gz_re %>% rasterToPoints() %>% tbl_df()

names(nl_gz_df)

nl_gz_df %>% summary()

# 1.4 Rasters in ggplot2
## Convert raster to matrix and then to data frame
nl_gz_df <- nl_gz %>% rasterToPoints() %>% tbl_df()
## Plot with ggplot
ggplot(data = nl_gz_df,
       aes(x = x, y = y, fill = F101992.v4b_web.stable_lights.avg_vis)) +
  geom_raster() +
  ylab("") + xlab("") +
  ggtitle("Lights at night near Guangzhou",
          subtitle = "1992") +
  theme_bw()

ggplot(data = nl_gz_df,
       aes(x = x, y = y, fill = F101992.v4b_web.stable_lights.avg_vis)) +
  geom_raster() +
  ylab("") + xlab("") +
  ggtitle("Lights at night near Guangzhou",
          subtitle = "1992") +
  theme_bw()+
  theme(legend.position="bottom")


# 1.5 Masks (rgeos): take the union of the shenzhen block polygons---dissolve in GIS
sz_t <-  readOGR("sz.shp")
plot(sz_t)
sz_union <- gUnaryUnion(sz_t) #Functions for joining intersecting geometries.
plot(sz_union)

nl_sz <- crop(nl_world, sz_t)

# Mask night lights raster with Shenzhen's outline
lights_masked <- mask(x = nl_sz, mask = sz_union)
plot(lights_masked)
plot(nl_sz)

# Convert raster to matrix and then to data frame
masked_df <- lights_masked %>% rasterToPoints() %>% tbl_df()

# Plot with ggplot
ggplot(data = masked_df,
       aes(x = x, y = y, fill = F101992.v4b_web.stable_lights.avg_vis)) +
  geom_raster() +
  ylab("") + xlab("") +
  ggtitle("Lights at night in Shenzhen, masked",
          subtitle = "1992") +
  scale_fill_viridis(option = "D") +
  theme_bw()+
  theme(legend.position="bottom")
  

# 1.6 Summarize rasters with polygons
# Take averages of lights raster within each street block (jiedao and xiangzhen) 
gz_block_extract <- raster::extract(
  x = nl_gz,
  y = gz_t,
  fun = mean,
  na.rm = T, #remove missing
  sp = T) #should the extracted values be added to the data.frame of the Spatial object y

# 1.7 Summarizing rasters with points
# Create a point shapefile from xy coordinate table using SpatialPointsDataFrame()
## read in the csv file
sz_cases <- read.csv("sz_cases.csv")
# rearrange the order of cols in the dataframe 
sz_cases <- sz_cases[, c(1:4, 6, 5)]
str(sz_cases) # we have longitude/latitude information inside(show data structure)
## create object containing the CRS arguments
crs_sz <- crs(sz_t)
crs_sz
## create a pointshapefile
sz_cases_sp <- SpatialPointsDataFrame(sz_cases[, 5:6],
                                      sz_cases,
                                      proj4string = crs_sz)
plot(sz_t) 
plot(sz_cases_sp, add=T)

sz_pts_extract <- raster::extract(
  x = nl_sz,
  y = sz_cases_sp,
  fun = mean,
  na.rm = T,
  sp = T)

# 1.7.2 Compare the (approximate) densities of (logged) light for (1) all of shenzhen, (2) coronavirus locations
library(dplyr)
# Convert sz_pts_extract to a data frame
sz_pts_extract <- tbl_df(sz_pts_extract)
# Density plots
ggplot() +
  geom_density(
    data = masked_df,
    aes(x = log(F101992.v4b_web.stable_lights.avg_vis), color = "All of Shenzhen", fill = "All of Shenzhen"),
    alpha = 0.6) + # the opacity of a geom
  geom_density(
    data = sz_pts_extract,
    aes(x = log(F101992.v4b_web.stable_lights.avg_vis), color = "Locations of coronavirus confirmed case activities", fill = "Locations of coronavirus confirmed case activities"),
    alpha = 0.6) +
  ylab("Density") +
  xlab("Light radiance") +
  scale_color_viridis("", discrete = T) +
  scale_fill_viridis("", discrete = T) +
  theme_bw() 

## read in the shenzhen poi csv file
sz_poi <- read_excel("sz_poi.xlsx")
#sz_poi <- read_excel("sz_poi.xlsx", 1, encoding="UTF-8")
str(sz_poi) # we have longitude/latitude information inside

## create a pointshapefile
sz_poi$lon <- as.numeric(sz_poi$lon)
sz_poi$lat <- as.numeric(sz_poi$lat)
sz_poi <- subset(sz_poi, !is.na(sz_poi$lon) & !is.na(sz_poi$lat))
sz_poi_sp <- SpatialPointsDataFrame(sz_poi[, 1:2], #coordinates
                                    sz_poi, #data
                                    proj4string = crs_sz) #assign projection

sz_poi_extract <- raster::extract(
  x = nl_sz,
  y = sz_poi_sp,
  fun = mean,
  na.rm = T,
  sp = T)

# Convert sz_pts_extract to a data frame
sz_poi_extract <- tbl_df(sz_poi_extract)

# Density plots
ggplot() +
  geom_density(
    data = masked_df,
    aes(x = log(F101992.v4b_web.stable_lights.avg_vis), color = "All of Shenzhen", fill = "All of Shenzhen"),
    alpha = 0.6) + # the opacity of a geom
  geom_density(
    data = sz_pts_extract,
    aes(x = log(F101992.v4b_web.stable_lights.avg_vis), color = "Coronavirus confirmed case", fill = "Coronavirus confirmed case"),
    alpha = 0.6) +
  geom_density(
    data = sz_poi_extract,
    aes(x = log(F101992.v4b_web.stable_lights.avg_vis), color = "POI Locations", fill = "POI Locations"),
    alpha = 0.6) +
  ylab("Density") +
  xlab("log(Light radiance)") +
  scale_color_viridis("", discrete = T) +
  scale_fill_viridis("", discrete = T) +
  theme_bw() 


# 1.8 read in NetCDFs data
## Download the pm2.5 raster data from http://fizz.phys.dal.ca/~atmos/martin/?page_id=140
#install.packages("ncdf4")
library(ncdf4)
pm_2016 <- raster("GlobalGWRwUni_PM25_GL_201601_201612-RH35_Median.nc")
pm_2010 <- raster("GlobalGWRwUni_PM25_GL_201001_201012-RH35_Median.nc")
pm_2016
pm_2010
plot(pm_2016, col = inferno(1e3), axes = F, box = F)
plot(pm_2010, col = inferno(1e3), axes = F, box = F)

plot(pm_2016 - pm_2010, col = inferno(1e3), axes = F, box = F)


## Load multiple raster files into a raster stack
pm_stack <- stack(
  "GlobalGWRwUni_PM25_GL_201601_201612-RH35_Median.nc",
  "GlobalGWRwUni_PM25_GL_201001_201012-RH35_Median.nc") 
# Check
pm_stack

## Min and max values of PM difference (2016 - 2010)
diff_min <- getValues(pm_2016 - pm_2010) %>% min(na.rm = T)
diff_max <- getValues(pm_2016 - pm_2010) %>% max(na.rm = T)

## Create the color scale
library(maps)
library(fields)
c_scale <- designer.colors(
  n = 500, #Number of color levels
  col = c(inferno(100)[25], "grey92", inferno(100)[70]), #A list of colors (names or hex values) to interpolate
  x = c(diff_min, 0, diff_max), #Positions of colors on a [0,1] scale
  alpha = 1 # the opacity of a geom
)
## Plot again
plot(pm_2016 - pm_2010, col = c_scale, axes = F, box = F)

## zoom in China
crop(pm_2016 - pm_2010, extent(75, 150, 10, 55)) %>%
  plot(col = c_scale, axes = F, box = F)



# 2 Geocoding
library(ggmap)

# get a google api to download the basemap from google
register_google(key = "your key") 
##apply and use your own API from https://console.cloud.google.com/, and enable geocode (might require a billing account)

# Geocode with Google using geocode()
geo_google <- geocode(
  "香港浸会大学逸夫校园永隆银行商学大楼",
  output = "more",
  source = "google")


## geocoding using baidu map service 
# devtools::install_github("ChrisMuir/baidugeo")
library(baidugeo)
bmap_set_key("your key")
##apply and use your own API from https://lbsyun.baidu.com/

bmap_rate_limit_info()
bmap_set_daily_rate_limit(100)

locs <- c(
  "中百超市有限公司长堤街二分店", 
  "浙江省杭州市余杭区径山镇小古城村", 
  "成都高粱红餐饮管理有限公司"
)

coords_df <- bmap_get_coords(locs)
coords_df

## Reverse Geocoding
addrs_df <- bmap_get_location(coords_df$lat, coords_df$lon)
addrs_df

#there might be some problem if your newly apply a key...http://lbsyun.baidu.com/index.php?title=webapi/guide/webservice-geocoding
#use python instead! https://zhuanlan.zhihu.com/p/81275754

