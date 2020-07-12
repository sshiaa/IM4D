##################################
# Dummy for Dummies---Lecture 1.1 #
#ggplt2#
# Lecturer: SHI Shuang #
##################################

#install.packages(c("tidyverse", "readxl", "scales", "ggspatial", "ggrepel", "foreign", "lubridate" ))
#install.packages(c("quantmod", "gapminder"))
#install.packages(c("sf"))
#install.packages(c("rnaturalearth", "rnaturalearthdata"))
#install.packages("rmarkdown")

library(tidyverse) #include ggplot2, dplyr, forcats etc
library(readxl) #read excel
library(scales) #change breaks in x and y axis
library(ggspatial) #create a scale bar on a map
library(ggrepel) #repel overlapping text labels
library(foreign) #reading and wirting data from other source. e.g dta file
library(lubridate) #work with dates and times

library(quantmod) #Quantitative Financial Modelling Framework
library(gapminder) #cross country level social-economic factors

library(sf) #simple features, a standardized way to encode spatial vector data

library(rnaturalearth) #https://www.naturalearthdata.com/
library(rnaturalearthdata)