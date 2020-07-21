##################################
# Dummy for Dummies---Lecture 1 #
# ggplt2 #
# Shawn SHI #
##################################

#install.packages(c("tidyverse", "readxl", "scales", "ggspatial", "ggrepel", "foreign", "lubridate", "classInt"))
#install.packages(c("quantmod", "gapminder"))
#install.packages(c("sf", "ggmap"))
#install.packages(c("rnaturalearth", "rnaturalearthdata", "tigris", "osmdata", "gganimate"))


library(tidyverse) #include ggplot2, dplyr, forcats etc
library(readxl) #read excel
library(scales) #change breaks in x and y axis
library(ggspatial) #create a scale bar on a map
library(ggrepel) #repel overlapping text labels
library(foreign) #reading and wirting data from other source. e.g dta file
library(lubridate) #work with dates and times
library(classInt) #making intervals

library(quantmod) #Quantitative Financial Modelling Framework
library(gapminder) #cross country level social-economic factors

library(sf) #simple features, a standardized way to encode spatial vector data
library(classInt) #making intervals for choropleth map
library(ggmap) #load in basemap from other source

library(rnaturalearth) #https://www.naturalearthdata.com/
library(rnaturalearthdata)
library(tigris) #TIGER/Line shapefiles from the United States Census Bureau (<https://www.census.gov/geo/maps-data/data/tiger-line.html>)
library(osmdata)
library(gganimate) #moving maps!

#set working directory
setwd("D:/Dropbox/SOE0330/LaTex/R course/ECON4016-7930/Lecture4_2020Spring")

#import dataset
df <- data.frame(dose=c("D0.5", "D1", "D2"),len=c(4.2, 10, 29.5))
head(df) #have a look at the data

excel_df <- read_excel("df.xlsx")

######################
# ggplot---bar plot #
######################

df <- read_excel("df.xlsx")

# Simple barplot
ggplot(data=df, aes(x=dose, y=len)) +geom_bar(stat="identity") #aesthetic

# Horizontal bar plot
ggplot(data=df, aes(x=dose, y=len)) +geom_bar(stat="identity") + coord_flip()

# Change the width of bars
ggplot(data=df, aes(x=dose, y=len)) + geom_bar(stat="identity", width=0.5)

# Change colors
ggplot(data=df, aes(x=dose, y=len)) +geom_bar(stat="identity", width=0.5, color="blue", fill="white") #http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

# Minimal theme + blue fill color
basic <-ggplot(data=df, aes(x=dose, y=len)) + geom_bar(stat="identity", width=0.5, color="blue", fill="white")+ theme_minimal()
basic

# Choose which ones to display
basic + scale_x_discrete(limits=c("D0.5", "D2")) 

# Labels: outside bars
basic + geom_text(aes(label=len), vjust=-.3, size=3.5)

# Inside bars
basic + geom_text(aes(label=len), vjust=1.6, color="blue", size=3.5)

# Stacked barplot with multiple groups
insurance <- read_excel("insurance.xlsx") #https://coronavirus.jhu.edu/map.html

insurance

ggplot(data=insurance, aes(x=type, y=rate, fill=age)) + geom_bar(stat="identity") 

basic <-ggplot(data=insurance, aes(x=reorder(type, order), y=rate, fill=age)) + geom_bar(stat="identity", position="stack") 
basic

# Use custom colors
basic + scale_fill_manual(values=c('#76232f','#ff9e1b', '#cf4520'))

# Use brewer color palettes
basic + scale_fill_brewer(palette="Blues")

# Modify legend titles
basic + scale_fill_manual(values=c('#76232f','#ff9e1b', '#cf4520')) + labs(fill = "Age Groups")

# Modify legend labels
basic + scale_fill_manual(values=c('#76232f','#ff9e1b', '#cf4520'), labels = c("65+", "35-64", "19-35")) + labs(fill = "Age Groups") 

# Edit x and y label
basic  + scale_fill_manual(values=c('#76232f','#ff9e1b', '#cf4520'), labels = c("65+", "35-64", "19-35")) + labs(fill = "Age Groups") + 
  xlab("Insurance Type") + ylab("Percentage of Population") + ggtitle("HEALTHCARE FACTS")

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

# Add x-axis sclae 
advance + labs(title = "Personal Savings Rate", x = "Date", y = "Personal Savings Rate") + geom_smooth() +
  scale_x_date(date_breaks = '5 years', labels = date_format("%b-%y")) + 
  labs(title = "Personal Savings Rate", subtitle = "1967 to 2015", x = "", y = "Personal Savings Rate") +
  theme_minimal()

############################
# multivariate time series #
############################

## get apple (AAPL) closing prices
getSymbols("AAPL", return.class = "data.frame", from="2019-07-01")

apple <- AAPL %>% 
  mutate(Date = as.Date(row.names(.))) %>% #mutate: generate new var
  select(Date, AAPL.Close) %>%
  rename(Close = AAPL.Close) %>% #pay attention to capital letters!
  mutate(Company = "Apple") 

#have a look at the data
head(AAPL)
head(apple)

## get facebook (FB) closing prices
getSymbols("FB", return.class = "data.frame", from="2019-07-01") #Load and Manage Data from Multiple Sources

facebook <- FB %>% 
  mutate(Date = as.Date(row.names(.))) %>%
  select(Date, FB.Close) %>%
  rename(Close = FB.Close) %>%
  mutate(Company = "Facebook")

## Append data for both companies
stockprice <- rbind(apple, facebook) 
stockprice

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
install.packages("gcookbook") #Data sets used in the book "R Graphics Cookbook" by Winston Chang, published by O'Reilly Media.

data(uspopage, package = "gcookbook") #Age distribution of population in the United States, 1900-2002
ggplot(uspopage, aes(x = Year, y = Thousands, fill = AgeGroup)) + geom_area() + labs(title = "US Population by age",
       x = "Year",
       y = "Population in Thousands")

# Make it prettier 
ggplot(uspopage, aes(x = Year, y = Thousands/1000,  fill = forcats::fct_rev(AgeGroup))) + geom_area() + #reverse the order by age group
  labs(title = "US Population by age",
       subtitle = "1900 to 2002",
       caption = "source: U.S. Census Bureau, 2003, HS-3",
       x = "Year",
       y = "Population in Millions",
       fill = "Age Group") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()


#scatter plot
head(gapminder)

basic <- ggplot(data = subset(gapminder, continent=="Asia" & year==2007),aes(x=log(gdpPercap), y=lifeExp))+ geom_point()
basic + geom_text(mapping = aes(label=country))
basic + geom_point() + geom_text_repel(mapping = aes(label=country))

#something exciting! racing bar chart! 
ranked_by_year <- gapminder %>%  
  select(country, pop, year, continent) %>%  
  # for each year we assign a rank
  group_by(year) %>%  
  arrange(year, -pop) %>%  
  # assign ranking
  mutate(rank = 1:n()) %>%  
  filter(rank <= 10) 

ranked_by_year

my_theme <- theme_classic(base_family = "Times") + #https://ggplot2.tidyverse.org/reference/theme.html
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line.y = element_blank()) +
  theme(legend.background = element_rect(fill = "gainsboro")) +
  theme(plot.background = element_rect(fill = "gainsboro")) +
  theme(panel.background = element_rect(fill = "gainsboro"))

ranked_by_year %>%  
  ggplot() +  
  aes(xmin = 0 ,  
      xmax = pop / 1000000) +  
  aes(ymin = rank - .45,  
      ymax = rank + .45,  
      y = rank) +  
  facet_wrap(~ year) +  #https://ggplot2-book.org/facet.html
  geom_rect(alpha = .7) +  
  aes(fill = continent) +  
  scale_fill_viridis_d(option = "magma",  
                       direction = -1) +  
  scale_x_continuous(  
    limits = c(-800, 1400),  
    breaks = c(0, 400, 800, 1200)) +  
  geom_text(col = "gray13",  
            hjust = "right",  
            aes(label = country),  
            x = -50) +  
  scale_y_reverse() +  
  labs(fill = NULL) +  
  labs(x = 'Population (millions)') +  
  labs(y = "") +  
  my_theme ->  
  my_plot


my_plot +  
  facet_null() +  
  scale_x_continuous(  
    limits = c(-355, 1400),  
    breaks = c(0, 400, 800, 1200)) +  
  geom_text(x = 1000 , y = -10,  
            family = "Times",  
            aes(label = as.character(year)),  
            size = 30, col = "grey18") +  
  aes(group = country) +  
  gganimate::transition_time(year)



#Draw maps

# Download the shapefile. (note that I store it in a folder called DATA. You have to change that if needed.)
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="shapefile/world_shape_file.zip")

# Unzip this file. You can do it with R (as below), or clicking on the object you downloaded.
system("unzip shapefile/world_shape_file.zip")

# Read this shape file with the rgdal library. 
library(rgdal)
library(sp)
my_spdf <- readOGR("shapefile/world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp") 
plot(my_spdf)

summary(my_spdf) #tells you the max and min coordinates, the kind of projection in use
length(my_spdf) #how many regions you have
head(my_spdf@data) #the firs few rows of the data slot associated with the regions

