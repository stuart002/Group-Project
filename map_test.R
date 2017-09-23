#x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap")
#install.packages(x)
#lapply(x, library, character.only = TRUE) 
library(raster)
library(rgdal)
library(dplyr)
library(plyr)
library(tmap) # load tmap package 

#reads the full data
well_being_index_data <- read.csv("london-ward-well-being-probability-scores-data.csv")

#selects relevant columns: borough (4) and all crime levels (25:29) and stores in crime_data. Note boroughs are repeated because data is for wards
crime_data <- well_being_index_data[c(2:661),c(4,25:29)]
crime_data$Crime.rate...2009 <- as.numeric(as.character(crime_data$Crime.rate...2009))

#adds new column names to crime_data
colnames(crime_data) <- c("borough", "crime_rate_2009", "crime_rate_2010", "crime_rate_2011","crime_rate_2012","crime_rate_2013")

#calculates the average crime level data per year for each borough
crime_data_borough_averages <- aggregate(crime_data[, 2:6], list(crime_data$borough), mean)

#sets new column names
colnames(crime_data_borough_averages) <- c("borough", "crime_rate_2009", "crime_rate_2010", "crime_rate_2011","crime_rate_2012","crime_rate_2013")

#Reads boroughs data as SpatialPolygonsDataFrame
london_boroughs <- readOGR(dsn = "LondonBoroughs.shp") 

#the line belows does the same as the merging 
london_boroughs@data <- left_join(london_boroughs@data, crime_data_borough_averages, by = c('name' = 'borough'))
plot(london_boroughs)
library(tmap)
qtm(shp = london_boroughs, fill = "crime_rate_2009", fill.palette = "Blues") # not shown
library(ggplot2)
library(rgeos)
london_boroughs_f <- fortify(london_boroughs)
london_boroughs$id <- row.names(london_boroughs)# allocate an id variable to the sp d
london_boroughs_f <- left_join(london_boroughs_f, london_boroughs@data) # join the data


#this maps the crime rates for one year
map <- ggplot(london_boroughs_f, aes(long, lat, group = group, fill=crime_rate_2009)) +
  geom_polygon() + coord_equal() +
  labs(x = "lat", y = "lon",
       fill = "Crime rate") +
  ggtitle("Crime rates by borough (2009) ") + scale_fill_gradient(low = "grey", high = "blue")
map

london_boroughs_f <- gather(london_boroughs_f, crime_rate_year, crime_rate, -long, -lat, -order, -hole,-piece,-id,-group,-ons_label,-name,-Partic_Per,-Pop_2001, -PopDensity, -AREA, -PERIMETER, -PopDen)
ggplot(data = london_boroughs_f, # the input data
       aes(x = long, y = lat, fill = crime_rate, group = group)) + # define variables
  geom_polygon() + # plot the boroughs
  geom_path(colour="white", lwd=0.05) + # borough borders
  coord_equal() + # fixed x and y scales
  facet_wrap(~ crime_rate_year) + # one plot per year
  scale_fill_gradient2(low = "blue", mid = "grey", high = "red", # colors
                       midpoint = 150, name = "Crime rate") + # legend options
  theme(axis.text = element_blank(), # change the theme options
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank()) # remove axis ticks