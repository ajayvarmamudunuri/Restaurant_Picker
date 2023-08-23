#Installing The Necessary R Packages

install.packages("tidyverse")
install.packages("lubridate")
install.packages("sqldf")
install.packages("ggmap")
install.packages("dplyr")
install.packages("rjson")
install.packages("leaflet")
install.packages("leaflet.extras")
install.packages("kableExtra")


#Libraries Used For This Project 

library(rjson) # The google location history file is a JSON 
library(ggmap) # For ggplot2 background layer maps
library(tidyverse) # For ease of data wrangling
library(lubridate) # For date manipulation
library(leaflet)
library(leaflet.extras)
library(kableExtra)
library(sqldf)

proc.time()
Sys.time()

#Reading the raw google location history file of a user stored as a .json file 

# Dataset 1 : Location History
raw <- fromJSON(file = "C:\\Codes\\FDA Project\\Takeout\\Location History\\Records.json") 

#Flattening the imported raw dataset by using flatten() command 
flat <- flatten(flatten(raw)) 

# Extracting and storing the timestamp, latitude, longitude and accuracy data from the flattened dataset, for future calculations in their respective variables:

ts <- flat[names(flat) == "timestamp"] %>% unlist() %>% unname() 
ts <- gsub("T.*","",ts)
lat <- flat[names(flat) == "latitudeE7"] %>% unlist() %>% unname()
lon <- flat[names(flat) == "longitudeE7"] %>% unlist() %>% unname()
acc <- flat[names(flat) == "accuracy"] %>% unlist() %>% unname()

# Creating a new dataframe that has the columns of time, month, week, latitude, longitude, xstart, xend, ystart, yend and distance

df <- 
  tibble(ts=ts, lat=lat, lon=lon, acc=acc) %>%
  mutate(myts = as_datetime(ts)) %>% # Convert raw timestamp to datetime
  arrange(myts) %>% 
  mutate(year = year(myts),
         time  = as_datetime(ts),
         month = match(month(myts, label=T),month.abb),
         week = isoweek(time),
         lat = lat / 1e7, # I think these need to be divided by 1e7
         lon = lon / 1e7, # I think these need to be divided by 1e7
         xstart = lon, # Create start and end columns for plotting segments later on
         xend = lead(lon), 
         ystart = lat, 
         yend = lead(lat),
         dist = (((xend - xstart)^2) + ((yend - ystart)^2))^0.5) 

#To check if the created dataframe has any NULL values and if they do perform data cleaning by removing such rows.
summary(df)
which(is.na(df), arr.ind=TRUE)
proc.time()
Sys.time()

df_cleaned <- na.omit(df)
# Descriptive Analytics Of a user's Google location history
# To display the number of observations(locations) recorded every year by Google for a particular user 

kable(df_cleaned %>% group_by(year) %>% 
        summarise(n=n()),col.names=c("Year","No. Observations"), 
      align=c('c','r'),caption="Data collected by year") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive"), full_width = F )

proc.time()
Sys.time()


# To display in a neat bar chart format how much location data Google has collected on you every month across each year

df_cleaned %>%  group_by(month,year) %>% 
  summarise(n = n()) %>%
  ggplot( aes(x=month, y=n)) +
  geom_bar(stat="identity", aes(fill = n)) +
  scale_fill_gradient(low = "yellow", high = "red") +
  facet_grid(facets = year ~ .) +
  scale_x_continuous(breaks = c(1:12)) +
  labs(x = "month", y = "Count", 
       title="How many locations have Google tracked about me?", 
       subtitle = "Tracks by month per year") +
  theme_bw()

proc.time()
Sys.time()

# To display in a neat bar chart format how much location data Google has collected on you every week across each year

df_cleaned %>%  group_by(week,year) %>% 
  summarise(n = n()) %>%
  ggplot( aes(x=week, y=n)) +
  geom_bar(stat="identity", aes(fill = n)) +
  scale_fill_gradient(low = "yellow", high = "red") +
  facet_grid(facets = year ~ .) +
  scale_x_continuous(breaks = c(1:53)) +
  labs(x = "week", y = "Count", 
       title="How many locations have Google tracked about me?", 
       subtitle = "Tracks by week per year") +
  theme_bw()

proc.time()
Sys.time()

# To plot a geographical map containing points depicting the location points captured by Google of the user

leaflet(df_cleaned) %>% 
  addTiles() %>%
  addWebGLHeatmap(size=10,units='px')

proc.time()
Sys.time()

# To generate a geographical heatmap of all location points collected by Google, which shows the intensity of locations visited in a clear manner

myMap = leaflet(df_cleaned) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat)) %>%  
  addHeatmap(lng = ~lon, lat = ~lat, group = "HeatMap", blur = 20, max = 0.01, radius = 15) %>%
  addMarkers(data = df_cleaned, ~lon, ~lat, clusterOptions = markerClusterOptions(), group = "Points")
myMap
proc.time()
Sys.time()


#Importing the Zomato Indian Restaurants dataset

df1 <- read.csv("C:\\Codes\\FDA Project\\dataset\\zomato_new1.csv")

#Extracting only those columns which are needed for implementing our recommendation system

df2 <- subset(df1, select = c(name, City ,Locality ,Latitude ,Longitude ,range, ranks))


# Descriptive Analysis Of Zomato Indian Restaurants dataset

# To show how many number of restaurants are present in each city in the above dataset

kable(df2 %>% group_by(City) %>% 
        summarise(n=n()),col.names=c("City","No. Observations"), 
      align=c('c','r'),caption="Restaurant Data Per City") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive"), full_width = F )


# Creating a new dataset containing only restaurants present in Chennai

df_chennai <- sqldf("select * from  df2 where city='Chennai'")

# Filtering and grouping the Chennai restaurants based on their price level (1-4) and displaying them. The cost increases with, increase in price level

kable(df_chennai %>% group_by(range) %>% 
        summarise(n=n()),col.names=c("Price Level","Number Of Restaurants"), 
      align=c('c','r'),caption="Restaurant's Price Range in Chennai") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive"), full_width = F )


# Filtering and grouping the Chennai restaurants based on their ratings(0.0-5.0) and displaying them.

kable(df_chennai %>% group_by(ranks) %>% 
        summarise(n=n()),col.names=c("Ratings","Number Of Restaurants"), 
      align=c('c','r'),caption="Restaurant's Ratings in Chennai") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive"), full_width = F )

# Selecting 500-1000 locations captured from the Google location dataset for the Recommendation System

dfloc<-sample_n(df_cleaned,500)

# Creating a new dataframe with only those restaurants where the ratings are greater than or equal to 3.5 and sampling it

df3 <- sqldf("select * from df2 where ranks >= 2" )
dfzom<-sample_n(df3,1)


l1 <- list()
l2 <- list()
l3 <- list()

# Running a loop against each location captured by Google across the Restaurants dataset and setting a constraint for the difference between lattitude and longitude values of the restaurant and observed location in each case to recommend nearby restaurants 

for(i in 1:500)
{
  for(j in 1:500)
  {
    if((abs(dfzom$Latitude[j]- dfloc$lat[i]) < 0.05)&& (abs(dfloc$lon[i]- dfzom$Longitude[j])<0.05))
    {
      l1 <- c(l1,dfzom$name[j])
      l2 <- c(l2,dfzom$City[j])
      l3 <- c(l3,dfzom$Locality[j])
    }
  }
}

# Converting the lists obtained from the above loop into a dataframe format, consisting of all the recommended restaurants

Recommender = data.frame(unlist(l1),unlist(l2),unlist(l3))
names(Recommender) = c("Name","City","Locality")

#Displaying only the unique values from that dataframe such that there are no repetition of restaurant names

uniquedf <- unique(Recommender)

#Performing the same Recommendation technique but choosing only those locations which were recently recorded by Google for a better recommendation system

df_recent <- sqldf("select * from df_cleaned where year=(select MAX(year) from df_cleaned) " )
df_r2 <- sqldf("select * from df_recent where week=(select MAX(week) from df_recent) " )
l4 <- list()
l5 <- list()
l6 <- list()

counter <- 0
for(i in 1:length(df_r2))
{
  for(j in 1:500)
  {
    if(counter < 5 && dfzom$City[j]=="Chennai")
    {
      l4 <- append(l4,dfzom$name[j])
      l5 <- c(l5,dfzom$City[j])
      l6 <- c(l6,dfzom$Locality[j])
      counter = counter + 1
      print(counter)
    }
  }
}

RecentRecommender = data.frame(unlist(l4),unlist(l5),unlist(l6))
names(RecentRecommender) = c("Name","City","Locality")
Recentuniquedf <- unique(RecentRecommender)
