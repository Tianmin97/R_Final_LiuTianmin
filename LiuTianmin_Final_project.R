#########association rule mining#########
rm(list=ls())
library(ggplot2)
mydata <- read.csv("E:/my paper/data/origin.csv",encoding = 'UTF-8')
View(mydata)
datag <- read.csv("E:/my paper/data/new.csv", encoding = 'UTF-8')

View(datag)
attach(datag)
newdata<-datag[which(category_big == "(백)식품"), ]
View(newdata)
library(arules)
library(arulesViz)
library(dplyr)
newdata<-data.frame(newdata)
showdata<- group_by(newdata, category_midddle)
delay <- summarise(showdata,
                   count = n(),               
                   avg_sales = mean(quantity),   
                   sum_sales = sum(quantity))
delay
ass<- read.csv("E:/my paper/data/association.csv", encoding = "UTF-8")
ass<-data.frame(ass)
ass<-t(ass)
grocery_rules <- apriori(data=ass,parameter=list(support = 0.1,confidence = 0.5))
grocery_rules
summary(grocery_rules)
inspect(grocery_rules[1:5])
#########point data display and heat map#########
library(pacman)
p_load(spatstat,sp,rgeos,maptools,GISTools,tmap,
       sf,geojson,geojsonio,tmaptools,tidyverse,raster,fpc,dbscan)
cpv <- st_read("E:/GIS_Data-20210615T035225Z-001/GIS_Data/Cities(KTDB_2018).shp")
scpv <- cpv %>%
    dplyr::filter(str_detect(Province, "서울특별시"))
tmap_mode("plot")
qtm(scpv)
grocery <- read_csv("E:/E/so_grocery.csv") %>%
    st_as_sf(., coords = c("longitude", "latitude"), 
             crs = 4326)
tm_shape(scpv) +
    tm_polygons(col = NA, alpha = 0.5) +
    tm_shape(grocery) +
    tm_dots(col = "blue")
library(leaflet)
install.packages("leaflet.extras")
library(leaflet.extras)
location <- read.csv("E:/E/location.csv", encoding = 'UTF-8')
o <- leaflet(data = location)
o <- addTiles(o)
addCircles(o,lng=~longitude,lat=~latitude)
addHeatmap(o,lng=~longitude,lat=~latitude,blur = 20, max = 0.05, radius = 3, cellSize = 0.1)
slocation <- read.csv("E:/E/lotte_s.csv", encoding = 'UTF-8')
s <- leaflet(data = slocation)
s <- addTiles(s)
addCircleMarkers(s,lng=~longitude,lat=~latitude)
dlocation <- read.csv("E:/E/seoul_grocery.csv", encoding = 'UTF-8')
d <- leaflet(data = dlocation)
d <- addTiles(d)
addCircles(d,lng=~longitude,lat=~latitude)
addHeatmap(d,lng=~longitude,lat=~latitude,blur = 30, radius = 3, cellSize = 0.1)