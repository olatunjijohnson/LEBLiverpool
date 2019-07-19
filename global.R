library(shiny)
library(leaflet)
library(dplyr)

# predsamples <- read.csv("predsamples.csv")
# predcoord <- read.csv("predcoord.csv")
predsamples <- readRDS("predsamples.rds")
predcoord <- readRDS("predcoord.rds")
bound <- readRDS("bound.rds")


#saveRDS(Con_pred, file = "./SDAWResult.rds")
#df <- readRDS("./SDAWResult.rds")
# proj4string(df$my_shp) = CRS("+init=epsg:27700")
# df <- spTransform(df$my_shp,CRS('+init=epsg:4326'))