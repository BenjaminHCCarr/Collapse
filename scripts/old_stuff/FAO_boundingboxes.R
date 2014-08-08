# find what the "high seas" areas are in ram myers database

require(sp); require(rgdal); require(maptools); require(rgeos); require(reshape2)


# load data
  stocks <- read.csv("data_tables/stocks.csv",stringsAsFactors=F)
# list of high seas bounding boxes
  high_seas = unique(subset(stocks, LME_NUMBER<0, c(LME_NAME, LME_NUMBER)))
# plan: will take all FAO shape files that cover the high seas, sew together. Eventually will subtract existing continental LMEs from them to prevent overlap. See here for reference: http://www.seaaroundus.org/eez/FAOarea.htm
  high_seas$FAO_codes <- c(
    "48, 58, 88",
    "51, 57",
    "unknown",
    "87, 81, 77, 67, 71, 61",
    "41, 47, 34, 31, 21, 27"
    ) 
  
# loading FAO shapefiles and finding bounding boxes
  ogrListLayers("original_data/FAO_areas/World_Fao_Zones.shp")


# load data
shape=readOGR("original_data/FAO_areas/World_Fao_Zones.shp", layer="World_Fao_Zones") #will load the shapefile to your dataset.

bounding <- data.frame(x_min=rep(NA,nrow(high_seas)), ymin=rep(NA,nrow(high_seas)), xmax=rep(NA,nrow(high_seas)), ymax=rep(NA,nrow(high_seas)), centroid_x = rep(NA, nrow(high_seas)), centroid_y = rep(NA, nrow(high_seas)))

# try indian high seas
  indian <- shape[shape$zone==51 | shape$zone==57,]
  bounding[2,]<-c(t(melt(indian@bbox)[,3]),coordinates(gBuffer(indian, width = 0.001)))
  

#subantarctic
  subant <- shape[shape$zone==48 | shape$zone==58|  shape$zone==88,]
  bounding[1,] <- c(t(melt(subant@bbox)[,3]), coordinates(gBuffer(subant, width = 0.001)))

# pacific
  pacific <- shape[shape$zone==87 | shape$zone==81 | shape$zone==77 | shape$zone==67 | shape$zone==71 | shape$zone==61,]
bounding[4,] <- c(t(melt(pacific@bbox)[,3]), coordinates(gBuffer(pacific, width = 0.001)))

# Atlantic
  atlantic <- shape[shape$zone==41 | shape$zone==47 | shape$zone==34 | shape$zone==31 | shape$zone==21 | shape$zone==27,]
plot(atlantic)
bounding[5,] <-c(t(melt(atlantic@bbox)[,3]), coordinates(gBuffer(atlantic, width = 0.001)))

high_seas <- cbind(high_seas, bounding)
write.csv(high_seas, file="original_data/FAO_areas/high_seasBB.csv")
