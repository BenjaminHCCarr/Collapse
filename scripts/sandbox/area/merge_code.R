regions = read.csv("original_data/Region_centroid.csv")
head(regions)

# area = read.csv("original_data/area.csv")
# head(area)

stock = read.csv("original_data/stock.csv")
head(stock)

area <- merge(stock, regions, by = "AREAID", all.x=T, all.y=F)

assessment = read.csv("original_data/assessment.csv")
head(assessment)

assessment <- merge(assessment, area, by="STOCKID", all.x=T, all.y=F)

points(area$lon, area$lat, pch=20, bg = "red",col = "red")

stocks = read.csv("data_tables/stocks.csv")
head(stocks)

coords = cbind(area$km2,area$lat,area$lon)
coords = as.data.frame(coords)
head(coords)

area_coords = cbind(area$AREANAME,coords)
names(area_coords) = c("AREA","km2","lat","lon")
head(area_coords)

stocks <- merge(stocks, area_coords, by = "AREA", all.x=T, all.y=F)

test = cbind(stocks$ASSESSID, stocks$AREA, stocks$lat, stocks$lon)
names(test) = c("ASSESSID","AREA","lat","lon")


stocks <- subset(stocks, !is.na(final_collapse))
stocks<- subset(stocks,LME_NUMBER > 0)
write.csv(stocks, file = "test.csv")