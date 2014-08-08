# purpose: explore LMEs
# date: 2014-07-29

# local to emma
# setwd("/Users/efuller/Documents/Projects/Regional_Collapse/")
# set your local path yourself to Regional_Collapse. 

require(maptools); require(rgdal); require(RColorBrewer)

# nice website for good examples of using R with polygons: http://mazamascience.com/WorkingWithData/?p=1277

# read in shapefile
shape <- readShapePoly("LME66/LME66.shp")
# assign projection
proj4string(shape) <- CRS("+proj=longlat")
# plot to check
paint <- colorRampPalette(brewer.pal(9,"Blues"))(length(shape))
plot(shape,col=paint,bor=F)
# what's in the data?
names(shape)

# let's rename the data to be better, subset just to what we're interested in
attributes <- c("OBJECTID","LME_NUMBER","LME_NAME")
newNames <- c("number","area","name")
# subset
shape_subset <- shape[,attributes]
# rename
names(shape_subset) <- newNames

# assign to new .Rdata 
data_name <- "LMES_new"
assign(data_name, spTransform(shape_subset, CRS("+proj=longlat")))
#save(list=c(data_name),file="sandbox/LME_explore.Rdata") # have already saved, not necessary to do again

# just look at California Current (US west coast LME)
CC <- LMES_new[LMES_new$number == 10, ]
plot(CC,col="steelblue",bor=F)

# we can also get the bounding box for each LME
CC@bbox
# not sure, but looks like we have to subset the polygons we want, and do this seperately. Because get all of the coordiantes when you do it for entire dataset
LMES_new@bbox

# write a bunch of little text files that have the bounding boxes

for(i in 1:length(shape)){
  one_lme <- LMES_new[LMES_new$number == i, ]
  write.table(one_lme@bbox,file=paste("sandbox/explore_LME/LME_",i,".txt",sep=""))
}

