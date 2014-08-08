# plotting LMEs used in analysis

# notes: gBuffer will complain with following error. Doesn't seem to be doing anything, so I ignore it. 
# Warning message:
#   In gBuffer(subant, width = 1e-04) :
#   Spatial object is not projected; GEOS expects planar coordinates

rm(list=ls()) # clean working environment
require(RColorBrewer); require(maptools); require(sp); require(rgdal); require(rgeos);

# loading FAO shapefiles and finding bounding boxes
#ogrListLayers("original_data/FAO_areas/World_Fao_Zones.shp")

shape=readOGR("original_data/FAO_areas/World_Fao_Zones.shp", layer="World_Fao_Zones") #will load the shapefile to your dataset.

# Subsetting each of the high seas LMEs (sew together FAO areas)
#  indian high seas
  indian <- shape[shape$zone==51 | shape$zone==57,]
#subantarctic
  subant <- shape[shape$zone==48 | shape$zone==58|  shape$zone==88,]
# pacific
  pacific <- shape[shape$zone==87 | shape$zone==81 | shape$zone==77 | shape$zone==67 | shape$zone==71 | shape$zone==61,]
# Atlantic
  atlantic <- shape[shape$zone==41 | shape$zone==47 | shape$zone==34 | shape$zone==31 | shape$zone==21 | shape$zone==27,]

# load lmes
  lmes_orig <- readShapePoly("original_data/LME66/LME66.shp")
# assign projection
  proj4string(lmes_orig) <- CRS("+proj=longlat")

# these are hard coded LME numbers from looking at the excel sheet, should be able to pull this in automatically
  lmes_to_plot <- c(1,2,3,5,6,7,8,9,13,14,20,21,22,23,24,25,26,29,30,39,42,43,46,52,53,59,60)

# making figure

# make colors
  paint <- colorRampPalette(brewer.pal(3,"YlGnBu")[2:3])(length(lmes_to_plot)+4)# plot all FAOs
  paint <- rev(paint)

png(file="Rscripts/figures/LMEs/LMEs_fig.png",width=2000, height=1948)

# plot all FAOs
	par(bg="wheat")
	plot(shape, col="grey96",bor="grey96")

# plot FAOs we have
	plot(gBuffer(indian, width=.0001), col=paint[1], bor="white",add=T)
	plot(gBuffer(atlantic, width=.0001), col=paint[2],bor="white",add=T)
	plot(gBuffer(pacific, width=0.0001), col=paint[3], bor="white",add=T)
	plot(gBuffer(subant, width=0.0001), col=paint[4],bor="white",add=T)

# plot all LMEs
	plot(gBuffer(lmes_orig, width=0.01), col="grey96",bor="white",add=T)

	plot_order <- sample(lmes_to_plot)

#plot LMEs that we have
	for(i in 1:length(lmes_to_plot)){
  		plot(lmes_orig[lmes_orig$LME_NUMBER==plot_order[i],], col=paint[i+4], bor="white", add=T)
	}

dev.off()

