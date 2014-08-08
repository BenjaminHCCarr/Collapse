# https://github.com/ropensci/rfishbase/blob/master/inst/doc/rfishbase/rfishbase_github.md

library("rfishbase")

#setwd("~/Regional_Collapse/sandbox")
setwd("~/OSS2014/Regional_Collapse/sandbox")

data(fishbase) #create fish.data file from fishbase
loadCache("../datasets") #load cached copy of fish.data from fishbase

cat("\n Loading taxonomy.csv file \n")
taxa = read.csv2("taxonomy.csv",header=T, sep = ",", dec=".")
str(taxa)
cat("\n taxonomy.csv file loaded \n")

cods <- subset(taxa, grepl("cod", COMMONNAME1))
cods <- droplevels(cods)

for(codindex in 1:length(cods)){
#for(codindex in 1:9){ 
  cod_name<-droplevels(cods$SCIENTIFICNAME[codindex])
  cat("Current Cod is", cod_name, "\n")
}
newdata <- as.data.frame(cods)  

cod_name <- "Gadus morhua"
cod_name<-cods$SCIENTIFICNAME
cod_name<-c(cods$SCIENTIFICNAME)

cod_fb_data <- which_fish(cod_name, using="ScientificName", fish.data)

Gad <- which_fish("Gadiformes", "Genus", fish.data)
Gad_names <- fish_names(fish.data[Gad])

out <- getLengthWeight(fish.data[1:2])
out_tmp<-as.data.frame(out$Oreochromis_mossambicus)

# reef <- which_fish("reef", "habitat", fish.data)
# nocturnal <- which_fish("nocturnal", "trophic", fish.data)
