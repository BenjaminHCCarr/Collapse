## This R script grabs the BMSY from the .rep files and produces the BMSY.csv

# Read the lookup_table.csv
list = read.csv(file.choose(),header = T)
list1 = list[,-1]

BMSY = vector(length = dim(list1)[1])

root = getwd() # the root should be one level up from where the .rep file is
for(i in 1:length(BMSY))
	{
	path = file.path(root,list1[i,2])
	setwd(path)
	filename = paste(as.character(list1[i,2]),".rep",sep = "")
	data = read.csv(filename,header = F,comment.char = "\t")
	BMSY[i] = data[[1]][1]
	setwd(root)
	}
BMSY = cbind(list,BMSY)
BMSY = BMSY[,-1]
names(BMSY) = c("ASSESSID","short_ID","BMSY")

# Read the stocks.csv
stocks = read.csv(file.choose(),header = T)
names(BMSY) = c("ASSESSID","short_ID","BMSY")
stocks1 = merge(stocks,BMSY,by = "ASSESSID",all.x = T)

write.csv(stocks1,file = "BMSY.csv")