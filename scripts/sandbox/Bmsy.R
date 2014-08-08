# load data
setwd("/Users/efuller/Documents/Projects/Regional_Collapse/datasets")
require(xlsx)

# import sheet titled "reference_point_values_view" from RAM exel snapshot if haven't already pulled out spreadsheet.
if(!file.exists("Bmsy.csv")){
  Bmsy <- read.xlsx(file="RAM-Legacy-spreadsheet-snapshot-20110815.xlsx", sheetIndex=17)
  write.csv(Bmsy, file="Bmsy.csv")
}else{
  Bmsy <- read.csv("Bmsy.csv")
}

if(!file.exists("timeSeries.Rdata")){
  data <- read.xlsx(file="RAM-Legacy-spreadsheet-snapshot-20110815.xlsx", sheetIndex=15)
  save(data, file="timeSeries.Rdata")
}else{
  load("timeSeries.Rdata")
}

# proportion of stocks that have B_msy
  # number of stocks that have Bmsy
  length(which(!is.na(Bmsy$BMSY))) # 47

  # proportion
  cat(round(length(which(!is.na(Bmsy$BMSY)))/length(Bmsy$BMSY),3)*100,"%")

# which stocks go below Bmsy

  # subset to stocks that have a Bmsy
  have_it <- subset(Bmsy, !is.na(BMSY))
  bdata <- subset(data, ASSESSID %in% have_it)

  # for each of the unique stock IDs, which ever go below 20% of Bmsy
    # make a column for value of 20% of Bmsy
    have_it$BMSY <- as.numeric(have_it$BMSY)
    have_it$collapse_val <- 0.2*have_it$BMSY

# proportion of stocks that go below 20% of B_msy by region
  # for each 