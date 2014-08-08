#read in:  /Users/nina/Dropbox/NCEAS_OSS/RAM_fisheries/timeseries_values_view.csv
setwd('~/Regional_Collapse/CatchMSY/Output')

#logfile  <- file("make_csv_log.txt")
#sink(logfile, append=TRUE)
#sink(logfile, append=TRUE, type="message")

#replaced with explicit path
#tms <- read.csv(file.choose())
Inputdir <- "../../original_data/"
filename <- "timeseries_values_view.csv"
tms <- read.csv2(paste(Inputdir,filename,sep = ""), header=T, sep = ",", dec=".")

str(tms)
length(levels(tms[,1]))
id <- levels(tms[,1])

MSY_out_tmp<- data.frame(matrix(NA, nrow=nrow(tms), ncol=4))
colnames(MSY_out_tmp)<-c("stock","res","yr","ct")
MSY_out_tmp$stock <- tms$ASSESSID
MSY_out_tmp$yr <- tms$TSYEAR
MSY_out_tmp$ct <- tms$CATCH_LANDINGS
MSY_out_tmp$res <- "Medium"

MSY_out_tmp_no_NA <- MSY_out_tmp[-which(is.na(MSY_out_tmp$ct)),]
cat("Are there any NaNs in File: ", any(is.na(MSY_out_tmp_no_NA$ct)), "\n")

#split data into two frames
data1 <- subset(MSY_out_tmp_no_NA, stock %in% levels(MSY_out_tmp_no_NA$stock)[1:20])
data2 <- subset(MSY_out_tmp_no_NA, stock %in% levels(MSY_out_tmp_no_NA$stock)[21:40])
data3 <- subset(MSY_out_tmp_no_NA, stock %in% levels(MSY_out_tmp_no_NA$stock)[41:60])
data4 <- subset(MSY_out_tmp_no_NA, stock %in% levels(MSY_out_tmp_no_NA$stock)[61:80])
data5 <- subset(MSY_out_tmp_no_NA, stock %in% levels(MSY_out_tmp_no_NA$stock)[81:100])
data6 <- subset(MSY_out_tmp_no_NA, stock %in% levels(MSY_out_tmp_no_NA$stock)[101:120])
data7 <- subset(MSY_out_tmp_no_NA, stock %in% levels(MSY_out_tmp_no_NA$stock)[121:140])
data8 <- subset(MSY_out_tmp_no_NA, stock %in% levels(MSY_out_tmp_no_NA$stock)[141:160])
data9 <- subset(MSY_out_tmp_no_NA, stock %in% levels(MSY_out_tmp_no_NA$stock)[161:180])
data10<- subset(MSY_out_tmp_no_NA, stock %in% levels(MSY_out_tmp_no_NA$stock)[181:200])
data11<- subset(MSY_out_tmp_no_NA, stock %in% levels(MSY_out_tmp_no_NA$stock)[201:220])
data12<- subset(MSY_out_tmp_no_NA, stock %in% levels(MSY_out_tmp_no_NA$stock)[221:240])
data13<- subset(MSY_out_tmp_no_NA, stock %in% levels(MSY_out_tmp_no_NA$stock)[241:260])
data14<- subset(MSY_out_tmp_no_NA, stock %in% levels(MSY_out_tmp_no_NA$stock)[261:280])
data15<- subset(MSY_out_tmp_no_NA, stock %in% levels(MSY_out_tmp_no_NA$stock)[281:300])
data16 <- subset(MSY_out_tmp_no_NA, stock %in% levels(MSY_out_tmp_no_NA$stock)[301:length(levels(MSY_out_tmp_no_NA$stock))])

outfile1 <- "../Output/RC_CMSY_data1.csv"
outfile2 <- "../Output/RC_CMSY_data2.csv"
outfile3 <- "../Output/RC_CMSY_data3.csv"
outfile4 <- "../Output/RC_CMSY_data4.csv"
outfile5 <- "../Output/RC_CMSY_data5.csv"
outfile6 <- "../Output/RC_CMSY_data6.csv"
outfile7 <- "../Output/RC_CMSY_data7.csv"
outfile8 <- "../Output/RC_CMSY_data8.csv"
outfile9 <- "../Output/RC_CMSY_data9.csv"
outfile10<- "../Output/RC_CMSY_data10.csv"
outfile11<- "../Output/RC_CMSY_data11.csv"
outfile12<- "../Output/RC_CMSY_data12.csv"
outfile13<- "../Output/RC_CMSY_data13.csv"
outfile14<- "../Output/RC_CMSY_data14.csv"
outfile15<- "../Output/RC_CMSY_data15.csv"
outfile16<- "../Output/RC_CMSY_data16.csv"

write.csv(data1, outfile1, row.names=FALSE)
write.csv(data2, outfile2, row.names=FALSE)
write.csv(data3, outfile3, row.names=FALSE)
write.csv(data4, outfile4, row.names=FALSE)
write.csv(data5, outfile5, row.names=FALSE)
write.csv(data6, outfile6, row.names=FALSE)
write.csv(data7, outfile7, row.names=FALSE)
write.csv(data8, outfile8, row.names=FALSE)
write.csv(data9, outfile9, row.names=FALSE)
write.csv(data10, outfile10, row.names=FALSE)
write.csv(data11, outfile11, row.names=FALSE)
write.csv(data12, outfile12, row.names=FALSE)
write.csv(data13, outfile13, row.names=FALSE)
write.csv(data14, outfile14, row.names=FALSE)
write.csv(data15, outfile15, row.names=FALSE)
write.csv(data16, outfile16, row.names=FALSE)

sink()
