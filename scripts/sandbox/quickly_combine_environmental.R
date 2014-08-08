##make a temporary stocks file , merging in environmental and human covariates to easily plot pairs.
require(dplyr)
stocks_t <- read.csv("data_tables/stocks.csv")
stocks_t <- subset(stocks_t, LME_NUMBER>0)
stocks_t = stocks_t[-which(is.na(stocks_t$final_collapse)),]

Ingrid <- read.csv("original_data/Environmental_data/Environmental_variables_LMES.csv")
Ingrid <- select(Ingrid, LME_NAME, LME_NUMBER, Socioeconomic_Index_.HDI., Fish_species_richness, SST_Change_..C._1982.2006 , Chl.a.avg)
summary(Ingrid)
hist(Ingrid$Chl.a.avg) ###highly right skewed
Ingrid <- transform(Ingrid, ln_Chl = log(Chl.a.avg))
hist(Ingrid$ln_Chl) #perfect
pairs(Ingrid)

stocks_t <- merge(stocks_t, Ingrid, by = "LME_NUMBER", all.x=T, all.y=F)
stocks_t <- droplevels(stocks_t)
stocks_t <- transform(stocks_t, Socioeconomic_Index_.HDI. = as.numeric(Socioeconomic_Index_.HDI.), Fish_species_richness=as.numeric(Fish_species_richness), SST_Change_..C._1982.2006=as.numeric(SST_Change_..C._1982.2006))
str(stocks_t)

hist(stocks_t$q90_ut) #highly right skewed
hist(log(stocks_t$q90_ut)) #better
stocks_t <- transform(stocks_t, ln_q90_ut = log(q90_ut))

pairs(final_collapse ~ ln_q90_ut + Fish_species_richness +Socioeconomic_Index_.HDI. + SST_Change_..C._1982.2006 + ln_Chl, data = stocks_t )
