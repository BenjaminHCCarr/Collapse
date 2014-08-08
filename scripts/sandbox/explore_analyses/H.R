# The code creates the H table of the nest survival model

fake = read.csv("fake_stocks.csv")
head(fake)
collapse = as.numeric(fake$collapse_admb)
y_start = as.numeric(fake$YEAR_START)
y_end = as.numeric(fake$YEAR_END)
year_first = 1945
year_last = 2010

years = seq(year_first,year_last,1)
table = matrix(nrow = length(fake$ASSESSID),ncol = length(years))

for(i in 1:length(fake$ASSESSID))
    for(j in 1:length(years))
        {
        if(collapse[i] == 0)
            {
            if(years[j] < y_start[i])
                table[i,j] = NA
            if(years[j] >= y_start[i] && years[j] <= y_end[i])
                table[i,j] = 1
            if(years[j] > y_end[i])
                table[i,j] = NA
            }
        if(collapse[i] == 1)
            {
            if(years[j] < y_start[i])
                table[i,j] = NA
            if(years[j] >= y_start[i] && years[j] < y_end[i])
                table[i,j] = 1
            if(years[j] == y_end[i])
                table[i,j] = 0
            if(years[j] > y_end[i])
                table[i,j] = NA
            }
        }