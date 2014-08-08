
# ADMB

## Here is the lookup_table.csv whith the relation of the ASSESSID's and a shorthand of them: short_id.

## Here are the ADMB input/output files.

In each ADMB folder we have a .dat file for each stock and a folder for each stock. The name of the stock is short_id in lookup_table.csv. 

To run the ADMB you need two files with the same basename:

1. .tpl contains the ADMB code.
2. .dat contains the database which in turn contains
2.a nobs = the number of observations in the time series
2.b maxB = the maximum biomass in the time series
2.c data = a two column table with the landings and the biomass observed each year (the number of rows must be nobs)

Both files can be written into .txt files and then changing their extensions to .tpl and .dat respectively.

To run on bash:

1.Download ADMB (to run on Mac) from http://www.admb-project.org/buildbot/documentation/QuickStartMacOS.html
2. In bash go to the folder where the .tpl and .dat files are
3. In bash: admb <tpl basename>
4. If everything went well an executable with the tpl basename must be created
5. Then run the model in bash: ./<basename>
6. If everything went well .rep will contain:
6.a BMSY = biomass of maximal sustainable yield 
6.b FMSY = fishing mortality of MSY
6.c MSY
6.d K = carrying capacity
6.e Pobs = vector (length = nobs) of observed surplus production
6.f Ppred = vector (length = nobs) of predicted surplus production

## Here is the R script that grabs the BMSY from the .rep files and produces the BMSY.csv

