library(lubridate)
library(tidyverse)
library(dplyr)
crutsprecip <- read.csv(file = 'CRUTS/CRUTS_pre_mm_per_day.csv')
merra2precip <- read.csv(file = 'MERRA2/MERRA2_PRECTOT_kg_m-2s-1.csv')
local <- read.csv(file = 'Precipitation_in_situ.csv')
crutsprecip$row_mean_all <- rowMeans(crutsprecip[2:5], na.rm=TRUE) /25.4
crutsprecip$row_mean_mountain_out <- rowMeans(crutsprecip[3:5], na.rm=TRUE) /25.4
merra2precip$row_mean_all <- ((rowMeans(merra2precip[2:3], na.rm=TRUE) *86400)*30)/25.4
merra2precip$row_mean_mountain_out <- ((rowMeans(merra2precip[2], na.rm=TRUE) *86400)*30)/25.4
crutsprecip$Date <- as.Date(crutsprecip$Date, "%m/%d/%Y")
local$Date <- as.Date(local$Date, "%m/%d/%Y")
merra2precip$Date <- as.Date(merra2precip$Date, "%m/%d/%Y")
crutsprecip <- merge(x=crutsprecip,y=local, 
           by="Date", all=FALSE)
merra2precip <- merge(x=merra2precip,y=local, 
                     by="Date", all=FALSE)


par(mfrow=c(2,1))
par(mar = c(0,0,0,0))
par(oma = c(3,4,1,3))
plot(crutsprecip$Date[1000:1120],crutsprecip$row_mean_all[1000:1120],type = "l", col="red" , ylab="y")
lines(crutsprecip$Date[1000:1120],crutsprecip$row_mean_mountain_out[1000:1120], col="blue", ylab="y")
lines(crutsprecip$Date[1000:1120],crutsprecip$CAIN[1000:1120], col="orange", ylab="y")

legend(x = "topright",          # Position
       legend = c("All", "Adj","CAIN"),  # Legend texts
       lty = c(1, 1,1),           # Line types
       col = c("red", "blue","orange"),           # Line colors
       lwd = 1)    
plot(merra2precip$Date[0:120],merra2precip$row_mean_all[0:120],type = "l", col="red")
lines(merra2precip$Date[0:120],merra2precip$row_mean_mountain_out[0:120], col="blue")
lines(merra2precip$Date[0:120],merra2precip$CAIN[0:120], col="orange", ylab="y")


legend(x = "topright",          # Position
       legend = c("All", "Adj","CAIN"),  # Legend texts
       lty = c(1, 1,1),           # Line types
       col = c("red", "blue","orange"),           # Line colors
       lwd = 1)    
