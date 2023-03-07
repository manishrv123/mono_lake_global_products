library(RColorBrewer)
library(dplyr)
library(plyr)
library(tidyverse)
library(readxl)
library(openxlsx)



y = data.frame(read_excel("G:\\Shared drives\\SEAS-Hydro (MONO LAKE)\\Mono_Lake_All_Data_Spreadsheet.xlsx",na=c("NA",""),row.names(1)))
colnames = colnames(y)
header_rows = 6
data_rows = c(header_rows+1:1546)
y$Date[data_rows] = as.character(seq(from = as.Date("1895-01-01"), to = as.Date("2023-10-01"), by = 'month'))


y$years[data_rows] <- format(as.Date(y$Date[data_rows]), "%Y")
x_years<- as.numeric(unique(y$years[data_rows]))
startyear = strtoi(y$years[header_rows+1])
endyear = strtoi(y$years[length(y$years)])

global_precip = which(y[3,]=="p")
evap = which(y[3,]=="e")
in_situ_overlake_product = which(y[3,]=="ipa")  #Stations in OverLake Product: 040943 (Bodie), Cain, 040684, MNL, 044881, 045779, UNDC1, CAMN1 (CAMN001?) (only 11 months with discrepancies), CAMN6, CQ251, C4643




plotname=paste("Plots//Precip_Evap_Global_Yearly.pdf") #Comment to SIP3
pdf(file=plotname, height = 6, width = 9, paper = "special") 
par(xpd=TRUE)
par(mfrow=c(2,1))
par(mar = c(0, 4, 0, 0))
par(oma = c(3,4,1,3))

xpol = c(startMo, startMo,endMo,endMo)
cols <- brewer.pal(8,'Dark2')

## Precip_global_sample##
plot(c(0),c(0),xlab="Date", ylab="Precip Rate (In)", type = "n", col = 8, lwd = 3, 
     axes = FALSE,ylim=c(0,100), xlim = c(x_years[1],x_years[length(x_years)]))
legend=array(NA)
#precip_cols = which(grepl("yp",colnames(y))==TRUE)
legend_counter=1
for (i in global_precip) {
  print(i)
  temp = tapply(as.numeric(y[data_rows,i]),y$years[data_rows],sum)
  s = data.frame(template=names(temp),sum=temp)$sum
  lines(x_years, s, col = cols[legend_counter],  lwd = 1)
  legend[legend_counter]=strsplit(colnames(y)[i], split = "[.]")[[1]][1]
  legend_counter=legend_counter+1
}
axis(2, labels=T, cex.axis = 0.9)
axis(3, x_years, cex=0.9, labels=F,at=seq(1895,2025,by=10))
box()
legend("topleft", legend = legend, lty=c(1), 
       col=cols, cex=0.8,bty="n")

##Evaporation_global_products##
plot(c(0),c(0),xlab="Date", ylab="Evap Rate (In)", type = "n", col = 8, lwd = 3, axes = FALSE,
     ,ylim=c(0,100), xlim = c(x_years[1],x_years[length(x_years)]))
legend=array(NA)
#precip_cols = which(grepl("ye",colnames(y))==TRUE)

legend_counter=1
for (i in evap) {
  print(i)
  temp = tapply(as.numeric(as.numeric(y[data_rows,i])),y$years[data_rows],sum)
  s = as.numeric(data.frame(template=names(temp),sum=temp)$sum)
  lines(x_years, s, col = cols[legend_counter],  lwd = 1)
  legend[legend_counter]=strsplit(colnames(y)[i], split = "[.]")[[1]][1]
  legend_counter=legend_counter+1
}
axis(4, labels=T, cex.axis = 0.9)
axis(1, x_years, cex=0.9, labels=T,at=seq(1895,2025,by=10))
box()
legend("topleft", legend = legend, lty=c(1), 
       col=cols, cex=0.8,bty="n")



dev.off()


