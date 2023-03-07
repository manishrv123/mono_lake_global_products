library(RColorBrewer)
library(RColorBrewer)
library(dplyr)
library(plyr)
library(tidyverse)
library(readxl)
library(openxlsx)
## Takes a y vector, returns start and end points of data and number of segments


y = data.frame(read_excel("G:\\Shared drives\\SEAS-Hydro (MONO LAKE)\\Mono_Lake_All_Data_Spreadsheet.xlsx",na=c("NA",""),row.names(1)))
colnames = colnames(y)
header_rows = 6
data_rows = c(header_rows+1:1546)
y$Date[data_rows] = as.character(seq(from = as.Date("1895-01-01"), to = as.Date("2023-10-01"), by = 'month'))


#plotname=("Plots//Products_Comparison.pdf") #Comment to SIP3
#pdf(file=plotname, height = 6, width = 9, paper = "special") 
par(mfrow=c(3,1))
par(mar = c(0,4,0,0))
par(oma = c(3,4,1,3))
startdate = '2000-06-01'
enddate ='2010-06-01'
s_m = which(y$Date==startdate)
e_m = which(y$Date==enddate)

CAPA_in_situ_Avg = c(9,57)
in_situ_overlake_product = which(y[3,]=="ipa")  #Stations in OverLake Product: 040943 (Bodie), Cain, 040684, MNL, 044881, 045779, UNDC1, CAMN1 (CAMN001?) (only 11 months with discrepancies), CAMN6, CQ251, C4643
evap = which(y[3,]=="e")
global_precip = which(y[3,]=="p"&y[1,]!="CAPA")




cols <- array(NA)

plot(c(0),c(0),xlab="Date", ylab="Precip Rate (In)", type = "n", col = 8, lwd = 3, 
     axes = FALSE,ylim=c(0,10),xlim=c(as.Date(startdate),as.Date(enddate)))
legend=array(NA)
legend_counter=1


counter=2
temp_work =data.frame(date = y$Date[data_rows])

for (i in in_situ_overlake_product){ #Convert to Volumes
  temp_work[counter] = (as.numeric((y[data_rows,i])))
  counter=counter+1
}

y$in_situ_average[data_rows]= rowMeans(temp_work[2:12], na.rm=TRUE) #Take Average


lines(as.Date(y$Date[s_m:e_m]), as.numeric(y[,9][s_m:e_m]), col = "blue",  lwd = 1)
legend[1]=strsplit(colnames(y)[9], split = "[.]")[[1]][1]
cols[1]="blue"
lines(as.Date(y$Date[s_m:e_m]), as.numeric(y$in_situ_average[s_m:e_m]), col = "green",  lwd = 1)
legend[2]="in_situ_average"
legend_counter = legend_counter+1
cols[2]="green"
  
axis(2, labels=T, cex.axis = 0.9)
axis.Date(3, y$Date[s_m:e_m], cex=0.9, labels=F)
box()
legend("topleft", legend = legend, lty=c(1), 
       col=cols, cex=0.8,bty="n")
plot(c(0),c(0),xlab="Date", ylab="Precip Rate (In)", type = "n", col = 8, lwd = 3, 
     axes = FALSE,ylim=c(0,10),xlim=c(as.Date(startdate),as.Date(enddate)))
legend=array(NA)
legend_counter=1
cols=array(NA)
cols[1]="blue"
  lines(as.Date(y$Date[s_m:e_m]), as.numeric(y[,9][s_m:e_m]), col = "blue",  lwd = 1)
  legend[1] = "CAPA"
  for (i in in_situ_overlake_product) {
    lines(as.Date(y$Date[s_m:e_m]), as.numeric(y[,i][s_m:e_m]), col = "green",  lwd = 1)
    
  }
  axis(2, labels=T, cex.axis = 0.9)
  axis.Date(1, y$Date[s_m:e_m], cex=0.9, labels=T)
  box()
  cols[2] = "green"
  legend[2] ="in_situ_stations"
  legend("topleft", legend = legend, lty=c(1), 
         col=cols, cex=0.8,bty="n")
  
  
  plot(c(0),c(0),xlab="Date", ylab="Precip Rate (In)", type = "n", col = 8, lwd = 3, 
       axes = FALSE,ylim=c(0,10),xlim=c(as.Date(startdate),as.Date(enddate)))
  legend=array(NA)
  legend_counter=1
  cols=array(NA)
  cols[1]="blue"
    lines(as.Date(y$Date[s_m:e_m]), as.numeric(y[,9][s_m:e_m]), col = "blue",  lwd = 1)
    legend[1] = "CAPA"
    
    for (i in global_precip) {
      lines(as.Date(y$Date[s_m:e_m]), as.numeric(y[,i][s_m:e_m]), col = "green",  lwd = 1)
      
      legend[legend_counter+1]=strsplit(colnames(y)[i], split = "[.]")[[1]][1]
      legend_counter = legend_counter+1
      cols[legend_counter+1] = "green"
    }
    axis(2, labels=T, cex.axis = 0.9)
    axis.Date(1, y$Date[s_m:e_m], cex=0.9, labels=T)
    box()
    legend("topleft", legend = legend, lty=c(1), 
           col=cols, cex=0.8,bty="n")
    
    
#dev.off()
    