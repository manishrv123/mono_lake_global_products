library(RColorBrewer)
library(dplyr)
library(plyr)
### Mono Lake Stage-Storage-Area-Salinity Curves ###
wdir = paste("C:/Users/manis/OneDrive - Umich/Documents/PersonalMonoLakePlayground/DataOrganization")
indir = paste("C:/Users/manis/OneDrive - Umich/Documents/PersonalMonoLakePlayground/DataOrganization")
outdir = paste("C:/Users/manis/OneDrive - Umich/Documents/PersonalMonoLakePlayground/DataOrganization/Plots")
setwd(indir)
## Takes a y vector, returns start and end points of data and number of segments


p = data.frame(read.csv("Spreadsheets//Precipitation_global.csv",na.strings=""))
p$CAPA = as.numeric(p$CAPA)
p[,1] = as.Date(as.character(p[,1]), tryFormats=c("%m/%d/%Y"))
e = data.frame(read.csv("Spreadsheets//Evaporation_PET.csv"))
e[,1] = as.Date(as.character(e[,1]), tryFormats=c("%m/%d/%Y"))
y <- merge(x=p,y=e, 
           by="Date", all=TRUE)

y$years <- format(y$Date, "%Y")
x_years <- as.numeric(unique(y$years))
startyear = strtoi(y$years[1])
endyear = strtoi(y$years[length(y$years)])






plotname=paste(outdir,"Precip_Evap_Global_Yearly.pdf") #Comment to SIP
pdf(file=plotname, height = 6, width = 9, paper = "special") 
par(xpd=TRUE)
par(mfrow=c(2,1))
par(mar = c(0, 4, 0, 0))
par(oma = c(3,4,1,3))


#y[,1] = as.Date(as.character(y[,1]), tryFormats=c("%Y-%m-%d"))
#precip_cols = c(2:7)
#y$avg_in_situ = rowMeans(y[precip_cols], na.rm=TRUE)
#s_m = 1300
#e_m = s_m+120
startMo = y$Date[s_m]
endMo = y$Date[e_m]
xpol = c(startMo, startMo,endMo,endMo)
cols <- brewer.pal(8,'Dark2')

## Precip_global_sample##
plot(c(0),c(0),xlab="Date", ylab="Precip Rate (in)", type = "n", col = 8, lwd = 3, axes = FALSE,
     ylim = c(0,75)*1.1, xlim = c(1901,2022))
legend=array(NA)
precip_cols = c(2:7)
legend_counter=1
for (i in precip_cols) {
  print(i)
  temp = tapply(y[,i],y$years,sum)
  s = data.frame(template=names(temp),sum=temp)$sum
  lines(x_years, s, col = cols[legend_counter],  lwd = 1)
  legend[legend_counter]=colnames(y)[i]
  legend_counter=legend_counter+1
}
axis(2, labels=T, cex.axis = 0.9)
axis(3, pretty(x_years), cex=0.9, labels=F)
box()
legend("topleft", legend = legend, lty=c(1), 
       col=cols, cex=0.8)

##Evaporation_global_products##
plot(c(0),c(0),xlab="Date", ylab="Evap Rate (in)", type = "n", col = 8, lwd = 3, axes = FALSE,
     ylim = c(0,75)*1.1, xlim = c(1901,2022))
legend=array(NA)
precip_cols = c(8:13)

legend_counter=1
for (i in precip_cols) {
  print(i)
  temp = tapply(y[,i],y$years,sum)
  s = data.frame(template=names(temp),sum=temp)$sum
  lines(x_years, s, col = cols[legend_counter],  lwd = 1)
  legend[legend_counter]=colnames(y)[i]
  legend_counter=legend_counter+1
}
axis(4, labels=T, cex.axis = 0.9)
axis(1, pretty(x_years), cex=0.9, labels=T)
box()
legend("topleft", legend = legend, lty=c(1), 
       col=cols, cex=0.8)


dev.off()
