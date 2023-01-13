library(RColorBrewer)
library(dplyr)
### Mono Lake Stage-Storage-Area-Salinity Curves ###
wdir = paste("C:/Users/manis/OneDrive - Umich/Documents/PersonalMonoLakePlayground/DataOrganization")
indir = paste("C:/Users/manis/OneDrive - Umich/Documents/PersonalMonoLakePlayground/DataOrganization")
outdir = paste("C:/Users/manis/OneDrive - Umich/Documents/PersonalMonoLakePlayground/DataOrganization/Plots")
setwd(indir)

p = data.frame(read.csv("Precipitation_global.csv"))
p[,1] = as.Date(as.character(p[,1]), tryFormats=c("%m/%d/%Y"))
e = data.frame(read.csv("Evaporation_PET.csv"))
e[,1] = as.Date(as.character(e[,1]), tryFormats=c("%m/%d/%Y"))
y <- merge(x=p,y=e, 
           by="Date", all=TRUE)
plotname=paste(outdir,"Precip_Evap_Global.pdf") #Comment to SIP
pdf(file=plotname, height = 6, width = 9, paper = "special") 
par(xpd=TRUE)
par(mfrow=c(2,1))
par(mar = c(0, 4, 0, 0))
par(oma = c(3,4,1,3))


#y[,1] = as.Date(as.character(y[,1]), tryFormats=c("%Y-%m-%d"))
#precip_cols = c(2:7)
#y$avg_in_situ = rowMeans(y[precip_cols], na.rm=TRUE)
s_m = 1300
e_m = s_m+120
startMo = y$Date[s_m]
endMo = y$Date[e_m]
xpol = c(startMo, startMo,endMo,endMo)
cols <- brewer.pal(8,'Dark2')

## Precip_global_sample##
plot(c(0),c(0),xlab="Date", ylab="Precip Rate (in)", type = "n", col = 8, lwd = 3, axes = FALSE,
     ylim = c(0,15)*1.1, xlim = c(startMo,endMo))
legend=array(NA)
precip_cols = c(2:7)
legend_counter=1
for (i in precip_cols) {
  print(i)
  lines(y$Date[s_m:e_m], y[,i][s_m:e_m], col = cols[legend_counter],  lwd = 1)
  legend[legend_counter]=colnames(y)[i]
  legend_counter=legend_counter+1
}
axis(2, labels=T, cex.axis = 0.9)
axis.Date(3, y$Date[s_m:e_m], cex=0.9, labels=F)
box()
legend("topleft", legend = legend, lty=c(1), 
      col=cols, cex=0.8)

##Evaporation_global_products##
plot(c(0),c(0),xlab="Date", ylab="Evap Rate (in)", type = "n", col = 8, lwd = 3, axes = FALSE,
     ylim = c(0,15)*1.1, xlim = c(startMo,endMo))
legend=array(NA)
precip_cols = c(9:14)

legend_counter=1
for (i in precip_cols) {
  print(i)
  lines(y$Date[s_m:e_m], y[,i][s_m:e_m], col = cols[legend_counter],  lwd = 1)
  legend[legend_counter]=colnames(y)[i]
  legend_counter=legend_counter+1
}
axis(4, labels=T, cex.axis = 0.9)
axis.Date(1, y$Date[s_m:e_m], cex=0.9, labels=T)
box()
legend("topleft", legend = legend, lty=c(1), 
       col=cols, cex=0.8)


dev.off()

