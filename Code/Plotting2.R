library(RColorBrewer)

wdir = paste("C:/Users/manis/OneDrive - Umich/Documents/PersonalMonoLakePlayground/DataOrganization")
indir = paste("C:/Users/manis/OneDrive - Umich/Documents/PersonalMonoLakePlayground/DataOrganization")
outdir = paste("C:/Users/manis/OneDrive - Umich/Documents/PersonalMonoLakePlayground/DataOrganization")

setwd(indir)
## Takes a y vector, returns start and end points of data and number of segments



par(mfrow=c(2,1))
par(mar = c(0,0,0,0))
par(oma = c(3,4,1,3))
y = data.frame(read.csv("Precipitation_combined.csv"))
y[,1] = as.Date(as.character(y[,1]), tryFormats=c("%Y-%m-%d"))
s_m = 750
e_m = 1000
startMo = y$Date[s_m]
endMo = y$Date[e_m]
xpol = c(startMo, startMo,endMo,endMo)

## Precip ##

cols <- brewer.pal(8,'Set2')


plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE,
     ylim = c(min(y[,4], na.rm=T),max(y[,4], na.rm=T))*1.1, xlim = c(startMo,endMo))
legend=array(NA)
startind=19
for (i in startind:22) {
  lines(y$Date[s_m:e_m], y[,i][s_m:e_m], col = cols[i-startind+1],  lwd = 1)
  legend[i-startind+1]=colnames(y)[i]
}
axis(2, labels=T, cex.axis = 0.9)
axis.Date(1, y$Date[s_m:e_m], cex=0.9, labels=T)
box()
legend("topleft", legend = legend, lty=c(1), 
      col=cols, cex=0.8)

