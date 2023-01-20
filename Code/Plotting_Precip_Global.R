library(RColorBrewer)

### Mono Lake Stage-Storage-Area-Salinity Curves ###
wdir = paste("C:/Users/manis/OneDrive - Umich/Documents/PersonalMonoLakePlayground/DataOrganization")
indir = paste("C:/Users/manis/OneDrive - Umich/Documents/PersonalMonoLakePlayground/DataOrganization")
outdir = paste("C:/Users/manis/OneDrive - Umich/Documents/PersonalMonoLakePlayground/DataOrganization")

setwd(indir)
## Takes a y vector, returns start and end points of data and number of segments
where2plot <- function(datavector){
  ireal <- which(!is.na(datavector))
  ipts <- c()
  icount <- 0
  for (i in 1:length(ireal)){
    if (ireal[i]>1 && ireal[i]<length(datavector)&& is.na(datavector[ireal[i]-1]) || is.na(datavector[ireal[i]+1])){
      icount = icount + 1
      ipts[icount] = ireal[i]
    }
    if (ireal[i]>1 && ireal[i]<length(datavector)&& is.na(datavector[ireal[i]-1]) && is.na(datavector[ireal[i]+1])){
      print("CONTAINS INDIVIDUAL DATA POINTS. CHANGE FUNCTION.")
      break
    }
    else if (ireal[i]==1){
      print("added")
      icount=icount+1
      ipts[1]=0
      
    }
  }
  inum = length(ipts)/2
  isrtpts = ipts[c(TRUE,FALSE)]
  iendpts = ipts[c(FALSE, TRUE)]
  if (is.na(iendpts)){
    iendpts=length(datavector)
  }
  iret = list(Startpts = isrtpts, Endpts = iendpts, segNum = inum)
  return(iret)
}



overlap <- function(P,E,NR,D){
  start = min(which(!is.na(P) & !is.na(E) & !is.na(NR) & !is.na(D)))
  end = max(which(!is.na(P) & !is.na(E) & !is.na(NR) & !is.na(D)))
  iret = list(Startpt = start, Endpt = end)
}

cols = c(12,3, 4)
##########
### Summing
##########
par(mfrow=c(2,1))
par(mar = c(0,0,0,0))
par(oma = c(3,4,1,3))
y = data.frame(read.csv("Precipitation_global.csv"))
y[,1] = as.Date(as.character(y[,1]), tryFormats=c("%m/%d/%Y"))
startMo = y$Date[1]
endMo = y$Date[length(y$Date)]
xpol = c(startMo, startMo,endMo,endMo)

## Precip

cols <- brewer.pal(length(y),'Set2')
# Precip
plot(c(0), c(0), type = "n", col = 8, lwd = 3, axes = FALSE,
     ylim = c(min(y[,4], na.rm=T),max(y[,4], na.rm=T))*1.1, xlim = c(startMo, endMo))
legend=array(NA)
for (i in 2:7) {

  p1start1 = 0
  p1end1 = 1400
  lines(y$Date[p1start1:p1end1], y[,i][p1start1:p1end1], col = cols[i-1],  lwd = 1)
  legend[i-1]=colnames(y)[i]
}
axis(2, labels=T, cex.axis = 0.9)
axis.Date(1, y$Date, cex=0.9, labels=F)
box()
legend("topleft", legend = legend, lty=c(1), 
       col=cols, cex=0.8)
