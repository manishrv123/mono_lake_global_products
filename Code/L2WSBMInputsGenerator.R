library(dplyr)
p = data.frame(read.csv("Precipitation_global.csv"))
p[,1] = as.Date(as.character(p[,1]), tryFormats=c("%m/%d/%Y"))
years <- format(p$Date, "%Y")
s = tapply(p$MERRA2, years, sum)



e = data.frame(read.csv("Evaporation_PET.csv"))
e[,1] = as.Date(as.character(e[,1]), tryFormats=c("%m/%d/%Y"))
p2 = data.frame(read.csv("Precipitation_in_situ.csv"))
p2[,1] = as.Date(as.character(p2[,1]), tryFormats=c("%m/%d/%Y"))

df2 <- merge(x=p,y=e, 
             by="Date", all=TRUE)
write.csv(df2,"L2SWBM_inputs_orig.csv", row.names = FALSE,na='')
df2 <- merge(x=p,y=p2, 
             by="Date", all=TRUE)
write.csv(df2,"Precipitation_combined.csv", row.names = FALSE,na='')

