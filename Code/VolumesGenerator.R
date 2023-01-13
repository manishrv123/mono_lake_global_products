setwd("C://Users//manis//OneDrive - Umich//Documents//PersonalMonoLakePlayground//DataOrganization//Spreadsheets")

p = data.frame(read.csv("Precipitation_global.csv", na.strings=""))
p$CAPA = as.numeric(p$CAPA)
p[,1] = as.Date(as.character(p[,1]), tryFormats=c("%m/%d/%Y"))
e = data.frame(read.csv("Evaporation_PET.csv"))
e[,1] = as.Date(as.character(e[,1]), tryFormats=c("%m/%d/%Y"))
cols = c(2:7)
e_vol = data.frame(e[,1])
for (i in cols){
  e_vol[i] = (e[i]/12)*e$Area
}
cols = c(2:7)
p_vol = data.frame(p[,1])
for (i in cols){
  p_vol[i] = (p[i]/12)*p$Area
}

write.csv(e_vol,"Evaporation_Global_Volume.csv",na = "", row.names = FALSE)
write.csv(p_vol,"Precipitation_Global_Volume.csv",na = "", row.names = FALSE)
