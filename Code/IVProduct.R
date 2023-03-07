library(RColorBrewer)
library(RColorBrewer)
library(dplyr)
library(plyr)
library(readxl)
library(openxlsx)



y = data.frame(read_excel("G:\\Shared drives\\SEAS-Hydro (MONO LAKE)\\Mono_Lake_All_Data_Spreadsheet.xlsx",na=c("NA",""),row.names(1)))
header_rows = 6
data_rows = c(header_rows+1:1546)
y$Date[data_rows] = as.character(seq(from = as.Date("1895-01-01"), to = as.Date("2023-10-01"), by = 'month'))

in_situ_overlake_product = which(y[3,]=="ipa")  #Stations in OverLake Product: 040943 (Bodie), Cain, 040684, MNL, 044881, 045779, UNDC1, CAMN1 (CAMN001?) (only 11 months with discrepancies), CAMN6, CQ251, C4643
counter=2
temp_work =data.frame(date = y$Date[data_rows])
for (i in in_situ_overlake_product){ #Convert to Volumes
  temp_work[counter] = (as.numeric((y[data_rows,i])))
  counter=counter+1
}

y$in_situ_average[data_rows]= rowMeans(temp_work[2:12], na.rm=TRUE) #Take Average
caleb_yearly_avg = 11.22 #inches
vorster_yearly_avg = 8 #inches
y$years[data_rows] <- format(as.Date(y$Date[data_rows]), "%Y")
x_years<- as.numeric(unique(y$years[data_rows]))


temp = tapply(as.numeric(y$in_situ_average[data_rows]),y$years[data_rows],sum)
df <- data.frame(template=names(temp),mean=temp)
in_situ_yearly_average = mean(df$mean,na.rm=TRUE)
cain_ranch = as.numeric(y[data_rows,which(y[1,]=="CAIN")])
cain_yearly_aggregate = tapply(as.numeric(cain_ranch),y$years[data_rows],sum)
cain_yearly_means <- data.frame(template=names(cain_yearly_aggregate),mean=cain_yearly_aggregate)
cain_yearly_average = mean(cain_yearly_means$mean,na.rm=TRUE)



Caleb_product =(y$in_situ_average[data_rows] /in_situ_yearly_average)*caleb_yearly_avg
vorster_product =(cain_ranch/cain_yearly_average)*vorster_yearly_avg

s = data.frame(Date = y$Date[data_rows], caleb_prod = Caleb_product, vorster_prod = vorster_product)
write.csv(s, "DataSources\\Index_of_Variance\\IVproducts.csv", na = "",row.names = FALSE)
