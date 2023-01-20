library(tidyverse)
library(readxl)
library(openxlsx)

#Set the folder where the spreadsheet is in the setwd() function,
#or move the spreadsheet to the same folder the code is in and comment the line

setwd("C://Users//manis//OneDrive - Umich//Documents//PersonalMonoLakePlayground//DataOrganization//Spreadsheets")


#Function to remove zeroes -> Not Important
removezeroes <- function(startdate,enddate) {
startind = which(l2swbm_data$date==startdate)
endind = which(l2swbm_data$date==enddate)
priorRange = c(startind:endind)

for (col in c(2:length(l2swbm_data))) {
  indexes = which(l2swbm_data[priorRange,col]==0)
  if (length(indexes)>0){
    print(paste("Zeroes Found in: ", colnames(l2swbm_data)[col]))
    print(paste("First index in priorRange:",indexes[1]))
  }

  for (i in indexes){
    l2swbm_data[priorRange[i],col] = (l2swbm_data[priorRange[i]+1,col]+l2swbm_data[priorRange[i]-1,col])/2
  }
}

print(indexes)
}


#Read the Data Sheet
raw_data = data.frame(read_excel("Mono_Lake_All_Data_Spreadsheet.xlsx",na=c("NA","")))
colnames = colnames(raw_data)
header_rows = 5
data_rows = c(header_rows+1:1546)

#Select Columns for L2SWBM
in_situ_products_all = c(17:36)
in_situ_overlake_product = c(17,27, 23,33,22,24,26,29,30,28,36)  #Stations in OverLake Product: 040943 (Bodie), Cain, 040684, MNL, 044881, 045779, UNDC1, CAMN1 (CAMN001?) (only 11 months with discrepancies), CAMN6, CQ251, C4643
global_precip = c(5:10)
global_evap = c(11:16)
diversions = 40 #Only one currently to be used is East Portal IIRC
water_level = 4
natural_streamflow = c(41)
actual_streamflow = c()

#Create L2SWBM Spreadsheet
l2swbm_data = data.frame(date = seq(from = as.Date("1895-01-01"), to = as.Date("2023-10-01"), by = 'month'))
precip_counter=1
evap_counter=1
runoff_counter=1
temp_work =data.frame(date = raw_data$Date[data_rows])

#####DStore#####
l2swbm_data$yd1.DStore_StaffGage = as.numeric(unlist(raw_data[data_rows,water_level]))*as.numeric(unlist(data_rows,raw_data$Area))

#####Averages of Insitu############

counter=2
for (i in in_situ_overlake_product){ #Convert to Volumes
  temp_work[counter] = (as.numeric((raw_data[data_rows,i]))/12)*as.numeric(raw_data$Area[data_rows])
  counter=counter+1
}

l2swbm_data$yp1.in_situ_average= rowMeans(temp_work[2:12], na.rm=TRUE) #Take Average
l2swbm_data$yp1.in_situ_average[is.nan(l2swbm_data$yp1.in_situ_average)]<-NA
precip_counter=precip_counter+1

#####Global############

for (i in global_precip){ #Get Volumes of each product & Create Column Names
  raw_data[data_rows,i] = (as.numeric((raw_data[data_rows,i]))/12)*as.numeric(raw_data$Area[data_rows])
  coln = paste("yp",toString(precip_counter),".",strsplit(colnames[i], split = "[.]")[[1]][1], sep = "")
  l2swbm_data$temp = as.numeric(raw_data[data_rows,i])
  colnames(l2swbm_data)[which(names(l2swbm_data) == "temp")] <- coln
  precip_counter=precip_counter+1
}

for (i in global_evap){ #Get Volumes of each product & Create Column Names
  raw_data[data_rows,i] = (as.numeric((raw_data[data_rows,i]))/12)*as.numeric(raw_data$Area[data_rows])
  coln = paste("ye",toString(evap_counter),".",strsplit(colnames[i], split = "[.]")[[1]][1], sep = "")
  l2swbm_data$temp = as.numeric(raw_data[data_rows,i])
  colnames(l2swbm_data)[which(names(l2swbm_data) == "temp")] <- coln
  evap_counter=evap_counter+1
}

#####Runoff Calculation######

for (i in natural_streamflow){
  raw_data[data_rows,i] = (as.numeric(raw_data[data_rows,i])*0.000810714)-as.numeric(raw_data[data_rows,diversions]) #Cubic Meters to Acre Feet
  coln = paste("yr",toString(runoff_counter),".",strsplit(colnames[i], split = "[.]")[[1]][1], sep = "")
  l2swbm_data$temp = as.numeric(raw_data[data_rows,i])
  colnames(l2swbm_data)[which(names(l2swbm_data) == "temp")] <- coln
  runoff_counter=runoff_counter+1
}

#Use this function to remove zeroes in data with averages of surronding values
#removezeroes(as.Date("1905-01-01"),as.Date("2005-01-01"))

write.csv(l2swbm_data, "L2SWBM_data.csv", row.names=FALSE, na = "")

