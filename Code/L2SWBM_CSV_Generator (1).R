library(tidyverse)
library(readxl)
library(openxlsx)

#Set the folder where the spreadsheet is in the setwd() function,
#or move the spreadsheet to the same folder the code is in and comment the line
wd = getwd()
setwd(wd)


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
raw_data = data.frame(read_excel("G:\\Shared drives\\SEAS-Hydro (MONO LAKE)\\Mono_Lake_All_Data_Spreadsheet.xlsx",na=c("NA","")))
colnames = colnames(raw_data)
header_rows = 6
data_rows = c(header_rows+1:1546)

#Select Columns for L2SWBM
in_situ_products_all = which(TRUE==grepl("ipa|ip",raw_data[3,]))
in_situ_overlake_product = which(raw_data[3,]=="ipa")  #Stations in OverLake Product: 040943 (Bodie), Cain, 040684, MNL, 044881, 045779, UNDC1, CAMN1 (CAMN001?) (only 11 months with discrepancies), CAMN6, CQ251, C4643
global_precip = which(raw_data[3,]=="p")
evap = which(raw_data[3,]=="e")
water_level = which(raw_data[1,]=="Mono_Lake_Water_Levels")
TNC_rush = which(raw_data[1,]=="TNC - Rush" )
TNC_LV = which(raw_data[1,]=="TNC - Lee Vining" )
MTWP = which(raw_data[1,]=="MTWP - MONO TUNNEL AT WEST PORTAL" )
GLSC = which(raw_data[1,]=="GLRSC - GRANT LAKE RESERVOIR STORAGE CHANGE" )
DWP_LV = which(raw_data[1,]=="5009 - LEE VINING CREEK AT INTAKE" )
DWP_WC = which(raw_data[1,]=="5002 - WALKER CREEK UNDER CONDUIT" )
DWP_PC = which(raw_data[1,]=="5003 - PARKER CREEK UNDER CONDUIT" )
GLRML = which(raw_data[1,]=="GLRML - GRANT LAKE RELEASE TO MONO LAKE" )
water_volume = which(raw_data[3,]=="v")



#Create L2SWBM Spreadsheet
l2swbm_data = data.frame(date = seq(from = as.Date("1895-01-01"), to = as.Date("2023-10-01"), by = 'month'))
precip_counter=1
evap_counter=1
runoff_counter=1
temp_work =data.frame(date = raw_data$Date[data_rows])

#####DStore#####
l2swbm_data$yd1.Store_StaffGage = raw_data[data_rows,water_volume]
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

for (i in evap){ #Get Volumes of each product & Create Column Names
  raw_data[data_rows,i] = (as.numeric((raw_data[data_rows,i]))/12)*as.numeric(raw_data$Area[data_rows])
  coln = paste("ye",toString(evap_counter),".",strsplit(colnames[i], split = "[.]")[[1]][1], sep = "")
  l2swbm_data$temp = as.numeric(raw_data[data_rows,i])
  colnames(l2swbm_data)[which(names(l2swbm_data) == "temp")] <- coln
  evap_counter=evap_counter+1
}

#####Runoff Calculation######

l2swbm_data$DWP_act = as.numeric(raw_data[data_rows,DWP_PC])+as.numeric(raw_data[data_rows,DWP_WC])+as.numeric(raw_data[data_rows,DWP_LV])+as.numeric(raw_data[data_rows,GLRML])
l2swbm_data$TNC_act = as.numeric(raw_data[data_rows,TNC_rush])+as.numeric(raw_data[data_rows,TNC_LV])-as.numeric(raw_data[data_rows,MTWP])-as.numeric((raw_data[data_rows,GLSC]))
  

#for (i in natural_streamflow){
#  raw_data[data_rows,i] = (as.numeric(raw_data[data_rows,i])*0.000810714)-as.numeric(raw_data[data_rows,diversions]) #Cubic Meters to Acre Feet
#  coln = paste("yr",toString(runoff_counter),".",strsplit(colnames[i], split = "[.]")[[1]][1], sep = "")
#   l2swbm_data$temp = as.numeric(raw_data[data_rows,i])
#   colnames(l2swbm_data)[which(names(l2swbm_data) == "temp")] <- coln
#   runoff_counter=runoff_counter+1
# }

#Use this function to remove zeroes in data with averages of surronding values
#removezeroes(as.Date("1905-01-01"),as.Date("2005-01-01"))

#write.csv(l2swbm_data, "L2SWBM_data.csv", row.names=FALSE, na = "")

