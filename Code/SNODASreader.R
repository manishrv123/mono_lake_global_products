library("rwrfhydro")
library(SNODASR)
library(lubridate)
dates=ymd(20120509)

spatial.data <- c(-120,-118,37,39)
spatial <- matrix(spatial.data,nrow=2,ncol=2,byrow=TRUE)
# SNODASR::extract.SNODAS.subset(ymd(20040905),
#                                extent=spatial,
#                                masked = TRUE,
#                                read_path = "",
#                                compressed = FALSE,
#                                nodata_handling = "NA",
#                                write_file = TRUE,
#                                write_path = "./SNODAS/",
#                                filename_prefix = "",
#                                write_extension = ".img")
threeDaysBack <- Sys.Date() + lubridate::period(-10,'day')
snodasGot <- GetSnodasDepthSweDate(threeDaysBack, outputDir=snodasPath)
if(snodasGot) {
  snodasList <- ReadSnodasDepthSweDate(threeDaysBack, outputDir=snodasPath)
  snodasNcFile <- PutSnodasNcdf(snodasList, outputDir=snodasPath)
  snodasNcFile
}


UpdateSnodas <- function(POSIXct, outPath='.') {
  ## check if we already processed this date/POSIXct, if we didnt process, we'll
  ## download again and process it. (note we could have downloaded but not processed 
  ## so it might not be efficient). 
  file <- paste0(outPath, 'SNODAS_',format(POSIXct,'%Y%m%d'),'.nc')
  processed <- file.exists(file)
  if(processed) 
    return(data.frame(date=POSIXct, snodasGot=FALSE, ncdfFile=file))
  
  snodasGot <- GetSnodasDepthSweDate(POSIXct, outputDir=outPath)
  if(snodasGot) {
    snodasList <- ReadSnodasDepthSweDate(POSIXct, outputDir=outPath)
    ncdfFile <- PutSnodasNcdf(snodasList, outputDir=outPath)
  } else ncdfFile <- ''
  data.frame(date=POSIXct, snodasGot=snodasGot, ncdfFile=ncdfFile)
}
threeDaysBack <- Sys.Date() + lubridate::period(-19,'year')
threeDaysBack <- threeDaysBack + lubridate::period(-39,'days')
datesWanted <- seq(threeDaysBack,Sys.Date(), by = 'days')

#update <- plyr::ldply(NamedList(datesWanted), UpdateSnodas, outPath=snodasPath)
tempdate = GetSnodasPointTs(as.POSIXct('2004-12-01'),as.POSIXct("2005-04-05"),
                 snodasDir='C:/Users/manis/OneDrive - Umich/Documents/PersonalMonoLakePlayground/DataOrganization/SNODAS',lat=39.9,
                 lon=-105.1)
