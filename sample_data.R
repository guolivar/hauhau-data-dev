# Testing HauHau data display

library(readr)
library(RJSONIO)
library(curl)
library(base64enc)
library(openair)

base_url <- "http://stats.iaqsensors.com/hauhau/api/requestDeviceData?"
apiKey <- as.character(read_csv("./secret_hauhauapikey.txt",col_names = FALSE))
startDate <- "2020-09-10%2006:00:00"
endDate <- "2020-09-11%2005:59:00"

# Get list of devices
hauhau_devices <- read_delim("hauhau-devices.csv",
                             "\t",
                             escape_double = FALSE,
                             trim_ws = TRUE)
names(hauhau_devices) <- c('name','devID','type','location','user','guestflag')

for (device in hauhau_devices$devID){
  print(device)
  built_url <- paste0(base_url,
                      "apiKey=",apiKey,"&",
                      "devID=",device,"&",
                      "startDate=",startDate,"&",
                      "endDate=",endDate)
  req1 <- curl_fetch_memory(built_url)
  # First check that the request was successful, if not, move on to the next device
  reqOK <- req1$status_code==200
  if (!reqOK){
    print("Request error")
    next
  }
  # Successful response will have content
  server_response <- fromJSON(rawToChar(req1$content))
  # server_response:  sensorlog [list with the data]
  #                   status [integer 1==OK; 0==NO DATA]
  #                   name [device name given in dashboard]
  #                   location [location name as given in dashboard]
  #                   geoloc [CHAR <decimal lat>,<decimal lon>,<accuracy m>]
  # 
  
  # Now check that there is data in the content, if there isn't, move on to the next device
  data_available <- server_response$status==1
  if (!data_available){
    print("No data")
    next
  }
  # There is data in sensorlog so parse it
  jreq1 <- server_response$sensorlog
  npoints <- length(jreq1)
  dataHH <- data.frame(id = (1:npoints))
  dataHH$date <- as.POSIXct(jreq1[[1]][[1]])
  dataHH$temperature <- NA
  dataHH$humidity <- NA
  dataHH$co2 <- NA
  dataHH$pm25 <- NA
  
  for (i in (1:npoints)){
    dataHH$date[i] <- as.POSIXct(jreq1[[i]][[1]])
    dataHH$temperature[i] <- as.numeric(jreq1[[i]][[2]])
    dataHH$humidity[i] <- as.numeric(jreq1[[i]][[3]])
    dataHH$co2[i] <- as.numeric(jreq1[[i]][[4]])
    dataHH$pm25[i] <- as.numeric(jreq1[[i]][[5]])
  }
  png(paste0("./plots/",server_response$name,".png"))
  timePlot(dataHH,
           pollutant = c('co2',
                         'pm25',
                         'temperature',
                         'humidity'),
#           avg.time = "1 min",
           y.relation = 'free',
           main = paste0(server_response$name," - ",server_response$location))
  dev.off()
}
