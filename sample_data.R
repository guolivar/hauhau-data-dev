# Testing HauHau data display

library(readr)
library(RJSONIO)
library(curl)
library(base64enc)
library(openair)
library(aws.s3)

# S3 stuff
aws_secrets <- read_delim("./secret_aws.txt",
                          delim = ";",
                          col_names = FALSE)

Sys.setenv("AWS_ACCESS_KEY_ID" = aws_secrets$X1,
           "AWS_SECRET_ACCESS_KEY" = aws_secrets$X2,
           "AWS_DEFAULT_REGION" = "ap-southeast-2")
bucket_out <- "hau-hau-public"
if (!bucket_exists(bucket_out)){
  put_bucket(bucket_out)
}
#==================================
# HauHau API stuff

base_url <- "http://stats.iaqsensors.com/hauhau/api/requestDeviceData?"
apiKey <- as.character(read_csv("./secret_hauhauapikey.txt",col_names = FALSE))
#==================================
# Google Map API stuff
secret_google <- read_delim("./secret_googlemaps.txt", 
                            " ", escape_double = FALSE, trim_ws = TRUE,col_names = FALSE)
#==================================
# Hologram API stuff
secret_hologram <- read_delim("./secret_hologram.txt", 
                              " ", escape_double = FALSE, trim_ws = TRUE)
#==================================
# This call assumes it's UTC in the internal clock
x_now <- Sys.time()
x_now <- as.POSIXct("2020-09-01 00:00:00")
# Now it's moved to NZST
x_now <- x_now + 12 * 3600
# The beginning of this summary
x_start <- x_now - 24 * 3600
# Turn it to the format that the API wants
startDate <- strftime(x_start,format = "%Y-%m-%d%%20%H:%M:%S")
endDate <- strftime(x_now,format = "%Y-%m-%d%%20%H:%M:%S")
plot_startDate <- strftime(x_start,format = "%Y-%m-%d% %H:00:00")

# Get list of devices
hauhau_devices <- read_delim("hauhau-devices.csv",
                             "\t",
                             escape_double = FALSE,
                             trim_ws = TRUE)
names(hauhau_devices) <- c('name','devID','type','location','user','guestflag')
hauhau_devices$lat <- NA
hauhau_devices$lon <- NA
hauhau_devices$loc_accuracy <- NA

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
  id_dev <- which(hauhau_devices$devID==device)
  geoloc <- unlist(strsplit(server_response$geoloc, ","))
  hauhau_devices$lat[id_dev] <- geoloc[1]
  hauhau_devices$lon[id_dev] <- geoloc[2]
  hauhau_devices$loc_accuracy[id_dev] <- geoloc[3]
  
  for (i in (1:npoints)){
    dataHH$date[i] <- as.POSIXct(jreq1[[i]][[1]])
    dataHH$temperature[i] <- as.numeric(jreq1[[i]][[2]])
    dataHH$humidity[i] <- as.numeric(jreq1[[i]][[3]])
    dataHH$co2[i] <- as.numeric(jreq1[[i]][[4]])
    dataHH$pm25[i] <- as.numeric(jreq1[[i]][[5]])
  }
  
  data_plot <- timeAverage(dataHH,
                           start.date = plot_startDate,
                           avg.time = '1 min',
                           fill = TRUE)
  
  png(paste0("./plots/",server_response$name,".png"),
      width = 1024,
      height = 512)
  timePlot(data_plot,
           pollutant = c('co2',
                         'pm25',
                         'temperature',
                         'humidity'),
#           avg.time = "1 min",
           y.relation = 'free',
           main = paste0(server_response$name," - ",server_response$location))
  dev.off()

  # Save the plot to the S3 bucket
  put_object(file = paste0("./plots/",server_response$name,".png"),
             object = paste0(server_response$name,".png"),
             bucket = bucket_out,
             multipart = TRUE,
             acl = "public-read")
  
}

# Fetch location data from Hologram
# Fetch the ODIN details
# THIS VERSION ONLY FETCHES ODIN WITH WANTED TAG
base_url <- "https://dashboard.hologram.io/api/1/devices/locations?"
built_url <- paste0(base_url,
                    "orgid=",secret_hologram$orgid,"&",
                    "apikey=",secret_hologram$apikey,"&",
                    "tagname=HauHau")
req1 <- curl_fetch_memory(built_url)
jreq1 <- fromJSON(rawToChar(req1$content))$data
ndevices <- length(jreq1)
all_devices <- data.frame(id = (1:ndevices),name = NA, lon = NA, lat = NA)

for (i in (1:ndevices)){
  all_devices$id[i] <- jreq1[[i]]$deviceid
  all_devices$name[i] <- jreq1[[i]]$name
  all_devices$lon[i] <- jreq1[[i]]$longitude
  all_devices$lat[i] <- jreq1[[i]]$latitude
}

hauhau_devices$holoLAT <- NA
hauhau_devices$holoLON <- NA

# Add Hologram location to HauHau data frame
for (i in (1:length(hauhau_devices$devID))){
  id_match <- grep(hauhau_devices$name[i],all_devices$name)
  if (length(id_match)==0){
    next
  }
  hauhau_devices$holoLAT[i] <- all_devices$lat[id_match]
  hauhau_devices$holoLON[i] <- all_devices$lon[id_match]
}
