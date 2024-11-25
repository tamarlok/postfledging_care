## load GPS and ACC data of all juvenile birds
## Establish connection with UvA-BITS database:
## provide the name of the database file
db.file <-"GPS"
#Establish a connection to the database (if architecture error, change R to 64 bits, go to Tools - Global Options - General - R version)
# Furthermore, I should be logged in as adminlocal...
db <- odbcConnect(db.file)

# make sure that time is set to UTC/GMT:
Sys.setenv(TZ="GMT")

# load bird data for all juveniles
juv.data <- read.csv("data/raw/bird.data.juvs.csv")
juv.data$start_deployment <- dmy(juv.data$start_deployment, tz='UTC')
juv.data$year.start <- year(juv.data$start_deployment)
juv.data$end_deployment <- dmy(juv.data$end_deployment, tz='UTC')
juv.data$year.end <- year(juv.data$end_deployment)

# create new gps.data.list and acc.data.list and device.infos and fill as much as possible with the above data:
gps.data.list <- vector(mode="list", length=dim(juv.data)[1])
names(gps.data.list) <- juv.data$birdID
acc.data.list <- vector(mode="list", length=dim(juv.data)[1])
names(acc.data.list) <- juv.data$birdID
device.infos <- ImportTrackerInfo(6292) # to get the columns required for device info

# download data per deployment (bird x tracker)
for (i in 1:dim(juv.data)[1]) { # 
  print(juv.data[i,"logger"])
  gps.data <- ImportAllGPSDataBird(juv.data[i,"logger"], paste(juv.data[i,"year.start"], "-01-01 00:00:00", sep=""), paste(juv.data[i,"end_deployment"], " 23:59:59", sep=""))
  acc.data <- ImportAllAccDataBird(juv.data[i,"logger"], paste(juv.data[i,"year.start"], "-01-01 00:00:00", sep=""), paste(juv.data[i,"end_deployment"], " 23:59:59", sep=""))
  device.info <- ImportTrackerInfo(juv.data[i,"logger"])
  gps.data.list[[i]] <- gps.data
  acc.data.list[[i]] <- acc.data
  device.infos[i,] <- device.info
}

# download SMS data separately, for both the juveniles and adults:
sms.data.juv.list <- list()
for (i in 1:dim(juv.data)[1]) { # 
  print(juv.data[i,"logger"])
  sms.data <- ImportSMSDataBird(juv.data[i,"logger"], paste(juv.data[i,"start_deployment"], " 00:00:00", sep=""), paste(juv.data[i,"end_deployment"], " 23:59:59", sep=""))
  sms.data$birdID = juv.data[i,"birdID"]
  sms.data.juv.list[[i]] <- sms.data
}

# load bird data for adults
ad.data <- read.csv("data/raw/bird.data.ads.csv")
ad.data$start_deployment <- dmy(ad.data$start_deployment, tz='UTC')
ad.data$year.start <- year(ad.data$start_deployment)
ad.data$end_deployment <- dmy(ad.data$end_deployment, tz='UTC')
ad.data$year.end <- year(ad.data$end_deployment)
ad.data$'animal-id' = ad.data$birdID
ad.data$'deploy-on-timestamp' = ad.data$start_deployment
ad.data$'deploy-off-timestamp' = ad.data$end_deployment
ad.data$'deployment-id' <- paste(ad.data$'animal-id', ad.data$'logger', sep="-")
sms.data.ad.list <- list()
for (i in 1:dim(ad.data)[1]) { # 
  print(ad.data[i,"logger"])
  sms.data <- ImportSMSDataBird(ad.data[i,"logger"], paste(ad.data[i,"start_deployment"], " 00:00:00", sep=""), paste(ad.data[i,"end_deployment"], " 23:59:59", sep=""))
  if (dim(sms.data)[1]>0) sms.data$birdID = ad.data[i,"birdID"]
  sms.data.ad.list[[i]] <- sms.data
}

# export data in format suitable for Movebank:

# only select columns from juv.data that can be stored in Movebank:
juv.data.mb <- juv.data[,c('logger','birdID','ringnumber','colourcode','sex','start_deployment','end_deployment','bodymass','tracker_mass')]

# rename columns in juv.data:
names(juv.data.mb) <- c('tag-id', 'animal-id', 'animal-ring-id','animal-nickname','animal-sex','deploy-on-timestamp','deploy-off-timestamp','animal-mass','tag-mass')

# add additional columns:
juv.data.mb$'deployment-id' <- paste(juv.data.mb$'animal-id', juv.data.mb$'tag-id', sep="-")
juv.data.mb$'animal-taxon' <- 'Platalea leucorodia'
juv.data.mb$'animal-taxon-detail' <- 'Platalea leucorodia leucorodia (Linneaus, 1758)'
juv.data.mb$'animal-life-stage' <- 'juvenile'
juv.data.mb$'attachment-type' <- 'backpack-harness'
juv.data.mb$'capture-latitude' <- 53.48
juv.data.mb$'capture-longitude' <- 6.25
juv.data.mb$'deploy-on-person' <- 'Petra de Goeij'
juv.data.mb$'manipulation-type' <- 'none'
juv.data.mb$'study-site' <- 'Schiermonnikoog, The Netherlands'
juv.data.mb$'tag-readout-method' <- 'other-wireless'
juv.data.mb$'tag-manufacturer-name' <- 'UvA-BiTS'

write.csv(juv.data.mb, "data/raw/movebank-files/movebank_ref_data.csv", na="")

# go to Movebank -> Upload -> Import Data -> Reference data about animals, tracking tags, or deployments -> Use custom reference data format -> Combination (animal, tag, and deployment data in a single file) -> Match each column to a Movebank column. Be careful to correctly format the Timestamp string in Movebank for deploy-on-timestamp and deploy-off-timestamp (yyyy-MM-dd; check csv-file for this when opened with Notepad (as Excel changes the format)), and select correct timezone (Fixed offset from UTC)!

# upload the first csv file via: Upload -> Import data -> GPS data -> custom GPS data
# "Height above mean sea level", "location-error-numerical", "manually marked outlier" and "temperature-external" can be found under "Other event attributes"; location-lat and location-long can be assigned at once under "Location". Then this format can be saved under a given name.
# subsequent imports are then easily performed using the saved format found under Upload -> Import data -> Jump to pre-defined upload channel directly, and then select the custom format at the very bottom of the list.  

# create a csv file with GPS and one with ACC data for each deployment (bird x tag), with a GPS interval of at least 10 minutes and maximally 32 samples of accelerometer data per GPS-fix. ACC data should be imported via "Other accessory data". The columns Index and Sensor Type cannot be imported.

for (i in 1:dim(juv.data.mb)[1]) {  
  
  gps.data <- gps.data.list[[i]]
  acc.data <- acc.data.list[[i]]

  ### PREPARE GPS DATA FOR EXPORT ###
  
  gps.data$date <- floor_date(gps.data$date_time, 'day')
  # select period between start and end of deployment (including start and end date of deployment)
  gps.data <- gps.data[gps.data$date>=juv.data.mb$`deploy-on-timestamp`[i] & gps.data$date<=juv.data.mb$`deploy-off-timestamp`[i],] 
  
  # where applicable (for deployments < 2016), downsample to 10 minute interval.  
  if (year(juv.data.mb$`deploy-on-timestamp`[i])<2016) {
    gps.data$date_time_10min <- round_date(gps.data$date_time, "10min")
    # this rounding sometimes gives problems, for example with 15:05:01 and 15:14:59, both rounded to 15:10:00
    # calculate time between rounded 10 minute timestamps and actual time:
    gps.data$time_diff <- abs(gps.data$date_time - gps.data$date_time_10min)  
    # calculate the minimum time_diff_rnd and time_diff_flr for each rounded 10 minute timestamp:
    time_diff_min <- aggregate(time_diff~date_time_10min, gps.data, min)
    # merge original data with time_diff_min to select only those timestamps that are closest to the 10 minute   timestamps:
    gps.data.sel <- merge(gps.data, time_diff_min)
  } else gps.data.sel <- gps.data # for deployments from 2016 onward, as 10 minute intervals have always been used since then.
  
  # select and rename columns to fit with Movebank columns (after Peter Desmet code https://github.com/inbo/bird-tracking/blob/master/sql/uvabits_to_movebank_gps.sql):
  gps.data.sel <- gps.data.sel[,c('device_info_serial','pressure','positiondop','satellites_used',
                                  'gps_fixtime','speed_2d','direction','altitude','latitude','longitude',
                                  'h_accuracy','userflag','temperature','date_time','v_accuracy')]
  # put variables on the right scale or in the right format:
  gps.data.sel$direction <- ifelse(gps.data.sel$direction<0, 360 + gps.data.sel$direction, gps.data.sel$direction<0)
  gps.data.sel$userflag <- ifelse(gps.data.sel$userflag!=0, TRUE, FALSE)
  #gps.data.sel$date_time <- as.character(gps.data.sel$date_time)
  
  names(gps.data.sel) <- c('tag-id','barometric-pressure','gps-dop','gps-satellite-count','gps-time-to-fix','ground-speed','heading','height-above-mean-sea-level','location-lat','location-long','location-error-numerical','manually-marked-outlier','temperature-external','timestamp','vertical-error-numerical')
  
  # add the animal-id:
  gps.data.sel$'animal-id'<- juv.data.mb$'animal-id'[i]
  
  data_file = file.path("data/raw/movebank-files", paste0("movebank_gps", "_", juv.data.mb$'deployment-id'[i], ".csv"))
  write.csv(gps.data.sel, data_file, na="")

  ### PREPARE ACC DATA FOR EXPORT ###
  
  if (dim(acc.data)[1]>0) { # only if there is acc data available
    
    # only select acc date_times that correspond to gps date_times:
    acc.data.sel <- acc.data[acc.data$date_time%in%gps.data.sel$timestamp,]

    # only select first 32 indices per acc-sample: 
    acc.data.sel <- acc.data.sel[acc.data.sel$index<33,]
    
    # rename columns to fit with Movebank columns (after Peter Desmet code https://github.com/inbo/bird-tracking/blob/master/sql/uvabits_to_movebank_acc.sql):
    names(acc.data.sel) <- c('tag-id','start-timestamp','index','acceleration-raw-x','acceleration-raw-y','acceleration-raw-z')
  
    # add additional columns:
    acc.data.sel$'animal-id'<- juv.data.mb$'animal-id'[i]
    acc.data.sel$'sensor-type' <- 'Acceleration'
    acc.data.sel$timestamp <- acc.data.sel$'start-timestamp' + seconds((acc.data.sel$index-1)/20)
    acc.data.sel$'tilt-x' <- (acc.data.sel$'acceleration-raw-x' - device.infos$x_o[i]) / device.infos$x_s[i] 
    acc.data.sel$'tilt-y' <- (acc.data.sel$'acceleration-raw-y' - device.infos$y_o[i]) / device.infos$y_s[i] 
    acc.data.sel$'tilt-z' <- (acc.data.sel$'acceleration-raw-z' - device.infos$z_o[i]) / device.infos$z_s[i] 
  
    options(digits.secs=3) # to show milliseconds
  
    # if datafile is larger than 5 million rows, then split up into multiple csv-files:
    size.max <- 5000000
    if (dim(acc.data.sel)[1]<=size.max) {
      data_file = file.path("data/raw/movebank-files", paste0("movebank_acc", "_", juv.data.mb$'deployment-id'[i], ".csv"))
      write.csv(acc.data.sel, data_file, na="")
    } else {
    # determine number of files to create:
      n_files <- ceiling(dim(acc.data.sel)[1]/size.max)
      for (j in 1:n_files) {
        data_file_n = file.path("data/raw/movebank-files", paste0("movebank_acc", j, "_", juv.data.mb$'deployment-id'[i], ".csv"))
        if (j<n_files) acc.data.sel_n <- acc.data.sel[((j-1)*size.max+1):(j*size.max),] else acc.data.sel_n <- acc.data.sel[((j-1)*size.max+1):dim(acc.data.sel)[1],] 
      write.csv(acc.data.sel_n, data_file_n, na="")
      }
    }
  }
}

# STILL TO UPLOAD SMS DATA, BUT ONLY FOR DATETIMES THAT WERE NOT ALREADY IMPORTED VIA THE NORMAL ROUTE (RADIO ANTENNA NETWORK)
# double data should be automatically filtered out via the tag x timestamp combination.
# as the downloading via the radio antenna network downloads much more information than the info via SMS, the "radio antenna downloaded" data should be uploaded to Movebank first.
for (i in 1:dim(juv.data.mb)[1]) {  
  sms.data <- sms.data.juv.list[[i]]
  sms.data$date <- floor_date(sms.data$date_time, 'day')
  # select period between start and end of deployment (including start and end date of deployment)
  sms.data <- sms.data[sms.data$date>=juv.data.mb$`deploy-on-timestamp`[i] & sms.data$date<=juv.data.mb$`deploy-off-timestamp`[i],] 
  # select and rename columns to fit with Movebank columns (after Peter Desmet code https://github.com/inbo/bird-tracking/blob/master/sql/uvabits_to_movebank_gps.sql):
  sms.data <- sms.data[,c('device_info_serial','latitude','longitude','userflag','date_time')]
  sms.data$userflag <- ifelse(sms.data$userflag!=0, TRUE, FALSE)
  names(sms.data) <- c('tag-id','location-lat','location-long','manually-marked-outlier','timestamp')
  sms.data$'animal-id'<- juv.data.mb$'animal-id'[i]
  data_file = file.path("data/raw/movebank-files", paste0("movebank_sms", "_", juv.data.mb$'deployment-id'[i], ".csv"))
  write.csv(sms.data, data_file, na="", row.names=F)
}

## BELOW DATA SHOULD BE UPLOADED TO THE METAWAD MOVEBANK STUDY!
for (i in 1:dim(ad.data)[1]) {  
  print(ad.data$birdID[i])
  sms.data <- sms.data.ad.list[[i]]
  sms.data$date <- floor_date(sms.data$date_time, 'day')
  # select period between start and end of deployment (including start and end date of deployment)
  sms.data <- sms.data[sms.data$date>=ad.data$`deploy-on-timestamp`[i] & sms.data$date<=ad.data$`deploy-off-timestamp`[i],] 
  # select and rename columns to fit with Movebank columns (after Peter Desmet code https://github.com/inbo/bird-tracking/blob/master/sql/uvabits_to_movebank_gps.sql):
  sms.data <- sms.data[,c('device_info_serial','latitude','longitude','userflag','date_time')]
  sms.data$userflag <- ifelse(sms.data$userflag!=0, TRUE, FALSE)
  names(sms.data) <- c('tag-id','location-lat','location-long','manually-marked-outlier','timestamp')
  # only create csv-file for those individuals for which SMS data is available:
  if (dim(sms.data)[1]>0) {
    sms.data$'animal-id'<- ad.data$'animal-id'[i]
    data_file = file.path("data/raw/movebank-files/adult-sms", paste0("movebank_sms", "_", ad.data$'deployment-id'[i], ".csv"))
    write.csv(sms.data, data_file, na="", row.names=F)
  } 
}
