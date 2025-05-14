# load data of 6381 for the period when begging behaviour was filmed. 
# This should be done from the UvA-BiTS database, as in Movebank, data have been down-sampled to one GPS fix per 10 minutes
# See here how to set up connection to the UvA-BiTS database: https://wiki.e-ecology.nl/index.php/How_to_access_the_e-Ecology_database#Setting_up_an_ODBC_connection_.28Windows.29
db.file <-"GPS"
db <- odbcConnect(db.file)
Sys.setenv(TZ="GMT")

# select data during filming day (2018-07-04 and 2018-07-05):
acc.data.6381 <- ImportAllAccDataBird(6381, "2018-07-04 00:00:00", "2018-07-05 23:59:59")
gps.data.6381 <- ImportAllGPSDataBird(6381, "2018-07-04 00:00:00", "2018-07-05 23:59:59")
device.info.6381 <- ImportTrackerInfo(6381)
acc.data.6381$x <- (acc.data.6381$x_acceleration - device.info.6381$x_o) / device.info.6381$x_s 
acc.data.6381$y <- (acc.data.6381$y_acceleration - device.info.6381$y_o) / device.info.6381$y_s
acc.data.6381$z <- (acc.data.6381$z_acceleration - device.info.6381$z_o) / device.info.6381$z_s
# filming was done between 10:02:10 and 10:11:29 and 11:07:18-11:14:18 UTC
# Select relevant hours of data: 
acc.data.6381 <- acc.data.6381[hour(acc.data.6381$date_time)%in%8:12,]
acc.data.6381$Index = acc.data.6381$index

timestamps <- unique(acc.data.6381$date_time)

# plot the potentially relevant accelerometer data (takes quite a while, therefore, I blocked this code):
pdf('output/accelerometer.plots.6381.pdf')
n = 1
for (i in timestamps[1:20]) {
  if(n%in%seq(1,length(timestamps),16)) {
    layout(matrix(1:16,ncol=4))
    par(mar=c(0,0,1,0), oma=c(1,1,1,1))
  } 
    plot(x~Index, acc.data.6381[acc.data.6381$date_time==i,], type='l', col='red', xlim=c(1,200), ylim=c(-1,1), main=as.POSIXct(i, origin="1970-01-01"), xaxt='n', yaxt='n')
    lines(y~Index, acc.data.6381[acc.data.6381$date_time==i,], type='l', col='blue')
    lines(z~Index, acc.data.6381[acc.data.6381$date_time==i,], type='l', col='green')
    n=n+1
}
dev.off()

# annotate the acceleration data manually, based on what was seen on the videos:
acc.data.6381$behaviour = NA
acc.data.6381$behaviour[acc.data.6381$date_time>=ymd_hms("2018-07-04 10:02:10") & acc.data.6381$date_time<=ymd_hms("2018-07-04 10:03:32")] = "beg"
# in the 10:03:45 timestamp, the bird only begs during the first 5 seconds: 
data.end.begging <- acc.data.6381[acc.data.6381$date_time==ymd_hms("2018-07-04 10:03:45"),]                       
windows()
plot(z~Index, data.end.begging, type='l', col='green', ylim=c(-1,1))
lines(x~Index, data.end.begging, col='red')
lines(y~Index, data.end.begging, col='blue')
lines(c(100,100),c(-4,3))
lines(c(120,120),c(-4,3))
acc.data.6381$behaviour[acc.data.6381$date_time==ymd_hms("2018-07-04 10:03:45") & 
                          acc.data.6381$Index%in%1:100] = "beg"
# from Index 120 onward, the bird is preening its back around the tag:
acc.data.6381$behaviour[acc.data.6381$date_time==ymd_hms("2018-07-04 10:03:45") & 
                          acc.data.6381$Index%in%120:200] = "preen_tag"
#
acc.data.6381$behaviour[acc.data.6381$date_time==ymd_hms("2018-07-04 10:03:58") & 
                          acc.data.6381$Index%in%1:(7*20)] = "beg"


# noisy data in 10:03:32 timestamp is where the bird shakes its feathers:
data.shake.feathers <- acc.data.6381[acc.data.6381$date_time==ymd_hms("2018-07-04 10:03:32"),]                                  
plot(z~Index, data.shake.feathers, type='l', col='green')
lines(x~Index, data.shake.feathers, col='red')
lines(y~Index, data.shake.feathers, col='blue')
lines(c(69,69),c(-4,3))
lines(c(82,82),c(-4,3))
acc.data.6381$behaviour[acc.data.6381$date_time==ymd_hms("2018-07-04 10:03:32") & 
                          acc.data.6381$Index%in%69:82] = "shake_feathers"

acc.data.6381$behaviour[acc.data.6381$date_time>=ymd_hms("2018-07-05 11:08:07") & acc.data.6381$date_time<=ymd_hms("2018-07-05 11:09:30")] = "beg"
data.beg.flap <- acc.data.6381[acc.data.6381$date_time==ymd_hms("2018-07-05 11:08:26"),]                                  
plot(z~Index, data.beg.flap, type='l', col='green', ylim=c(-3,3))
lines(x~Index, data.beg.flap, col='red')
lines(y~Index, data.beg.flap, col='blue')
lines(c(122,122),c(-4,3))
lines(c(132,132),c(-4,3))
# this is the period where the bird also flaps with wings while begging
acc.data.6381$behaviour[acc.data.6381$date_time==ymd_hms("2018-07-05 11:08:26") &
                          acc.data.6381$Index%in%122:132] = "beg_flap"

data.beg.flap <- acc.data.6381[acc.data.6381$date_time==ymd_hms("2018-07-05 11:08:52"),]                                  
plot(z~Index, data.beg.flap, type='l', col='green', ylim=c(-3,3))
lines(x~Index, data.beg.flap, col='red')
lines(y~Index, data.beg.flap, col='blue')
lines(c(74,74),c(-4,3))
lines(c(120,120),c(-4,3))
acc.data.6381$behaviour[acc.data.6381$date_time==ymd_hms("2018-07-05 11:08:52") &
                          acc.data.6381$Index%in%74:120] = "beg_flap"

data.beg.flap <- acc.data.6381[acc.data.6381$date_time==ymd_hms("2018-07-05 11:09:05"),]                                  
plot(z~Index, data.beg.flap, type='l', col='green', ylim=c(-3,3))
lines(x~Index, data.beg.flap, col='red')
lines(y~Index, data.beg.flap, col='blue')
lines(c(37,37),c(-4,3))
lines(c(62,62),c(-4,3))
acc.data.6381$behaviour[acc.data.6381$date_time==ymd_hms("2018-07-05 11:09:05") &
                          acc.data.6381$Index%in%37:62] = "beg_flap"

# video-annotated data:
acc.data.6381$annotation.method = 'none'
acc.data.6381$annotation.method[!is.na(acc.data.6381$behaviour)] = 'video'

# Although we have no video-footage of it, the signals are so clear, that we can safely say that between 
# 10:39:05 and 10:42:07 everything is begging. And also between 11:06:29 and 11:08:00
acc.data.6381$behaviour[acc.data.6381$date_time>=ymd_hms("2018-07-05 10:39:05") &
                          acc.data.6381$date_time<=ymd_hms("2018-07-05 10:46:54")] = "beg"

acc.data.6381$behaviour[acc.data.6381$date_time>=ymd_hms("2018-07-05 11:06:29") &
                          acc.data.6381$date_time<=ymd_hms("2018-07-05 11:08:00")] = "beg"


# check that correct parts were annotated by plotting them:

acc.data.6381$ann.behav.col = ifelse(acc.data.6381$behaviour=='beg',"orange",
                                     ifelse(acc.data.6381$behaviour=='beg_flap',"purple",
                                            ifelse(acc.data.6381$behaviour=='preen_tag',"lightblue",
                                                   ifelse(acc.data.6381$behaviour=='shake_feathers',"black","white"))))
table(acc.data.6381$ann.behav.col)
acc.data.6381$ann.value = -1

timestamps.sel <- timestamps[which(timestamps>=ymd_hms("2018-07-05 10:39:05") & timestamps<=ymd_hms("2018-07-05 11:09:30") |
                                     timestamps>=ymd_hms("2018-07-04 10:02:10") & timestamps<=ymd_hms("2018-07-04 10:03:50"))]

pdf('output/accelerometer.plots.6381.with.annotation.pdf')
n = 1
for (i in timestamps.sel) {
  if(n%in%seq(1,length(timestamps.sel),16)) {
    layout(matrix(1:16,ncol=4))
    par(mar=c(0,0,1,0), oma=c(1,1,1,1))
  } 
  plot(x~Index, acc.data.6381[acc.data.6381$date_time==i,], type='l', col='red', xlim=c(1,200), ylim=c(-1,1), main=as.POSIXct(i, origin="1970-01-01"), xaxt='n', yaxt='n')
  lines(y~Index, acc.data.6381[acc.data.6381$date_time==i,], col='blue')
  lines(z~Index, acc.data.6381[acc.data.6381$date_time==i,], col='green')
  points(ann.value~Index, acc.data.6381[acc.data.6381$date_time==i,], col=acc.data.6381$ann.behav.col[acc.data.6381$date_time==i])
  n=n+1
}
dev.off()

# 
annotated.begging.data = acc.data.6381[!is.na(acc.data.6381$behaviour),c('device_info_serial','date_time','Index','x','y','z','behaviour','annotation.method')]
annotated.begging.data$annotation.method[annotated.begging.data$annotation.method=='none']='graphical'
begging.data.only = annotated.begging.data[annotated.begging.data$behaviour%in%c('beg','beg.no.video','beg_flap'),]
table(begging.data.only$annotation.method)/20 # 110 s of video-footage and 337 s of graphical annotated begging behaviour.
table(annotated.begging.data$behaviour, annotated.begging.data$annotation.method)

# link with gps data to get the speed:
annotated.begging.data = merge(annotated.begging.data, gps.data.6381[,c('date_time','speed_2d')], all.x=T)
annotated.begging.data = annotated.begging.data[,c('device_info_serial','date_time','Index','x','y','z','speed_2d','behaviour','annotation.method')]
names(annotated.begging.data)[1:2]=c('BirdID','date.time')
table(annotated.begging.data$Index) # here Index starts at 1
table(annotated.begging.data$behaviour, annotated.begging.data$annotation.method)

# export annotated begging behaviour (with some preening and shaking feathers included) file to raw data:
write.csv(annotated.begging.data, "data/raw/annotated.begging.data.csv", row.names = F)
