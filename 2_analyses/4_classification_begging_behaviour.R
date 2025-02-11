# load data of 6381 during filming of begging behaviour:
# This should be done from the UvA-BiTS database, as in Movebank, only data every 10 minutes is stored.
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
table(acc.data.6381$date_time)

#n = 1
#for (i in timestamps) {
#  if(n%in%seq(1,length(timestamps),16)) {
#    windows(12,8)
#    layout(matrix(1:16,ncol=4))
#    par(mar=c(0,0,1,0), oma=c(1,1,1,1))
#  } 
#    plot(x~Index, acc.data.6381.sel[acc.data.6381.sel$date_time==i,], type='l', col='red', xlim=c(1,200), ylim=c(-1,1), #main=as.POSIXct(i, origin="1970-01-01"), xaxt='n', yaxt='n')
#    lines(y~Index, acc.data.6381.sel[acc.data.6381.sel$date_time==i,], type='l', col='blue')
#    lines(z~Index, acc.data.6381.sel[acc.data.6381.sel$date_time==i,], type='l', col='green')
#    n=n+1
#}

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
# 10:39:05 and 10:42:07 everything is begging. And also between 11:06:29 and 11:08:00 (see ppt)
acc.data.6381$behaviour[acc.data.6381$date_time>=ymd_hms("2018-07-05 10:39:05") &
                          acc.data.6381$date_time<=ymd_hms("2018-07-05 10:46:54")] = "beg"

acc.data.6381$behaviour[acc.data.6381$date_time>=ymd_hms("2018-07-05 11:06:29") &
                          acc.data.6381$date_time<=ymd_hms("2018-07-05 11:08:00")] = "beg"


table(acc.data.6381$behaviour)

# check that correct parts were annotated by plotting them:

acc.data.6381$ann.behav.col = ifelse(acc.data.6381$behaviour=='beg',"orange",
                                 ifelse(acc.data.6381$behaviour=='beg_flap',"purple",
                                        ifelse(acc.data.6381$behaviour=='preen_tag',"lightblue",
                                          ifelse(acc.data.6381$behaviour=='shake_feathers',"black","white"))))
table(acc.data.6381$ann.behav.col)
acc.data.6381$ann.value = -1

timestamps.sel <- timestamps[which(timestamps>=ymd_hms("2018-07-05 10:39:05") & timestamps<=ymd_hms("2018-07-05 11:09:30") |
                                   timestamps>=ymd_hms("2018-07-04 10:02:10") & timestamps<=ymd_hms("2018-07-04 10:03:50"))]
#n = 1
#for (i in timestamps.sel) {
#  if(n%in%seq(1,length(timestamps.sel),16)) {
#    windows(12,8)
#    layout(matrix(1:16,ncol=4))
#    par(mar=c(0,0,1,0), oma=c(1,1,1,1))
#  } 
#  plot(x~Index, acc.data.6381[acc.data.6381$date_time==i,], type='l', col='red', xlim=c(1,200), ylim=c(-1,1), main=as.POSIXct(i, origin="1970-01-01"), xaxt='n', yaxt='n')
#  lines(y~Index, acc.data.6381[acc.data.6381$date_time==i,], col='blue')
#  lines(z~Index, acc.data.6381[acc.data.6381$date_time==i,], col='green')
#  points(ann.value~Index, acc.data.6381[acc.data.6381$date_time==i,], col=acc.data.6381$ann.behav.col[acc.data.6381$date_time==i])
#  n=n+1
#}

# 
annotated.begging.data = acc.data.6381[!is.na(acc.data.6381$behaviour),c('device_info_serial','date_time','Index','x','y','z','behaviour','annotation.method')]
annotated.begging.data$annotation.method[annotated.begging.data$annotation.method=='none']='graphical'
begging.data.only = annotated.begging.data[annotated.begging.data$behaviour%in%c('beg','beg.no.video','beg_flap'),]
table(begging.data.only$annotation.method)/20 # 110 s of video-footage and 337 s of graphical annotated begging behaviour.
table(annotated.begging.data$behaviour, annotated.begging.data$annotation.method)

# export annotated begging behaviour (with some preening and shaking feathers included) file to raw data:
write.csv(annotated.begging.data, "data/raw/annotated.begging.data.csv")

# link with gps data to get the speed:
annotated.begging.data = merge(annotated.begging.data, gps.data.6381[,c('date_time','speed_2d')], all.x=T)
annotated.begging.data = annotated.begging.data[,c('device_info_serial','date_time','Index','x','y','z','speed_2d','behaviour','annotation.method')]
names(annotated.begging.data)[1:2]=c('BirdID','date.time')
table(annotated.begging.data$Index) # here Index starts at 1
table(annotated.begging.data$behaviour, annotated.begging.data$annotation.method)

# combine annotated begging behaviour with previously annotated behaviours from Lok et al. 2023:
acc.annotated = read.csv("data/raw/acc.annotated.final.csv", header=T)
table(acc.annotated$Index) # here Index starts at 0
# make the Index in this datafile start at 1:
acc.annotated$Index = acc.annotated$Index+1
acc.annotated$annotation.method = 'video'

# combine the two datasets:
acc.annotated.comb = rbind(annotated.begging.data, acc.annotated[,c(2:9,11)])
acc.annotated.comb$behaviour[acc.annotated.comb$behaviour=='shake_feathers']='stand-shake-feathers'
acc.annotated.comb$behaviour[acc.annotated.comb$behaviour=='preen_tag']='stand-preen'
acc.annotated.comb$behaviour[acc.annotated.comb$behaviour=='beg_flap']='beg'
table(acc.annotated.comb$behaviour)

# pool certain behaviours:
acc.annotated.comb$behaviour.pooled = acc.annotated.comb$behaviour
acc.annotated.comb$behaviour.pooled[acc.annotated.comb$behaviour=='sit-alert']='sit'
acc.annotated.comb$behaviour.pooled[acc.annotated.comb$behaviour%in%c('stand-alert','stand-other','stand-rest','stand-shake-feathers','stand-preen','drink')] = 'stand'
table(acc.annotated.comb$behaviour.pooled)

behaviour.pooled = names(table(acc.annotated.comb$behaviour.pooled))
behaviour.pooled2 = c('forage','walk','fly','rest','beg') # only used to determine the length of the arrays to calculate pooled sensitivity and precision

# assign id's
names(acc.annotated.comb)[1:2] = c('birdID','date_time')
acc.annotated.comb$obs.id <- paste(acc.annotated.comb$birdID, as.numeric(acc.annotated.comb$date_time), sep = ".")
acc.annotated.comb$segment.id <- paste(acc.annotated.comb$obs.id, ".a", sep = "")
acc.annotated.comb$ind.id <- paste(acc.annotated.comb$obs.id, ".", acc.annotated.comb$Index, sep = "")

# run the fixed segmentation models with different segment lengths 10 times
nsim <- 10
seg.lengths <- c(0.4,0.8,1.0,1.6) # segment length in seconds
sensitivity.fixed.seglength.nsim <- array(NA, c(nsim,length(behaviour.pooled),length(seg.lengths)), dimnames = list(1:nsim, behaviour.pooled, seg.lengths))
precision.fixed.seglength.nsim <- array(NA, c(nsim,length(behaviour.pooled),length(seg.lengths)), dimnames = list(1:nsim, behaviour.pooled, seg.lengths))
sensitivity.fixed.seglength.behav.pooled.nsim <- array(NA, c(nsim,length(behaviour.pooled2),length(seg.lengths)), dimnames = list(1:nsim, behaviour.pooled2, seg.lengths))
precision.fixed.seglength.behav.pooled.nsim <- array(NA, c(nsim,length(behaviour.pooled2),length(seg.lengths)), dimnames = list(1:nsim, behaviour.pooled2, seg.lengths))

df1.seg.length <- list(length(seg.lengths))
df2.seg.length <- list(length(seg.lengths))

for (j in 1:length(seg.lengths)) { 
  dfs <- create.fixed.segments(segment.length=seg.lengths[j], data=acc.annotated.comb, annotated.data=T)
  df1.seg.length[[j]] <- dfs[[1]]
  df2.seg.length[[j]] <- dfs[[2]]
}

for (j in 1:length(seg.lengths)) { 
  print(seg.lengths[j])
  for (i in 1:nsim) {
    RF.model.output <- RF.model(df1.seg.length[[j]], df2.seg.length[[j]], stand=4)
    performance.stats <- calculate.performance(RF.model.output[[2]])
    print(performance.stats[[5]])
    sensitivity.fixed.seglength.nsim[i,,j] <- performance.stats[[1]]
    precision.fixed.seglength.nsim[i,,j] <- performance.stats[[3]]
    # calculate performance when behaviours are pooled even further (into forage, walk, fly, rest or beg)
    performance.stats.pooled <- calculate.performance.pooled(RF.model.output[[2]])
    print(performance.stats.pooled[[5]])
    sensitivity.fixed.seglength.behav.pooled.nsim[i,,j] <- performance.stats.pooled[[1]]
    precision.fixed.seglength.behav.pooled.nsim[i,,j] <- performance.stats.pooled[[3]]
    print(i)
    print(precision.fixed.seglength.nsim[i,,j])
  }
}

### calculate mean, lcl and ucl over the N simulations
mean.CRI.sensitivity.fixed.seglength <- calculate.mean.CRI(sensitivity.fixed.seglength.nsim)
mean.CRI.precision.fixed.seglength <- calculate.mean.CRI(precision.fixed.seglength.nsim)
F.measure.fixed.seglength.nsim <- calculate.F.measure(sensitivity.fixed.seglength.nsim, precision.fixed.seglength.nsim)
mean.CRI.Fmeasure.fixed.seglength <- calculate.mean.CRI(F.measure.fixed.seglength.nsim)
mean.CRI.Fmeasure.fixed.seglength[,,'mean']

# also for the pooled behaviours
mean.CRI.sensitivity.pooled.seglength <- calculate.mean.CRI(sensitivity.fixed.seglength.behav.pooled.nsim)
mean.CRI.precision.pooled.seglength <- calculate.mean.CRI(precision.fixed.seglength.behav.pooled.nsim)
F.measure.pooled.seglength.nsim <- calculate.F.measure(sensitivity.fixed.seglength.behav.pooled.nsim, precision.fixed.seglength.behav.pooled.nsim)
mean.CRI.Fmeasure.pooled.seglength <- calculate.mean.CRI(F.measure.pooled.seglength.nsim)
round(mean.CRI.Fmeasure.pooled.seglength[,,'mean'],2)
write.csv(round(mean.CRI.Fmeasure.pooled.seglength[,,'mean'],2), "output/Fmeasures.pooled.behaviours.csv")

# train best-supported RF model (with segment length 1.6 s) on all annotated data:
RF.model.with.begging <- RF.model.start(df1.seg.length[[4]], stand=4) # the 4th df1 is the dataset with the 4th segment length (=1.6 s)
RF.model.with.begging$importance

keep(mean.CRI.sensitivity.fixed.seglength, mean.CRI.precision.fixed.seglength, mean.CRI.Fmeasure.fixed.seglength, 
     mean.CRI.sensitivity.pooled.seglength, mean.CRI.precision.pooled.seglength, mean.CRI.Fmeasure.pooled.seglength,
     acc.annotated.comb, dfs, RF.model.with.begging, sure=T)

table(is.na(dfs[[1]]$skew.z)) 
#save.image("data/processed/model.classification.begging.1202.RData")
load("data/processed/model.classification.begging.1202.RData")





### OLD:
# check some potential begging behaviour of 6295 at night:
db.file <-"GPS"
db <- odbcConnect(db.file)
Sys.setenv(TZ="GMT")
acc.data.6295 <- ImportAllAccDataBird(6295, "2016-06-18 00:00:00", "2016-06-22 23:59:59")
device.info.6295 <- ImportTrackerInfo(6295)
acc.data.6295$x <- (acc.data.6295$x_acceleration - device.info.6295$x_o) / device.info.6295$x_s 
acc.data.6295$y <- (acc.data.6295$y_acceleration - device.info.6295$y_o) / device.info.6295$y_s
acc.data.6295$z <- (acc.data.6295$z_acceleration - device.info.6295$z_o) / device.info.6295$z_s
# filming was done between 10:02:10 and 10:11:29 and 11:07:18-11:14:18 UTC
# Select relevant hours of data: 
acc.data.6295.sel <- acc.data.6295[hour(acc.data.6295$date_time)%in%22:23,]

timestamps <- unique(acc.data.6295.sel$date_time)
table(acc.data.6295.sel$date_time)

n = 1
for (i in timestamps) {
  if(n%in%seq(1,length(timestamps),16)) {
    windows(12,8)
    layout(matrix(1:16,ncol=4))
    par(mar=c(0,0,1,0), oma=c(1,1,1,1))
  } 
  plot(x~Index, acc.data.6295.sel[acc.data.6295.sel$date_time==i,], type='l', col='red', xlim=c(1,200), ylim=c(-1,1), main=as.POSIXct(i, origin="1970-01-01"), xaxt='n', yaxt='n')
  lines(y~Index, acc.data.6295.sel[acc.data.6295.sel$date_time==i,], type='l', col='blue')
  lines(z~Index, acc.data.6295.sel[acc.data.6295.sel$date_time==i,], type='l', col='green')
  n=n+1
}
## End of checking...


# behaviours per segment:
behav.segment = table(acc.annotated.comb$segment.id, acc.annotated.comb$behaviour.pooled)
behav.segment[behav.segment[,'beg']>0,]
beg.example.segment = acc.annotated.comb[acc.annotated.comb$segment.id=='6381.1530698560.a',]

# plot a segment of begging as an example:
plot(x~Index, beg.example.segment, type='l', col='red')
# check how Fast Fourier Transform works on these data:
fft_result_beg = fft(beg.example.segment$x)
windows()
par(mfrow = c(2, 1))
plot(beg.example.segment$x, type = "l", main = "Original Signal")
plot(Mod(fft_result_beg), type = "l", main = "FFT Magnitudes")
magnitudes<- abs(fft_result_beg)  ## or mod()

# Find the frequency with the largest amplitude
max_Index_sine <- which.max(magnitudes)

paste("magnitude value: ", magnitudes[max_Index_sine] )
paste("Index position: ",max_Index_sine)

# how does this look for a foraging segment without a cyclic pattern in it...
behav.segment[behav.segment[,'for-search']>0,]
search.example.segment = acc.annotated.comb[acc.annotated.comb$segment.id=='760.1402229161.a',]
fft_result_search = fft(search.example.segment$x)
par(mfrow = c(2, 1))
plot(x~Index, search.example.segment, type='l', col='red')
lines(x~Index, beg.example.segment, col='purple')
plot(Mod(fft_result_search), type = "l", main = "FFT Magnitudes", col='red')
lines(Mod(fft_result_beg), col='purple')

# function to determine the "dominant" wave length of a pattern: 
dom.wave.length.freq <- function(x) {
  fft_result <- fft(x)
  magnitudes <- abs(fft_result)
  max_Index_sine <- which.max(magnitudes[2:length(magnitudes)])
  freq_max <- magnitudes[2:length(magnitudes)][max_Index_sine]
  c(max_Index_sine, freq_max)
}

dom.wave.length.freq(search.example.segment$x)
dom.wave.length.freq(beg.example.segment$x)
# frequency at dom.wave.length:


# do this for all segments:
search.segments <- unique(acc.annotated.comb$segment.id[acc.annotated.comb$behaviour=="for-search"])
beg.segments <- unique(acc.annotated.comb$segment.id[acc.annotated.comb$behaviour.pooled=="beg"])

dom.wave.length.search = NA
for (i in 1:length(search.segments)) {
  segment.id = search.segments[i]
  dom.wave.length.search[i] <- dom.wave.length(acc.annotated.comb$x[acc.annotated.comb$segment.id==segment.id])
}
hist(dom.wave.length.search, breaks=40)
dom.wave.length.beg = NA
for (i in 1:length(beg.segments)) {
  segment.id = beg.segments[i]
  dom.wave.length.beg[i] <- dom.wave.length(acc.annotated.comb$x[acc.annotated.comb$segment.id==segment.id])
}
hist(dom.wave.length.beg, xlim=c(0,200), breaks=40)

# Find the frequency with the largest amplitude
max_Index_sine <- which.max(magnitudes)

paste("magnitude value: ", magnitudes[max_Index_sine] )
paste("Index position: ",max_Index_sine)

# how well does the flexible segmentation work when including begging?
