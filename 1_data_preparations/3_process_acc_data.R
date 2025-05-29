gps.data.10min = read.csv('data/processed/gps.data.csv', as.is='datetime_CEST')
gps.data.10min$datetime_CEST = ymd_hms(gps.data.10min$datetime_CEST, tz='Europe/Amsterdam')
gps.data.10min$year = year(gps.data.10min$datetime_CEST)

gps.behav.data.list <- list()

for (i in 1:length(acc.data.list)) {
  acc.data.bird <- acc.data.list[[i]]
  acc.data.bird <- acc.data.bird[,c('individual_local_identifier','start_timestamp','timestamp','tilt_x','tilt_y','tilt_z')]
  names(acc.data.bird) = c('birdID','date_time','date_time_acc','x','y','z')
  print(acc.data.bird$birdID[1])
  acc.data.bird$year = year(acc.data.bird$date_time)
  acc.data.bird$month = month(acc.data.bird$date_time)
  acc.data.bird$datetime_CEST = with_tz(acc.data.bird$date_time, tz="Europe/Amsterdam")
  # link gps and acc data for behavioural classification that also requires gps speed:
  gps.data.bird <- gps.data.10min[gps.data.10min$birdID==acc.data.bird$birdID[1],]
  data <- merge(gps.data.bird, acc.data.bird, by=c('birdID','datetime_CEST'))
  # order according to the acc data timestamp:
  data <- data[order(data$date_time_acc),]
  # add column with Index for acc data:
  date_time_freq <- table(data$datetime_CEST)
  Index = NULL
  for (j in 1:length(date_time_freq)) Index = c(Index, seq(1,date_time_freq[j]))
  data$Index <- Index
  # predict behaviour from acc data and gps speed:
  data$obs.id <- paste(data$birdID, as.numeric(data$datetime_CEST), sep = ".")
  names(data)[names(data)=='ground.speed'] = 'speed_2d'
  seg.df <- create.fixed.segments(segment.length=1.6, data=data)
  seg.df$datetime_CEST = with_tz(seg.df$date_time, tz='Europe/Amsterdam')
  seg.df$pred.behav <- predict(RF.model.with.begging, seg.df)
  # define 'pooled behavioural categories
  seg.df$behaviour <- as.character(seg.df$pred.behav)
  seg.df$behaviour[seg.df$pred.behav%in%c("for-handle","for-intake","for-search")] = "foraging"
  seg.df$behaviour[seg.df$pred.behav%in%c("walk","drink")] = "other"
  seg.df$behaviour[seg.df$pred.behav%in%c("rest")] = "resting"
  seg.df$behaviour[seg.df$pred.behav%in%c("fly-flap","fly-soar")] = "flying"
  # link again to gps data to also keep the gps locations without acc data (for determining breeding phases):
  gps.behav.data = merge(gps.data.bird, seg.df[,c('datetime_CEST','behaviour','odba')])
  gps.behav.data$odba = round(gps.behav.data$odba, 3)
  gps.behav.data <- gps.behav.data[order(gps.behav.data$datetime_CEST),]
  gps.behav.data.list[[i]] <- gps.behav.data
  acc.data.list[[i]] <- 1 # remove the already processed acc data to save memory
}

gps.behav.data <- from.list.to.df(gps.behav.data.list)
gps.behav.data = gps.behav.data[order(gps.behav.data$birdID, gps.behav.data$datetime_CEST),c('birdID','datetime_CEST','latitude','longitude','altitude','ground.speed','sex','ageclass','behaviour','odba')]
gps.behav.data$datetime_CEST = format(gps.behav.data$datetime_CEST, "%Y-%m-%d %H:%M:%S")

save.image("data/processed/gps.behav.data.0528.RData")

rm(gps.behav.data.list)

write.csv(gps.behav.data, 'data/raw/gps.behav.data.csv', row.names = F)
