gps.behav.data.list <- list()

for (i in 1:length(acc.data.list)) {
  acc.data.bird <- acc.data.list[[i]]
  acc.data.bird <- acc.data.bird[,c('individual_local_identifier','tag_local_identifier','start_timestamp','timestamp','tilt_x','tilt_y','tilt_z')]
  names(acc.data.bird) = c('birdID','tagID','date_time','date_time_acc','x','y','z')
  print(acc.data.bird$birdID[1])
  acc.data.bird$year = year(acc.data.bird$date_time)
  acc.data.bird$month = month(acc.data.bird$date_time)
  # only select relevant data (during period with data of both chick and parent)
  # only select data of months 6-10
  acc.data.bird = acc.data.bird[acc.data.bird$month%in%6:10,]
  parent_offspring_data$use=1
  acc.data.bird <- merge(acc.data.bird, parent_offspring_data[,c('parentID','year','use')], by.x=c('birdID','year'), by.y=c('parentID','year'), all.x=T)
  acc.data.bird <- merge(acc.data.bird, parent_offspring_data[,c('chickID','year','use')], by.x=c('birdID','year'), by.y=c('chickID','year'), all.x=T)
  # select only the data where use.x (for parents) or use.y (for chicks) is not NA
  acc.data.sel <- acc.data.bird[!is.na(acc.data.bird$use.x)|!is.na(acc.data.bird$use.y),1:9]
  # link gps and acc data for behavioural classification that also requires gps speed:
  gps.data.bird <- gps.data[gps.data$birdID==acc.data.bird$birdID[1],]
  data <- merge(gps.data.bird, acc.data.sel, by=c('birdID','tagID','date_time'))
  # order according to the acc data timestamp:
  data <- data[order(data$date_time_acc),]
  # add column with Index for acc data:
  date_time_freq <- table(data$date_time)
  Index = NULL
  for (j in 1:length(date_time_freq)) Index = c(Index, seq(1,date_time_freq[j]))
  data$Index <- Index
  # predict behaviour from acc data and gps speed:
  data$obs.id <- paste(data$birdID, as.numeric(data$date_time), sep = ".")
  names(data)[7] = 'speed_2d'
  seg.df <- create.fixed.segments(segment.length=1.6, data=data)
  seg.df$pred.behav <- predict(RF.model.with.begging, seg.df)
  # define 'pooled behavioural categories
  seg.df$behaviour <- as.character(seg.df$pred.behav)
  seg.df$behaviour[seg.df$pred.behav%in%c("for-handle","for-intake","for-search")] = "foraging"
  seg.df$behaviour[seg.df$pred.behav%in%c("walk","drink")] = "other"
  seg.df$behaviour[seg.df$pred.behav%in%c("rest")] = "resting"
  seg.df$behaviour[seg.df$pred.behav%in%c("fly-flap","fly-soar")] = "flying"
  # calculate sum ODBA per birdID & date_time combination, and add this to the dataframe with selected behaviour per bird & date_time:
  ODBA.datetime <- aggregate(odba~date_time, seg.df, sum)
  ODBA.datetime$odba <- round(ODBA.datetime$odba,3)
  seg.df.ODBA <- merge(seg.df, ODBA.datetime)
  seg.df.ODBA <- seg.df.ODBA[order(seg.df.ODBA$date_time),]
  # link again to gps data to also keep the gps locations without acc data (for determining breeding phases):
  gps.behav.data = merge(gps.data.bird, seg.df.ODBA[,c('date_time','behaviour','odba')])
  gps.behav.data <- gps.behav.data[order(gps.behav.data$date_time),]
  gps.behav.data.list[[i]] <- gps.behav.data
  acc.data.list[[i]] <- 1 # remove the already processed acc data to save memory
}

lapply(gps.behav.data.list, function(x) print(c(dim(x)[1], x$birdID[1])))

keep(gps.behav.data.list, parent_offspring_data, gps.data, refdata_adults, refdata_juvs, sure=T)
