# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


### Functions to import tracking data from UvA-BiTS website ###
# make R data file
ImportGPSDataBird <- function(birdID, date_start, date_end) {
  df <- sqlQuery(db, query = paste("SELECT device_info_serial,  
                                     date_time, longitude,
                                     latitude,altitude,speed_2d, gps_fixtime FROM gps.ee_tracking_speed_limited 
                                     WHERE device_info_serial = ",birdID,"AND  date_time >= '" , as.POSIXct(date_start, origin='1970-01-01', tz="UTC"),
                                   "' AND date_time <= '", as.POSIXct(date_end, origin='1970-01-01', tz="UTC"), "'", sep = ''))
  df[order(df$date_time),]
}

ImportAllGPSDataBird <- function(birdID, date_start, date_end) {
  df <- sqlQuery(db, query = paste("SELECT * FROM gps.ee_tracking_speed_limited 
                                     WHERE device_info_serial = ",birdID,"AND  date_time >= '" , as.POSIXct(date_start, origin='1970-01-01', tz="UTC"),
                                   "' AND date_time <= '", as.POSIXct(date_end, origin='1970-01-01', tz="UTC"), "'", sep = ''))
  df[order(df$date_time),]
}


ImportSMSDataBird <- function(birdID, date_start, date_end) {
  df <- sqlQuery(db, query = paste("SELECT * FROM gps.ee_sms_position_limited
                                     WHERE device_info_serial = ",birdID,"AND  date_time >= '" , as.POSIXct(date_start, origin='1970-01-01', tz="UTC"),
                                   "' AND date_time <= '", as.POSIXct(date_end, origin='1970-01-01', tz="UTC"), "'", sep = ''))
  df[order(df$date_time),]
}

ImportAccDataBird <- function(birdID, date_start, date_end) {
  df <- sqlQuery(db, query = paste("SELECT device_info_serial,  
                                   date_time, index, x_acceleration, y_acceleration, z_acceleration FROM gps.ee_acceleration_limited 
                                   WHERE device_info_serial = ",birdID,"AND  date_time >= '" , as.POSIXct(date_start, origin='1970-01-01', tz="UTC"),
                                   "' AND date_time <= '", as.POSIXct(date_end, origin='1970-01-01', tz="UTC"), "'", sep = ''))
  df[order(df$date_time),]
}

ImportAllAccDataBird <- function(birdID, date_start, date_end) {
  df <- sqlQuery(db, query = paste("SELECT * FROM gps.ee_acceleration_limited 
                                   WHERE device_info_serial = ",birdID,"AND  date_time >= '" , as.POSIXct(date_start, origin='1970-01-01', tz="UTC"),
                                   "' AND date_time <= '", as.POSIXct(date_end, origin='1970-01-01', tz="UTC"), "'", sep = ''))
  df[order(df$date_time),]
}

ImportTrackerInfo <- function(birdID) {
  df <- sqlQuery(db, query = paste("SELECT device_info_serial, firmware_version, mass, start_date, end_date, x_o, x_s, y_o, y_s, z_o, z_s, tracker_id FROM gps.ee_tracker_limited 
                                   WHERE device_info_serial = ",birdID, sep = ''))
  df
}

### End of tracking data loading functions

### Functions needed to calculate summary statistics over acceleration segments ###

# Dominant Power Spectrum
dps <- function(x){
  d.x <- x - mean(x, na.rm = T)   
  return(max(((Mod(fft(d.x))^2)/length(x))[1:(length(x)/2)]))
}

# Frequency at the Dominant Power Spectrum
fdps <- function(x){
  fq <- (1:(length(x)/2)) / (length(x)*0.05)
  d.x <- x - mean(x, na.rm = T)   
  return(fq[which.max(((Mod(fft(d.x))^2)/length(x))[1:(length(x)/2)])])
}

odba <- function(x){
  d.x <- x - mean(x, na.rm = T)   
  return(sum(abs(d.x), na.rm = T)/length(x))
}

trend <- function(x){
  dx <- lm(x ~ c(1:length(x)))
  return(dx$coeff[2])
}

noise <- function(x){
  noise.tmp <- NA
  noise.mean <- NA
  if (length(x)>2) {
    for (i in 2:(length(x)-1)) noise.tmp[i] <- x[i]-(x[i-1]+x[i+1])/2
    noise.mean <- mean(na.omit(noise.tmp))
  }
  return(noise.mean)
}

### End of summary statistics function ###

create.fixed.segments <- function(segment.length, data, sampling.freq=20, annotated.data=F) { # segment length is expressed in seconds
  
  samples.per.segment <- ceiling(segment.length * sampling.freq) 
  indices.to.use <- seq(min(data$Index),max(data$Index),by=20/sampling.freq)
  data.sel = data[data$Index %in% indices.to.use,] 
  
  data.sel$segment.id.cut <- paste(data.sel$obs.id, formatC(format="d", ceiling((data.sel$Index)/(segment.length*20)),flag="0",width=ceiling(log10(max(ceiling((data.sel$Index)/(segment.length*20)))))), sep = ".") # 20 is the sampling frequency at which the data was originally collected
  
  table(table(data.sel$segment.id.cut))
  data.sel <- na.omit(data.sel)
  
  ## calculate summary statistics for each segment: 
  seg.df <- ddply(data.sel, .(segment.id.cut, birdID, date_time), summarize, 
                  nobs.segments  = length (x), speed_2d = mean(speed_2d),
                  mean.x = mean(x), mean.z = mean(z), mean.y = mean(y), 
                  min.x = min (x), min.y = min (y), min.z = min (z),
                  max.x = max (x), max.y = max (y), max.z = max (z), 
                  trend.x = trend (x), trend.y = trend (y), trend.z = trend (z),
                  odba.x = odba(x), odba.y = odba(y), odba.z = odba(z), 
                  dps.x = dps(x), dps.y = dps(y), dps.z = dps(z),
                  fdps.x = fdps(x),  fdps.y = fdps(y), fdps.z = fdps(z), 
                  kurt.x = kurtosis(x), kurt.y = kurtosis(y), kurt.z = kurtosis(z), 
                  skew.x = skewness(x), skew.y = skewness(y), skew.z = skewness(z),
                  noise.x = noise(x), noise.y = noise(y), noise.z = noise(z)
  ) 
  
  seg.df$odba <- seg.df$odba.x + seg.df$odba.y + seg.df$odba.z 
  
  seg.df <- seg.df[seg.df$nobs.segments==samples.per.segment,] # only use segments of the specified segment length
  
  if (annotated.data==T) {
    seg.df <- assign.behaviour.to.segments(seg.df, data.sel)
    return(list(seg.df, data.sel))
  } 
  else {
    return(seg.df)
  }
}

assign.behaviour.to.segments <- function(seg.df, acc) {
  # match most occurring behaviour during a segment with seg.df data frame:
  acc$freq <- 1
  segment.id.cut.behaviours <- aggregate(freq~segment.id.cut+behaviour.pooled, acc, sum)
  segment.id.cut.nobsmax <- aggregate(freq~segment.id.cut, segment.id.cut.behaviours, max) # the number of points that the longest expressed behaviour is expressed
  segment.id.cut.behavdom <- merge(segment.id.cut.behaviours, segment.id.cut.nobsmax, by=c('segment.id.cut','freq')) # behaviour that is expressed the longest
  # randomly select one of the equally often expressed behaviours:
  segment.id.cut.behavdom$rnd <- runif(dim(segment.id.cut.behavdom)[1])
  segment.id.cut.rndmax <- aggregate(rnd~segment.id.cut, segment.id.cut.behavdom, max)
  segment.id.cut.behavsel <- merge(segment.id.cut.behavdom, segment.id.cut.rndmax, by=c("segment.id.cut","rnd"))
  
  # determine whether a segment consists of a single behaviour ("clean" segments)
  num.obs <- cast(melt(acc), segment.id.cut ~ behaviour.pooled, length, subset = variable == 'birdID')
  nobs.segment <- aggregate(freq~segment.id.cut, acc, sum)
  names(nobs.segment)[2]<-"nobs"
  segment.id.cut.behavsel <- merge(segment.id.cut.behavsel, nobs.segment, all.x=T)
  segment.id.cut.behavsel$single.behaviour <- 0
  segment.id.cut.behavsel$single.behaviour[segment.id.cut.behavsel$freq==segment.id.cut.behavsel$nobs] <- 1
  
  seg.df$behaviour.pooled <- segment.id.cut.behavsel$behaviour.pooled[match(seg.df$segment.id.cut,  segment.id.cut.behavsel$segment.id.cut)] 
  seg.df$single.behaviour <- segment.id.cut.behavsel$single.behaviour[match(seg.df$segment.id.cut,  segment.id.cut.behavsel$segment.id.cut)] 
  seg.df <- seg.df[is.na(seg.df$behaviour.pooled)==F,] # remove cases where behaviour was not classified
  seg.df
}


# to make the occurrence of different behaviours similar, downsample certain very frequently occurring behaviours
downsampling.behaviours <- function(seg.df, stand=1, search=1) {
  segments.to.keep <- c(seg.df$segment.id.cut[seg.df$behaviour.pooled!="stand"&seg.df$behaviour.pooled!="search"],
                        sample(seg.df$segment.id.cut[seg.df$behaviour.pooled=="stand"],length(seg.df$segment.id.cut[seg.df$behaviour.pooled=="stand"])/stand),
                        sample(seg.df$segment.id.cut[seg.df$behaviour.pooled=="search"],length(seg.df$segment.id.cut[seg.df$behaviour.pooled=="search"])/search))
  seg.df <- seg.df[seg.df$segment.id.cut %in% segments.to.keep,]
  seg.df
}

RF.model <- function(seg.df, acc, clean.segments.train = FALSE, clean.segments.test = FALSE, stand=1, search=1) {
  seg.df <- seg.df[,apply(!is.na(seg.df), 2, any)]
  seg.df <- na.omit(seg.df)
  
  ind <- sample(1:2, nrow(seg.df), replace = TRUE, prob=c(0.7, 0.3)) # divide data into 70% training (ind=1) and 30% testing (ind=2) data 
  data.train <- seg.df[ind == 1,] # the training dataset
  data.test <- seg.df[ind ==2,] # the testing dataset
  
  # perform up- and downsampling on the train dataset only, to be able to properly interpret the effects on sensitivity and precision for the test dataset
  data.train <- downsampling.behaviours(data.train, stand=stand, search=search)
  data.train$behaviour.pooled <-  factor(data.train$behaviour.pooled) 
  if (clean.segments.train == T) data.train <- data.train[data.train$single.behaviour==1,]
  if (clean.segments.test == T) data.test <- data.test[data.test$single.behaviour==1,]
  
  # remove the predictor variables from selected.variables that contained all NA in seg.df (e.g. noise when there are only 2 samples per segment, at 2 Hz)
  #selected.variables <- selected.variables[selected.variables%in%names(seg.df)]
  data.train <- data.train[,5:37]
  
  # fit the model on the train dataset
  fit.RF <- randomForest(behaviour.pooled ~ ., data = data.train, importance=T)
  behav.pred <- predict(fit.RF, data.test) # do the prediction on a random selection of the dataset (the testing/validation dataset)
  df.pred <- cbind(data.test, behav.pred)
  acc.pred  <- merge(acc, df.pred[,c("segment.id.cut","behav.pred")])
  mytable <- table(predicted = acc.pred$behav.pred, observed = acc.pred$behaviour.pooled)
  mytable <- mytable[, match(rownames(mytable), colnames(mytable))]
  list(fit.RF, mytable, df.pred)
}

RF.model.start <- function(seg.df, stand=1) {
  seg.df <- downsampling.behaviours(seg.df, stand=stand)
  seg.df$behaviour.pooled <-  factor(seg.df$behaviour.pooled)
  seg.df <- na.omit(seg.df) # not sure where the NA's come from...
  # fit the model on the train dataset
  fit.RF <- randomForest(behaviour.pooled ~ mean.x + mean.y + mean.z + 
                           min.x + min.y + min.z + max.x + max.y + max.z + 
                           trend.x + trend.y + trend.z + odba.x + odba.y + odba.z + odba + 
                           dps.x + dps.y + dps.z + fdps.x + fdps.y + fdps.z + 
                           kurt.x + kurt.y + kurt.z + skew.x + skew.y + skew.z +
                           noise.x + noise.y + noise.z + 
                           speed_2d, 
                         data = seg.df, importance=T)
  fit.RF
}

## Calculate measures of classification performance for each behaviour class 
calculate.performance <- function(RF.model.results) {
  RF.model.results <- RF.model.results[,is.na(colnames(RF.model.results))==F]

  # if a certain behaviour was never observed in the test data, make it 0:
  if (max(colnames(RF.model.results)=="for-intake")==0) RF.model.results <- cbind(RF.model.results,"for-intake"=0)
  if (max(colnames(RF.model.results)=="for-handle")==0) RF.model.results <- cbind(RF.model.results,"for-handle"=0)
  if (max(colnames(RF.model.results)=="fly-soar")==0) RF.model.results <- cbind(RF.model.results,"fly-soar"=0)
  
  # if a certain behaviour was never predicted in the test data, make it 0:
  if (max(rownames(RF.model.results)=="for-intake")==0) RF.model.results <- rbind(RF.model.results,"for-intake"=0)
  if (max(rownames(RF.model.results)=="for-handle")==0) RF.model.results <- rbind(RF.model.results,"for-handle"=0)
  if (max(rownames(RF.model.results)=="fly-soar")==0) RF.model.results <- rbind(RF.model.results,"fly-soar"=0)

  RF.model.results <- RF.model.results[order(rownames(RF.model.results)),order(colnames(RF.model.results))] # order by column- and rowname
  cM <- confusionMatrix(as.table(RF.model.results))
  sensitivity <- cM$byClass[,"Sensitivity"] # 
  specificity <- cM$byClass[,"Specificity"] # or recall
  precision <- cM$byClass[,"Pos Pred Value"] # or precision
  accuracy.overall <- sum(diag(RF.model.results))/sum(RF.model.results) # this is the "global" accuracy = (TP+TN)/(TP+FP+FN+TN)
  list(sensitivity, specificity, precision, accuracy.overall, cM)
}
### End of classification performance calculation

## Calculate measures of classification performance for pooled behavours: resting, walking, foraging, flying, begging 
calculate.performance.pooled <- function(RF.model.results) {
  
  RF.model.results <- RF.model.results[,is.na(colnames(RF.model.results))==F]
  
  # if a certain behaviour was never observed in the test data, make it 0:
  if (max(colnames(RF.model.results)=="for-intake")==0) RF.model.results <- cbind(RF.model.results,"for-intake"=0)
  if (max(colnames(RF.model.results)=="for-handle")==0) RF.model.results <- cbind(RF.model.results,"for-handle"=0)
  if (max(colnames(RF.model.results)=="fly-soar")==0) RF.model.results <- cbind(RF.model.results,"fly-soar"=0)

  # if a certain behaviour was never predicted in the test data, make it 0:
  if (max(rownames(RF.model.results)=="for-intake")==0) RF.model.results <- rbind(RF.model.results,"for-intake"=0)
  if (max(rownames(RF.model.results)=="for-handle")==0) RF.model.results <- rbind(RF.model.results,"for-handle"=0)
  if (max(rownames(RF.model.results)=="fly-soar")==0) RF.model.results <- rbind(RF.model.results,"fly-soar"=0)

  RF.model.results.pooled.cols = cbind(forage=rowSums(RF.model.results[,c('for-search','for-handle','for-intake')]), 
                                  walk=RF.model.results[,'walk'],
                                  rest=rowSums(RF.model.results[,c('sit','stand')]),
                                  fly=rowSums(RF.model.results[,c('fly-flap','fly-soar')]),
                                  beg=RF.model.results[,'beg']
                                  )
  
  RF.model.results.pooled = rbind(forage=colSums(RF.model.results.pooled.cols[c('for-search','for-handle','for-intake'),]), 
                                  walk=RF.model.results.pooled.cols['walk',],
                                  rest=colSums(RF.model.results.pooled.cols[c('sit','stand'),]),
                                  fly=colSums(RF.model.results.pooled.cols[c('fly-flap','fly-soar'),]),
                                  beg=RF.model.results.pooled.cols['beg',]
                                  )

  cM <- confusionMatrix(as.table(RF.model.results.pooled))
  sensitivity <- cM$byClass[,"Sensitivity"] # 
  specificity <- cM$byClass[,"Specificity"] # or recall
  precision <- cM$byClass[,"Pos Pred Value"] # or precision
  accuracy.overall <- sum(diag(RF.model.results))/sum(RF.model.results) # this is the "global" accuracy = (TP+TN)/(TP+FP+FN+TN)
  list(sensitivity, specificity, precision, accuracy.overall, cM)
}
### End of classification performance calculation of pooled behaviours

### Functions to calculate means and quantiles on matrices and arrays
quantile.0.025 <- function(x) quantile(na.omit(x), 0.025)
quantile.0.975 <- function(x) quantile(na.omit(x), 0.975)
calculate.F.measure <- function(sensitivity, precision) 2/(1/(sensitivity+0.001)+1/(precision+0.001)) # 0.001 is added to be able to still calculate an F measure when sensitivity or precision is 0. 

# calculate mean and CRI over simulations
calculate.mean.CRI <- function(x) {
  mean.CRI <- array(NA, c(dim(x)[2],dim(x)[3],3), dimnames = list(dimnames(x)[[2]], dimnames(x)[[3]], c("mean","lcl","ucl")))
  mean.CRI[,,"mean"] <- apply(x,c(2,3),mean) 
  mean.CRI[,,"lcl"] <- apply(x,c(2,3),quantile.0.025)
  mean.CRI[,,"ucl"] <- apply(x,c(2,3),quantile.0.975)
  mean.CRI
}

from.list.to.df <- function(list) {   # change from list to dataframe
  df <- list[[1]]
  if (length(list)>1) for (i in 2:length(list)) df <- rbind(df, list[[i]])
  df
}

# Function to find the closest matching datetime in df2 for each datetime in df1
findClosestDatetime <- function(datetime, datetime_vec) {
  index <- findInterval(datetime, datetime_vec)
  closest_datetime <- ifelse(datetime - datetime_vec[index] < datetime_vec[index + 1] - datetime,
                             datetime_vec[index], datetime_vec[index + 1])
  return(closest_datetime)
}

determine.nest.location <- function(df) {
  df <- na.omit(df[,c('latitude','longitude','datetime_CEST')])
  df$yday_CEST <- yday(df$datetime_CEST)
  df$lat_rnd=round(df$latitude, digit=5)
  df$lon_rnd=round(df$longitude, digit=6)
  df$duration=NA
  df$duration[1:(dim(df)[1]-1)]=interval(df$datetime_CEST[1:(dim(df)[1]-1)], df$datetime_CEST[2:dim(df)[1]])/60 
  # duration in minutes
  # for nest determination, only select durations of <11, to exclude intervals longer than 10 minutes
  # determine the total time spent per rounded coordinate, selecting the position with the longest total time spent as (the potential) nest:
  duration.per.coord = aggregate(duration~lat_rnd+lon_rnd, df[df$duration<10,], sum)
  duration.per.coord = duration.per.coord[order(duration.per.coord$duration, decreasing=TRUE),][1:20,]
  coord.nest = duration.per.coord[1,1:2] # determine coordinates nest as the coordinates visited for the longest time 
  coord.nest
}

link.chick.parent.data <- function(chick_data, parent_data, behaviour=FALSE) {
  if (behaviour==T) {
    # prepare chick data (do not remove NA's, as latitude and speed are not measured for the GPS data sent via GSM)
    chick_data = chick_data[order(chick_data$datetime_CEST),
                                      c('birdID','datetime_CEST','latitude','longitude','altitude','ground.speed','sex','behaviour')]
    names(chick_data) = c('birdID.chick', 'datetime.chick', 'latitude.chick', 'longitude.chick','altitude.chick',
                          'ground.speed.chick','sex.chick','behaviour.chick')
    # prepare parent data
    parent_data = parent_data[order(parent_data$datetime_CEST),
                                      c('birdID','datetime_CEST','latitude','longitude','altitude','ground.speed','sex','behaviour')]
    names(parent_data) = c('birdID.parent', 'datetime.parent', 'latitude.parent', 'longitude.parent','altitude.parent',
                           'ground.speed.parent','sex.parent','behaviour.parent')
  }
  else {
    # prepare chick data (do not remove NA's, as latitude and speed are not measured for the GPS data sent via GSM)
    chick_data = chick_data[order(chick_data$datetime_CEST),
                                    c('birdID','datetime_CEST','latitude','longitude','altitude','ground.speed','sex')]
    names(chick_data) = c('birdID.chick', 'datetime.chick', 'latitude.chick', 'longitude.chick','altitude.chick','ground.speed.chick','sex.chick')
    # prepare parent data
    parent_data = parent_data[order(parent_data$datetime_CEST),c('birdID','datetime_CEST','latitude','longitude','altitude','ground.speed','sex')]
    names(parent_data) = c('birdID.parent', 'datetime.parent', 'latitude.parent', 'longitude.parent','altitude.parent','ground.speed.parent','sex.parent')
  }
  # link chick and parent data
  chick_data$closest_datetime <- sapply(chick_data$datetime.chick, findClosestDatetime, datetime_vec = parent_data$datetime.parent)
  chick_parent_data <- merge(chick_data, parent_data, by.x = "closest_datetime", by.y = "datetime.parent", all.x = TRUE)
  # in case there is no data of the parent available (as all closest_datetimes have already been linked), fill in the relevant columns with parent information:
  chick_parent_data[,c('birdID.parent','sex.parent')] = parent_data[1,c('birdID.parent','sex.parent')]
  chick_parent_data$datetime.parent = as.POSIXct(chick_parent_data$closest_datetime, tz="Europe/Amsterdam", origin="1970-01-01 00:00:00")
  # calculate distance between chick and parent
  chick_parent_data$distance <- round(distGeo(matrix(c(chick_parent_data$longitude.chick, chick_parent_data$latitude.chick), ncol=2, byrow=F), 
                                        matrix(c(chick_parent_data$longitude.parent, chick_parent_data$latitude.parent), ncol=2, byrow=F)),0)
  chick_parent_data
}

determine.departure.info <- function(data) { # data has columns in the order of birdID, datetime, latitude, longitude for the time window of autumn migration (August-December) for one individual and one year
  data = unique(data)
  names(data) <- c('birdID','datetime','latitude','longitude')
  data$date <- date(data$datetime)
  # only select data in autumn migration period:
  data = data[month(data$datetime)%in%8:12,]
  year <- year(data$datetime)[1]
  birdID <- data$birdID[1]
  time.min.day <- aggregate(datetime~date, data, min)
  time.max.day <- aggregate(datetime~date, data, max)
  # link min and max time per date to the locations
  first.location.day <- merge(time.min.day, data[,2:5], all.x=T)
  last.location.day <- merge(time.max.day, data[,2:5], all.x=T)
  first.last.location.day <- cbind(first.location.day, last.location.day[c('datetime','latitude','longitude')])
  names(first.last.location.day)[c(2:7)]<-c('datetime.first','lat.first','lon.first','datetime.last','lat.last','lon.last') 
  first.last.location.day$distance.first.last <- round(distGeo(as.matrix(first.last.location.day[,c('lon.first','lat.first')]), as.matrix(first.last.location.day[,c('lon.last','lat.last')]))/1000,3)
  first.last.location.day <- first.last.location.day[order(first.last.location.day$date),]
  first.last.location.day$distance.south <- round((first.last.location.day$lat.first - first.last.location.day$lat.last)*111,3) # this is approximately (varies slightly depending on the latitude)
  # problematic is that juveniles sometimes go on exploratory flights, often in northward direction, that may be followed by a return flight back to the area around the natal colony. as such, the definition of the departure on migration may be better defined as the first day on which there was considerable southward movement from the colony
  # now calculate the distance south from the colony:
  lat.colony=53.48
  first.last.location.day$distance.south.of.colony.first <- (lat.colony-first.last.location.day$lat.first)*111
  first.last.location.day$distance.south.of.colony.last <- (lat.colony-first.last.location.day$lat.last)*111
  # to exclude such return flights after northward exploration, we use the criterion distance.south>50 & distance.south.of.colony.first<50 & distance.south.of.colony.last>50
  # label migration days as days with more than 100 km southward migration
  first.last.location.day$migration.day <- 0
  first.last.location.day$migration.day[first.last.location.day$distance.south>100]<-1
  # label days where the bird was more than 50 km south of the colony
  first.last.location.day$south.of.colony <- 0
  first.last.location.day$south.of.colony[first.last.location.day$distance.south.of.colony.last>50]<-1 # 
  # departure day is defined as the first migration day where the distance south of the colony was more than 50 km by the end of the day; by doing so, this excluded return flights after a northward exploration.
  migration.days <- first.last.location.day[first.last.location.day$migration.day==1 & first.last.location.day$south.of.colony==1,]
  if (dim(migration.days)[1]>0) {
    departure.date <- min(migration.days$date)
    # determine time and location of departure as the datetime (and location) preceding 10 consecutive 10-min intervals with considerable (>1 km) displacement, selecting the day of migration, as well as the preceding day, as departure may have occurred just before midnight:
    # however, this code does not work for the sms data, as intervals are considerably longer than 10 minutes! For those cases however, the exact time of departure cannot be determined... So it should say the earliest time of departure in those cases... 
    departure.window = data[data$date%in%c(departure.date, departure.date-days(1)),]
    migratory.flight.info = retrieve.flight.information(departure.window)
    migratory.flight.info$interval.length.avg = round(migratory.flight.info$flight.duration / migratory.flight.info$n.intervals,2)
    departure.flight.info = migratory.flight.info[1,][c('datetime.start','lat.start','lon.start','interval.length.avg')]
    names(departure.flight.info) = c('datetime','latitude','longitude','interval.avg')
    departure.flight.info$birdID=birdID
    departure.flight.info$year = year
    departure.flight.info = departure.flight.info[,c('birdID','year','datetime','latitude','longitude','interval.avg')]
    departure.flight.info
  } 
}

# function to define a migratory flight:
retrieve.flight.information <- function(data) {
  data$distance.to.next = NA
  data$duration.to.next = NA
  data$distance.to.next[1:(dim(data)[1]-1)] =   distGeo(matrix(c(data$longitude[2:dim(data)[1]], data$latitude[2:dim(data)[1]]), ncol=2, byrow=F), 
                                                        matrix(c(data$longitude[1:(dim(data)[1]-1)], data$latitude[1:(dim(data)[1]-1)]), ncol=2, byrow=F))/1000
  data$duration.to.next[1:(dim(data)[1]-1)] =   round(time_length(interval(data$datetime[1:(dim(data)[1]-1)], data$datetime[2:dim(data)[1]]), unit="minutes"),2)
  
  data$travelling <- 0
  data$travelling[data$distance.to.next/(data$duration.to.next/60)>5] <- 1 # a bird is defined to be travelling when the average speed is at least 5 km/hour
  
  # first determine if the data starts with travelling or not,
  if(data$travelling[1]==0) {
    data$within.flight.count <- 0
    data$flight.count <- 0
  } else {
    data$within.flight.count <- 1 
    data$flight.count <- 1 
  }
  
  # then start counting the number of flights and within-flight intervals
  for (i in 2:dim(data)[1]) {
    # first fill the within-flight number
    if (data$travelling[i-1]==1) data$within.flight.count[i] = data$within.flight.count[i-1]+1 else 
      if(data$travelling[i]==1 & data$travelling[i-1]==0) data$within.flight.count[i]=1 else data$within.flight.count[i]=0
    # the determine the flight number
    if (data$within.flight.count[i-1]==0 & data$within.flight.count[i]>0) data$flight.count[i]=data$flight.count[i-1]+1 else 
      if (data$within.flight.count[i]>0 & data$travelling[i-1]==0) data$flight.count[i]=data$flight.count[i-1]+1 else data$flight.count[i]=data$flight.count[i-1] 
  }
  
  # the within.flight.count stops counting only 1 interval AFTER the last travelling interval, which allows for more easy selection of the GPS location at the end of a migratory flight. 
  
  # calculate the number of intervals, total duration, and distance between first and last GPS location during each flight:
  data$duration.within.flight.count <- 0
  data$duration.within.flight.count[data$travelling==1] <- data$duration.to.next[data$travelling==1]
  flight.info <- aggregate(cbind(duration.within.flight.count, distance.to.next)~flight.count, data, sum)
  names(flight.info) = c('flight.count','flight.duration','flight.distance.cum')
  # retrieve and add start GPS location and date_time per migratory flight:
  flight.start.info = data[data$within.flight.count==1,]
  flight.info <- merge(flight.info, flight.start.info[,c('flight.count','latitude','longitude','datetime')])
  names(flight.info)[4:6]=c('lat.start','lon.start','datetime.start')
  # determine number of GPS locations associated with each migratory flight
  flight.n.loc <- aggregate(within.flight.count~flight.count, data, max)
  flight.n.loc = flight.n.loc[flight.n.loc$flight.count>0,]
  # retrieve the GPS location at the end of a migratory flight:
  flight.end.info = merge(data, flight.n.loc)
  flight.info <- merge(flight.info, flight.end.info[,c('flight.count','latitude','longitude','datetime','within.flight.count')])
  flight.info$n.intervals = flight.info$within.flight.count-1
  names(flight.info)[7:9]=c('lat.end','lon.end','datetime.end')
  # calculate the distance between first and last GPS location of migratory flight:
  flight.info$flight.distance.start.end = round(distGeo(matrix(c(flight.info$lon.start, flight.info$lat.start), ncol=2, byrow=F), 
          matrix(c(flight.info$lon.end, flight.info$lat.end), ncol=2, byrow=F))/1000,3)
  flight.info$distance.southward <- (flight.info$lat.start-flight.info$lat.end)*111

  migratory.flights <- flight.info[flight.info$flight.distance.start.end>30,] # a migratory flight is defined as a flight during which an individual travels at least 30 km between start and end GPS location of the flight which started on the day or the day before the first 'migration day' (as defined above). 
  
  migratory.flights
}

# make a neat table for manuscript from dredge output
make.table.from.dredge.output <- function(dredge_output) {
  table.output <- as.data.frame(dredge_output)
  for (i in 1:dim(table.output)[1]) {
    expl.vars = table.output[i,2:(which(names(table.output)=='df')-1)]
    expl.vars = names(expl.vars)[!is.na(expl.vars)]
    model.formula = paste(expl.vars,collapse=" + ")
    table.output$model.name[i] <- model.formula
  }
  table.output$'-2logL' <- table.output$logLik * -2
  table.output$'d-2logL' <- table.output$'-2logL' - min(table.output$'-2logL')
  table.output <- table.output[,c('model.name','df','d-2logL','delta','weight')]
  names(table.output)[c(2,4,5)] <- c('K','dAICc','Akaike.weight')
  table.output[,3:5] <- format(round(table.output[,3:5],2), trim=T)
  table.output
}

round_to_nearest <- function(x, multiple) {
  round(x / multiple) * multiple
}

calculate.bootstrap.interval <- function(x) {
  boot_result <- boot(data = x, statistic = mean_func, R = 10000)
  boot_ci <- boot.ci(boot_result, type = "perc")
  boot_ci$percent
}

mean_func <- function(data, indices) {
  mean(data[indices])
}

calculate.se <- function(x) {
  N = length(x)
  sd = sd(x)
  se = sd/sqrt(N)
  se
}

calculate.mean.dist.km = function(x) round(mean(x)/1000,2)