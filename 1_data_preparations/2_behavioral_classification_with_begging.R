load("data/processed/model.classification.begging.0526.RData")
load("data/processed/acc.data.from.movebank.0327.RData")
#load("D:/Tamar - Backup 2024-03-18/Analyses/22. Post-fledging parental care in spoonbills/postfledging_care/data/processed/acc.data.from.movebank.0327.RData")
load("data/processed/gps.data.from.movebank.0327.RData")
source('functions.R')
gps.data <- from.list.to.df(gps.data.list)
# keep relevant columns from gps data:
gps.data <- gps.data[,c('individual.local.identifier','tag.local.identifier','timestamp','location.lat','location.long','height.above.msl','ground.speed')]
names(gps.data) = c('birdID','tagID','date_time','latitude','longitude','altitude','ground.speed')
rm(gps.data.list)

gps.behav.data.list <- list()

for (i in 13:length(acc.data.list)) {
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
  # make sure that data are order according the acc data timestamp:
  data <- data[order(data$date_time_acc),]
  
  # add column with Index for acc data
  # try a faster way, first get all date_time and how often they occur:
  date_time_freq <- table(data$date_time)
  
  #date_time_freq_test=date_time_freq[1:3]
  Index = NULL
  for (j in 1:length(date_time_freq)) Index = c(Index, seq(1,date_time_freq[j]))
  data$Index <- Index
  
  # predict behaviour from acc data and gps speed:
  data$obs.id <- paste(data$birdID, as.numeric(data$date_time), sep = ".")
  names(data)[7] = 'speed_2d'
  seg.df <- create.fixed.segments(segment.length=.8, data=data)
  seg.df$pred.behav <- predict(RF.model.with.begging, seg.df)
  # select the most occurring behaviour per GPS fix (i.e., date_time)
  # first pool behavioural categories
  seg.df$behaviour <- as.character(seg.df$pred.behav)
  seg.df$behaviour[seg.df$pred.behav%in%c("for-handle","for-intake","for-search")] = "foraging"
  seg.df$behaviour[seg.df$pred.behav%in%c("walk","drink")] = "other"
  seg.df$behaviour[seg.df$pred.behav%in%c("rest")] = "resting"
  seg.df$behaviour[seg.df$pred.behav%in%c("fly-flap","fly-soar")] = "flying"
  # select the most often classified behaviour per GPS fix (as there are usually multiple segments (up to 4) of 0.4 s per GPS fix)
  seg.df$freq <- 1
  datetime.behaviours <- aggregate(freq~date_time+behaviour, seg.df, sum)
  datetime.behav.max <- aggregate(freq~date_time, datetime.behaviours, max) # the number of segments that the most often classified behaviour is classified
  datetime.behavdom <- merge(datetime.behaviours, datetime.behav.max, by=c('date_time','freq')) # behaviour that is expressed most often. This could still be more than one behaviour. If this is the case, then randomly choose one of these behaviours: 
  datetime.behavdom$rnd <- runif(dim(datetime.behavdom)[1])
  datetime.rndmax <- aggregate(rnd~date_time, datetime.behavdom, max)
  datetime.behavsel <- merge(datetime.behavdom, datetime.rndmax, by=c("date_time","rnd"))[,c("date_time","behaviour")]
  # calculate sum ODBA per birdID & date_time combination, and add this to the dataframe with selected behaviour per bird & date_time:
  ODBA.datetime <- aggregate(odba~date_time, seg.df, sum)
  datetime.behavsel <- merge(datetime.behavsel, ODBA.datetime)
  datetime.behavsel$odba <- round(datetime.behavsel$odba,3)
  datetime.behavsel <- datetime.behavsel[order(datetime.behavsel$date_time),]
  # link again to gps data to also keep the gps locations without acc data (for determining breeding phases):
  gps.behav.data = merge(gps.data.bird, datetime.behavsel[,c('date_time','behaviour','odba')])
  gps.behav.data <- gps.behav.data[order(gps.behav.data$date_time),]
  gps.behav.data.list[[i]] <- gps.behav.data
  acc.data.list[[i]] <- 1 # remove the already processed acc data to save memory
}

lapply(gps.behav.data.list, function(x) print(c(dim(x)[1], x$birdID[1])))

keep(gps.behav.data.list, parent_offspring_data, gps.data, refdata_adults, refdata_juvs, sure=T)

save.image("data/processed/gps.behav.beg.data.0527.RData")

load("data/processed/gps.behav.beg.data.0526.RData") # on segments of 1.0 s, but not sure if the RF model went right, omdat er iets mis leek te gaan met de Indexing bij de create.fixed.segments functie (er stond toen nog Index+1, wsl omdat eerder Index met 0 begon, maar nu met 1).
for(i in 1:length(gps.behav.data.list)) print(c(birdID=gps.behav.data.list[[i]]$birdID[1],round(table(gps.behav.data.list[[i]]$behaviour)/dim(gps.behav.data.list[[i]])[1],2)))

load("data/processed/gps.behav.beg.data.0527.RData") # on segments of 0.8 s, 
for(i in 1:length(gps.behav.data.list)) print(c(birdID=gps.behav.data.list[[i]]$birdID[1],round(table(gps.behav.data.list[[i]]$behaviour)/dim(gps.behav.data.list[[i]])[1],2)))

head(gps.behav.data.list[[23]])

gps.behav.data.list[[23]]$week <- week(gps.behav.data.list[[23]]$date_time)

behav.6385 = round(table(gps.behav.data.list[[23]]$week, gps.behav.data.list[[23]]$behaviour),0)
round(behav.6385[,'beg']/(behav.6385[,'beg']+behav.6385[,'foraging']),2)
