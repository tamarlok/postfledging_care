# re-import all required files:
gps.data.10min = read.csv('data/raw/gps.data.csv', as.is='datetime_CEST')
gps.data.10min$datetime_CEST = ymd_hms(gps.data.10min$datetime_CEST, tz='Europe/Amsterdam')
gps.data.10min$year = year(gps.data.10min$datetime_CEST)
parent_offspring_data <- read.csv("data/raw/parent.offspring.data.csv")
parents <- unique(parent_offspring_data$parentID)
chicks <- unique(parent_offspring_data$chickID)

# determine autumn migration departure dates per bird per year:
# migration is defined as the first day during which the bird travelled >50 km southward of the colony. 
# to exclude (return flights after) northward (or east- or westward) exploration, we use the criterion distance.south>50 & distance.south.of.colony.start<50 & distance.south.of.colony.end>50
# determine for each bird and each year the departure date during autumn migration
birds = unique(gps.data.10min$birdID)
departure.dates = data.frame(birdID=NA, year=NA, datetime=NA, latitude=NA, longitude=NA, interval.avg=NA, sex=NA)
i=0
for (bird in birds) {
  gps.data.bird = gps.data.10min[gps.data.10min$birdID==bird,]
  sex.bird = as.character(gps.data.bird$sex[1])
  years = unique(gps.data.bird$year)
  for (yr in years) {
    gps.data.bird.year = gps.data.bird[gps.data.bird$year==yr,]
    gps.data.bird.year.autumn = gps.data.bird.year[month(gps.data.bird.year$datetime_CEST)%in%8:12,c('birdID','datetime_CEST','latitude','longitude')]
    if (dim(gps.data.bird.year.autumn)[1]>0) {
      departure.info <- determine.departure.info(gps.data.bird.year.autumn)
      if (!is.null(departure.info)) {
        print(departure.info)
        i=i+1
        departure.dates[i,] <- cbind(departure.info, sex.bird)
      }
    }
  }
}

departure.dates$datetime <- as.POSIXct(departure.dates$datetime, tz="Europe/Amsterdam", origin = "1970-01-01")
names(departure.dates)[3]='datetime_departure'
departure.dates

# link departure info to parent_offspring_data:
departure.dates.chick.parent = merge(parent_offspring_data[,1:3], departure.dates, by.x=c('parentID','year'), by.y=c('birdID','year'), all.x=T)
departure.dates.chick.parent = merge(departure.dates.chick.parent, departure.dates, by.x=c('chickID','year'), by.y=c('birdID','year'), all.x=T)
names(departure.dates.chick.parent)[4:13]=c('departure.datetime.parent','departure.lat.parent','departure.lon.parent','interval.parent','sex.parent','departure.datetime.chick','departure.lat.chick','departure.lon.chick','interval.chick','sex.chick')
departure.dates.chick.parent$sex.parent.n = 1
departure.dates.chick.parent$sex.parent.n[departure.dates.chick.parent$sex.parent=='m']=2

# pretend it's all the same year in which these departures occur, so datetime can be plotted instead of yday:
departure.dates.chick.parent$departure.datetime.chick.2018 <- departure.dates.chick.parent$departure.datetime.chick + years(2018-year(departure.dates.chick.parent$departure.datetime.chick))
departure.dates.chick.parent$departure.datetime.parent.2018 <- departure.dates.chick.parent$departure.datetime.parent + years(2018-year(departure.dates.chick.parent$departure.datetime.parent))

# link data of chick to parent, based on the smallest time difference between their GPS fixes, and calculate the distance between them:
chick.parent.data.list = NULL
nest.data = data.frame(latitude=NA, longitude=NA)
i=0

for (chick in chicks) {
  print(chick)
  parent = parent_offspring_data$parentID[parent_offspring_data$chickID==chick]
  chick_data = gps.data.10min[gps.data.10min$birdID==chick,]
  year = chick_data$year[1]
  # if there is only one parent GPS-tracked, then run the following line to retrieve parent data:
  if (length(parent)==1) {
    i = i+1
    print(parent)
    parent_data = gps.data.10min[gps.data.10min$birdID==parent & gps.data.10min$year==year,]
    if (dim(parent_data)[1]>0) {
      chick_parent_data <- link.chick.parent.data(chick_data, parent_data)
      chick.parent.data.list[[i]] = chick_parent_data
    }
    # determine nest location based on parent_data from the data between the 1st of May (which has already been selected in the 2_process_gps_data file) until the tag deployment of the chick: 
    parent_data_nest <- parent_data[parent_data$datetime_CEST < min(chick_data$datetime.chick),]
    nest.location = determine.nest.location(parent_data_nest)
    nest.data[i,] = nest.location
    print(nest.location)
    }
  if (length(parent)==2) {
    # link chick data to parent 1 data
    i = i+1
    print(parent[1])
    parent1_data = gps.data.10min[gps.data.10min$birdID==parent[1] & gps.data.10min$year==year,]
    if (dim(parent1_data)[1]>0) {
      #parent1_data$sex.parent = refdata_adults$animal_sex[refdata_adults$animal_local_identifier==parent[1]][1]
      chick_parent1_data <- link.chick.parent.data(chick_data, parent1_data)
      chick.parent.data.list[[i]] = chick_parent1_data
      }
    # link chick data to parent 2 data
    i = i+1
    print(parent[2])
    parent2_data = gps.data.10min[gps.data.10min$birdID==parent[2] & gps.data.10min$year==year,]
    if (dim(parent2_data)[1]>0) {
      #parent2_data$sex.parent = refdata_adults$animal_sex[refdata_adults$animal_local_identifier==parent[2]][1]
      chick_parent2_data <- link.chick.parent.data(chick_data, parent2_data)
      chick.parent.data.list[[i]] = chick_parent2_data
      }
    # determine nest location based on combined data of the two parents:
    parents_data = rbind(parent1_data, parent2_data)
    parents_data_nest <- parents_data[parents_data$datetime_CEST < min(chick_data$datetime.chick),]
    nest.location = determine.nest.location(parents_data_nest)
    nest.data[i-1,] = nest.location
    nest.data[i,] = nest.location
    print(nest.location)
    }
}

parent_offspring_nest_data <- cbind(parent_offspring_data, nest.data)

chick.parent.data <- from.list.to.df(chick.parent.data.list)

rm(chick.parent.data.list, parent_data, parent1_data, parent2_data, parents_data, gps.data.ref)

# add column with age of chick:
chick.biometrics <- read.csv('data/raw/chick.biometrics.csv')
chick.biometrics$start_deployment <- dmy(chick.biometrics$start_deployment)
chick.biometrics$age_deployment <- round(-log(-log(chick.biometrics$P8/247))/0.095 + 19.3,0)
chick.parent.data <- merge(chick.parent.data, chick.biometrics[,c('birdID','start_deployment','age_deployment')], by.x='birdID.chick', by.y='birdID')

# calculate age: 
chick.parent.data$age.chick <- yday(chick.parent.data$datetime.chick)-yday(chick.parent.data$start_deployment)+chick.parent.data$age_deployment
chick.parent.data$chick.parent <- paste(chick.parent.data$birdID.chick, chick.parent.data$birdID.parent, sep='-') 
chick.parent.data$sex.chick.parent <- paste(chick.parent.data$sex.chick, chick.parent.data$sex.parent, sep='-') 
chick.parent.data$yday <- yday(chick.parent.data$datetime.chick)

# calculate the distance of the chick from its colony/nest:
chick.nest = unique(parent_offspring_nest_data[,c('chickID','latitude','longitude')])
names(chick.nest)=c('chickID','lat.nest','lon.nest')
chick.parent.data = merge(chick.parent.data, chick.nest, by.x='birdID.chick', by.y='chickID', all.x=T)
chick.parent.data$dist.nest.chick = round(distGeo(as.matrix(chick.parent.data[,c('longitude.chick','latitude.chick')]),
                                         as.matrix(chick.parent.data[,c('lon.nest','lat.nest')])),0)
# order the data:
chick.parent.data <- chick.parent.data[order(chick.parent.data$birdID.chick, chick.parent.data$birdID.parent, chick.parent.data$datetime.chick),]

# calculate the time difference between the timestamp of the chick and the parent
chick.parent.data$timediff <- difftime(chick.parent.data$datetime.chick, chick.parent.data$datetime.parent, units="secs")
chick.parent.data$timediff.abs <- abs(chick.parent.data$timediff)
data.much.timediff = chick.parent.data[chick.parent.data$timediff.abs>1000,] # check which data had more than 10 minutes time difference
data.much.timediff$date <- date(data.much.timediff$datetime.chick)
unique(data.much.timediff[,c('date','birdID.chick','birdID.parent','datetime.chick','datetime.parent','timediff.abs')]) # the same datetime of the parent can be linked multiple times to different datetimes of the chick. 

# link departure data to the chick parent data
chick.parent.data.departure = merge(chick.parent.data, departure.dates.chick.parent[,c('chickID','parentID','departure.datetime.chick','interval.chick','departure.datetime.parent','interval.parent')], by.x=c('birdID.chick','birdID.parent'), by.y=c('chickID','parentID'), all.x=T)

# select data after departure of the chick, to see what is the minimum distance to the closest (in time) timestamp of the parent:
chick.parent.data.after.departure = chick.parent.data.departure[
  chick.parent.data.departure$datetime.chick > chick.parent.data.departure$departure.datetime.chick &
  chick.parent.data.departure$datetime.parent > chick.parent.data.departure$departure.datetime.parent,]

table(round(chick.parent.data.after.departure$distance/1000,1))[1:10] # only 4 cases where distance < 10 km. 
# all these 4 cases involved 6295-6288 on or before 30 September 2016. Among the other chick-parent pairs, the distance was always > 40 km: 
chick.parent.data.after.departure[which(chick.parent.data.after.departure$distance<50000),c('chick.parent','datetime.chick','timediff.abs','distance')]

# calculate contact moments:
chick.parent.data.departure$contact <- ifelse(chick.parent.data.departure$distance<10,1,0)
chick.parent.data.departure$freq <- 1
table(chick.parent.data.departure$chick.parent[chick.parent.data.departure$contact==1], chick.parent.data.departure$age.chick[chick.parent.data.departure$contact==1])
# last contact was on 142 d old, but this was the short contact between 6295 and 6288 in the Dutch Delta after departure.
# the last contact at the breeding grounds was at 136 d (also of 6295-6288)

# For analysis, only select data where the time difference between chick and parent is less than 10 minutes, OR selecting the period when the parent already departed while the chick did not (using the departure.dates.chick.parent information) or the chick had already departed while the parent had not, as - despite not having joined data with less than 10 minutes difference - it is certain that the chick was not in contact with the parent during this period. 
# As high resolution data of the chicks usually stops after the chick's departure, we remove the data after the chick's departure. 
# Add a date in the far future for chicks that never departed:
chick.parent.data.departure$departure.datetime.chick[is.na(chick.parent.data.departure$departure.datetime.chick)] = ymd('2099-12-31', tz='Europe/Amsterdam')
chick.parent.data.sel <- chick.parent.data.departure[which(chick.parent.data.departure$datetime.chick < chick.parent.data.departure$departure.datetime.chick &
                                                             (chick.parent.data.departure$timediff.abs < 600 | 
                              chick.parent.data.departure$datetime.chick > chick.parent.data.departure$departure.datetime.parent)),]

chick.parent.data.sel$sex.chick.parent <- factor(chick.parent.data.sel$sex.chick.parent, levels=c("f-f","m-f","f-m","m-m"))

# number of GPS locations per yday per parent x chick pair:
nlocs.pair.yday = table(chick.parent.data.sel$yday, chick.parent.data.sel$chick.parent)
#hist(nlocs.pair.yday[nlocs.pair.yday])
nlocs.pair.yday[nlocs.pair.yday>100]
df.nlocs.pair.yday = as.data.frame(nlocs.pair.yday)
names(df.nlocs.pair.yday) = c('yday','chick.parent','nlocs')

# data selection for analysis of contact data
chick.parent.data.contact <- merge(chick.parent.data.sel, df.nlocs.pair.yday) 
chick.parent.data.contact <- chick.parent.data.contact[chick.parent.data.contact$nlocs>=130,] # only select data with more than 130 linked locations per yday per chick-parent pair
# number of days with complete contact data per chick-parent pair:
ydays.chick.parent.pair = unique(chick.parent.data.contact[,c('chick.parent','yday')])
ydays.chick.parent.pair = as.data.frame(table(ydays.chick.parent.pair$chick.parent)) # only 2 complete days of data for 1607-6284.2.
names(ydays.chick.parent.pair)=c('chick.parent','ndays')

# only use data of chick-parent pairs with at least 10 days of nearly complete joint data.
chick.parent.data.contact = merge(chick.parent.data.contact, ydays.chick.parent.pair)
chick.parent.data.contact = chick.parent.data.contact[chick.parent.data.contact$ndays>10,]