load("data/processed/gps.data.from.movebank.0327.RData")
source("functions.R")
parents <- unique(parent_offspring_data$parentID)
chicks <- unique(parent_offspring_data$chickID)

gps.data <- from.list.to.df(gps.data.list)

# keep relevant columns from gps data:
gps.data <- gps.data[,c('individual.local.identifier','tag.local.identifier','timestamp','location.lat','location.long','height.above.msl','ground.speed')]
names(gps.data) = c('birdID','tagID','datetime','latitude','longitude','altitude','ground.speed')
table(gps.data$birdID)

gps_refdata_juvs = refdata_juvs[refdata_juvs$sensor_type_id==653,]
gps_refdata_adults = refdata_adults[refdata_adults$sensor_type_id==653,]

gps_refdata_all <- na.omit(rbind(gps_refdata_juvs[,c('animal_local_identifier','tag_local_identifier','animal_nick_name','animal_sex','deploy_on_timestamp','deploy_off_timestamp','animal_life_stage')], gps_refdata_adults[,c('animal_local_identifier','tag_local_identifier','animal_nick_name','animal_sex','deploy_on_timestamp','deploy_off_timestamp','animal_life_stage')]))

names(gps_refdata_all) = c('birdID','tagID','colourcode','sex','start_deployment','end_deployment','ageclass')
gps_refdata_all$start_deployment <- ymd_hms(gps_refdata_all$start_deployment)
gps_refdata_all$end_deployment <- ymd_hms(gps_refdata_all$end_deployment)

# link gps reference data to gps data:
gps.data.ref <- merge(gps.data, gps_refdata_all, by=c('birdID','tagID'), all.x=T)

# remove data of the date of tracking:
gps.data.ref <- gps.data.ref[date(gps.data.ref$datetime)>gps.data.ref$start_deployment,]

# change time zone of datetime so that yday is determined on local time (CEST) instead of UTC.
gps.data.ref$datetime_CEST <- with_tz(gps.data.ref$datetime, tz="Europe/Amsterdam")
gps.data.ref$yday_CEST <- yday(gps.data.ref$datetime_CEST)
gps.data.ref$year <- year(gps.data.ref$datetime_CEST)

# select one location per 10 minutes:
gps.data.ref <- gps.data.ref %>%
  mutate(time_interval = floor_date(datetime_CEST, unit = "10 mins"))

gps.data.10min <- gps.data.ref %>%
  group_by(time_interval, year, birdID, yday_CEST) %>%
  slice(1)

# this still includes all GPS-tagged birds, from 2012-2023. And also lots of different GPS intervals (from 10 minutes to 1 hour, perhaps sometimes even longer intervals...) 

# determine autumn migration departure dates per bird per year:
# migration is defined as the first day during which the bird travelled >50 km southward of the colony. 
# to exclude (return flights after) northward (or east- or westward) exploration, we use the criterion distance.south>50 & distance.south.of.colony.start<50 & distance.south.of.colony.end>50
# determine for each bird and each year the departure date during autumn migration
birds = unique(gps.data.10min$birdID)
departure.dates = data.frame(birdID=NA, year=NA, datetime=NA, latitude=NA, longitude=NA, interval.avg=NA, sex=NA)
i=0
for (bird in birds) {
  gps.data.bird = gps.data.10min[gps.data.10min$birdID==bird,]
  sex.bird = gps.data.bird$sex[1]
  years = unique(gps.data.bird$year)
  for (yr in years) {
    gps.data.bird.year = gps.data.bird[gps.data.bird$year==yr,]
    gps.data.bird.year.autumn = gps.data.bird.year[month(gps.data.bird.year$datetime_CEST)%in%8:12,c('birdID','datetime_CEST','latitude','longitude')]
    if (dim(gps.data.bird.year.autumn)[1]>0) {
      departure.info <- determine.departure.info(gps.data.bird.year.autumn)
      if (!is.null(departure.info)) {
        print(departure.info)
        i=i+1
        departure.dates[i,] <- c(departure.info, sex.bird)
      }
    }
  }
}

departure.dates$datetime <- as.POSIXct(departure.dates$datetime, tz="Europe/Amsterdam", origin = "1970-01-01")
names(departure.dates)[3]='datetime_departure'
write.table(departure.dates, 'clipboard', sep='\t')

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
  # if there is only one parent GPS-tracked, then run the following line to retrieve parent data:
  if (length(parent)==1) {
    i = i+1
    print(parent)
    parent_data = gps.data.10min[gps.data.10min$birdID==parent,]
    # determine nest location based on parent_data in the 8 weeks before the first chick location:
    nest.location = determine.nest.location(parent_data)
    nest.data[i,] = nest.location
    chick_parent_data <- link.chick.parent.data(chick_data, parent_data)
    chick.parent.data.list[[i]] = chick_parent_data
    print(nest.data)
    }
  if (length(parent)==2) {
    # link chick data to parent 1 data
    i = i+1
    print(parent[1])
    parent1_data = gps.data.10min[gps.data.10min$birdID==parent[1],]
    parent1_data$sex.parent = refdata_adults$animal_sex[refdata_adults$animal_local_identifier==parent[1]][1]
    chick_parent1_data <- link.chick.parent.data(chick_data, parent1_data)
    chick.parent.data.list[[i]] = chick_parent1_data
    # link chick data to parent 2 data
    i = i+1
    print(parent[2])
    parent2_data = gps.data.10min[gps.data.10min$birdID==parent[2],]
    parent2_data$sex.parent = refdata_adults$animal_sex[refdata_adults$animal_local_identifier==parent[2]][1]
    chick_parent2_data <- link.chick.parent.data(chick_data, parent2_data)
    # determine nest location based on combined data of the two parents:
    parents_data = rbind(parent1_data, parent2_data)
    nest.location = determine.nest.location(parents_data)
    nest.data[i-1,] = nest.location
    nest.data[i,] = nest.location
    chick.parent.data.list[[i]] = chick_parent2_data
    print(nest.data)
  }
}

parent_offspring_nest_data <- cbind(parent_offspring_data, nest.data)

lapply(chick.parent.data.list, function(x) print(c(x$birdID.chick[1], x$birdID.parent[1]))) 

#save.image("data/processed/chick.parent.data.1008.RData")

chick.parent.data <- from.list.to.df(chick.parent.data.list)

rm(chick.parent.data.list, parent_data, parent1_data, parent2_data, parents_data, gps.data, gps.data.10min, gps.data.ref)

head(chick.parent.data)
table(chick.parent.data$birdID.chick[is.na(chick.parent.data$distance)], chick.parent.data$birdID.parent[is.na(chick.parent.data$distance)])
# distance=NA occurs when there are data of the chick but not of the parent. 

# add column with age of chick:
chick.biometrics <- read.csv('data/raw/bird.data.juvs.csv')
chick.biometrics$start_deployment <- dmy(chick.biometrics$start_deployment)
chick.biometrics$age_deployment <- round(-log(-log(chick.biometrics$P8/247))/0.095 + 19.3,0)
chick.parent.data <- merge(chick.parent.data, chick.biometrics[,c('birdID','start_deployment','age_deployment','birthyear')], by.x='birdID.chick', by.y='birdID')
head(chick.parent.data)

# calculate age: 
chick.parent.data$age.chick <- yday(chick.parent.data$datetime.chick)-yday(chick.parent.data$start_deployment)+chick.parent.data$age_deployment
chick.parent.data <- chick.parent.data[year(chick.parent.data$datetime.chick)==chick.parent.data$birthyear,] # only include data in the first calendar year of a bird
chick.parent.data$chick.parent <- paste(chick.parent.data$birdID.chick, chick.parent.data$birdID.parent, sep='-') 
chick.parent.data$sex.chick.parent <- paste(chick.parent.data$sex.chick, chick.parent.data$sex.parent, sep='-') 
chick.parent.data$yday <- yday(chick.parent.data$datetime.chick)
table(chick.parent.data$yday, chick.parent.data$chick.parent)

# calculate the distance of the chick from its colony/nest:
chick.nest = unique(parent_offspring_nest_data[,c('chickID','latitude','longitude')])
names(chick.nest)=c('chickID','lat.nest','lon.nest')
chick.parent.data = merge(chick.parent.data, chick.nest, by.x='birdID.chick', by.y='chickID', all.x=T)
chick.parent.data$dist.nest = round(distGeo(as.matrix(chick.parent.data[,c('longitude.chick','latitude.chick')]),
                                         as.matrix(chick.parent.data[,c('lon.nest','lat.nest')])),0)
# order the data:
chick.parent.data <- chick.parent.data[order(chick.parent.data$birdID.chick, chick.parent.data$birdID.parent, chick.parent.data$datetime.chick),]

# calculate the time difference between the timestamp of the chick and the parent
chick.parent.data$timediff <- difftime(chick.parent.data$datetime.chick, chick.parent.data$datetime.parent, units="secs")
chick.parent.data$timediff.abs <- abs(chick.parent.data$timediff)
data.much.timediff = chick.parent.data[chick.parent.data$timediff.abs>1000,] # check which data had more than 10 minutes time difference
data.much.timediff$date <- date(data.much.timediff$datetime.chick)
unique(data.much.timediff[,c('date','birdID.chick','birdID.parent','datetime.chick','datetime.parent')]) # the same datetime of the parent can be linked multiple times to different datetimes of the chick. 

# link departure data to the chick parent data
chick.parent.data.sel = merge(chick.parent.data, departure.dates.chick.parent[,c('chickID','parentID','departure.datetime.chick','interval.chick','departure.datetime.parent','interval.parent')], by.x=c('birdID.chick','birdID.parent'), by.y=c('chickID','parentID'), all.x=T)

# only select data where the time difference between chick and parent is less than 10 minutes, or selecting the period when the parent already departed while the chick did not (using the departure.dates.chick.parent information). The second criterion adds data when the parent never downloaded its data after its departure, while it is certain that the chick was not in contact with the parent during the period that the parent had already departed but the chick had not. 
# As it turns out that parents and chicks always left independently of their parents, we focus on the analysis where the chick is still at the breeding grounds (i.e. data prior to chick departure)
# Data points with more than 10 minutes difference should not be included in the analysis, as it is uncertain whether parent and chick were in contact.
chick.parent.data.sel <- chick.parent.data.sel[which(chick.parent.data.sel$timediff.abs < 600 | (chick.parent.data.sel$datetime.chick < date(chick.parent.data.sel$departure.datetime.parent) & chick.parent.data.sel$datetime.chick < date(chick.parent.data.sel$departure.datetime.chick))),]

chick.parent.data.sel$sex.chick.parent <- factor(chick.parent.data.sel$sex.chick.parent, levels=c("f-f","m-f","f-m","m-m"))
# which years?
table(year(chick.parent.data.sel$datetime.chick))
# which chick-parent pairs were used?
unique(chick.parent.data.sel[,c('chick.parent')])

# number of GPS locations per yday per parent x chick pair:
nlocs.pair.yday = table(chick.parent.data.sel$yday, chick.parent.data.sel$chick.parent)
hist(nlocs.pair.yday[nlocs.pair.yday>100])
nlocs.pair.yday[nlocs.pair.yday>100]
nlocs.pair.yday[nlocs.pair.yday>144] # how can there be more than 144 (6*24) points per yday?
df.nlocs.pair.yday = as.data.frame(nlocs.pair.yday)
names(df.nlocs.pair.yday) = c('yday','chick.parent','nlocs')
df.nlocs.pair.yday[df.nlocs.pair.yday$nlocs>144,] # check the cases with more than 144 points
check1 = chick.parent.data.sel[chick.parent.data.sel$yday==304 & chick.parent.data.sel$chick.parent=='6298.1-6118',]
check1$datediff = 0
check1$datediff[2:length(check1$datediff)] = check1$datetime.chick[2:length(check1$datediff)] - check1$datetime.chick[1:(length(check1$datediff)-1)]
table(round(check1$datediff,1)) # lots of datediffs <10, so adding up over an entire day, this gives more than 144 locations.
# only select data with at least 130 joint locations per day (this will kick out the migration part though where there is only GSM data for most chicks and some parents...)

# in the cases where timediff.abs>600 while the parent had already departed but the chick did not (automatically the case from the above selection), distance is manually set to 1000000:
chick.parent.data.sel$distance.corr <- chick.parent.data.sel$distance
chick.parent.data.sel$distance.corr[chick.parent.data.sel$timediff.abs >= 600] = 10000000
hist(as.numeric(chick.parent.data.sel$timediff)) # it occurs equally often that the datetime of the chick is later than of the parent and vice versa


# calculate contact moments:
chick.parent.data.sel$contact <- ifelse(chick.parent.data.sel$distance<10,1,0)
chick.parent.data.sel$freq <- 1

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