gps.data <- from.list.to.df(gps.data.list)
# keep relevant columns from gps data:
gps.data <- gps.data[,c('individual.local.identifier','tag.local.identifier','timestamp','location.lat','location.long','height.above.msl','ground.speed')]
names(gps.data) = c('birdID','tagID','date_time','latitude','longitude','altitude','ground.speed')
rm(gps.data.list)
gps.data$datetime_CEST <- with_tz(gps.data$date_time, tz="Europe/Amsterdam")
gps.data$year = year(gps.data$datetime_CEST)

# only select data in the year in which the chicks were born, and the months May - December, of parents and their chicks: 
# The last contact occurred in October as can be seen by running the script 3_determine_departure_dates_and_distance). 
parent.data = parent_offspring_data[,c('parentID','year')]
names(parent.data)[1] = c('birdID')
chick.data = parent_offspring_data[,c('chickID','year')]
names(chick.data)[1] = c('birdID')
birdyears = unique(rbind(parent.data, chick.data))

gps.data = merge(gps.data, birdyears)
gps.data = gps.data[month(gps.data$datetime_CEST)>4,]

gps_refdata_juvs = refdata_juvs[refdata_juvs$sensor_type_id==653,]
gps_refdata_adults = refdata_adults[refdata_adults$sensor_type_id==653,]

gps_refdata_all <- na.omit(rbind(gps_refdata_juvs[,c('animal_local_identifier','tag_local_identifier','animal_nick_name','animal_sex','deploy_on_timestamp','deploy_off_timestamp','animal_life_stage','animal_mortality_date')], gps_refdata_adults[,c('animal_local_identifier','tag_local_identifier','animal_nick_name','animal_sex','deploy_on_timestamp','deploy_off_timestamp','animal_life_stage','animal_mortality_date')]))

names(gps_refdata_all) = c('birdID','tagID','colourcode','sex','start_deployment','end_deployment','ageclass','mortality_date')
gps_refdata_all$start_deployment <- round_date(ymd_hms(gps_refdata_all$start_deployment), 'days')
gps_refdata_all$end_deployment <- round_date(ymd_hms(gps_refdata_all$end_deployment),'days')
gps_refdata_all$mortality_date <- ymd(gps_refdata_all$mortality_date)
gps_refdata_all$mortality_date[is.na(gps_refdata_all$mortality_date)] <- ymd('2099-12-31')
# only select relevant birds:
gps_refdata_sel = gps_refdata_all[gps_refdata_all$birdID%in%c(chicks, parents),]
write.csv(gps_refdata_sel[order(gps_refdata_sel$ageclass, gps_refdata_sel$start_deployment),c('birdID','tagID','colourcode','sex','ageclass','start_deployment','end_deployment','mortality_date')], 'data/raw/metadata.tagged.birds.csv', row.names=F)

# link gps reference data to gps data:
gps.data.ref <- merge(gps.data, gps_refdata_sel, by=c('birdID','tagID'), all.x=T)

# remove data of the date of tracking and data after the mortality date (of either chick or parent)
gps.data.ref <- gps.data.ref[date(gps.data.ref$date_time)>gps.data.ref$start_deployment & 
                               date(gps.data.ref$date_time)<=gps.data.ref$mortality_date,]

# select one location per 10 minutes:
gps.data.ref = gps.data.ref[order(gps.data.ref$birdID, gps.data.ref$datetime_CEST),]

gps.data.ref <- gps.data.ref %>%
  mutate(time_interval = floor_date(datetime_CEST, unit = "10 mins"))

gps.data.10min <- gps.data.ref %>%
  group_by(time_interval, birdID) %>%
  slice(1)

gps.data.10min = gps.data.10min[order(gps.data.10min$birdID, gps.data.10min$datetime_CEST),]
gps.data.10min[,c('latitude','longitude')] = round(gps.data.10min[,c('latitude','longitude')],6)
gps.data.10min$ground.speed = round(gps.data.10min$ground.speed,3)
gps.data.10min$datetime_CEST = format(gps.data.10min$datetime_CEST, "%Y-%m-%d %H:%M:%S")
gps.data.10min = gps.data.10min[,c('birdID','datetime_CEST','latitude','longitude','altitude','ground.speed','sex','ageclass')]
write.csv(gps.data.10min, 'data/raw/gps.data.csv', row.names = F)