#rm(list=ls())
load("data/processed/gps.behav.data.0327.RData")
gps.behav.data.list.original <- gps.behav.data.list
load("data/processed/gps.behav.beg.data.0527.RData")
gps.behav.data.list.with.beg <- gps.behav.data.list
rm(gps.data, gps.data.bird, gps.data.bird.year, gps.data.bird.year.autumn, gps.behav.data.list,
   chick_data, chick_parent_data, chick_parent1_data, chick_parent2_data)

source("functions.R")

# link contact data to the behaviour of the chick (and its parent)
# for this, first change date_time in behav datafile to local time
gps.behav.data.original <- from.list.to.df(gps.behav.data.list.original)
gps.behav.data.beg <- from.list.to.df(gps.behav.data.list.with.beg)
names(gps.behav.data.beg)[8]='behaviour.beg'
rm(gps.behav.data.list.original, gps.behav.data.list.with.beg)
# not sure why the two gps.behav.data files differ in length...
# this seems to be caused by the fact that the begging model uses segments of 0.8 s, which more often include an NA than segments of 0.4 s. 
# then we have to use the (slow) merge function to combine the two:
gps.behav.data = merge(gps.behav.data.original, gps.behav.data.beg[,c('date_time','birdID','behaviour.beg')], all.x=T)
rm(gps.behav.data.original, gps.behav.data.beg)
head(gps.behav.data)
# I checked the 763 case on 2016-06-01 00:01:54, where the original model estimated flying, and the beg-model resting. Looking at the raw ACC data, it looks like resting. Looking at the speed, it looks like flying. Bird was in the colony at this time, also the 10 min before and after. So most likely the bird was resting. The tracker of 763 was malfunctioning, so potentially remove these data...
gps.behav.data[is.na(gps.behav.data$behaviour.beg),] # I checked the first case (6067 on 2016-06-01 02:52:56, and there was an NA in the z-axis (the 16th measurement), with only 16 ACC-measurements. Therefore, the model didn't work on this segment of 0.8 s, while it did work when splitting the data into two segments of 0.4 s, but only for the first segment. 
gps.behav.data$datetime_CEST <- with_tz(gps.behav.data$date_time, tz="Europe/Amsterdam")
chick.parent.data.behav = merge(chick.parent.data.contact, gps.behav.data[,c('birdID','datetime_CEST','behaviour','behaviour.beg')], by.x=c('birdID.chick','datetime.chick'), by.y=c('birdID','datetime_CEST'), all.x=T)
head(chick.parent.data.behav)
names(chick.parent.data.behav)[which(names(chick.parent.data.behav)%in%c('behaviour','behaviour.beg'))] = c('behaviour.chick','behaviour.beg.chick')
# now determine behaviour of the parent
chick.parent.data.behav = merge(chick.parent.data.behav, gps.behav.data[,c('birdID','datetime_CEST','behaviour','behaviour.beg')], by.x=c('birdID.parent','datetime.parent'), by.y=c('birdID','datetime_CEST'), all.x=T)
head(chick.parent.data.behav)
names(chick.parent.data.behav)[which(names(chick.parent.data.behav)%in%c('behaviour','behaviour.beg'))] = c('behaviour.parent','behaviour.beg.parent')
chick.parent.data.behav$age5 = floor(chick.parent.data.behav$age.chick/5)*5
chick.parent.data.behav$freq = 1
rm(gps.behav.data)

ydays.bird = unique(chick.parent.data.behav[,c('tagID.parent','tagID.chick','birdID.parent','birdID.chick','yday','birthyear')])
table(paste(ydays.bird$tagID.parent, ydays.bird$birdID.parent)) # fill into Table S1
table(paste(ydays.bird$tagID.chick, ydays.bird$birdID.chick)) # fill into Table S1

table(is.na(chick.parent.data.behav$behaviour.chick)) # 36529 GPS locations for which no (ACC) data on behaviour of the chick was available.
chick.parent.data.behav.sel <- chick.parent.data.behav[!is.na(chick.parent.data.behav$behaviour.chick),]

# distinguish behaviour of chick when it was and was not in contact with its tracked parent:
chick.parent.data.behav.sel$behaviour.chick.contact = paste(chick.parent.data.behav.sel$behaviour.chick, chick.parent.data.behav.sel$contact, sep="")
chick.parent.data.behav.sel$behaviour.parent.contact = paste(chick.parent.data.behav.sel$behaviour.parent, chick.parent.data.behav.sel$contact, sep="")
# pool other and foraging behaviour, as walking and foraging are often confused, so we can't really make this distinction. Drinking rarely occurs, so hardly affects the plots.
chick.parent.data.behav.sel$behaviour.chick.contact[chick.parent.data.behav.sel$behaviour.chick.contact%in%c('foraging1','other1')] = 'active1'
chick.parent.data.behav.sel$behaviour.chick.contact[chick.parent.data.behav.sel$behaviour.chick.contact%in%c('foraging0','other0')] = 'active0'

### WHERE (IN WHICH HABITAT) DID PARENT AND CHICK HAVE CONTACT?

# load the habitat shapefile of Schiermonnikoog and surroundings, made by the RUG Geodienst:
schier_new84_sel <- read_sf(dsn = "data/raw/study_area_shapefile/study_area_shapefile.shp", crs="+proj=latlong +datum=WGS84")
schier_new84_sel$habitat <- as.character(schier_new84_sel$Habitat)
schier_new84_sel$habitat[is.na(schier_new84_sel$habitat)] <- "unknown"
schier_new84_sel$habitat[schier_new84_sel$habitat=="Wadgeulen_Diep"|schier_new84_sel$habitat=="Wadgeulen_Ondiep"|schier_new84_sel$habitat=="Wadplaten"|schier_new84_sel$habitat=="Wad_Kweldergeul_Brak"]="waddenzee" 
schier_new84_sel$habitat[schier_new84_sel$habitat=="Schier_Kweldergeul_Brak"|schier_new84_sel$habitat=="Schier_Brak_Rest"]="Schier_brak"
schier_new84_sel$habitat[schier_new84_sel$habitat=="Wal_Zoet_Ondiep"|schier_new84_sel$habitat=="Wal_Zoet_Diep"]="wal_rest_zoet"
schier_new84_sel$habitat[schier_new84_sel$habitat=="Wal_Kwelder"|schier_new84_sel$habitat=="Wal_Land_Rest"|schier_new84_sel$habitat=="Wal_Moeras"]="wal_rest_land"
schier_new84_sel$habitat[schier_new84_sel$habitat=="LG_Land_Rest"|schier_new84_sel$habitat=="LG_Moeras"]="LM_land"
schier_new84_sel$habitat[schier_new84_sel$habitat=="LG_Zoet_Ondiep"|schier_new84_sel$habitat=="LG_Zoet_Diep"]="LM_zoet"

# turn chick.parent.data.behav.sel into an sf object to link habitat:
chick.parent.data.sf <- st_as_sf(chick.parent.data.behav.sel, coords=c("longitude.chick","latitude.chick"), crs="+proj=latlong +datum=WGS84")

# overlay <- st_join(chick.parent.data.sf, schier_new84_sel, join=st_intersects) # this gives an error: "Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented, : Loop 0 is not valid: Edge 156 has duplicate vertex with edge 204" 
# this error can be avoided by using:
sf_use_s2(FALSE) 
overlay <- st_join(chick.parent.data.sf, schier_new84_sel, join=st_intersects)
dim(chick.parent.data.sf)
dim(overlay)
# from 172952 to 172964 rows... How is that possible? Are some locations associated with two polygons?

datacheck = table(paste(overlay$birdID.chick, overlay$datetime.chick, overlay$birdID.parent, overlay$datetime.parent))
datacheck[datacheck>1] # 4 rows with duplicate values
overlay[overlay$datetime.chick==ymd_hms("2016-07-19 21:09:24", tz="CEST"),]
# this is indeed caused by multiple polygons being associated with the same location... somehow... 

# is this solved with using largest=T?
overlay <- st_join(chick.parent.data.sf, schier_new84_sel, join=st_intersects, largest=T) # using largest=T takes MUCH longer
# but solves the issue!
dim(chick.parent.data.behav.sel)
dim(chick.parent.data.sf)
dim(overlay) # same # rows, just a few more columns (6 more than chick.parent.data.behav.sel, and 7 more than chick.parent.data.sf where columns latitude.chick and longitude.chick have been combined into 1 geometry column)
names(chick.parent.data.behav.sel)
names(chick.parent.data.sf)
names(overlay) # same # rows, just a few more columns (6 more than chick.parent.data.behav.sel, and 7 more than chick.parent.data.sf 
table(overlay$chick.parent, overlay$birthyear)

chick.parent.data.behav.sel = overlay
rm(overlay)
# still to check which versions of chick.parent.data are used in subsequent analyses... (and remove unused versions)