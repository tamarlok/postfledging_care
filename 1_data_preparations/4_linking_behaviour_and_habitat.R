### LINKING BEHAVIOUR OF CHICKS AND PARENTS:
gps.behav.data <- from.list.to.df(gps.behav.data.list)
rm(gps.behav.data.list)

write.csv(gps.behav.data, 'data/processed/gps.behav.data.csv', row.names = F)
gps.behav.data <- read.csv('data/processed/gps.behav.data.csv', row.names = 'X')

gps.behav.data$datetime_CEST <- with_tz(gps.behav.data$date_time, tz="Europe/Amsterdam")
chick.parent.data.behav = merge(chick.parent.data.contact, gps.behav.data[,c('birdID','datetime_CEST','behaviour')], by.x=c('birdID.chick','datetime.chick'), by.y=c('birdID','datetime_CEST'), all.x=T)
names(chick.parent.data.behav)[names(chick.parent.data.behav)=='behaviour'] = 'behaviour.chick'
# now determine behaviour of the parent
chick.parent.data.behav = merge(chick.parent.data.behav, gps.behav.data[,c('birdID','datetime_CEST','behaviour')], by.x=c('birdID.parent','datetime.parent'), by.y=c('birdID','datetime_CEST'), all.x=T)
names(chick.parent.data.behav)[names(chick.parent.data.behav)=='behaviour'] = 'behaviour.parent'
chick.parent.data.behav$age5 = floor(chick.parent.data.behav$age.chick/5)*5
chick.parent.data.behav$freq = 1
rm(gps.behav.data)

ydays.bird = unique(chick.parent.data.behav[,c('tagID.parent','tagID.chick','birdID.parent','birdID.chick','yday','birthyear')])

table(is.na(chick.parent.data.behav$behaviour.chick)) # 44437 GPS locations for which no (ACC) data on behaviour of the chick was available.
chick.parent.data.behav.sel <- chick.parent.data.behav[!is.na(chick.parent.data.behav$behaviour.chick),]

table(chick.parent.data.behav.sel$chick.parent, chick.parent.data.behav.sel$yday) # quite often missing ACC samples, or ACC-samples < 32 points 
table(chick.parent.data.contact$chick.parent, chick.parent.data.contact$yday)

# distinguish behaviour of chick when it was and was not in contact with its tracked parent:
chick.parent.data.behav.sel$behaviour.chick.contact = paste(chick.parent.data.behav.sel$behaviour.chick, chick.parent.data.behav.sel$contact, sep="")
chick.parent.data.behav.sel$behaviour.parent.contact = paste(chick.parent.data.behav.sel$behaviour.parent, chick.parent.data.behav.sel$contact, sep="")

### LINKING HABITAT OF CHICK

# load the habitat shapefile of Schiermonnikoog and surroundings, made by the RUG Geodienst:
schier_new84_sel <- read_sf(dsn = "data/raw/study_area_shapefile/study_area_shapefile.shp", crs="+proj=latlong +datum=WGS84") # file available at https://doi.org/10.5281/zenodo.10804302
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
sf_use_s2(FALSE)
chick.parent.data.behav.sel <- st_join(chick.parent.data.sf, schier_new84_sel, join=st_intersects, largest=T) # using largest=T makes sure that polygons are not overlapping
chick.parent.data.behav.sel$habitat.chick = chick.parent.data.behav.sel$habitat

# number of GPS locations with behavioural classification of the chick per yday per parent x chick pair:
nlocs.behav.pair.yday = table(chick.parent.data.behav.sel$yday, chick.parent.data.behav.sel$chick.parent)
table(nlocs.behav.pair.yday)
df.nlocs.behav.pair.yday = as.data.frame(nlocs.behav.pair.yday)
names(df.nlocs.behav.pair.yday) = c('yday','chick.parent','nlocs.behav')
# data selection for analysis of behaviour-specific contact data:
chick.parent.data.behav.sel <- merge(chick.parent.data.behav.sel, df.nlocs.behav.pair.yday)
chick.parent.data.behav.sel <- chick.parent.data.behav.sel[chick.parent.data.behav.sel$nlocs.behav>=100,]
