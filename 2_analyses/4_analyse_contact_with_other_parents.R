# I checked the tracks of the parent birds during the month August per year to see which adults were using the same post-summer gathering places / kindergartens. 

# This was the case in 2016 for:
# 6291 (partner of 763), 6067 and 656 (with tracker 6282), who used the monding of the 4th gully
# 6284 & 6285, who gathered at Wad Willemsduin.

# Therefore, link the chick of 6291 (6304) to 6067 and 656
# the chick of 6067 (6302) to 6291 and 656
# and the chick of 656 (6296) to 6291 and 6067.
# the chick of 6284 (6299) to 6285 
# and the chick of 6285 (6301) to 6284

# In 2017, the four adults with properly working GPS-trackers (6298, 6066, 6287 and 6284) had really non-overlapping ranges in August. Moreover, in this year, there was hardly any contact between chicks and their own parents. Making a comparison with unrelated parents rather useless.
# In 2018, there were only three adults with relevant data in working trackers in August (6358, 6289 and 6291). Of those, 6291 and 6289 both regularly use Willemsduin Wad. 
# However, 6289 also spent part of her time south of her colony near the 3rd gully. Moreover, 6315 spent all of his time in and around the Westerplas during this period, having no contact with either 6289 nor 6291. 

# run the script to link chick and adult data again, but now for both unrelated adults and their own parent:
chick_parents_unrelated = data.frame(chick = c(6304,6304,6304,
                                                 6302,6302,6302,
                                                 6296,6296,6296,
                                                 6299,6299,
                                                 6301,6301),
                                       parent = c(6291,6067,656,
                                                  6291,6067,656,
                                                  6291,6067,656,
                                                  6284,6285,
                                                  6284,6285),
                                       real.parent = c(1,0,0,
                                                       0,1,0,
                                                       0,0,1,
                                                       1,0,
                                                       0,1)
                                       )

load("data/processed/chick.parent.data.sel.0418.RData")

# link data of chick to (unrelated) parents, based on the smallest time difference between their GPS fixes, and calculate the distance between them:
chick.parent.data.unrelated.list = NULL
year = 2016

for (i in 1:dim(chick_parents_unrelated)[1]) {
  print(i)
  chick = chick_parents_unrelated$chick[i]
  parent = chick_parents_unrelated$parent[i]
  real.parent = chick_parents_unrelated$real.parent[i]
  chick_data = gps.data.10min[gps.data.10min$birdID==chick & year(gps.data.10min$datetime_CEST)==year(gps.data.10min$start_deployment),]
  parent_data = gps.data.10min[gps.data.10min$birdID==parent & year(gps.data.10min$datetime_CEST)==year,]
  chick_parent_data <- link.chick.parent.data(chick_data, parent_data)
  chick_parent_data$real.parent <- real.parent
  chick.parent.data.unrelated.list[[i]] = chick_parent_data
}

chick.parent.data.unrelated <- from.list.to.df(chick.parent.data.unrelated.list)

# calculate time difference between chick and adult:
chick.parent.data.unrelated$timediff.abs <- abs(difftime(chick.parent.data.unrelated$datetime.chick, chick.parent.data.unrelated$datetime.parent, units="secs"))
# only select data where time difference was less than 600 s (= 10 minutes):
chick.parent.data.unrelated.sel <- chick.parent.data.unrelated[which(chick.parent.data.unrelated$timediff.abs < 600),]
                                                           
# determine contact (10 m):
chick.parent.data.unrelated.sel$contact <- 0
chick.parent.data.unrelated.sel$contact[chick.parent.data.unrelated.sel$distance<10] <- 1

# calculate the number of locations per day per chick-adult pair:
chick.parent.data.unrelated.sel$yday <- yday(chick.parent.data.unrelated.sel$datetime.chick)
chick.parent.data.unrelated.sel$chick.parent <- paste(chick.parent.data.unrelated.sel$birdID.chick, chick.parent.data.unrelated.sel$birdID.parent, sep='-')
nlocs.pair.yday = table(chick.parent.data.unrelated.sel$yday, chick.parent.data.unrelated.sel$chick.parent)
df.nlocs.pair.yday = as.data.frame(nlocs.pair.yday)
names(df.nlocs.pair.yday) = c('yday','chick.parent','nlocs')

# data selection for analysis of contact data
chick.parent.data.unrelated.contact <- merge(chick.parent.data.unrelated.sel, df.nlocs.pair.yday) 
chick.parent.data.unrelated.contact <- chick.parent.data.unrelated.contact[chick.parent.data.unrelated.contact$nlocs>=130,] # only select data with more than 130 linked locations per yday per chick-parent pair
chick.parent.data.unrelated.contact$freq=1

# select data in August:
chick.parent.data.unrelated.contact.August <- chick.parent.data.unrelated.contact[month(chick.parent.data.unrelated.contact$datetime.chick)==8,]
# calculate average proportion of contact between all chick-adult pairs:
contact.per.pair = aggregate(cbind(contact,freq)~chick.parent+real.parent, chick.parent.data.unrelated.contact.August, sum)
contact.per.pair$prop.contact <- contact.per.pair$contact / contact.per.pair$freq
contact.per.pair[order(contact.per.pair$chick.parent),]

# link this GPS-data to the behaviour of the chick from the 
load("data/processed/chick.parent.behav.habitat.data.0418.RData")
# add habitat.simple:
chick.parent.data.behav.sel$habitat.simple <- "unknown"
chick.parent.data.behav.sel$habitat.simple[chick.parent.data.behav.sel$habitat%in%c("LM_land","Schier_Kwelder","Schier_Land_Rest","wal_rest_land")] <- "land"
chick.parent.data.behav.sel$habitat.simple[chick.parent.data.behav.sel$habitat%in%c("LM_zoet","Schier_Zoet")] <- "freshwater"
chick.parent.data.behav.sel$habitat.simple[chick.parent.data.behav.sel$habitat%in%c("waddenzee","Schier_brak")] <- "marine"
chick.parent.data.behav.sel$habitat[is.na(chick.parent.data.behav.sel$habitat)]='unknown'

chick.parent.data.unrelated.contact.behav <- merge(chick.parent.data.unrelated.contact, unique(chick.parent.data.behav.sel[,c('birdID.chick','datetime.chick','habitat.simple','behaviour.chick')]))
chick.parent.data.unrelated.contact.behav$contact.foraging <- 0
chick.parent.data.unrelated.contact.behav$contact.foraging[chick.parent.data.unrelated.contact.behav$contact==1 & chick.parent.data.unrelated.contact.behav$behaviour.chick=='foraging' & chick.parent.data.unrelated.contact.behav$habitat.simple%in%c('freshwater','marine')] <- 1
chick.parent.data.unrelated.contact.behav$contact.begging <- 0
chick.parent.data.unrelated.contact.behav$contact.begging[chick.parent.data.unrelated.contact.behav$contact==1 & chick.parent.data.unrelated.contact.behav$behaviour.chick=='beg'] <- 1
chick.parent.data.unrelated.contact.behav.August <- chick.parent.data.unrelated.contact.behav[month(chick.parent.data.unrelated.contact.behav$datetime.chick)==8,]
contact.behav.per.pair = aggregate(cbind(contact,contact.begging,contact.foraging,freq)~chick.parent+real.parent, chick.parent.data.unrelated.contact.behav.August, sum)

contact.behav.per.pair$prop.contact = round(contact.behav.per.pair$contact / contact.behav.per.pair$freq,3)
contact.behav.per.pair$prop.contact.begging = round(contact.behav.per.pair$contact.begging / contact.behav.per.pair$freq,3)
contact.behav.per.pair$prop.contact.foraging = round(contact.behav.per.pair$contact.foraging / contact.behav.per.pair$freq,3)
contact.behav.per.pair[order(contact.behav.per.pair$chick.parent),c('chick.parent','real.parent','prop.contact','prop.contact.begging','prop.contact.foraging')]
