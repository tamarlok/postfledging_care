rm(list=ls())
source('functions.r')
# load the ringing data from the database:
con<-odbcConnectAccess2007("C:/Users/Tamar Lok/Documents/LepelaarDB/LepelaarDB_linked.accdb") # based on version 2024-12-19.
ringings = sqlFetch(con, "Qy_CaptureOverview_NL")
sightings_raw = sqlFetch(con, "tbl Sightings")
sightings_with_info = sqlFetch(con, "Qy_SightingOverview_without_capture_info") # sightings with info about the sightings, not about the captures of the birds involved
# odbcClose(con)
# sightings_raw and sightings_with_info should have the same number of rows.
# if this is not the case, then check which occasions/sightings have missing data in certain required fields:
# as the query uses 'group by' it could also be caused by double entries of sightings
missing_sightings = sightings_raw[which(!sightings_raw$SightingID%in%sightings_with_info$SightingID),]
missing_sightings

# retrieve the first ringing event per bird (which is either as chick or adult), and, for birds with ringloss, select the complete code: 
# first select the latest ringing event per individual per year (for GPS-tags, individuals are sometimes recaptured)
ringings$YearRinged = year(ringings$FldDate)
ringings_latest = aggregate(FldDate~BirdID+YearRinged, ringings, max)
# for birds recaptured at different dates within a season, this selects the latest capture:
# however, in some cases (mostly for birds with ringloss) there have been two ringing events registered on the same date:
# although one row is retained per bird, when merging it with the original ringings file, the two rows are returned:
ringings_uni1 = merge(ringings, ringings_latest)
# for these cases, select the row with the longest ColourCode (i.e. the code without ringloss), as this row likely contains the most complete information for the other columns 
ringings_uni1$code_length = nchar(ringings_uni1$ColourCode)
ringings_longest_code = aggregate(code_length~BirdID+FldDate, ringings_uni1, max)
ringings_uni2 = merge(ringings_uni1, ringings_longest_code)
# ringings_uni2 still doesn't have the same length as ringings_latest
ringings_uni2$freq=1
check_doubles = aggregate(freq~BirdID+YearRinged, ringings_uni2, sum)
doubles = check_doubles$BirdID[check_doubles$freq>1]
ringings_uni2[ringings_uni2$BirdID%in%doubles,]
# these are all cases with potential mirrored colourcodes, and doubly used codes. These will be removed for analysis.
# but to get the number of ringed individuals right (even if codes are mirrored or used double), select for these individuals the lowest CaptureID:
ringings_first_captureID = aggregate(CaptureID~BirdID+YearRinged, ringings_uni2, min)
ringings_uni3 = merge(ringings_uni2, ringings_first_captureID) # YES, now ringings_latest has the same length as ringings_uni3.
# but it has multiple rows for birds that are recaptured in different years (also birds with ringloss for which this is registered/reported in a later year than the ringing year)

# remove double used and potentially mirrored ringcodes and select birds from The Netherlands:
ringings_sel = ringings_uni3[ringings_uni3$SuspRingCode<2 & ringings_uni3$Country=="The Netherlands",]
ringings_sel <- ringings_sel[is.na(ringings_sel$Latitude)==F,c('BirdID','ColourCode','Ageclass','FldDate','Location','Latitude','Longitude','SexMol','8thPrimary')] # this removes the fake ringing sessions of UFO birds
ringings_sel$SexMol[is.na(ringings_sel$SexMol)]="u"
ringings.first = aggregate(FldDate~BirdID, data=ringings_sel, min)
bird.minsex = aggregate(SexMol~BirdID, data=ringings_sel, min) # selecting m or f over u
# link first ringing event to minimum molsex
ringings.first = merge(ringings.first, bird.minsex)
# merge the ageclass and P8 at ringings.first:
ringings.first = merge(ringings.first, unique(ringings_sel[,c('BirdID','ColourCode','FldDate','Location','Latitude','Longitude','Ageclass','8thPrimary')]), all.x=T)
names(ringings.first)[c(2,5:7,9)]=c('RingDate','RingLocation','RingLatitude','RingLongitude','P8')
head(ringings.first)
BirdID.ColourCode = unique(ringings.first[,c('BirdID','ColourCode')]) # first registered colourcode per bird. 

# link ringing data to sighting data to be able to filter the total number of observations during autumn migration of juveniles (i.e. south of The Netherlands between July and December at age=0)
sightings_with_ringing_info = merge(sightings_with_info, ringings.first[,c('BirdID','RingDate','RingLocation','Ageclass')])
sightings_with_ringing_info$Year <- year(sightings_with_ringing_info$Date)
table(sightings_with_ringing_info$RingLocation)
sightings_juvs_autumn_migration = sightings_with_ringing_info[sightings_with_ringing_info$Ageclass=='pullus' & 
                                                                sightings_with_ringing_info$Year - year(sightings_with_ringing_info$RingDate)==0 & month(sightings_with_ringing_info$Date)%in%7:12 & sightings_with_ringing_info$Latitude<52 & sightings_with_ringing_info$Country!='The Netherlands' & sightings_with_ringing_info$Year>=1994,]
# >10,000 observations of juvenile birds during autumn migration of
length(unique(sightings_juvs_autumn_migration$BirdID))
min(na.omit(sightings_juvs_autumn_migration$Date))
max(na.omit(sightings_juvs_autumn_migration$Date))

# filter observation data on sex information: 
table(sightings_with_info$SexObs)
sex_sightings = sightings_with_info[sightings_with_info$SexObs!='u',]
sex_sightings = sex_sightings[,c('BirdID','ColourCode','SexObs','Sex certainty','Remarks')]

bird.sex.overview = table(sex_sightings$BirdID, paste(sex_sightings$SexObs, sex_sightings$`Sex certainty`,sep=""))
bird.sex.overview[order(bird.sex.overview[,'f1'], decreasing = T),] # birds have never been seen copulating as female AND as male
bird.sex.overview[order(bird.sex.overview[,'m1'], decreasing = T),] # birds have never been seen copulating as female AND as male
# however, bird 874 has been seen copulating as male, but was 23 times visually sexed as a female!
sex_sightings[which(sex_sightings$BirdID==874),]
# this indeed seems to be a case of a woman pretending (or wanting) to be a man...
# so copulation is not 100% certain!
# however, we will label copulation, clear difference when seen as pair, having a partner of known sex and seen incubating at night, as certain sex determinations, and all others (including unknown methods) as uncertain.
class(matrix(bird.sex.overview))
bird.sex.overview = as.data.frame(matrix(bird.sex.overview, nrow=dim(bird.sex.overview)[1], dimnames = dimnames(bird.sex.overview)))
bird.sex.overview$fc =  bird.sex.overview$f1+bird.sex.overview$f2+bird.sex.overview$f6+bird.sex.overview$f8
bird.sex.overview$fu =  bird.sex.overview$f3+bird.sex.overview$f4+bird.sex.overview$f5+bird.sex.overview$f7+bird.sex.overview$fNA
bird.sex.overview$mc =  bird.sex.overview$m1+bird.sex.overview$m2+bird.sex.overview$m6+bird.sex.overview$m8
bird.sex.overview$mu =  bird.sex.overview$m3+bird.sex.overview$m4+bird.sex.overview$m5+bird.sex.overview$m7+bird.sex.overview$mNA
# sex determined:
bird.sex.overview$sexobs = "u"
bird.sex.overview$sexobs[bird.sex.overview$fc>0 & bird.sex.overview$mc==0] = 'f'
bird.sex.overview$sexobs[bird.sex.overview$mc>0 & bird.sex.overview$fc==0] = 'm'
bird.sex.overview$sexobs[(bird.sex.overview$mc+bird.sex.overview$mu) - (bird.sex.overview$fc+bird.sex.overview$fu) > 2] = 'm'
bird.sex.overview$sexobs[(bird.sex.overview$fc+bird.sex.overview$fu) - (bird.sex.overview$mc+bird.sex.overview$mu) > 2] = 'f'
bird.sex.overview$BirdID = rownames(bird.sex.overview)

# link observed sex to ringing data:
ringings.first.sexobs = merge(ringings.first, bird.sex.overview[,c('BirdID','sexobs')], all.x=T)
ringings.first.sexobs$sexobs[is.na(ringings.first.sexobs$sexobs)]='u'
ringings.first.sexobs.sexknown = ringings.first.sexobs[ringings.first.sexobs$sexobs!='u'&ringings.first.sexobs$SexMol!='u',]
dim(ringings.first.sexobs.sexknown)
birds.different.sexobs.sexmol = ringings.first.sexobs.sexknown[which(ringings.first.sexobs.sexknown$sexobs!=ringings.first.sexobs.sexknown$SexMol),] # for 6 of the 54 birds, visually determined sex did not match the molecularly determined sex: 
birds.different.sexobs.sexmol
# interestingly, 5 of the 6 cases are molecularly sexed birds from 1999-2001... 
# Check these cases:
bird.sex.overview[bird.sex.overview$BirdID%in%birds.different.sexobs.sexmol$BirdID,]
# the only case in recent years is a bird that was observed once as a female in a copulation, but this is also not 100% certain...
# However, for now we keep the sexmol as the true sex, and if not available, then we use the sexobs.
ringings.first.sexobs$sexsel = "u"
ringings.first.sexobs$sexsel[ringings.first.sexobs$sexobs!='u'] = ringings.first.sexobs$sexobs[ringings.first.sexobs$sexobs!='u']
ringings.first.sexobs$sexsel[ringings.first.sexobs$SexMol!='u'] = ringings.first.sexobs$SexMol[ringings.first.sexobs$SexMol!='u'] 

table(ringings.first.sexobs$sexsel)
# link unique ringing first data and sexsel data with birdID-colourcode combinations:
ringings.first.sexobs.code = merge(ringings.first.sexobs, BirdID.ColourCode)
check.sex.table = table(ringings.first.sexobs.code$ColourCode, ringings.first.sexobs.code$sexsel)
check.sex.table = matrix(check.sex.table, dimnames=dimnames(check.sex.table), nrow=dim(check.sex.table)[1])
check.sex.table[rowSums(check.sex.table)>1,]
check.sex.table[1:2,] # only for -/a there are double entries (multiple birds with this code after ringloss)

# open file
feeding.obs = read.csv("data/raw/feeding.observations.csv", header=T)
# fill an adult age for birds ringed as adult:
feeding.obs$Age[feeding.obs$Ageclass=='adult'] = 99

# clean data: the column ColourCode contains either the chick or the parent, which is given by the column Age. 
feeding.obs$Parent.code[feeding.obs$Parent.code=="" & feeding.obs$Age>0] <- feeding.obs$ColourCode[feeding.obs$Parent.code=="" & feeding.obs$Age>0]
feeding.obs$Chick.code.1[feeding.obs$Chick.code.1=="" & feeding.obs$Age==0] <- feeding.obs$ColourCode[feeding.obs$Chick.code.1=="" & feeding.obs$Age==0]

# Only select certain observations and relevant columns:
feeding.obs <- unique(feeding.obs[feeding.obs$Certainty=="sure",c('Parent.code','Chick.code.1','Chick.code.2','Date','Location','Latitude','Longitude')]) # use only one feeding observation per chick per location per day

# link feeding data with ringing data:
feeding.obs = merge(feeding.obs, ringings.first.sexobs.code[,c('ColourCode','sexsel','SexMol')], by.x='Parent.code', by.y='ColourCode', all.x=T)
names(feeding.obs)[8:9]=c('SexParentSel','SexParentMol')
# SexParent=NA when the code of the parent is used multiple times or when the parent is from a different country.
feeding.obs = unique(feeding.obs)

feeding.obs = merge(feeding.obs[,1:9], ringings.first.sexobs.code[,c('ColourCode','sexsel','SexMol','RingDate','RingLocation','RingLatitude','RingLongitude','P8')], by.x='Chick.code.1', by.y='ColourCode', all.x=T)
names(feeding.obs)[10:11]=c('SexChickSel','SexChickMol')
head(feeding.obs)

feeding.obs$ChickRingAge = round(-log(-log(feeding.obs$P8/247))/0.095 + 19.3, 0)
feeding.obs$DaysSinceRinging = round(difftime(dmy(feeding.obs$Date),feeding.obs$RingDate,units='days'),0)
feeding.obs$ChickAge = as.numeric(feeding.obs$ChickRingAge + feeding.obs$DaysSinceRinging)
# check the most extreme case:
feeding.obs[which(feeding.obs$ChickAge>120),] # This was still in The Netherlands.

# start post-fledging period at 35 days
# this automatically only selects data where the chick is colour-ringed, so that chick age and chick colony location are known.
feeding.obs.pf = feeding.obs[which(feeding.obs$ChickAge>=35),]
dim(feeding.obs.pf)[1]

# for plotting, keep one observation per chick-parent pair per date:
names(feeding.obs.pf)
feeding.obs.pf <- unique(feeding.obs.pf[,c('Chick.code.1','Parent.code','Date','SexParentSel','SexParentMol','SexChickSel','SexChickMol','ChickAge','Location','Latitude','Longitude','RingLocation','RingLatitude','RingLongitude')])
names(feeding.obs.pf)[1]='Chick.code'
feeding.obs.pf$Date <- dmy(feeding.obs.pf$Date)

# select one random observation per chick for statistical analysis (for now, only create the column so that results can be reproduced):
feeding.obs.pf$rnd <- sample((1:1000000)/1000000,dim(feeding.obs.pf)[1])


write.csv(feeding.obs.pf, 'data/raw/feeding.observations.with.sex.info.csv', row.names=F)