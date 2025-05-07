rm(list=ls())
source('functions.r')
# load the ringing data from the database:
con<-odbcConnectAccess2007("C:/Users/Tamar Lok/Documents/LepelaarDB/LepelaarDB_BACKUP_2024-12-19.accdb") 
ringings = sqlFetch(con, "Qy_CaptureOverview")
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
table(sightings_with_ringing_info$RingLocation)
sightings_juvs_autumn_migration = sightings_with_ringing_info[sightings_with_ringing_info$Ageclass=='pullus' & 
                                                                sightings_with_ringing_info$Year - year(sightings_with_ringing_info$RingDate)==0 & month(sightings_with_ringing_info$Date)%in%7:12 & sightings_with_ringing_info$Latitude<52 & sightings_with_ringing_info$Country!='The Netherlands' & sightings_with_ringing_info$Year>=1994,]
# 10,801 observations of juvenile birds during autumn migration of
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
feeding.obs = read.csv("data/raw/Feeding observations of colour-ringed chicks and parents 0107.csv", header=T)
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

# make ageclasses of 10 days, where age 40 includes the range of 35-44:
feeding.obs.pf$ChickAge10 <- floor((feeding.obs.pf$ChickAge+5)/10)*10
table(feeding.obs.pf$ChickAge, feeding.obs.pf$ChickAge10)
table(feeding.obs.pf$ChickAge10)

# calculate distance to the colony
feeding.obs.pf$distance.from.colony = round(geosphere::distCosine(
  matrix(c(feeding.obs.pf$Longitude, feeding.obs.pf$Latitude), ncol=2, byrow=F), 
  matrix(c(feeding.obs.pf$RingLongitude, feeding.obs.pf$RingLatitude), ncol=2, byrow=F))/1000,0)

# which years are included?
feeding.obs.pf$Date <- dmy(feeding.obs.pf$Date)
table(year(feeding.obs.pf$Date))
# which colonies? (roughly assessed from latitude)
table(feeding.obs.pf$RingLocation)
table(round(feeding.obs.pf$RingLatitude, 1)) # 19 feedings of chicks from the Delta (N<52), 11 from Onderdijk, Vooroever, 4 from Den Oever, 3 from Texel, 1 from Griend, 3 from Terschelling, and 139 (!!!) from Schiermonnikoog.    

# frequency table in relation to chick and parent sex
table(paste(feeding.obs.pf$SexChickSel, feeding.obs.pf$SexParentSel), feeding.obs.pf$ChickAge)
table(feeding.obs.pf$SexChickSel, feeding.obs.pf$SexParentSel) # chick sex in rows
table(feeding.obs.pf$SexChickMol, feeding.obs.pf$SexParentMol)
# now make these tables separately for Schier and elsewhere:
feeding.obs.pf$site = 'Elsewhere'
feeding.obs.pf$site[feeding.obs.pf$RingLocation=='Schiermonnikoog, Oosterkwelder'] = 'Schier'
table(feeding.obs.pf$SexChickSel, feeding.obs.pf$SexParentSel, feeding.obs.pf$site) # hardly any bird with known/observed sex outside Schier.

# make figure of # observed feeding events for all chicks combined (whether sex is known or unknown):
feeding.obs.pf$schier = ifelse(feeding.obs.pf$Location=='Schiermonnikoog, Oosterkwelder',1,0)

# statistical analysis of the number and distance from the colony of observed feedings.
# analyse number of feedings with poisson regression:
# to have large enough sample sizes, pool chick ages per 10 days:
# first test the effect of chick age while randomly selecting one observation per chick:
feeding.obs.pf$rnd <- sample((1:1000000)/1000000,dim(feeding.obs.pf)[1])
feeding.obs.pf$freq=1

# save this random selection to be able to repeat the analysis; the set.seed function appears not working for this.
#gdata::keep(feeding.obs.pf, bird.sex.overview, sure=T)
#save.image("data/processed/feeding.obs.pf.rnd.0116.RData") 
load("data/processed/feeding.obs.pf.rnd.0116.RData")

# how many chicks are involved in this dataset?
dim(feeding.obs.pf) # 184 observations on
chicks = unique(feeding.obs.pf[,c('Chick.code.1','SexChickSel')]) # 127 different chicks; 
table(chicks$SexChickSel) # 40 females, 46 males and 41 chicks with unknown sex
table(feeding.obs.pf$Parent.code)
parents = unique(feeding.obs.pf[,c('Parent.code','SexParentSel')]) # 78 different parents (where unringed parents are counted as 1 parent, so 77 colour-ringed parents)
table(parents$SexParentSel) # 23 females, 34 males and 17 with unknown sex (16 if the unringed birds are removed)

# select max rnd per chick:
rnd.max.chick = aggregate(rnd~Chick.code.1, feeding.obs.pf, max)
# merge with original file:
feeding.obs.pf.chick.uni = merge(feeding.obs.pf, rnd.max.chick) # 127 unique observations of known-age chicks. 
nfeeds.age10 = aggregate(freq~ChickAge10, feeding.obs.pf.chick.uni, sum)
m.nfeeds.age10 = glm(freq~ChickAge10, data=nfeeds.age10, family="poisson")
# Calculate overdispersion statistic
resid_dev <- sum(residuals(m.nfeeds.age10, type = "deviance")^2)
df_resid <- df.residual(m.nfeeds.age10)
dispersion_ratio <- resid_dev / df_resid
dispersion_ratio # underdispersion... no problem!
plot(fitted(m.nfeeds.age10)~residuals(m.nfeeds.age10))

summary(m.nfeeds.age10) # highly significant effect of chick age.
# effect of chick sex (while accounting for effect chick age):
# selecting only obs where chick sex is known
feeding.obs.chicksex.known <- feeding.obs.pf.chick.uni[feeding.obs.pf.chick.uni$SexChickSel%in%c('f','m'),]
dim(feeding.obs.chicksex.known) # N=86
table(feeding.obs.chicksex.known$SexChickSel)
nfeeds.sexc.age10 = aggregate(freq~ChickAge10+SexChickSel, feeding.obs.chicksex.known, sum)
m.nfeeds.sexc.age10 = glm(freq~ChickAge10+SexChickSel, data=nfeeds.sexc.age10, family="poisson")
summary(m.nfeeds.sexc.age10) # effect of chick sex is not significant.
# effect of parent sex (while accounting for effect chick age):
# randomly selecting one observation per sexed parent: 
# select max rnd per chick:
rnd.max.parent = aggregate(rnd~Parent.code, feeding.obs.pf, max)
rnd.max.parent = rnd.max.parent[rnd.max.parent$Parent.code!="",] # removing the unringed parents
# merge with original file:
feeding.obs.pf.parent.uni = merge(feeding.obs.pf, rnd.max.parent) # 78 unique observations of colour-ringed parents 
feeding.obs.parentsex.known <- feeding.obs.pf.parent.uni[feeding.obs.pf.parent.uni$SexParentSel%in%c('m','f'),] 
dim(feeding.obs.parentsex.known) # 57 unique obs of sexed parents
table(feeding.obs.parentsex.known$SexParentSel) # 23 by females, 34 by males.
table(feeding.obs.parentsex.known$site)
nfeeds.sexp.age10 = aggregate(freq~ChickAge10+SexParentSel, feeding.obs.parentsex.known, sum)
m.nfeeds.sexp.age10 = glm(freq~ChickAge10+SexParentSel, data=nfeeds.sexp.age10, family="poisson")
summary(m.nfeeds.sexp.age10) # effect of parent sex is N.S.
# when pooling all chick age classes:
nfeeds.sexp = aggregate(freq~SexParentSel, feeding.obs.parentsex.known, sum)
m.nfeeds.sexp = glm(freq~SexParentSel, data=nfeeds.sexp, family="poisson")
summary(m.nfeeds.sexp) # the sex effect is still not significant.

# Analysis of distance from the colony of feedings
feeding.obs.pf.chick.uni.naomit = na.omit(feeding.obs.pf.chick.uni[,c('ChickAge','SexParentSel','distance.from.colony')])
m.feeddist.age.sexp = lm(distance.from.colony~ChickAge+SexParentSel, feeding.obs.pf.chick.uni.naomit, na.action='na.fail') # this also includes the parents with sex='u'
# test model assumptions:
# 1. Linearity & homoscedasticity
plot(residuals(m.feeddist.age.sexp)~fitted(m.feeddist.age.sexp))
curve(0*x, lty='dashed', add=T)
ncvTest(m.feeddist.age.sexp)
# 2. Normality of residuals
plot(m.feeddist.age.sexp, which=2) # apart from 5 outliers, residuals are pretty normally distributed.
feeding.obs.pf.chick.uni.no.outliers = feeding.obs.pf.chick.uni.naomit[which(!rownames(feeding.obs.pf.chick.uni.naomit)%in%c('20','94','95','99','127')),]
m.feeddist.age.sexp.no.outliers = lm(distance.from.colony~ChickAge+SexParentSel, feeding.obs.pf.chick.uni.no.outliers, na.action='na.fail') # this also 
plot(m.feeddist.age.sexp.no.outliers, which=2) # apart from 5 outliers, residuals are pretty normally distributed.
shapiro.test(residuals(m.feeddist.age.sexp.no.outliers))

# does taking the log of distance.from.colony solve the issue?
feeding.obs.pf.chick.uni.nozero = feeding.obs.pf.chick.uni.naomit
feeding.obs.pf.chick.uni.nozero$distance.from.colony[feeding.obs.pf.chick.uni.nozero$distance.from.colony==0]=0.01
feeding.obs.pf.chick.uni.nozero$log.distance.from.colony = log(feeding.obs.pf.chick.uni.nozero$distance.from.colony)
m.feeddist.log.age.sexp = lm(log.distance.from.colony~ChickAge+SexParentSel, feeding.obs.pf.chick.uni.nozero, na.action='na.fail')
plot(m.feeddist.log.age.sexp, which=1)
plot(m.feeddist.log.age.sexp, which=2)
# this doesn't really improve the plots... 

# therefore, we continue with 'normal' linear models:
dredge(m.feeddist.age.sexp)
# when only using data with known sexes:
feeding.obs.pf.chick.uni.knownsex = feeding.obs.pf.chick.uni.naomit[feeding.obs.pf.chick.uni.naomit$SexParentSel!='u',]
m.feeddist.age.sexp = lm(distance.from.colony~ChickAge+SexParentSel, feeding.obs.pf.chick.uni.knownsex, na.action='na.fail') # this also includes the parents with sex='u'
dredge(m.feeddist.age.sexp) # still no support for a sex effect.
m.feeddist.parsim = lm(distance.from.colony~ChickAge, feeding.obs.pf.chick.uni.naomit, na.action='na.fail')
summary(m.feeddist.parsim)$r.squared
anova(m.feeddist.parsim)

# Perhaps a logistic curve gives a better fit? 
logistic_model <- nls(distance.from.colony~ L / (1 + exp(-k * (ChickAge - x0))), feeding.obs.pf.chick.uni.naomit, start = list(L = 30, k = 0.1, x0 = 80))
AIC(logistic_model, m.feeddist.parsim) # not really