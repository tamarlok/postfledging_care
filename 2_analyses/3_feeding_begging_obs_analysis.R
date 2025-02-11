#rm(list=ls())
# load the ringing data from the database:
con<-odbcConnectAccess2007("C:/Users/Tamar Lok/Documents/LepelaarDB/LepelaarDB_linked.accdb")
ringings = sqlFetch(con, "Qy_CaptureOverview_all")
sightings = sqlFetch(con, "tbl Sightings")
sexcertainty = sqlFetch(con, "lkp Sex certainty")
sex = sqlFetch(con, "tbl Sex")

# retrieve the first ringing event per bird (which is either as chick or adult) 
# remove double used ringcodes and birds from The Netherlands:
ringings = ringings[ringings$SuspRingCode<2 & ringings$Country=="The Netherlands",]
ringings$SexMol[is.na(ringings$SexMol)]="u"
BirdID.ColourCode = unique(ringings[,c('BirdID','ColourCode')]) # retrieve all codes for a given BirdID (potentially multiple entries per bird when a bird lost rings or received a new colourcode combination)
ringings <- ringings[is.na(ringings$Latitude)==F,c('BirdID','ColourCode','Ageclass','FldDate','Location','Latitude','Longitude','SexMol','8thPrimary')] # this removes the fake ringing sessions of UFO birds
table(is.na(ringings$Location))
ringings.first = aggregate(FldDate~BirdID, data=ringings, min)
bird.minsex = aggregate(SexMol~BirdID, data=ringings, min)
# link first ringing event to minimum molsex (which is f or m instead of u)
ringings.first = merge(ringings.first, bird.minsex)
# merge the ageclass and P8 at ringings.first:
ringings.first = merge(ringings.first, unique(ringings[,c('BirdID','FldDate','Location','Latitude','Longitude','Ageclass','8thPrimary')]), all.x=T)
names(ringings.first)[c(2,4:6,8)]=c('RingDate','RingLocation','RingLatitude','RingLongitude','P8')
head(ringings.first)

# filter observation data on sex information: 
sex_sightings = sightings[sightings$`Sex/Type RUG`!=0,]
table(sex_sightings$`Sex/Type RUG`)
sex_sightings$sex = 'f'
sex_sightings$sex[sex_sightings$`Sex/Type RUG`==2] = 'm'
sex_sightings = sex_sightings[,c('BirdID','ColourCode','sex','Sex certainty','Remarks')]

bird.sex.overview = table(sex_sightings$BirdID, paste(sex_sightings$sex, sex_sightings$`Sex certainty`,sep=""))
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
# there are six cases in which the observed sex does not match the molecular sex. This is for BirdID's 2752, 2782 and 16038 (observed as female, but molsex=male) and 2443, 2972 and 3025 (observed as male, but molsex=female):
ringings.first.sexobs[which(ringings.first.sexobs$sexobs!=ringings.first.sexobs$SexMol & ringings.first.sexobs$SexMol!='u' & ringings.first.sexobs$sexobs!='u'),]
# interestingly, 5 of the 6 cases are molecularly sexed birds from 1999-2001... 
# Check these cases:
bird.sex.overview[bird.sex.overview$BirdID%in%c(2752, 2782, 16038, 2443, 2972, 3025),]
# the only case in recent years is a bird that was observed once as a female in a copulation, but this is also not 100% certain...
# However, for now we keep the sexmol as the true sex, and if not available, then we use the sexobs.
ringings.first.sexobs$sexsel = ringings.first.sexobs$SexMol
ringings.first.sexobs$sexsel[ringings.first.sexobs$sexsel=='u'] = ringings.first.sexobs$sexobs[ringings.first.sexobs$sexsel=='u']

# link unique ringing first data and sexsel data with birdID-colourcode combinations:
ringings.first.sexobs.code = merge(ringings.first.sexobs, BirdID.ColourCode) # now multiple entries for some birds (with ringloss or re-ringed)
check.sex.table = table(ringings.first.sexobs.code$ColourCode, ringings.first.sexobs.code$sexsel)
check.sex.table = matrix(check.sex.table, dimnames=dimnames(check.sex.table), nrow=dim(check.sex.table)[1])
check.sex.table[rowSums(check.sex.table)>1,] # only for -/a there are double entries (multiple birds with this code after ringloss)

# open file
feeding.obs = read.csv("data/raw/Feeding observations of colour-ringed chicks and parents.csv", header=T)
feeding.obs$behav.cat = 'feeding'
begging.obs = read.csv("data/raw/Begging_observations_lepelaarDB.csv", header=T)
begging.obs$behav.cat = 'begging'

# Combine the feeding and begging data:
feeding.obs.names = names(feeding.obs)
begging.obs.names = names(begging.obs)
matching.names = feeding.obs.names[feeding.obs.names%in%begging.obs.names]
feeding.obs = feeding.obs[,matching.names]
begging.obs = begging.obs[,matching.names]
feed.beg.obs = rbind(feeding.obs, begging.obs)

# fill an adult age for birds ringed as adult:
feed.beg.obs$Age[feed.beg.obs$Ageclass=='adult'] = 99

# clean data: the column ColourCode contains either the chick or the parent, which is given by the column Age. 
feed.beg.obs$Parent.code[feed.beg.obs$Parent.code=="" & feed.beg.obs$Age>0] <- feed.beg.obs$ColourCode[feed.beg.obs$Parent.code=="" & feed.beg.obs$Age>0]
feed.beg.obs$Chick.code.1[feed.beg.obs$Chick.code.1=="" & feed.beg.obs$Age==0] <- feed.beg.obs$ColourCode[feed.beg.obs$Chick.code.1=="" & feed.beg.obs$Age==0]

# Only select certain observations and relevant columns:
feed.beg.obs <- unique(feed.beg.obs[feed.beg.obs$Certainty=="sure",c('Parent.code','Chick.code.1','Chick.code.2','Date','Location','Latitude','Longitude','behav.cat')])

# link feeding data with ringing data:
feed.beg.obs = merge(feed.beg.obs, ringings.first.sexobs.code[,c('ColourCode','sexsel','SexMol')], by.x='Parent.code', by.y='ColourCode', all.x=T)
names(feed.beg.obs)[9:10]=c('SexParentSel','SexParentMol')
# SexParent=NA when the code of the parent is used multiple times or when the parent is from a different country.
feed.beg.obs = unique(feed.beg.obs)

feed.beg.obs = merge(feed.beg.obs[,1:10], ringings.first.sexobs.code[,c('ColourCode','sexsel','SexMol','RingDate','RingLocation','RingLatitude','RingLongitude','P8')], by.x='Chick.code.1', by.y='ColourCode', all.x=T)
names(feed.beg.obs)[11:12]=c('SexChickSel','SexChickMol')
head(feed.beg.obs)

feed.beg.obs$ChickRingAge = round(-log(-log(feed.beg.obs$P8/247))/0.095 + 19.3, 0)
feed.beg.obs$DaysSinceRinging = round(difftime(dmy(feed.beg.obs$Date),feed.beg.obs$RingDate,units='days'),0)
feed.beg.obs$ChickAge = as.numeric(feed.beg.obs$ChickRingAge + feed.beg.obs$DaysSinceRinging)
# check the most extreme case:
feed.beg.obs[which(feed.beg.obs$ChickAge>120),] # This was still in The Netherlands.

# make ageclasses of 10 days
feed.beg.obs$ChickAge10 <- floor(feed.beg.obs$ChickAge/10)*10
# start post-fledging period at 30 days
# this automatically only selects data where the chick is colour-ringed, so that chick age and chick colony location are known.
feed.beg.obs.pf = feed.beg.obs[which(feed.beg.obs$ChickAge10>30),]
dim(feed.beg.obs.pf)[1]

# which years are included?
feed.beg.obs.pf$Date <- dmy(feed.beg.obs.pf$Date)
table(year(feed.beg.obs.pf$Date))
# which colonies? (roughly assessed from latitude)
table(feed.beg.obs.pf$RingLocation)
table(round(feed.beg.obs.pf$RingLatitude, 1)) # 20 feedings of chicks from the Delta, 15 from Onderdijk, Vooroever, 5 from Den Oever, 3 from Texel, 1 from Griend, 4 from Terschelling, and 119 from Schiermonnikoog.    

# frequency table in relation to chick and parent sex
table(paste(feed.beg.obs$SexChickSel, feed.beg.obs$SexParentSel), feed.beg.obs$ChickAge)
table(feed.beg.obs$SexParentSel)
table(feed.beg.obs$SexChickSel, feed.beg.obs$SexParentSel)
table(feed.beg.obs$SexChickMol, feed.beg.obs$SexParentMol)
# now make these tables separately for schier and elsewhere:
feed.beg.obs$site = 'Elsewhere'
feed.beg.obs$site[feed.beg.obs$RingLocation=='Schiermonnikoog, Oosterkwelder'] = 'Schier'
table(feed.beg.obs$SexChickSel, feed.beg.obs$SexParentSel, feed.beg.obs$site) # hardly any bird with known/observed sex outside Schier.
table(feed.beg.obs$SexChickMol, feed.beg.obs$SexParentMol)


# make figure of # observed feeding events for all chicks combined (whether sex is known or unknown):
feed.beg.obs.pf$schier = ifelse(feed.beg.obs.pf$Location=='Schiermonnikoog, Oosterkwelder',1,0)
n.feedings.chickage.schier <- table(feed.beg.obs.pf$ChickAge10, feed.beg.obs.pf$schier) # only observations on the Oosterkwelder of Schiermonnikoog for chicks in the age classes 40 and 50
n.feedings.chickage <- table(feed.beg.obs.pf$ChickAge10)
chick.age=as.numeric(names(n.feedings.chickage))
n.obs=as.numeric(n.feedings.chickage)

### FIGURE 1 ###
windows(10,12)
layout(1:2)
par(mar=c(1,4,0,0),oma=c(3,0,1,1))
plot(chick.age, n.obs, xlab="Chick age (days)", ylab="# observed feeding events", las=1, cex=2, xaxt='n',xlim=c(35,135), ylim=c(0,100))
axis(1,at=seq(40,135,10),labels=F)

# modelling # observations as a function of chick age:
m.nobs = glm(n.obs~chick.age, family="poisson")
summary(m.nobs)
chick.age.pred = data.frame(chick.age=40:170)
chick.age.pred$n.obs.pred = predict(m.nobs, newdata = chick.age.pred, type='response')
lines(n.obs.pred~chick.age, chick.age.pred[chick.age.pred$chick.age<=130,])

# what about the distance of the feeding from the colony?
feed.beg.obs.pf$distance.from.colony = round(distCosine(
  matrix(c(feed.beg.obs.pf$Longitude, feed.beg.obs.pf$Latitude), ncol=2, byrow=F), 
  matrix(c(feed.beg.obs.pf$RingLongitude, feed.beg.obs.pf$RingLatitude), ncol=2, byrow=F))/1000,0)

table(floor(feed.beg.obs.pf$distance.from.colony/10)*10) 
table(floor(feed.beg.obs.pf$distance.from.colony/10)*10)/dim(feed.beg.obs.pf)[1] 

# does distance from colony increase with age?
boxdata = boxplot(distance.from.colony~ChickAge10, feed.beg.obs.pf, xlab="", ylab="Distance from colony (km)", ylim=c(0,40), las=1)
mtext("Chick age (days)",1,2.5)
### END FIGURE 1 ###
# Adding sample size is not needed, as this is reflected by the top panel showing the number of feeding observations!

max(feed.beg.obs.pf$distance.from.colony)

# Does the distance to the colony differ between the feedings by mothers and fathers?
windows(10,12)
layout(1:2)
par(mar=c(1,4,0,0),oma=c(3,0,1,1))
boxplot(distance.from.colony~ChickAge10, feed.beg.obs.pf[feed.beg.obs.pf$SexParentSel=='m',], xlab="", ylab="Distance from colony (km)", ylim=c(0,40), las=1, col='skyblue')
boxplot(distance.from.colony~ChickAge10, feed.beg.obs.pf[feed.beg.obs.pf$SexParentSel=='f',], xlab="", ylab="Distance from colony (km)", ylim=c(0,40), las=1, col='lightcoral')

m.feeddist.age = lm(distance.from.colony~ChickAge, feed.beg.obs.pf)
summary(m.feeddist.age)
predict(m.feeddist.age)

# how to test if a feeding is more likely to occur by the mother or the father, and whether female chicks are more likely fed by mothers and males by fathers... (or vice versa)?
# it seems to me that this is a form of presence-only data; however, we are not really interested in the situation where feedings do NOT occur, but more whether they are more likely to occur among female parents than male parents, and more likely among female chicks than male chicks.
feed.beg.obs.pf$freq = 1
# only select data of which sex is known for both parents and chicks:
feed.beg.obs.pf.sexpc = feed.beg.obs.pf[feed.beg.obs.pf$SexChickSel!='u'&feed.beg.obs.pf$SexParentSel!='u',]  
dim(feed.beg.obs.pf.sexpc) # this reduces sample size from 171 to 106 pf feedings
sexpc.table = table(feed.beg.obs.pf.sexpc$SexParentSel, feed.beg.obs.pf.sexpc$SexChickSel)
sexp.table = table(feed.beg.obs.pf.sexpc$SexParentSel)
sexc.table = table(feed.beg.obs.pf.sexpc$SexChickSel)
sexp.table # males overall feed more than females (or at least are observed feeding a chick more often; or there are more male parents of which the sex is known than there are female parents)
table(bird.sex.overview$sexobs) # there are indeed more males in the sex determination dataset than females... 
bird.sex.overview = merge(bird.sex.overview, ringings[,c('BirdID','Location')], all.x=T)
bird.sex.overview$Schier = 0
bird.sex.overview$Schier[bird.sex.overview$Location=='Schiermonnikoog, Oosterkwelder'] = 1
table(bird.sex.overview$sexobs, bird.sex.overview$Schier) # this sex-bias is mainly caused by birds that are not born (i.e. are probably not breeding) on Schier...
table(bird.sex.overview$Location, bird.sex.overview$sexobs)
# where were the parents born that are in the feeding obs dataset?
bird.ringlocations = ringings[,c('ColourCode','BirdID','Location')]
names(bird.ringlocations)[3] = 'RingLocationParent'
feed.beg.obs.pf.sexpc.parentringloc = merge(feed.beg.obs.pf.sexpc, bird.ringlocations, by.x='Parent.code', by.y='ColourCode', all.x=T)
head(feed.beg.obs.pf.sexpc.parentringloc)
table(feed.beg.obs.pf.sexpc.parentringloc$SexParentSel, feed.beg.obs.pf.sexpc.parentringloc$RingLocationParent)
# the sex-bias is mostly found among parents born on Schiermonnikoog, whereas in the entire sex determination dataset (bird.sex.overview), there was not that much a sex bias among the birds born on Schier. So this seems to be driven by something else... Let's first check the ages at which these observations took place:

table(feed.beg.obs.pf.sexpc$SexParentSel, feed.beg.obs.pf.sexpc$ChickAge10)
# these are mostly feedings at 40-60 days old, prior to when the chicks really start to disperse themselves... At these ages, there might still be a parental sex bias in WHEN they have contact (and feed) their chicks.
table(feed.beg.obs.pf.sexpc$SexParentSel, feed.beg.obs.pf.sexpc$distance.from.colony)
# majority of feedings very close to the colony. 

# this means that in the contingency table, parent sex are the rows, and chick sex the columns.
?chisq.test
chisq.test(sexpc.table) # this is significant (p=0.014), but it is uncertain whether it is significant because male parents feed more than female parents, or whether there is also an association with the sex of the chick, depending on the sex of the parent.
chisq.test(sexp.table) # when analysing parent sex only, p=0.008. 
chisq.test(sexc.table) # when analysing chick sex only, p=0.63, so not at all significant, meaning that female and male chicks receive similar amounts of pf feedings.  
# the combined parent x chick sex table gives the most significant outcome; can we conclude from this that male parents are more likely to feed female chicks and female parents are more likely to feed male chicks? 
# seems like we could analyse this with a Poisson glm model using the summed counts per parent and chick sex:
nfeeds.sexpc = aggregate(freq~SexParentSel+SexChickSel, feed.beg.obs.pf.sexpc, sum)
m.nfeeds.sexp.sexc = glm(freq~SexParentSel+SexChickSel, data=nfeeds.sexpc, family="poisson")
summary(m.nfeeds.sexp.sexc) # this almost gives the same p-values as the chi-square tests...
# so what about the interaction between SexParentSel and SexChickSel?
m.nfeeds.sexpxsexc = glm(freq~SexParentSel*SexChickSel, data=nfeeds.sexpc, family="poisson")
summary(m.nfeeds.sexpxsexc) # the is highly significant, p=0.0003, suggesting indeed that male parents are more likely to feed female chicks and female parents are more likely to feed male chicks!

# does the same come out if we add chick age?
nfeeds.sexpc.age = aggregate(freq~SexParentSel+SexChickSel+ChickAge, feed.beg.obs.pf.sexpc, sum)
m.nfeeds.sexpxsexc.chickage = glm(freq~SexParentSel*SexChickSel+ChickAge, data=nfeeds.sexpc.age, family="poisson")
summary(m.nfeeds.sexpxsexc.chickage) # this makes the model less strong in detecting the sex parent x chick interaction. 
# what if we pool age by 10 days:
nfeeds.sexpc.age10 = aggregate(freq~SexParentSel+SexChickSel+ChickAge10, feed.beg.obs.pf.sexpc, sum)
m.nfeeds.sexpxsexc.age10 = glm(freq~SexParentSel*SexChickSel+ChickAge10, data=nfeeds.sexpc.age10, family="poisson")
summary(m.nfeeds.sexpxsexc.age10) # yes, then suddenly everything is significant, but it should not be the case that the way the data are pooled affects whether the outcome is or is not significant... 

# what if we analyse the probability that a begging/feeding is performed at/by the female parent:
feed.beg.obs.known.sexes = na.omit(feed.beg.obs.pf.sexpc[feed.beg.obs.pf.sexpc$SexChickSel!='u' &
                                                   feed.beg.obs.pf.sexpc$SexParentSel!='u',])
feed.beg.obs.known.sexes$prntF = 0
feed.beg.obs.known.sexes$prntF[feed.beg.obs.known.sexes$SexParentSel=='f'] = 1
m.binom.sex.age = glm(prntF~SexChickSel*ChickAge, feed.beg.obs.known.sexes, family='binomial', na.action='na.fail')  
summary(m.binom.sex.age)
dredge(m.binom.sex.age) # some support for interaction age and sex chick
m.binom.sexchick = glm(prntF~SexChickSel, feed.beg.obs.known.sexes, family='binomial', na.action='na.fail')  
summary(m.binom.sexchick) # male chicks more likely to be fed by female parent.
m.binom.overall = glm(prntF~1, feed.beg.obs.known.sexes, family='binomial', na.action='na.fail')  
summary(m.binom.overall) # intercept is significant, and slope negative, implying that female parents are less likely feeding a chick than male parents. 
# analyse separately for beggings and feedings:
feed.obs.known.sexes = feed.beg.obs.known.sexes[feed.beg.obs.known.sexes$behav.cat=='feeding',]
summary(glm(prntF~1, feed.obs.known.sexes, family='binomial', na.action='na.fail'))  
beg.obs.known.sexes = feed.beg.obs.known.sexes[feed.beg.obs.known.sexes$behav.cat=='begging',]
summary(glm(prntF~1, beg.obs.known.sexes, family='binomial', na.action='na.fail'))# N.S. but slope is in same direction, and sample size considerably smaller.
# So overall the conclusion remains the same when analysing feedings only, or combining it with beggings. 
# Male parents are more likely to be (observed) feeding than female parents, and there is an interaction between sex parent and sex chick in the probability of a feeding. 


# plot colonies and feeding locations
windows(6,8)
plot(c(feed.beg.obs.pf$Longitude, feed.beg.obs.pf$RingLongitude),
     c(feed.beg.obs.pf$Latitude, feed.beg.obs.pf$RingLatitude),
     xlab="Longitude", ylab="Latitude", type='n')
for (i in 1:dim(feed.beg.obs.pf)[1]) {
  lines(c(feed.beg.obs.pf$Longitude[i], feed.beg.obs.pf$RingLongitude[i]),
        c(feed.beg.obs.pf$Latitude[i], feed.beg.obs.pf$RingLatitude[i]))
  points(feed.beg.obs.pf$RingLongitude[i], feed.beg.obs.pf$RingLatitude[i], col='orange')
  points(feed.beg.obs.pf$Longitude[i], feed.beg.obs.pf$Latitude[i], col='green')
}


# Does this distance change with age and depend on a chick's sex?
# now also by sex, by making the unknown sexes a separate group:
feed.beg.obs.pf$SexChickSel[is.na(feed.beg.obs.pf$SexChickSel)]="u"
distance.age.sex <- aggregate(distance.from.colony~ChickAge10+SexChickSel, feed.beg.obs.pf, mean)
distance.age.sex.sd <- aggregate(distance.from.colony~ChickAge10+SexChickSel, feed.beg.obs.pf, sd)
distance.age.sex.N <- aggregate(freq~ChickAge10+SexChickSel, feed.beg.obs.pf, sum)
distance.age.sex$N <- distance.age.sex.N$freq
distance.age.sex$se = distance.age.sex.sd$distance.from.colony/sqrt(distance.age.sex.N$freq)
distance.age.sex$sexn = 1
distance.age.sex$sexn[distance.age.sex$SexChickSel=='u'] = 2
distance.age.sex$sexn[distance.age.sex$SexChickSel=='m'] = 3
plotCI(distance.age.sex$ChickAge10+(distance.age.sex$sexn-2), distance.age.sex$distance.from.colony, uiw=distance.age.sex$se, col=c('red','black','blue')[distance.age.sex$sexn], sfrac=0, pch=19)

# Does this distance depend on the parent's sex?
feed.beg.obs.pf$SexParentSel[is.na(feed.beg.obs.pf$SexParentSel)]="u"
distance.age.sex <- aggregate(distance.from.colony~ChickAge10+SexParentSel, feed.beg.obs.pf, mean)
distance.age.sex.sd <- aggregate(distance.from.colony~ChickAge10+SexParentSel, feed.beg.obs.pf, sd)
distance.age.sex.N <- aggregate(freq~ChickAge10+SexParentSel, feed.beg.obs.pf, sum)
distance.age.sex$N <- distance.age.sex.N$freq
distance.age.sex$se = distance.age.sex.sd$distance.from.colony/sqrt(distance.age.sex.N$freq)
distance.age.sex$sexn = 1
distance.age.sex$sexn[distance.age.sex$SexParentSel=='u'] = 2
distance.age.sex$sexn[distance.age.sex$SexParentSel=='m'] = 3
plotCI(distance.age.sex$ChickAge10+(distance.age.sex$sexn-2), distance.age.sex$distance.from.colony, uiw=distance.age.sex$se, col=c('red','black','blue')[distance.age.sex$sexn], sfrac=0, pch=19)


# this shows that females are generally fed further away from the colony. Is this because females are the more dispersive sex during post-fledging (potentially also because they are independent sooner as they are smaller and need less energy)?
# or is this a particular Schier-phenomenon, driven by female chicks often joining their fathers that are often foraging in Lauwersmeer? (Why would female chicks join their father, and male chicks stay with their mother?)


# make figure of # observed feeding events in relation to chick sex, parent sex and chick x parent sex
windows(12,4)
layout(matrix(1:3,ncol=3))

# visualise data of which chick sex is known, to see if male chicks are more often seen being fed than female chicks...
feed.beg.obs.chicksex.known <- feed.beg.obs.pf[!is.na(feed.beg.obs.pf$SexChickSel),]
freq.age.sex = table(feed.beg.obs.chicksex.known$SexChickSel, feed.beg.obs.chicksex.known$ChickAge10)
# Panel A
plot(as.numeric(colnames(freq.age.sex)), freq.age.sex[1,], ylim=c(0,max(freq.age.sex)), col='red', xlab="Chick age (days)", ylab="Number of observed feeding events", las=1, cex=2)
lines(as.numeric(colnames(freq.age.sex)), freq.age.sex[1,], col='red')
points(as.numeric(colnames(freq.age.sex)), freq.age.sex[2,], col='blue', cex=2)
lines(as.numeric(colnames(freq.age.sex)), freq.age.sex[2,], col='blue')
legend("topright", bty="n", legend=c("female chick","male chick"), col=c("red","blue"), pch=21)

# visualise data of which parent sex is known, to see whether female or male parents are more likely to feed during post-fleding
feed.beg.obs.parent.sex.known <- feed.beg.obs.pf[!is.na(feed.beg.obs.pf$SexParentSel),]
freq.age.parentsex = table(feed.beg.obs.parent.sex.known$SexParentSel, feed.beg.obs.parent.sex.known$ChickAge10)
plot(as.numeric(colnames(freq.age.parentsex)), freq.age.parentsex[1,], ylim=c(0,max(freq.age.parentsex[!is.na(freq.age.parentsex)])), col='red', xlab="Chick age", ylab="Number of observed feeding events", las=1, cex=2)
lines(as.numeric(colnames(freq.age.parentsex)), freq.age.parentsex[1,], col='red')
points(as.numeric(colnames(freq.age.parentsex)), freq.age.parentsex[2,], col='blue', cex=2)
lines(as.numeric(colnames(freq.age.parentsex)), freq.age.parentsex[2,], col='blue')
legend("topright", bty="n", legend=c("female parent","male parent"), col=c("red","blue"), pch=21)
# this graph shows that there are more feedings by male parents observed; however, this can also be caused by the time of the day when male and female parents feed... 

# Data where both chick and parent sex is known:
feed.beg.obs.all.sex.known <- feed.beg.obs.parent.sex.known[feed.beg.obs.parent.sex.known$SexChickSel!='u',]
freq.age.sexes = table(paste(feed.beg.obs.all.sex.known$SexParentSel, feed.beg.obs.all.sex.known$SexChickSel), feed.beg.obs.all.sex.known$ChickAge10)
plot(as.numeric(colnames(freq.age.sexes)), freq.age.sexes[1,], ylim=c(0,max(freq.age.sexes[!is.na(freq.age.sexes)])), col='red', xlab="Chick age", ylab="Number of observed feeding events", las=1, cex=2)
lines(as.numeric(colnames(freq.age.sexes)), freq.age.sexes[1,], col='red')
points(as.numeric(colnames(freq.age.sexes)), freq.age.sexes[2,], col='orange', cex=2)
lines(as.numeric(colnames(freq.age.sexes)), freq.age.sexes[2,], col='orange')
points(as.numeric(colnames(freq.age.sexes)), freq.age.sexes[3,], col='green', cex=2)
lines(as.numeric(colnames(freq.age.sexes)), freq.age.sexes[3,], col='green')
points(as.numeric(colnames(freq.age.sexes)), freq.age.sexes[4,], col='blue', cex=2)
lines(as.numeric(colnames(freq.age.sexes)), freq.age.sexes[4,], col='blue')
legend("topright", bty="n", legend=c("female parent - female chick","female parent - male chick",
                                     "male parent - female chick", "male parent - male chick"), 
       col=c("red","orange","green","blue"), pch=21)
# if there is a difference at all (which is likely not significant), then it appears that parents are more likely to feed a chick of the opposite sex. 

# observed feeding events strongly decreases with age, but this could also be caused by the intensive monitoring on Schiermonnikoog. What do we see if we remove that part?
# alternatively, we could argue that we get the most accurate pattern from this intensive monitoring. Both on the saltmarsh and in the surrounding areas (Lutjewad), lots of observations are done. 
feed.beg.obs.pf.notschier <- feed.beg.obs.pf[feed.beg.obs.pf$Location!='Schiermonnikoog, Oosterkwelder',]
feed.beg.obs.chicksex.known <- feed.beg.obs.pf.notschier[!is.na(feed.beg.obs.pf.notschier$SexChickSel),]
freq.age.sex = table(feed.beg.obs.chicksex.known$SexChickSel, feed.beg.obs.chicksex.known$ChickAge10)
windows(12,5)
layout(matrix(1:3,ncol=3))
plot(as.numeric(colnames(freq.age.sex)), freq.age.sex[1,], ylim=c(0,max(freq.age.sex)), col='red', xlab="Chick age (days)", ylab="Number of observed feeding events", las=1)
lines(as.numeric(colnames(freq.age.sex)), freq.age.sex[1,], col='red')
points(as.numeric(colnames(freq.age.sex)), freq.age.sex[2,], col='blue')
lines(as.numeric(colnames(freq.age.sex)), freq.age.sex[2,], col='blue')
legend("topright", bty="n", legend=c("female chick","male chick"), col=c("red","blue"), pch=21)
feed.beg.obs.parent.sex.known <- feed.beg.obs.pf.notschier[!is.na(feed.beg.obs.pf.notschier$SexParentSel),]
freq.age.parentsex = table(feed.beg.obs.parent.sex.known$SexParentSel, feed.beg.obs.parent.sex.known$ChickAge10)
plot(as.numeric(colnames(freq.age.parentsex)), freq.age.parentsex[1,], ylim=c(0,max(freq.age.parentsex[!is.na(freq.age.parentsex)])), col='red', xlab="Chick age", ylab="Number of observed feeding events", las=1)
lines(as.numeric(colnames(freq.age.parentsex)), freq.age.parentsex[1,], col='red')
points(as.numeric(colnames(freq.age.parentsex)), freq.age.parentsex[2,], col='blue')
lines(as.numeric(colnames(freq.age.parentsex)), freq.age.parentsex[2,], col='blue')
legend("topright", bty="n", legend=c("female parent","male parent"), col=c("red","blue"), pch=21)
feed.beg.obs.all.sex.known <- feed.beg.obs.parent.sex.known[!is.na(feed.beg.obs.parent.sex.known$SexChickSel),]
freq.age.sexes = table(paste(feed.beg.obs.all.sex.known$SexParentSel, feed.beg.obs.all.sex.known$SexChickSel), feed.beg.obs.all.sex.known$ChickAge10)
plot(as.numeric(colnames(freq.age.sexes)), freq.age.sexes[1,], ylim=c(0,max(freq.age.sexes[!is.na(freq.age.sexes)])), col='red', xlab="Chick age", ylab="Number of observed feeding events", las=1)
lines(as.numeric(colnames(freq.age.sexes)), freq.age.sexes[1,], col='red')
points(as.numeric(colnames(freq.age.sexes)), freq.age.sexes[2,], col='orange')
lines(as.numeric(colnames(freq.age.sexes)), freq.age.sexes[2,], col='orange')
points(as.numeric(colnames(freq.age.sexes)), freq.age.sexes[3,], col='green')
lines(as.numeric(colnames(freq.age.sexes)), freq.age.sexes[3,], col='green')
points(as.numeric(colnames(freq.age.sexes)), freq.age.sexes[4,], col='blue')
lines(as.numeric(colnames(freq.age.sexes)), freq.age.sexes[4,], col='blue')
legend("topright", bty="n", legend=c("female parent - female chick","female parent - male chick",
                                     "male parent - female chick", "male parent - male chick"), 
       col=c("red","orange","green","blue"), pch=21)
mtext("Feedings excluding data from Schiermonnikoog chicks",3,-2,outer=T,cex=1.5)



# out of curiosity (to see if females generally disperse further from the colony than male chicks) let's check the distance from the colony with age for males and females in the entire colour-ringing dataset. 
resightings = sqlFetch(con, "Qy_SightingOverview_NLbirds_analysis")
resightings.juvs <- resightings[resightings$Age==0,]
resightings.juvs$distance.to.colony = distCosine(
  matrix(c(resightings.juvs$Longitude, resightings.juvs$Latitude),byrow=F,ncol=2), 
  matrix(c(resightings.juvs$LonBorn, resightings.juvs$LatBorn),byrow=F,ncol=2))/1000
# calculate chick age by assuming they were 25 days old when being ringed (as for many chicks, no biometric measurements were taken):
resightings.juvs$ChickAge <- resightings.juvs$Date-resightings.juvs$FldDate+25
resightings.juvs$SexMol[is.na(resightings.juvs$SexMol)]='u'
# let it run until 120 days, as by that age, most chicks have departed on autumn migration.
resightings.juvs.sel <- resightings.juvs[resightings.juvs$ChickAge<=120,]
# calculate mean distance per bird per day:
distance.bird.day <- aggregate(distance.to.colony~BirdID+ChickAge+SexMol+SiteBorn+LatBorn+LonBorn, resightings.juvs.sel, mean) # from 35929 obs to 7942
mean.distance.age.sex <- aggregate(distance.to.colony~ChickAge+SexMol, resightings.juvs.sel, mean)
resightings.juvs.sel$freq=1
sd.distance.age.sex <- aggregate(distance.to.colony~ChickAge+SexMol, resightings.juvs.sel, sd)
N.distance.age.sex <- aggregate(freq~ChickAge+SexMol, resightings.juvs.sel, sum)
mean.distance.age.sex$se = sd.distance.age.sex$distance.to.colony/sqrt(N.distance.age.sex$freq)
mean.distance.age.sex$sexn = 1
mean.distance.age.sex$sexn[mean.distance.age.sex$SexMol=='u'] = 2
mean.distance.age.sex$sexn[mean.distance.age.sex$SexMol=='m'] = 3
mean.distance.age.sex$Age <- as.numeric(mean.distance.age.sex$ChickAge)
windows()
plotCI(as.numeric(mean.distance.age.sex$ChickAge)+(mean.distance.age.sex$sexn-2)*.1, mean.distance.age.sex$distance.to.colony, uiw=mean.distance.age.sex$se, col=c('red','black','blue')[mean.distance.age.sex$sexn], sfrac=0, pch=19, xlab="Age", ylab="", ylim=c(0,500))

# do the same while excluding distance more than 1 degree latitude to the south (ca. 100 km), indicating that the bird started its autumn migration.
birds.siteborn <- unique(resightings.juvs.sel[,c('BirdID','SiteBorn','LatBorn','LonBorn')])
n_birds.siteborn <- table(birds.siteborn$SiteBorn)
n_birds.siteborn[order(n_birds.siteborn, decreasing=T)]
mean.distance.age.sex <- aggregate(distance.to.colony~ChickAge+SexMol, resightings.juvs.sel[resightings.juvs.sel$Latitude-resightings.juvs.sel$LatBorn>-1,], mean)
sd.distance.age.sex <- aggregate(distance.to.colony~ChickAge+SexMol, resightings.juvs.sel[resightings.juvs.sel$Latitude-resightings.juvs.sel$LatBorn>-1,], sd)
N.distance.age.sex <- aggregate(freq~ChickAge+SexMol, resightings.juvs.sel[resightings.juvs.sel$Latitude-resightings.juvs.sel$LatBorn>-1,], sum)
mean.distance.age.sex$se = sd.distance.age.sex$distance.to.colony/sqrt(N.distance.age.sex$freq)
mean.distance.age.sex$sexn = 1
mean.distance.age.sex$sexn[mean.distance.age.sex$SexMol=='u'] = 2
mean.distance.age.sex$sexn[mean.distance.age.sex$SexMol=='m'] = 3
windows()
plotCI(as.numeric(mean.distance.age.sex$ChickAge)+(mean.distance.age.sex$sexn-2)*.1, mean.distance.age.sex$distance.to.colony, uiw=mean.distance.age.sex$se, col=c('red','black','blue')[mean.distance.age.sex$sexn], sfrac=0, pch=19, xlab="Age", ylab="Distance from colony (km)", ylim=c(0,60))
legend("bottomright", legend=c("female","male","unknown"), col=c("red","blue","black"), pch=19)
# interestingly, the birds with unknown sex (freshwater birds or Delta birds?) often disperse further than the Wadden Sea birds at older ages...  
# make the same plot with respect to region (Delta, mainland, Waddensea):
siteborn.coords = unique(birds.siteborn[,c('SiteBorn','LatBorn','LonBorn')])
siteborn.coords$RegionBorn = 'Waddensea'
siteborn.coords$RegionBorn[siteborn.coords$LatBorn<52.8 & siteborn.coords$LatBorn>52] = "Mainland"
siteborn.coords$RegionBorn[siteborn.coords$LatBorn<52] = "Delta"
resightings.juvs.sel2 = merge(resightings.juvs.sel, siteborn.coords, all.x=T)
mean.distance.age.region <- aggregate(distance.to.colony~ChickAge+RegionBorn, resightings.juvs.sel2[resightings.juvs.sel2$Latitude-resightings.juvs.sel2$LatBorn>-1,], mean)
sd.distance.age.region <- aggregate(distance.to.colony~ChickAge+RegionBorn, resightings.juvs.sel2[resightings.juvs.sel2$Latitude-resightings.juvs.sel2$LatBorn>-1,], sd)
N.distance.age.region <- aggregate(freq~ChickAge+RegionBorn, resightings.juvs.sel2[resightings.juvs.sel2$Latitude-resightings.juvs.sel2$LatBorn>-1,], sum)
mean.distance.age.region$se = sd.distance.age.region$distance.to.colony/sqrt(N.distance.age.region$freq)
mean.distance.age.region$regionn = 1
mean.distance.age.region$regionn[mean.distance.age.region$RegionBorn=='Delta'] = 2
mean.distance.age.region$regionn[mean.distance.age.region$RegionBorn=='Mainland'] = 3
windows()
plotCI(as.numeric(mean.distance.age.region$ChickAge)+(mean.distance.age.region$regionn-2)*.1, mean.distance.age.region$distance.to.colony, uiw=mean.distance.age.region$se, col=c('red','black','blue')[mean.distance.age.region$regionn], sfrac=0, pch=19, xlab="Age", ylab="Distance from colony (km)", ylim=c(0,80))
legend("bottomright", legend=c("WaddenSea","Delta","Mainland"), col=c("red","black","blue"), pch=19)
# Interestingly, the birds born on the mainland are the ones dispersing the furthest. This might be due to the fact that inland waters (freshwater) become less suitable for foraging towards the end of the season. Based on GPS-tracking data, it indeed seems that many juveniles from Durgerdam go to the Wadden Sea during post-fledging.  


# But what about the difference between males and females? Remove the unknowns for plotting:
windows()
mean.distance.age.knownsex = mean.distance.age.sex[mean.distance.age.sex$SexMol!='u',]
plotCI(as.numeric(mean.distance.age.knownsex$ChickAge)+(mean.distance.age.knownsex$sexn-2)*.1, mean.distance.age.knownsex$distance.to.colony, uiw=mean.distance.age.knownsex$se, col=c('red','black','blue')[mean.distance.age.knownsex$sexn], sfrac=0, pch=19, xlab="Age", ylab="", ylim=c(0,60))
# no difference between males and females

plot(distance.to.colony~as.factor(ChickAge), resightings.juvs.sel[resightings.juvs.sel$Latitude-resightings.juvs.sel$LatBorn>-1,], xlab="Age", ylab="", ylim=c(0,200), pt.cex=0.1)

mean.distance.age.sex

resightings.juvs.sel[resightings.juvs.sel$SexMol=='m'&resightings.juvs.sel$ChickAge==31,]

# what is the final observation before the first >1 latitude southward displacement? Does this depend on sex, or siteborn? 
# for this, first determine per bird the first observation of >1 latitude southward displacement
resightings.juvs$migration <- ifelse(resightings.juvs$Latitude-resightings.juvs$LatBorn<(-1),1,0)
# determine first obs of migration per bird
start.mig.bird = aggregate(Date~BirdID+LatBorn+SexMol+Year, resightings.juvs[resightings.juvs$migration==1,], min)
names(start.mig.bird)[5]='migration.date'
# link this to the original data:
resightings.juvs.mig <- merge(resightings.juvs, start.mig.bird) # this only includes birds that have a migration observation
# only select data before the migration.date:
resightings.juvs.before.mig <- resightings.juvs.mig[resightings.juvs.mig$Date<resightings.juvs.mig$migration.date,]
# select last date before migration per bird:
last.date.before.mig.bird = aggregate(Date~BirdID+SiteBorn+LatBorn+SexMol+Year, resightings.juvs.before.mig, max)
last.date.before.mig.bird$yday <- yday(last.date.before.mig.bird$Date)
last.date.before.mig.bird$sexn = 0
last.date.before.mig.bird$sexn[last.date.before.mig.bird$SexMol=='u'] = 0.5
last.date.before.mig.bird$sexn[last.date.before.mig.bird$SexMol=='m'] = 1

plot(yday~as.factor(SexMol), last.date.before.mig.bird)
m.timing.departure = lmer(yday~sexn+LatBorn+(1|Year), last.date.before.mig.bird, na.action='na.fail', REML=F)
dredge(m.timing.departure) # some support for sex effect, but not very strong
summary(m.timing.departure)
# evidence for both a latitude effect, and a (small) sex effect:
# male chicks depart later than females
# birds born at higher latitudes depart later (that is an interesting result... could also be driven by the higher observation efforts there)

# what if we only analyse birds of known sex?
last.date.before.mig.bird.sexknown <- last.date.before.mig.bird[last.date.before.mig.bird$SexMol!='u',]
m.timing.departure = lmer(yday~sexn+LatBorn+(1|Year), last.date.before.mig.bird.sexknown, na.action='na.fail', REML=F)
dredge(m.timing.departure) # then the sex effect is just significant
m.timing.departure = lmer(yday~sexn+(1|Year), last.date.before.mig.bird.sexknown, na.action='na.fail', REML=F)
summary(m.timing.departure) # males depart later than females

# do we see the same result when taking the first observation during migration?
start.mig.bird$yday = yday(start.mig.bird$migration.date)
start.mig.bird$sexn = 0
start.mig.bird$sexn[start.mig.bird$SexMol=='u'] = 0.5
start.mig.bird$sexn[start.mig.bird$SexMol=='m'] = 1
m.timing.migration = lmer(yday~sexn+LatBorn+(1|Year), start.mig.bird, na.action='na.fail', REML=F)
dredge(m.timing.migration) # very similar result, though hardly a sex effect
summary(m.timing.migration)
# therefore, the later departure of higher latitude birds is not driven by the higher obesrvation efforts in the Wadden Sea, but it is a true result, 
# as it is also very significant when analysing the first observation with >1 latitude southward displacement. 
# perhaps conditions in the Wadden Sea remain more favourable than in other places, therefore causing birds to leave earlier from other places. 
start.mig.bird.knownsex <- start.mig.bird[start.mig.bird$SexMol!='u',]
m.timing.migration = lmer(yday~sexn+LatBorn+(1|Year), start.mig.bird.knownsex, na.action='na.fail', REML=F)
dredge(m.timing.migration) # no evidence for a sex effect
