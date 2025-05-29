# statistical analysis of the number and distance from the colony of observed feedings.
# analyse number of feedings with poisson regression

feeding.obs.pf = read.csv('data/raw/feeding.observations.with.sex.info.csv')

# which years are included?
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

# to have large enough sample sizes, pool chick ages per 10 days:
# make ageclasses of 10 days, where age 40 includes the range of 35-44:
feeding.obs.pf$ChickAge10 <- floor((feeding.obs.pf$ChickAge+5)/10)*10
table(feeding.obs.pf$ChickAge10)

# calculate distance to the colony
feeding.obs.pf$distance.from.colony = round(geosphere::distCosine(
  matrix(c(feeding.obs.pf$Longitude, feeding.obs.pf$Latitude), ncol=2, byrow=F), 
  matrix(c(feeding.obs.pf$RingLongitude, feeding.obs.pf$RingLatitude), ncol=2, byrow=F))/1000,0)

# how many chicks are involved in this dataset?
feeding.obs.pf$freq=1
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
# write.csv(feeding.obs.pf.chick.uni, 'feeding.obs.pf.chick.uni.csv', row.names=F)
# feeding.obs.pf.chick.uni = read.csv(feeding.obs.pf.chick.uni, 'feeding.obs.pf.chick.uni.csv')
nfeeds.age10 = aggregate(freq~ChickAge10, feeding.obs.pf.chick.uni, sum)
m.nfeeds.age10 = glm(freq~ChickAge10, data=nfeeds.age10, family="poisson")
# Calculate overdispersion statistic
resid_dev <- sum(residuals(m.nfeeds.age10, type = "deviance")^2)
df_resid <- df.residual(m.nfeeds.age10)
dispersion_ratio <- resid_dev / df_resid
dispersion_ratio # underdispersion... no problem!
plot(fitted(m.nfeeds.age10)~residuals(m.nfeeds.age10))

summary(m.nfeeds.age10) # highly significant effect of chick age.
max(feeding.obs.pf.chick.uni$ChickAge) # max age: 125 days old

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