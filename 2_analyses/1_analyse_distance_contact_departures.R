# what is the max age of the chick where there is still matched data between parent and chick:
aggregate(age.chick~chick.parent, chick.parent.data.contact, max) # three cases where data ends when chick is between 80 and 90 days old 
table(chick.parent.data.contact$contact) # this datafile contains also contains the data without contact.

# what is the maximum chick age when there was still contact between parent and chick?
max.age.contact.sex = aggregate(age.chick~chick.parent+sex.chick+sex.parent, chick.parent.data.contact[chick.parent.data.contact$contact==1,], max) # here, contact is defined as a distance of less than 10m between chick and parent.
max.age.contact.sex # 136 days. Therefore, we run the analysis until 136 days

model.max.age.sex = lm(age.chick~sex.chick*sex.parent, max.age.contact.sex, na.action='na.fail')
dredge(model.max.age.sex) # no support for max age of contact to depend on sex of the chick or the parent. 

# is the amount of contact explained by the sex of the chick and/or parent (potentially in interaction with chick age)? For this, select data of chicks between 35 and 136 days old:
chick.parent.data.contact.sel <- chick.parent.data.contact[which(chick.parent.data.contact$age.chick>=35 & chick.parent.data.contact$age.chick<=136),]
# and select data up until either the chick or the parent has departed on autumn migration. 
# first add a fictive date into the far future for chicks or parents that never departed or of which departure date could not be determined:
chick.parent.data.contact$departure.datetime.chick[is.na(chick.parent.data.contact$departure.datetime.chick)] = dmy("01/01/3000")
chick.parent.data.contact$departure.datetime.parent[is.na(chick.parent.data.contact$departure.datetime.parent)] = dmy("01/01/3000")
chick.parent.data.contact.sel <- chick.parent.data.contact[which(chick.parent.data.contact$age.chick>=35 & chick.parent.data.contact$age.chick<=136 & chick.parent.data.contact$datetime.chick < chick.parent.data.contact$departure.datetime.chick & chick.parent.data.contact$datetime.chick < chick.parent.data.contact$departure.datetime.parent),] 
# check of how many chicks we have data per age:
chick.data.age = table(chick.parent.data.contact.sel$age.chick, chick.parent.data.contact.sel$birdID.chick)
chick.data.age.yesno = ifelse(chick.data.age>0,1,0)
rowSums(chick.data.age.yesno) # until 90 days, there are still 12 chicks in the dataset; until 100 days 8 chicks. 
chick.data.age.yesno[56:66,]
# the four chicks that drop out between 90 and 100 days, are 6296 (son with father), 6301 (daughter with mother), 6304 (son with father), and 6374.1 (son with mother). 6358.1 (daughter with father) and 6381 (son with father) dropped out earlier already.  
# To keep a balanced and comparable dataset, we could apply the analysis up until 90 days old. However, most birds depart when chicks are 100 days old or older, so it is also a shame to ignore the available data at these ages... 
# chick.parent.data.contact.sel <- chick.parent.data.contact.sel[chick.parent.data.contact.sel$age.chick<=90,]
# In addition, perhaps the covariate "days until departure" is a better explanatory variable than age in explaining contact and amount of begging behaviour... Kind of already including individual variation in how fast chicks develop to independence...
chick.parent.data.contact.sel$days.until.departure = date(chick.parent.data.contact.sel$departure.datetime.chick)-date(chick.parent.data.contact.sel$datetime.chick)
unique(chick.parent.data.contact.sel$departure.datetime.chick)
chick.parent.data.contact.sel$days.until.departure[chick.parent.data.contact.sel$departure.datetime.chick=='3000-01-01']=NA # make this variable NA for those chicks that never departed.
table(chick.parent.data.contact.sel$days.until.departure)

# standardize chick age so that model convergence gets easier:
chick.parent.data.contact.sel$z.age = (chick.parent.data.contact.sel$age.chick-mean(chick.parent.data.contact.sel$age.chick))/sd(chick.parent.data.contact.sel$age.chick)

### 1. TOTAL AMOUNT OF CONTACT ###

# compare support for different random effect structures, using the full model structure for the fixed effects:
m.contact.chickrnd <- glmer(contact~sex.parent*sex.chick+sex.parent*z.age+sex.chick*z.age+(1|birdID.chick), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail') # takes some minutes...
m.contact.chickagernd <- glmer(contact~sex.parent*sex.chick+sex.parent*z.age+sex.chick*z.age+(z.age|birdID.chick), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail') # takes some minutes...
m.contact.prntrnd <- glmer(contact~sex.parent*sex.chick+sex.parent*z.age+sex.chick*z.age+(1|birdID.parent), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail')
m.contact.prntagernd <- glmer(contact~sex.parent*sex.chick+sex.parent*z.age+sex.chick*z.age+(z.age|birdID.parent), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail')
chick.parent.data.contact.sel$chick.parent = as.factor(chick.parent.data.contact.sel$chick.parent)
m.contact.chickprntrnd <- glmer(contact~sex.parent*sex.chick+sex.parent*z.age+sex.chick*z.age+(1|chick.parent), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail') 
m.contact.chickprntagernd <- glmer(contact~sex.parent*sex.chick+sex.parent*z.age+sex.chick*z.age+(z.age|chick.parent), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail') 
AIC(m.contact.chickrnd, m.contact.chickagernd, m.contact.prntrnd, m.contact.prntagernd, m.contact.chickprntrnd, m.contact.chickprntagernd)
summary(m.contact.chickprntagernd)

# Even though the model with age as random slope varying among chick-parent pairs is best-supported, this model ignores the fact that for two chicks, we have data of both parents. When using chick-parent pair is random effect, this is ignored, while if using chickID as random effect, it is not. 
# However, as a result, results might be particularly affected by the differences between the father and mother for these two chicks. 
# Nevertheless, we decided to continue with chickID as random effect, but award some discussion on how results would change if we used chick-parent pair instead. (although this also causes some convergence issues)

m.contact.chickagernd.simple <- glmer(contact~sex.parent*sex.chick+z.age+(z.age|birdID.chick), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail') # takes some minutes...

# check whether the global model meets the model assumptions:

# (1) homogeneity of variances
# (2) 

# check for over- or underdispersion of the data:
simOutput.m.contact.chickagernd <- simulateResiduals(m.contact.chickagernd, plot=F)
residuals(simOutput.m.contact.chickagernd) # why are there two lines?
windows()
plot(simOutput.m.contact.chickagernd)
# the two plots plotted separately:
windows()
plotQQunif(simOutput.m.contact.chickagernd)
testOutliers(simOutput.m.contact.chickagernd, type='bootstrap') # this refers to the estimated p-value of the "Outlier test" reported in the bottomright corner of the plot; using bootstrap, p=0.76
testOutliers(simOutput.m.contact.chickagernd, type='binomial') # p=0.0002; I guess this is the value shown in the figure... 
plotResiduals(simOutput.m.contact.chickagernd)

dredge(m.contact.chickagernd.simple)

model.sel.contact.chickagernd <- dredge(m.contact.chickagernd) 
TableS1 = make.table.from.dredge.output(model.sel.contact.chickagernd)
write.csv(TableS1, "output/TableS1 - Proportion of overall contact.csv")

model.sel.contact.chickagernd # then the model with only chick age is really poorly supported... (dAIC=677!), while the only difference is that the two chicks with two parents are considered as two random levels (i.e. according to chick) in the chickagernd models, but as four 'independent' random levels in the chickprntage models(according to chick-parent ID).
m.contact.chickagernd.parsim <- glmer(contact~sex.parent*sex.chick+sex.parent*z.age+(z.age|birdID.chick), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail')
summary(m.contact.chickagernd.parsim)
emmeans.overall.contact = emmeans(m.contact.chickagernd.parsim, pairwise~sex.chick*sex.parent)
emmeans.overall.contact.df <- as.data.frame(emmeans.overall.contact$emmeans)
emmeans.overall.contact.df  <- cbind(emmeans.overall.contact.df[,c('sex.chick','sex.parent','df')], round(plogis(as.matrix(emmeans.overall.contact.df[,c('emmean','asymp.LCL','asymp.UCL')])),3))
emmeans.overall.contact.df
emmeans.overall.contact
emmeans.overall.contact.chicksex = emmeans(m.contact.chickagernd.parsim, pairwise~sex.chick)
emmeans.overall.contact.chicksex.df = as.data.frame(emmeans.overall.contact.chicksex$emmeans)
cbind(emmeans.overall.contact.chicksex.df[,c('sex.chick','df')], round(plogis(as.matrix(emmeans.overall.contact.chicksex.df[,c('emmean','asymp.LCL','asymp.UCL')])),3))
emmeans.overall.contact.chicksex


# how does distance from nest during contact relate to chick age (and - potentially to chick sex and parent sex)?
# select one data point per chick timestamp, and selecting the maximum contact (i.e. for the chicks with both parents tagged, this makes it more likely to have a data point with contact between chick and parent)
chick.contact.data = unique(chick.parent.data.contact[,c('birdID.chick','datetime.chick','latitude.chick','longitude.chick','ground.speed.chick','sex.chick','age.chick','sex.parent','dist.nest','contact')])
chick.data.actual.contact = chick.contact.data[chick.contact.data$contact==1,]
chick.data.actual.contact$z.age = (chick.data.actual.contact$age.chick-mean(chick.data.actual.contact$age.chick))/sd(chick.data.actual.contact$age.chick)
m.dist.nest.full = lmer(dist.nest~sex.chick*sex.parent+sex.chick*z.age+sex.parent*z.age+(z.age|birdID.chick), chick.data.actual.contact, na.action='na.fail', REML=F)
modsel.dist.nest = dredge(m.dist.nest.full) # full model as by far the best. 
m.dist.nest.pars = lmer(dist.nest~sex.chick*sex.parent+sex.parent*z.age+(z.age|birdID.chick), chick.data.actual.contact, na.action='na.fail', REML=F)
summary(m.dist.nest.pars) # no main effect of chick sex. Distance to nest largest for fathers (who tend to move to Lauwersmeer) and increase in distance with increasing chick age also strongest for fathers; particularly for fathers with female chicks.  
TableS2 = make.table.from.dredge.output(modsel.dist.nest)
write.csv(TableS2, "output/TableS2 - Distance to nest during contact.csv")


#save.image("data/processed/results.analyses.1118.RData")
load("data/processed/results.analyses.1118.RData")

### 2. BEHAVIOUR DURING CONTACT AND AMOUNT OF ACTIVE CONTACT ###

# make the same selection for chick.parent.data.behav.sel in relation to ages analysed:
chick.parent.data.behav.sel$departure.datetime.chick[is.na(chick.parent.data.behav.sel$departure.datetime.chick)] = dmy("01/01/3000")
chick.parent.data.behav.sel$departure.datetime.parent[is.na(chick.parent.data.behav.sel$departure.datetime.parent)] = dmy("01/01/3000")
chick.parent.data.behav.sel <- chick.parent.data.behav.sel[which(chick.parent.data.behav.sel$age.chick>=35 & chick.parent.data.behav.sel$age.chick<=136 & chick.parent.data.behav.sel$datetime.chick < chick.parent.data.behav.sel$departure.datetime.chick & chick.parent.data.behav.sel$datetime.chick < chick.parent.data.behav.sel$departure.datetime.parent),] # start analysis at the same age for all chicks. As the oldest chicks were tagged at 35 days, start at age 35. 

### Analyse the probability of begging (i.e. classified as begging while being in contact)
chick.parent.data.behav.sel$z.age = (chick.parent.data.behav.sel$age.chick-mean(chick.parent.data.behav.sel$age.chick))/sd(chick.parent.data.behav.sel$age.chick)
chick.parent.data.behav.sel$begging <- ifelse(chick.parent.data.behav.sel$behaviour.beg.chick=='beg',1,0)
chick.parent.data.behav.beg = chick.parent.data.behav.sel[is.na(chick.parent.data.behav.sel$behaviour.beg.chick)==F,] # only select data where behaviour could be classified with the begging RF model.
chick.parent.data.behav.beg$contact.begging = chick.parent.data.behav.beg$contact * chick.parent.data.behav.beg$begging
m.begging.contact <- glmer(contact.begging~sex.chick*sex.parent+sex.chick*z.age+sex.parent*z.age+(z.age|birdID.chick), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail')
summary(m.begging.contact)
modsel.begging.contact <- dredge(m.begging.contact)
# effect of chick sex is n.s., begging while in contact with parent decreases with age, chicks are more likely to beg at mother than at father, and begging probability decreases more strongly with age in fathers. 
m.begging.contact.pars <- glmer(contact.begging~sex.parent*z.age+(z.age|birdID.chick), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail')
summary(m.begging.contact.pars)
fixef.begging = fixef(m.begging.contact.pars)
ranef.begging = ranef(m.begging.contact.pars)$birdID.chick
names(ranef.begging)[1]='Intercept'
plot(z.age~Intercept, ranef.begging)
cor(ranef.begging$Intercept, ranef.begging$z.age) # this is very close to the estimated Corr by the model of 0.797
# plot individual relationships:
plot(c(-3,3), c(-3,3), type='n', xlab='z.age', ylab='logit prob beg')
for (i in 1:dim(ranef.begging)[1]) curve(ranef.begging$Intercept[i]+ranef.begging$z.age[i]*x, add=T)
TableS3 = make.table.from.dredge.output(modsel.begging.contact)
write.csv(TableS3, "output/TableS3 - Amount of begging at parent.csv")
emmeans(m.begging.contact.pars, pairwise~sex.parent)
emmeans.begging.contact.df <- as.data.frame(emmeans(m.begging.contact.pars, pairwise~sex.parent)$emmeans)
emmeans.begging.contact.df  <- cbind(emmeans.begging.contact.df[,c('sex.parent','df')], round(plogis(as.matrix(emmeans.begging.contact.df[,c('emmean','asymp.LCL','asymp.UCL')])),5))
emmeans.begging.contact.df

# probability of foraging while in contact:
chick.parent.data.behav.beg$contact.foraging = chick.parent.data.behav.beg$contact * chick.parent.data.behav.beg$foraging
m.foraging.contact <- glmer(contact.foraging~sex.chick*sex.parent+sex.chick*z.age+sex.parent*z.age+(z.age|birdID.chick), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail')
summary(m.foraging.contact)
modsel.foraging.contact <- dredge(m.foraging.contact)
m.foraging.contact.pars = glmer(contact.foraging~sex.chick*sex.parent+z.age+(z.age|birdID.chick), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail') 
TableS4 = make.table.from.dredge.output(modsel.foraging.contact)
write.csv(TableS4, "output/TableS4 - Amount of foraging with parent.csv")

# Alternatively, we could look at active behaviour on land versus active behaviour in foraging habitat while being in contact with a parent.
# This might be a solution to deal with the uncertainty in the classification of walking/foraging/begging.
# in which habitats did contact.begging take place?
table(chick.parent.data.behav.beg$habitat.simple[chick.parent.data.behav.beg$contact.begging==1]) # the vast majority took place on land!
table(chick.parent.data.behav.beg$habitat.simple[chick.parent.data.behav.beg$contact.foraging==1]) # half on land and half in foraging habitat. 
# active contact on land:
chick.parent.data.behav.beg$active.contact.land = ifelse(chick.parent.data.behav.beg$habitat.simple=='land' & chick.parent.data.behav.beg$behaviour.beg.chick!='resting' & chick.parent.data.behav.beg$contact==1,1,0)
chick.parent.data.behav.beg$active.contact.water = ifelse(chick.parent.data.behav.beg$habitat.simple!='land' & chick.parent.data.behav.beg$behaviour.beg.chick!='resting' & chick.parent.data.behav.beg$contact==1,1,0)
chick.parent.data.behav.beg$foraging.contact.water = ifelse(chick.parent.data.behav.beg$habitat.simple!='land' & chick.parent.data.behav.beg$behaviour.beg.chick=='foraging' & chick.parent.data.behav.beg$contact==1,1,0)
m.active.contact.land <- glmer(active.contact.land~sex.chick*sex.parent+sex.chick*z.age+sex.parent*z.age+(z.age|birdID.chick), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail')
summary(m.active.contact.land)
m.active.contact.water <- glmer(active.contact.water~sex.chick*sex.parent+sex.chick*z.age+sex.parent*z.age+(z.age|birdID.chick), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail')
summary(m.active.contact.water)
m.foraging.contact.water <- glmer(foraging.contact.water~sex.chick*sex.parent+sex.chick*z.age+sex.parent*z.age+(z.age|birdID.chick), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail')
summary(m.foraging.contact.water)
emmeans(m.foraging.contact.water, pairwise~sex.parent) # males are significantly less likely to have contact with their chick while they are foraging in foraging habitat. 
emmeans.foraging.contact.water.df <- as.data.frame(emmeans(m.foraging.contact.water, pairwise~sex.parent)$emmeans)
emmeans.foraging.contact.water.df  <- cbind(emmeans.foraging.contact.water.df[,c('sex.parent','df')], round(plogis(as.matrix(emmeans.foraging.contact.water.df[,c('emmean','asymp.LCL','asymp.UCL')])),5))
emmeans.foraging.contact.water.df

m.foraging.contact.water.sexp <- glmer(foraging.contact.water~sex.parent+(z.age|birdID.chick), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail')
summary(m.foraging.contact.water.sexp)

# check behaviours versus habitats:
table(chick.parent.data.behav.beg$behaviour.beg.chick, chick.parent.data.behav.beg$habitat.simple)

#save.image("data/processed/results.analyses.1118.RData")

load("data/processed/results.analyses.1118.RData")

# there appears to be a very strong year effect on the proportion of active contact. How does this affect the estimates?
chick.parent.data.behav.sel$yearf <- as.factor(chick.parent.data.behav.sel$birthyear)
m.active.contact.yr <- glmer(active.contact~z.age+yearf+(z.age|chick.parent), data=chick.parent.data.behav.sel, family='binomial', na.action='na.fail') # takes some minutes... 
AIC(m.active.contact.yr, m.active.contact.pars) # the model with year as fixed effect is better supported than the model without year.

# analysis of habitat use during active contact:
chick.parent.data.behav.sel
chick.parent.data.behav.sel$habitat.simple <- "marine"
chick.parent.data.behav.sel$habitat.simple[chick.parent.data.behav.sel$habitat%in%c("LM_land","Schier_Kwelder","Schier_Land_Rest","wal_rest_land")] <- "land"
chick.parent.data.behav.sel$habitat.simple[chick.parent.data.behav.sel$habitat%in%c("LM_zoet","Schier_Zoet")] <- "freshwater"

habitat.active.contacts <- chick.parent.data.behav.sel[chick.parent.data.behav.sel$active.contact==1,]
table(habitat.active.contacts$sex.parent)
table(habitat.active.contacts$habitat, habitat.active.contacts$sex.parent)
table(habitat.active.contacts$sex.parent, habitat.active.contacts$habitat.simple)

sexparent.habitat.activecontact = table(chick.parent.data.behav.sel$sex.parent, chick.parent.data.behav.sel$habitat.simple, chick.parent.data.behav.sel$active.contact)
round(sexparent.habitat.activecontact[1,,2]/sum(sexparent.habitat.activecontact[1,,2]),2) # prop. active contact of female parents
round(sexparent.habitat.activecontact[2,,2]/sum(sexparent.habitat.activecontact[2,,2]),2) # prop. active contact of male parents
# both males and females have very little active contact with their chick in freshwater habitat. Female parents have by far most active contact in marine habitat (66%) while male parents have similar amount of contact on land (46%) and in marine habitat (47%).  

# how does this look for all contact (i.e. also when birds are resting)?
sexparent.habitat.contact = table(chick.parent.data.behav.sel$sex.parent, chick.parent.data.behav.sel$habitat.simple, chick.parent.data.behav.sel$contact)
round(sexparent.habitat.contact[1,,2]/sum(sexparent.habitat.contact[1,,2]),2) # prop. overall contact of female parents
round(sexparent.habitat.contact[2,,2]/sum(sexparent.habitat.contact[2,,2]),2) # prop. overall contact of male parents
# around 70-75% of the contact of both fathers and mothers with their chick is on land, while females have about twice as much contact in marine habitats with their chick compared to males. 

# how does this look according to a chick's sex?
sexchick.habitat.activecontact = table(chick.parent.data.behav.sel$sex.chick, chick.parent.data.behav.sel$habitat.simple, chick.parent.data.behav.sel$active.contact)
round(sexchick.habitat.activecontact[1,,2]/sum(sexchick.habitat.activecontact[1,,2]),2) # prop. active contact of female chicks
round(sexchick.habitat.activecontact[2,,2]/sum(sexchick.habitat.activecontact[2,,2]),2) # prop. active contact of male chicks
# more or less similar figures for male and female chicks, though female chicks have slightly more active contact in marine habitat compared to male chicks who have slightly more contact in freshwater habitat. Active contact on land is similar. 

# These figures lump all data and do not account for individual differences. How can we properly analyse these data? 
table(chick.parent.data.behav.sel$habitat.simple)
chick.parent.data.behav.sel$freshwater = ifelse(chick.parent.data.behav.sel$habitat.simple=='freshwater',1,0)
chick.parent.data.behav.sel$marine = ifelse(chick.parent.data.behav.sel$habitat.simple=='marine',1,0)
chick.parent.data.behav.sel$land = ifelse(chick.parent.data.behav.sel$habitat.simple=='land',1,0)
chick.parent.data.behav.sel$begging = ifelse(chick.parent.data.behav.sel$behaviour.beg.chick=='beg',1,0)
chick.parent.data.behav.sel$foraging = ifelse(chick.parent.data.behav.sel$behaviour.beg.chick=='foraging',1,0)

# given that there is contact, in which habitat do these contacts take place, and does this depend on the chick and parent's sex?
# this addresses the relative habitat use during contact. Not sure if this is what we are interested in, given that mothers overall have more contact than fathers.
chick.parent.data.behav.contact = chick.parent.data.behav.sel[chick.parent.data.behav.sel$contact==1,]
m.contact.freshwater <- glmer(freshwater~sex.parent*sex.chick+(1|chick.parent), data=chick.parent.data.behav.contact, family='binomial', na.action='na.fail')
dredge(m.contact.freshwater) # no effect of sex of parent or chick on the probability of contact to take place in freshwater habitat. 
m.contact.marine <- glmer(marine~sex.parent*sex.chick+(1|chick.parent), data=chick.parent.data.behav.contact, family='binomial', na.action='na.fail')
dredge(m.contact.marine) # no effect of sex of parent or chick on the probability of contact to take place in marine habitat. 
m.contact.land <- glmer(land~sex.parent*sex.chick+(1|chick.parent), data=chick.parent.data.behav.contact, family='binomial', na.action='na.fail')
dredge(m.contact.land) # no effect of sex of parent or chick on the probability of contact to take place on land.

# given that there is (active) contact, in which habitat do these contacts take place, and does this depend on the chick and parent's sex?
chick.parent.data.behav.active.contact = chick.parent.data.behav.sel[chick.parent.data.behav.sel$active.contact==1,]
table(chick.parent.data.behav.active.contact$habitat.simple, chick.parent.data.behav.active.contact$sex.chick.parent)

# OVERALL contact:
m.contact.freshwater <- glmer(freshwater~sex.parent*sex.chick+(1|birdID.chick), data=chick.parent.data.behav.contact, family='binomial', na.action='na.fail')
dredge(m.contact.freshwater) # male parents are more likely to have contact with their chick in freshwater habitat compared to female parents.
m.contact.freshwater.pars <- glmer(freshwater~sex.parent+(1|birdID.chick), data=chick.parent.data.behav.contact, family='binomial', na.action='na.fail')
m.contact.freshwater.pars
m.contact.marine <- glmer(marine~sex.parent*sex.chick+(1|birdID.chick), data=chick.parent.data.behav.contact, family='binomial', na.action='na.fail')
dredge(m.contact.marine)
summary(m.contact.marine) # full model is most parsim.
emmeans(m.contact.marine, pairwise~sex.chick*sex.parent) # the only significant pairwise difference is that male chicks are more likely to have contact in marine habitat with their mother than with their father.
emmeans(m.contact.marine, pairwise~sex.parent) # overall, fathers are less likely to have contact with their chick in marine habitat than mothers. 
m.contact.land <- glmer(land~sex.parent*sex.chick+(1|birdID.chick), data=chick.parent.data.behav.contact, family='binomial', na.action='na.fail')
dredge(m.contact.land)
summary(m.contact.land) # male parents are less likely to have contact with their chick on land than female parents, particularly when their chick is a female.  
emmeans(m.contact.land, pairwise~sex.chick*sex.parent) # this is confirmed by the pairwise comparisons:
# chicks, irrespective of their sex, are significantly more likely to have contact on land with their mother than with their father.
# female chicks tend to have less contact on land with their father than male chicks. 

# ACTIVE contact:
m.active.contact.freshwater <- glmer(freshwater~sex.parent*sex.chick+(1|birdID.chick), data=chick.parent.data.behav.active.contact, family='binomial', na.action='na.fail')
dredge(m.active.contact.freshwater) # no effect of chick or parent sex on active contact in freshwater. 
m.active.contact.marine <- glmer(marine~sex.parent*sex.chick+(1|birdID.chick), data=chick.parent.data.behav.active.contact, family='binomial', na.action='na.fail')
dredge(m.active.contact.marine) # no effect of chick or parent sex on active contact in marine habitat. 
m.active.contact.land <- glmer(land~sex.parent*sex.chick+(1|birdID.chick), data=chick.parent.data.behav.active.contact, family='binomial', na.action='na.fail')
dredge(m.active.contact.land) # no effect of chick or parent sex on active contact on land.
# no significant differences depending on sex of chick or parent on the amount of active contact in different habitats.
# this could be due to the low sample size and difficulty to 'detect' active contact on the basis of the selection criterion of 10 m distance between the closest datetime stamps (which are never exactly synchronized) of chick and parent. Ideally, you'd want to 'interpolate' the trajectories between the 10 min locations to create for example locations with 1 sec intervals, and then estimate whether the distance for any of these 'interpolated' locations would be <10 m.  


# does contact take place during day or night?
contact.hour.sex = table(chick.parent.data.behav.contact$sex.chick.parent, hour(chick.parent.data.behav.contact$datetime.chick))
calculate.proportions <- function(x) x/sum(x)
contact.hour.sex.prop = round(apply(contact.hour.sex,1,calculate.proportions),3)
contact.hour.sex.prop = t(contact.hour.sex.prop)
rowSums(contact.hour.sex.prop)
windows()
plot(0:23,contact.hour.sex.prop[1,], type='l',ylim=c(0,max(contact.hour.sex.prop)), col='red', xlab= "Hour of the day", ylab="Proportion of the contact that takes place during this hour")
lines(0:23,contact.hour.sex.prop[2,], col='orange')
lines(0:23,contact.hour.sex.prop[3,], col='green')
lines(0:23,contact.hour.sex.prop[4,], col='blue')

# does ACTIVE contact take place during day or night?
active.contact.hour.sex = table(chick.parent.data.behav.active.contact$sex.chick.parent, hour(chick.parent.data.behav.active.contact$datetime.chick))
calculate.proportions <- function(x) x/sum(x)
active.contact.hour.sex.prop = round(apply(active.contact.hour.sex,1,calculate.proportions),3)
active.contact.hour.sex.prop = t(active.contact.hour.sex.prop)
rowSums(active.contact.hour.sex.prop)
windows()
plot(0:23,active.contact.hour.sex.prop[1,], type='l',ylim=c(0,max(active.contact.hour.sex.prop)), col='red', xlab= "Hour of the day", ylab="Proportion of the active contact that takes place during this hour")
lines(0:23,active.contact.hour.sex.prop[2,], col='orange')
lines(0:23,active.contact.hour.sex.prop[3,], col='green')
lines(0:23,active.contact.hour.sex.prop[4,], col='blue')

# overall chick behaviour and behaviour during contact:

# analyse the probability of begging:
lme.beg = glmer(begging~z.age*sex.chick+z.age*sex.parent+sex.chick*sex.parent+(z.age|birdID.chick), chick.parent.data.behav.beg, family='binomial', na.action='na.fail')
modsel.beg = dredge(lme.beg) # also with AIC, the model with age is better supported. 
# what if we only include age?
lme.beg.age = glmer(begging~z.age+(z.age|birdID.chick), chick.parent.data.behav.beg, family='binomial', na.action='na.fail')
summary(lme.beg.age) # n.s. 
lme.beg.age.chickrnd = glmer(begging~z.age+(1|birdID.chick), chick.parent.data.behav.beg, family='binomial', na.action='na.fail')
summary(lme.beg.age.chickrnd) # p=0.03; positive slope, increase in begging with age. 

# the problem with looking at the overall probability of begging is that begging is often confused with foraging, and as birds are foraging when getting older, also the false-positive cases of begging are increasing. This can be solved by focussing on the actual contact moments:
# analyse probability of begging during contact moments:
contact.behav.beg = na.omit(chick.parent.data.behav.beg[chick.parent.data.behav.beg$contact==1,c('birdID.chick','age.chick','z.age','begging','foraging')])
m.contact.beg.chickrnd = glmer(begging~z.age+(1|birdID.chick), contact.behav.beg, family='binomial', na.action='na.fail')
dredge(m.contact.beg.chickrnd) # the age effect is not supported. 
m.contact.beg.chickagernd = glmer(begging~z.age+(z.age|birdID.chick), contact.behav.beg, family='binomial', na.action='na.fail')
dredge(m.contact.beg.chickagernd) # some (but limited) support for a negative effect of age
summary(m.contact.beg.chickagernd) # p=0.10

# analyse probability of begging during active contact moments:
active.contact.behav.beg = na.omit(chick.parent.data.behav.beg[chick.parent.data.behav.beg$active.contact==1,c('birdID.chick','age.chick','z.age','sex.chick','sex.parent','begging','foraging')])
m.active.contact.beg.chickagernd = glmer(begging~z.age+(z.age|birdID.chick), active.contact.behav.beg, family='binomial', na.action='na.fail') # boundary fit...
modsel.active.contact = dredge(m.active.contact.beg.chickagernd)
summary(m.active.contact.beg.chickagernd) # negative age effect is supported (p=0.006).

#save.image("data/processed/results.analyses.1118.RData")


# data from 110 days onward strongly biased by 6315, who is still begging a lot...:
# think this was the year where the Westerplas dried out, with a lot of dying fish to be fed on. So maybe 6315 was also foraging in the Westerplas... 
active.contact.oldies <- active.contact[active.contact$age5>105,]
table(active.contact.oldies$behaviour.beg.chick, active.contact.oldies$birdID.chick)
data.old.6315 <- active.contact[active.contact$birdID.chick=='6315'&active.contact$age5>105,]
mapviewOptions(fgb = FALSE)
mapview(data.old.6315[data.old.6315$behaviour.beg.chick=='beg',], map.types="Esri.WorldImagery")
# this almost all occurs at an island in the Westerplas, strongly suggesting this is indeed begging. 

# plot locations during contact on interactive map:
# make an interactive map to check for the locations in the sea
mapview(chick.parent.data.behav.sel[chick.parent.data.behav.sel$behaviour.chick.contact=='active1',], map.types="Esri.WorldImagery")
mapview(chick.parent.data.behav.sel[chick.parent.data.behav.sel$behaviour.chick.contact=='flying1',], map.types="Esri.WorldImagery")
mapview(chick.parent.data.behav.sel[chick.parent.data.behav.sel$behaviour.chick.contact=='resting1',], map.types="Esri.WorldImagery")

# plot locations during contact >12,5 km from the colony
distant.contacts = chick.parent.data.behav.sel[chick.parent.data.behav.sel$contact==1 & chick.parent.data.behav.sel$dist.nest>12500,]
mapview(distant.contacts, map.types="Esri.WorldImagery")

# distance to colony ~ age during contact moments:















### BELOW CODE IS NOT USED - CONSIDER REMOVING ###

# contact decreases almost linearly with age until 97 days old, after that, there is a small 'revival' of contact, shortly before migrating south. 
# what if we only analyse the linear part?
chick.parent.data.contact.linear = chick.parent.data.contact.sel[chick.parent.data.contact.sel$age.chick<98,]
# first, ignore random effect:
glm.contact.lin <- glm(contact~sex.parent*sex.chick+sex.parent*z.age+sex.chick*z.age, data=chick.parent.data.contact.linear, family='binomial', na.action='na.fail')
modsel.glm.contact.lin = dredge(glm.contact.lin) # also full model best supported...
glmer.contact.lin <- glmer(contact~sex.parent*sex.chick+sex.parent*z.age+sex.chick*z.age+(1|birdID.chick), data=chick.parent.data.contact.linear, family='binomial', na.action='na.fail')
summary(glmer.contact.lin)
glmer.mod.sel.contact.lin = dredge(glmer.contact.lin) 
glmer.mod.sel.contact.lin # still full model is best-supported
library(emmeans)
emmeans(glmer.contact.lin, ~sex.parent*sex.chick+sex.parent*z.age+sex.chick*z.age)

chick.parent.data.contact.sel$predicted.contact <- predict(m.contact)
windows(12.8)
ggplot(data = chick.parent.data.contact.sel,
       aes(x = age.chick, y = predicted.contact, color=sex.parent)) +
  geom_point() +
  facet_wrap(~sex.chick.parent, scales = "fixed") +
  labs(x = "Chick age", y = "Proportion of contact") +
  theme_minimal()

# do hatch date and length of post-fledging period / timing of departure correlate? 
chick.age.departure = unique(chick.parent.data.sel[,c('birdID.chick','start_deployment','age_deployment','departure.datetime.chick')])
chick.age.departure$hatch_date = chick.age.departure$start_deployment - chick.age.departure$age_deployment
chick.age.departure$departure_day = yday(chick.age.departure$departure.datetime.chick)
chick.age.departure$hatch_day = yday(chick.age.departure$hatch_date)
chick.age.departure$age_departure = chick.age.departure$departure_day - chick.age.departure$hatch_day
windows(6,4)
plot(age_departure~hatch_day, na.omit(chick.age.departure), pch=21, bg='grey', cex=2, ylim=c(105,142)) # despite small sample size, this relationship is VERY significant!
summary(lm(age_departure~hatch_day, na.omit(chick.age.departure))) # p<0.001; however, the relationship is less than -1, implying that every day a bird is born later, it departs at 0.77 days younger age. Therefore, there could still be a correlation between hatch day and departure day:
windows(6,4)
plot(departure_day~hatch_day, na.omit(chick.age.departure), pch=21, bg='grey', cex=2)
summary(lm(departure_day~hatch_day, na.omit(chick.age.departure))) # p=0.07; nearly significant...
### remove above part? ###
