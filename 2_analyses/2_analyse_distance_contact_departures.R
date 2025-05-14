# add habitat.simple:
chick.parent.data.behav.sel$habitat.simple <- "unknown"
chick.parent.data.behav.sel$habitat.simple[chick.parent.data.behav.sel$habitat%in%c("LM_land","Schier_Kwelder","Schier_Land_Rest","wal_rest_land")] <- "land"
chick.parent.data.behav.sel$habitat.simple[chick.parent.data.behav.sel$habitat%in%c("LM_zoet","Schier_Zoet")] <- "freshwater"
chick.parent.data.behav.sel$habitat.simple[chick.parent.data.behav.sel$habitat%in%c("waddenzee","Schier_brak")] <- "marine"
chick.parent.data.behav.sel$habitat[is.na(chick.parent.data.behav.sel$habitat)]='unknown'

# max age with good quality data:
aggregate(age.chick~chick.parent+sex.chick+sex.parent, chick.parent.data.contact, max)

# what is the maximum chick age when there was still contact between parent and chick in the selected data?
max.age.contact.sex = aggregate(age.chick~chick.parent+sex.chick+sex.parent, chick.parent.data.contact[chick.parent.data.contact$contact==1,], max) # here, contact is defined as a distance of less than 10m between chick and parent.
max(max.age.contact.sex$age.chick) # 136 days. Therefore, we run the analysis until 136 days
median(max.age.contact.sex$age.chick)
range(max.age.contact.sex$age.chick)

# what is the interval between last contact and departure day of chick and parent?
chick.parent.data.contact$days.until.chick.departure = date(chick.parent.data.contact$departure.datetime.chick)-date(chick.parent.data.contact$datetime.chick)
chick.parent.data.contact$days.until.chick.departure[chick.parent.data.contact$days.until.chick.departure>300]=NA
chick.parent.data.contact$days.until.parent.departure = date(chick.parent.data.contact$departure.datetime.parent)-date(chick.parent.data.contact$datetime.parent)

dates.last.contacts = unique(merge(chick.parent.data.contact, max.age.contact.sex)[,c('birdID.chick','birdID.parent','chick.parent','age.chick','days.until.chick.departure','days.until.parent.departure')])
median(na.omit(dates.last.contacts$days.until.chick.departure))
range(na.omit(dates.last.contacts$days.until.chick.departure))
median(na.omit(dates.last.contacts$days.until.parent.departure))
range(na.omit(dates.last.contacts$days.until.parent.departure))

### 1. TOTAL AMOUNT OF CONTACT ###

# For this, select data of chicks between 35 and 136 days old:
chick.parent.data.contact.sel <- chick.parent.data.contact[which(chick.parent.data.contact$age.chick>=35 & chick.parent.data.contact$age.chick<=136),] 

# check of how many chicks we have data per age:
chick.data.age = table(chick.parent.data.contact.sel$age.chick, chick.parent.data.contact.sel$birdID.chick)
chick.data.age.yesno = ifelse(chick.data.age>0,1,0)
rowSums(chick.data.age.yesno) # until 90 days, there are still 12 chicks in the dataset; until 100 days 8 chicks. 
chick.data.age.yesno[56:66,]
# the four chicks that drop out between 90 and 100 days, are 6296 (son with father), 6301 (daughter with mother), 6304 (son with father), and 6374.1 (son with mother). 6358.1 (daughter with father) and 6381 (son with father) dropped out earlier already.  

# standardize chick age so that model convergence gets easier:
chick.parent.data.contact.sel$z.age = (chick.parent.data.contact.sel$age.chick-mean(chick.parent.data.contact.sel$age.chick))/sd(chick.parent.data.contact.sel$age.chick)
# create a column with categorical year:
chick.parent.data.contact.sel$yearf <- as.factor(chick.parent.data.contact.sel$birthyear)
table(chick.parent.data.contact.sel$contact)

# run full model:
m.contact.chickagernd <- glmer(contact~sex.parent+sex.chick+z.age+yearf+(z.age|birdID.chick), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail', control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))) # this gives the same result as (1+z.age|birdID.chick) as random effect

simOutput.m.contact.chickagernd <- simulateResiduals(m.contact.chickagernd, plot=F)
plot(simOutput.m.contact.chickagernd) # although the KS-test is highly significant (likely due to the very large sample size), the QQ plot looks very neat, suggesting no serious deviations between observed and predicted residuals. 
# Dispersion test is N.S. -> no overdispersion
testZeroInflation(simOutput.m.contact.chickagernd) # N.S.
# testTemporalAutocorrelation(simOutput.m.contact.chickagernd, time = chick.parent.data.contact.sel$datetime.chick)

summary(m.contact.chickagernd) # sigma intercept: 0.75, meaning that a "typical" range encompassing 95% of the variation in the probability of post-fledging parental care among chicks (at log-scale) would be about 4σ = 4*0.75 = 3, which, except for the year2017 effect, is much larger than the estimated fixed effects. 
# the same is true for the age-slope effect. See: https://stats.stackexchange.com/questions/153611/interpreting-random-effect-variance-in-glmer

modelsel.contact = dredge(m.contact.chickagernd, fixed="yearf")
Table1a = make.table.from.dredge.output(modelsel.contact)
m.contact.parsim <- glmer(contact~sex.parent+z.age+yearf+(z.age|birdID.chick), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail')

curmonthday = paste(month(Sys.Date()), sprintf("%02d", day(Sys.Date())), sep="")
write.csv(Table1a, paste("output/Table1a - Proportion of overall contact.csv", sep=""))

# reporting estimates of global model, using the broom.mixed package
# https://stats.stackexchange.com/questions/623851/reporting-results-from-a-glmer/623857#623857
m.contact.tidy = tidy(m.contact.chickagernd)[,1:5]
write.csv(m.contact.tidy, paste("output/TableS3 - Global model estimates overall contact.csv", sep=""))

### 2. PROBABILITY OF CONTACT WHILE CHICK IS BEGGING ###

table(chick.parent.data.behav.sel$nlocs.behav) # only days with at least 100 joint GPS fixes with behavioural classification of the chick are used

# make the same selection for chick.parent.data.behav.sel in relation to ages analysed:
chick.parent.data.behav.sel <- chick.parent.data.behav.sel[which(chick.parent.data.behav.sel$age.chick>=35 & chick.parent.data.behav.sel$age.chick<=136),] 
chick.parent.data.behav.sel$z.age = (chick.parent.data.behav.sel$age.chick-mean(chick.parent.data.behav.sel$age.chick))/sd(chick.parent.data.behav.sel$age.chick)
chick.parent.data.behav.sel$yearf <- as.factor(chick.parent.data.behav.sel$birthyear)
chick.parent.data.behav.sel$begging <- ifelse(chick.parent.data.behav.sel$behaviour.chick=='beg',1,0)
chick.parent.data.behav.sel$contact.begging = chick.parent.data.behav.sel$contact * chick.parent.data.behav.sel$begging
table(chick.parent.data.behav.sel$contact.begging)

m.begging.contact <- glmer(contact.begging~sex.parent+sex.chick+z.age+yearf+(z.age|birdID.chick), data=chick.parent.data.behav.sel, family='binomial', na.action='na.fail', control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
simOutput.m.begging.contact <- simulateResiduals(m.begging.contact, plot=F)
plot(simOutput.m.begging.contact) # why do I get a warning here, whereas not with the 'overall contact' model on:
# DHARMa:testOutliers with type = binomial may have inflated Type I error rates for integer-valued distributions. To get a more exact result, it is recommended to re-run testOutliers with type = 'bootstrap'. See ?testOutliers for details
testZeroInflation(simOutput.m.begging.contact) # N.S.

derivs <- m.begging.contact@optinfo$derivs
max(abs(derivs$gradient))

modsel.begging.contact <- dredge(m.begging.contact, fixed="yearf")
Table1b = make.table.from.dredge.output(modsel.begging.contact)
write.csv(Table1b, paste("output/Table1b - Proportion of begging contact.csv", sep=""))
m.begging.contact.parsim <- glmer(contact.begging~sex.parent+z.age+yearf+(z.age|birdID.chick), data=chick.parent.data.behav.sel, family='binomial', na.action='na.fail')

# reporting estimates of global model for begging at parent
m.begging.contact.tidy = tidy(m.begging.contact)[,1:5]
write.csv(m.begging.contact.tidy, paste("output/TableS4 - Global model estimates begging contact.csv", sep=""))

### 3. PROBABILITY OF CONTACT WHILE CHICK IS FORAGING (in foraging habitat) ###

chick.parent.data.behav.sel$foraging <- ifelse(chick.parent.data.behav.sel$behaviour.chick=='foraging',1,0)
chick.parent.data.behav.sel$contact.foraging = chick.parent.data.behav.sel$contact * chick.parent.data.behav.sel$foraging
chick.parent.data.behav.sel$contact.foraging[chick.parent.data.behav.sel$habitat.simple%in%c('land','unknown')]=0
table(chick.parent.data.behav.sel$contact.foraging)
m.foraging.contact <- glmer(contact.foraging~sex.parent+sex.chick+z.age+yearf+(z.age|birdID.chick), data=chick.parent.data.behav.sel, family='binomial', na.action='na.fail', control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))) 
summary(m.foraging.contact)
modsel.foraging.contact <- dredge(m.foraging.contact, fixed='yearf')
Table1c = make.table.from.dredge.output(modsel.foraging.contact)
write.csv(Table1c, paste("output/Table1c - Proportion of foraging contact.csv", sep=""))
m.foraging.contact.parsim <- glmer(contact.foraging~sex.parent+z.age+yearf+(z.age|birdID.chick), data=chick.parent.data.behav.sel, family='binomial', na.action='na.fail', control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# reporting estimates of global model for foraging with parent (in foraging habitat)
m.foraging.contact.tidy = tidy(m.foraging.contact)[,1:5]
write.csv(m.foraging.contact.tidy, paste("output/TableS5 - Global model estimates foraging contact.csv", sep=""))

### 4. DISTANCE TO THE NEST DURING CONTACT ###

# only select data during which there was contact:
chick.parent.data.contact.sel$dist.nest.chick = chick.parent.data.contact.sel$dist.nest
chick.data.actual.contact = chick.parent.data.contact.sel[chick.parent.data.contact.sel$contact==1,]

# As distance to the nest is bounded to 0, and not normally distributed, we used a Gamma distribution. For this, we make the zero distances very small distances. 
hist(chick.data.actual.contact$dist.nest.chick, breaks=50)
chick.data.actual.contact$dist.nest.chick.km = chick.data.actual.contact$dist.nest.chick/1000
chick.data.actual.contact$dist.nest.chick.km <- ifelse(chick.data.actual.contact$dist.nest.chick.km <= 0, 1e-3, chick.data.actual.contact$dist.nest.chick.km)
hist(log(chick.data.actual.contact$dist.nest.chick.km)) # taking the log gives a pretty normal distribution, suggesting that a Gamma dsitribution with a log-link function is appropriate
m.dist.nest.gamma = glmer(dist.nest.chick.km~sex.parent+sex.chick+z.age+(z.age|birdID.chick), chick.data.actual.contact, na.action='na.fail', family=Gamma(link='log'))
# visually check for homogeneity of residuals
sim_res <- simulateResiduals(fittedModel = m.dist.nest.gamma)
# Plot residuals
windows()
plot(sim_res)
# Although everything is significant, the plots look reasonably OK. 
# But let's check if log-transformed values giving better results:
m.dist.nest.log = lmer(log(dist.nest.chick.km)~sex.parent+sex.chick+z.age+(z.age|birdID.chick), chick.data.actual.contact, na.action='na.fail')
sim_res <- simulateResiduals(fittedModel = m.dist.nest.log)
plot(sim_res)
# slightly, but not really, therefore, we stick to the Gamma distribution. 

modsel.dist.nest = dredge(m.dist.nest.gamma)
Table1d = make.table.from.dredge.output(modsel.dist.nest)
write.csv(Table1d, paste("output/Table1d - Distance to nest during contact.csv", sep=""))

m.dist.nest.parsim.gamma = glmer(dist.nest.chick.km~z.age+sex.parent+(z.age|birdID.chick), chick.data.actual.contact, na.action='na.fail', family=Gamma(link='log'))

# reporting estimates of global model for distance to nest during contact
m.dist.nest.tidy = tidy(m.dist.nest.gamma)[,1:5]
write.csv(m.dist.nest.tidy, paste("output/TableS6 - Global model estimates distance to nest during contact.csv", sep=""))

### RE-DO ANALYSES WITH 50 M CONTACT DISTANCE ###

chick.parent.data.contact.sel$contact50m = ifelse(chick.parent.data.contact.sel$distance<50,1,0)

# calculate the total number of contact moments when using 10 or 50 m:
sum(chick.parent.data.contact.sel$contact) # total: 8694
table(chick.parent.data.contact.sel$contact, chick.parent.data.contact.sel$chick.parent)
rowMeans(table(chick.parent.data.contact.sel$contact, chick.parent.data.contact.sel$chick.parent))[2] # mean number of contacts per chick-parent pair: 543
sum(chick.parent.data.contact.sel$contact50m) # total: 19717
table(chick.parent.data.contact.sel$contact50m, chick.parent.data.contact.sel$chick.parent)
rowMeans(table(chick.parent.data.contact.sel$contact50m, chick.parent.data.contact.sel$chick.parent))[2] # 1232

### 1. TOTAL AMOUNT OF CONTACT 50 M
m.contact50m.chickagernd <- glmer(contact50m~sex.parent+sex.chick+z.age+yearf+(z.age|birdID.chick), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail', control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(m.contact50m.chickagernd)

model.sel.contact50m.chickagernd = dredge(m.contact50m.chickagernd, fixed="yearf")
TableS6a = make.table.from.dredge.output(model.sel.contact50m.chickagernd)
write.csv(TableS6a, paste("output/TableS7a - Proportion of overall contact 50m.csv", sep=""))

# run the model that was the most parsimonious when using the 10m contact distance:
m.contact50m.parsim <- glmer(contact50m~sex.parent+z.age+yearf+(z.age|birdID.chick), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail')

### 2. AMOUNT OF BEGGING CONTACT 50 M
chick.parent.data.behav.sel$contact50m = ifelse(chick.parent.data.behav.sel$distance<50,1,0)
chick.parent.data.behav.sel$contact50m.begging = chick.parent.data.behav.sel$contact50m * chick.parent.data.behav.sel$begging

m.begging.contact50m <- glmer(contact50m.begging~sex.parent+sex.chick+z.age+yearf+(z.age|birdID.chick), data=chick.parent.data.behav.sel, family='binomial', na.action='na.fail', control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
model.sel.beggingcontact50m = dredge(m.begging.contact50m, fixed="yearf")
TableS6b = make.table.from.dredge.output(model.sel.beggingcontact50m)
write.csv(TableS6b, paste("output/TableS7b - Proportion of begging contact 50m.csv", sep=""))

### 3. AMOUNT OF FORAGING CONTACT 50 M
chick.parent.data.behav.sel$contact50m.foraging = chick.parent.data.behav.sel$contact50m * chick.parent.data.behav.sel$foraging

m.foraging.contact50m <- glmer(contact50m.foraging~sex.parent+sex.chick+z.age+yearf+(z.age|birdID.chick), data=chick.parent.data.behav.sel, family='binomial', na.action='na.fail', control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(m.foraging.contact50m)
# based on this, it seems that only chick x age is not supported:
m.foraging.contact50m.pars <- glmer(contact50m.foraging~sex.parent+yearf+(z.age|birdID.chick), data=chick.parent.data.behav.sel, family='binomial', na.action='na.fail')
summary(m.foraging.contact50m.pars)

model.sel.foragingcontact50m = dredge(m.foraging.contact50m, fixed="yearf")
TableS6c = make.table.from.dredge.output(model.sel.foragingcontact50m)
write.csv(TableS6c, paste("output/TableS7c - Proportion of foraging contact 50m.csv", sep=""))

### 4. DISTANCE DURING CONTACT 50 M
chick.data.actual.contact.50m = chick.parent.data.contact.sel[chick.parent.data.contact.sel$contact50m==1,]
chick.data.actual.contact.50m$dist.nest.chick.km = chick.data.actual.contact.50m$dist.nest.chick/1000
chick.data.actual.contact.50m$dist.nest.chick.km <- ifelse(chick.data.actual.contact.50m$dist.nest.chick.km <= 0, 1e-3, chick.data.actual.contact.50m$dist.nest.chick.km)
m.dist.nest.50m.gamma = glmer(dist.nest.chick.km~sex.parent+sex.chick+z.age+(z.age|birdID.chick), chick.data.actual.contact.50m, na.action='na.fail', family=Gamma(link='log'))
modsel.dist.nest.50m = dredge(m.dist.nest.50m.gamma)
TableS6d = make.table.from.dredge.output(modsel.dist.nest.50m)
write.csv(TableS6d, paste("output/TableS7d - Distance to nest during contact 50m.csv", sep=""))


### RE-DO ANALYSES WITH CHICK-PARENT AS RANDOM EFFECT

# (1) TOTAL AMOUNT OF CONTACT
m.contact.chkprntagernd <- glmer(contact~sex.parent+sex.chick+z.age+yearf+(z.age|chick.parent), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail', control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))) 
model.sel.contact.chkprntagernd <- dredge(m.contact.chkprntagernd, fixed='yearf')
TableS8a.chkprntagernd = make.table.from.dredge.output(model.sel.contact.chkprntagernd)
write.csv(TableS8a.chkprntagernd, "output/TableS8a - chkprntagernd - Proportion of overall contact.csv")

# (2) BEGGING CONTACT 
m.begging.chkprntagernd <- glmer(contact.begging~sex.parent+sex.chick+z.age+yearf+(z.age|chick.parent), data=chick.parent.data.behav.sel, family='binomial', na.action='na.fail', control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))) # convergence issues
modsel.begging.chkprntagernd <- dredge(m.begging.chkprntagernd, fixed='yearf') # three models with convergence issues, but not the full model. Perhaps models where z.age is not a fixed effect while a random slope is included. 
TableS8b.chkprntagernd = make.table.from.dredge.output(modsel.begging.chkprntagernd)
write.csv(TableS8b.chkprntagernd, "output/TableS8b - chkprntagernd - Proportion of begging contact.csv")

# (3) FORAGING CONTACT
m.foraging.chkprntagernd <- glmer(contact.foraging~sex.parent+sex.chick+z.age+yearf+(z.age|chick.parent), data=chick.parent.data.behav.sel, family='binomial', na.action='na.fail', control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
modsel.foraging.chkprntagernd <- dredge(m.foraging.chkprntagernd, fixed='yearf')
TableS8c.chkprntagernd = make.table.from.dredge.output(modsel.foraging.chkprntagernd)
write.csv(TableS8c.chkprntagernd, "output/TableS8c - chkprntagernd - Proportion of foraging contact.csv")

# (4) DISTANCE TO NEST DURING CONTACT
m.dist.nest.chkprntagernd = glmer(dist.nest.chick.km~sex.parent+sex.chick+z.age+(z.age|chick.parent), chick.data.actual.contact, na.action='na.fail', family=Gamma(link='log'))
modsel.dist.nest.chkprntagernd = dredge(m.dist.nest.chkprntagernd)
TableS8d.chkprntagernd = make.table.from.dredge.output(modsel.dist.nest.chkprntagernd)
write.csv(TableS8d.chkprntagernd, "output/TableS8d - chkprntagernd - Distance to nest during contact.csv")


