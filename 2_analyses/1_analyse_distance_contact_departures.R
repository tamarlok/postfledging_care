# is there a difference in first-year survival between GPS-tagged and colour-ringed juveniles?
# survival GPS-tagged: 0.41 +/- 0.12 SE
mean1 = 0.41
se1 = 0.12
df1 = 16
# survival colour-ringed (Lok et al. 2013): 0.56 +/- 0.04
mean2=0.56
se2=0.04
# use a two-sample t-test
# t = (mean1 - mean2)/(sqrt(se1^2+se2^2))
t_value = (mean1 - mean2)/(sqrt(se1^2+se2^2))
# As sample size for mean2 is unknown (derived from large colour-ring dataset), we assume large sample sizes, and approximate the test with a normal distribution (Z-test):
p_value <- 2 * (1 - pnorm(abs(t_value)))
p_value

# add habitat.simple:
chick.parent.data.behav.sel$habitat.simple <- "unknown"
chick.parent.data.behav.sel$habitat.simple[chick.parent.data.behav.sel$habitat%in%c("LM_land","Schier_Kwelder","Schier_Land_Rest","wal_rest_land")] <- "land"
chick.parent.data.behav.sel$habitat.simple[chick.parent.data.behav.sel$habitat%in%c("LM_zoet","Schier_Zoet")] <- "freshwater"
chick.parent.data.behav.sel$habitat.simple[chick.parent.data.behav.sel$habitat%in%c("waddenzee","Schier_brak")] <- "marine"
chick.parent.data.behav.sel$habitat[is.na(chick.parent.data.behav.sel$habitat)]='unknown'
table(chick.parent.data.behav.sel$habitat.simple)

# what is the max age of the chick where there is still matched data between parent and chick:
aggregate(age.chick~chick.parent, chick.parent.data.contact, max) # three cases where data ends when chick is between 80 and 90 days old 
table(chick.parent.data.contact$contact) # this datafile also contains the data without contact.

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
# create a column with categorical year:
chick.parent.data.contact.sel$yearf <- as.factor(chick.parent.data.contact.sel$birthyear)

### 1. TOTAL AMOUNT OF CONTACT ###

# compare support for different random effect structures, using the full model structure for the fixed effects:
#m.contact.chickrnd <- glmer(contact~sex.parent+sex.chick+sex.parent*z.age+sex.chick*z.age+yearf+(1|birdID.chick), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail') # takes some minutes...
m.contact.chickagernd <- glmer(contact~sex.parent+sex.chick+z.age+yearf+(z.age|birdID.chick), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail') # takes some minutes...
#m.contact.prntrnd <- glmer(contact~sex.parent+sex.chick+sex.parent*z.age+sex.chick*z.age+yearf+(1|birdID.parent), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail')
#m.contact.prntagernd <- glmer(contact~sex.parent+sex.chick+sex.parent*z.age+sex.chick*z.age+yearf+(z.age|birdID.parent), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail')
chick.parent.data.contact.sel$chick.parent = as.factor(chick.parent.data.contact.sel$chick.parent)
#m.contact.chickprntrnd <- glmer(contact~sex.parent+sex.chick+sex.parent*z.age+sex.chick*z.age+yearf+(1|chick.parent), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail') 
m.contact.chickprntagernd <- glmer(contact~sex.parent+sex.chick+yearf+(z.age|chick.parent), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail') 
#AIC(m.contact.chickrnd, m.contact.chickagernd, m.contact.prntrnd, m.contact.prntagernd, m.contact.chickprntrnd, m.contact.chickprntagernd)
AIC(m.contact.chickagernd, m.contact.chickprntagernd)
summary(m.contact.chickagernd)

# Even though the model with age as random slope varying among chick-parent pairs is best-supported, this model ignores the fact that for two chicks, we have data of both parents. When using chick-parent pair is random effect, this is ignored, while if using chickID as random effect, it is not. 
# However, as a result, results might be particularly affected by the differences between the father and mother for these two chicks. 
# Nevertheless, we decided to continue with chickID as random effect, but award some discussion on how results would change if we used chick-parent pair instead. (although this also causes some convergence issues)

# check whether the global model meets the model assumptions:

# (1) homogeneity of variances
# (2) overdispersion?

# check for over- or underdispersion of the data:
simOutput.m.contact.chickagernd <- simulateResiduals(m.contact.chickagernd, plot=F)
windows()
plot(simOutput.m.contact.chickagernd)
testUniformity(simOutput.m.contact.chickagernd)
# the two plots plotted separately:
windows()
plotQQunif(simOutput.m.contact.chickagernd) # although the KS-test is highly significant, the QQ plot looks very neat, suggesting no serious deviations between observed and predicted residuals. The significance may be caused by the large sample size.  
# testOutliers(simOutput.m.contact.chickagernd, type='bootstrap') # alternative (and more robust) way of calculating estimates for "Outlier test". Only needed if warning is produced by plotQQunif.
windows()
testOutliers(simOutput.m.contact.chickagernd, type='binomial') # this produces the estimates shown in the figure produced by plotQQunif. 
windows()
hist(residuals(simOutput.m.contact.chickagernd), breaks=10*5)
windows()
plotResiduals(simOutput.m.contact.chickagernd)

summary(m.contact.chickagernd) # sigma intercept: 0.75, meaning that a "typical" range encompassing 95% of the variation in the probability of post-fledging parental care among chicks (at log-scale) would be about 4σ = 4*0.75 = 3, which, except for the year2017 effect, is much larger than the estimated fixed effects. 
# the same is true for the age-slope effect. See: https://stats.stackexchange.com/questions/153611/interpreting-random-effect-variance-in-glmer

modelsel.contact = dredge(m.contact.chickagernd, fixed="yearf")
Table1a = make.table.from.dredge.output(modelsel.contact)
m.contact.parsim <- glmer(contact~sex.parent+z.age+yearf+(z.age|birdID.chick), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail')

curmonthday = paste(month(Sys.Date()), sprintf("%02d", day(Sys.Date())), sep="")
write.csv(Table1a, paste("output/Table1a - Proportion of overall contact ", curmonthday, ".csv", sep=""))

# reporting estimates of global model, using the broom.mixed package
# https://stats.stackexchange.com/questions/623851/reporting-results-from-a-glmer/623857#623857
m.contact.tidy = tidy(m.contact.chickagernd)[,1:5]
write.csv(m.contact.tidy, paste("output/TableS1 - Global model estimates overall contact ", curmonthday, ".csv", sep=""))


emmeans.overall.contact = emmeans(m.contact.chickagernd, pairwise~sex.chick*sex.parent)
emmeans.overall.contact.df <- as.data.frame(emmeans.overall.contact$emmeans)
emmeans.overall.contact.df  <- cbind(emmeans.overall.contact.df[,c('sex.chick','sex.parent','df')], round(plogis(as.matrix(emmeans.overall.contact.df[,c('emmean','asymp.LCL','asymp.UCL')])),3))
emmeans.overall.contact.df
emmeans.overall.contact
emmeans.overall.contact.chicksex = emmeans(m.contact.chickagernd, pairwise~sex.chick)
emmeans.overall.contact.chicksex.df = as.data.frame(emmeans.overall.contact.chicksex$emmeans)
cbind(emmeans.overall.contact.chicksex.df[,c('sex.chick','df')], round(plogis(as.matrix(emmeans.overall.contact.chicksex.df[,c('emmean','asymp.LCL','asymp.UCL')])),3))
emmeans.overall.contact.chicksex

### 2. PROBABILITY OF CONTACT WHILE CHICK IS BEGGING OR FORAGING (in foraging habitat) ###

# make the same selection for chick.parent.data.behav.sel in relation to ages analysed:
chick.parent.data.behav.sel$departure.datetime.chick[is.na(chick.parent.data.behav.sel$departure.datetime.chick)] = dmy("01/01/3000")
chick.parent.data.behav.sel$departure.datetime.parent[is.na(chick.parent.data.behav.sel$departure.datetime.parent)] = dmy("01/01/3000")
chick.parent.data.behav.sel <- chick.parent.data.behav.sel[which(chick.parent.data.behav.sel$age.chick>=35 & chick.parent.data.behav.sel$age.chick<=136 & chick.parent.data.behav.sel$datetime.chick < chick.parent.data.behav.sel$departure.datetime.chick & chick.parent.data.behav.sel$datetime.chick < chick.parent.data.behav.sel$departure.datetime.parent),] # start analysis at the same age for all chicks. As the oldest chicks were tagged at 35 days, start at age 35. 

### Analyse the probability of begging (i.e. classified as begging while being in contact)
chick.parent.data.behav.sel$z.age = (chick.parent.data.behav.sel$age.chick-mean(chick.parent.data.behav.sel$age.chick))/sd(chick.parent.data.behav.sel$age.chick)
chick.parent.data.behav.sel$yearf <- as.factor(chick.parent.data.behav.sel$birthyear)
chick.parent.data.behav.sel$begging <- ifelse(chick.parent.data.behav.sel$behaviour.beg.chick=='beg',1,0)
chick.parent.data.behav.sel$foraging <- ifelse(chick.parent.data.behav.sel$behaviour.beg.chick=='foraging',1,0)
chick.parent.data.behav.beg = chick.parent.data.behav.sel[is.na(chick.parent.data.behav.sel$behaviour.beg.chick)==F,] # only select data where behaviour could be classified with the begging RF model.
chick.parent.data.behav.beg$contact.begging = chick.parent.data.behav.beg$contact * chick.parent.data.behav.beg$begging


m.begging.contact <- glmer(contact.begging~sex.parent+sex.chick+z.age+yearf+(z.age|birdID.chick), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail')
modsel.begging.contact <- dredge(m.begging.contact, fixed="yearf")
Table1b = make.table.from.dredge.output(modsel.begging.contact)
write.csv(Table1b, paste("output/Table1b - Proportion of begging contact ", curmonthday, ".csv", sep=""))
m.begging.contact.parsim <- glmer(contact.begging~sex.parent+z.age+yearf+(z.age|birdID.chick), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail')

# reporting estimates of global model for begging at parent
m.begging.contact.tidy = tidy(m.begging.contact)[,1:5]
write.csv(m.begging.contact.tidy, paste("output/TableS2 - Global model estimates begging contact ", curmonthday, ".csv", sep=""))

# probability of foraging while in contact and in foraging habitat:
chick.parent.data.behav.beg$contact.foraging = chick.parent.data.behav.beg$contact * chick.parent.data.behav.beg$foraging
chick.parent.data.behav.beg$contact.foraging[chick.parent.data.behav.beg$habitat.simple%in%c('land','unknown')]=0
table(chick.parent.data.behav.beg$contact.foraging)
m.foraging.contact <- glmer(contact.foraging~sex.parent+sex.chick+z.age+yearf+(z.age|birdID.chick), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail') 
summary(m.foraging.contact)
# some convergence issues.
modsel.foraging.contact <- dredge(m.foraging.contact, fixed='yearf')
Table1c = make.table.from.dredge.output(modsel.foraging.contact)
write.csv(Table1c, paste("output/Table1c - Proportion of foraging contact ", curmonthday, ".csv", sep=""))
m.foraging.contact.parsim <- glmer(contact.foraging~sex.parent+z.age+yearf+(z.age|birdID.chick), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail') 

# reporting estimates of global model for foraging with parent (in foraging habitat)
m.foraging.contact.tidy = tidy(m.foraging.contact)[,1:5]
write.csv(m.foraging.contact.tidy, paste("output/TableS3 - Global model estimates foraging contact ", curmonthday, ".csv", sep=""))

save.image("data/processed/results.analyses.0117.RData")
#load("data/processed/results.analyses.0117.RData")

# As can be seen, there are many cases where chicks (and adults) are classified to be foraging on land, while this must be walking or begging. This is 
# Alternatively, we could look at active behaviour on land versus active behaviour in foraging habitat while being in contact with a parent.
# This might be a solution to deal with the uncertainty in the classification of walking/foraging/begging.
# in which habitats did contact.begging take place?
table(chick.parent.data.behav.beg$habitat.simple[chick.parent.data.behav.beg$contact.begging==1]) # the vast majority took place on land!
table(chick.parent.data.behav.beg$habitat.simple[chick.parent.data.behav.beg$contact.foraging==1]) # half on land and half in foraging habitat. 
# active contact on land:
chick.parent.data.behav.beg$active.contact.land = ifelse(chick.parent.data.behav.beg$habitat.simple=='land' & chick.parent.data.behav.beg$behaviour.beg.chick!='resting' & chick.parent.data.behav.beg$contact==1,1,0)
chick.parent.data.behav.beg$active.contact.water = ifelse(chick.parent.data.behav.beg$habitat.simple!='land' & chick.parent.data.behav.beg$behaviour.beg.chick!='resting' & chick.parent.data.behav.beg$contact==1,1,0)
chick.parent.data.behav.beg$foraging.contact.water = ifelse(chick.parent.data.behav.beg$habitat.simple!='land' & chick.parent.data.behav.beg$behaviour.beg.chick=='foraging' & chick.parent.data.behav.beg$contact==1,1,0)
m.active.contact.land <- glmer(active.contact.land~sex.parent+sex.chick+z.age+(z.age|birdID.chick), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail')
summary(m.active.contact.land)
m.active.contact.water <- glmer(active.contact.water~sex.parent+sex.chick+z.age+(z.age|birdID.chick), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail')
summary(m.active.contact.water)
m.foraging.contact.water <- glmer(foraging.contact.water~sex.parent+sex.chick+z.age+(z.age|birdID.chick), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail')
summary(m.foraging.contact.water)
emmeans(m.foraging.contact.water, pairwise~sex.parent) # males are significantly less likely to have contact with their chick while they are foraging in foraging habitat. 
emmeans.foraging.contact.water.df <- as.data.frame(emmeans(m.foraging.contact.water, pairwise~sex.parent)$emmeans)
emmeans.foraging.contact.water.df  <- cbind(emmeans.foraging.contact.water.df[,c('sex.parent','df')], round(plogis(as.matrix(emmeans.foraging.contact.water.df[,c('emmean','asymp.LCL','asymp.UCL')])),5))
emmeans.foraging.contact.water.df

m.foraging.contact.water.sexp <- glmer(foraging.contact.water~sex.parent+(z.age|birdID.chick), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail')
summary(m.foraging.contact.water.sexp)

# check behaviours versus habitats:
table(chick.parent.data.behav.beg$behaviour.beg.chick, chick.parent.data.behav.beg$habitat.simple)

# how does distance from nest during contact relate to chick age (and - potentially to chick sex and parent sex)?
# only select data during which there was contact:
chick.parent.data.contact$dist.nest.chick = chick.parent.data.contact$dist.nest
chick.data.actual.contact = chick.parent.data.contact[chick.parent.data.contact$contact==1,]
chick.data.actual.contact$z.age = (chick.data.actual.contact$age.chick-mean(chick.data.actual.contact$age.chick))/sd(chick.data.actual.contact$age.chick)

# As distance to the nest is bounded to 0, and not normally distributed, we used a Gamma distribution.
hist(chick.data.actual.contact$dist.nest.chick, breaks=50)
chick.data.actual.contact$dist.nest.chick.km = chick.data.actual.contact$dist.nest.chick/1000
chick.data.actual.contact$dist.nest.chick.km <- ifelse(chick.data.actual.contact$dist.nest.chick.km <= 0, 1e-3, chick.data.actual.contact$dist.nest.chick.km)
m.dist.nest.gamma = glmer(dist.nest.chick.km~sex.parent+sex.chick+z.age+z.age.sq+(z.age|birdID.chick), chick.data.actual.contact, na.action='na.fail', family=Gamma(link='log'))
chick.data.actual.contact$log.dist.nest.chick = log(chick.data.actual.contact$dist.nest.chick.km)
hist(chick.data.actual.contact$log.dist.nest.chick) # that results in a pretty normal distribution
plot(log.dist.nest.chick~age.chick, chick.data.actual.contact)
chick.data.actual.contact$z.age.sq = chick.data.actual.contact$z.age^2
m.dist.nest = lmer(log.dist.nest.chick~sex.parent+sex.chick+z.age+z.age.sq+(z.age|birdID.chick), chick.data.actual.contact, na.action='na.fail')
# test whether model assumptions are met:
# Simulate residuals for the Gamma model
sim_res <- simulateResiduals(fittedModel = m.dist.nest)
# Plot residuals
plot(sim_res)
fitted_vals <- fitted(m.dist.nest)
residuals_vals <- residuals(m.dist.nest, type = "pearson")

# Plot residuals vs. fitted values
plot(fitted_vals, residuals_vals,
     xlab = "Fitted Values",
     ylab = "Pearson Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

modsel.dist.nest = dredge(m.dist.nest)
Table1d = make.table.from.dredge.output(modsel.dist.nest)
write.csv(Table1d, paste("output/Table1d - Distance to nest during contact ", curmonthday, ".csv", sep=""))

# reporting estimates of global model for distance to nest during contact
m.dist.nest.tidy = tidy(m.dist.nest)[,1:5]
write.csv(m.dist.nest.tidy, paste("output/TableS4 - Global model estimates distance to nest during contact ", curmonthday, ".csv", sep=""))