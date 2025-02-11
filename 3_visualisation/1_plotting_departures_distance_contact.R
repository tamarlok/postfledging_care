chick.parent.data.contact$dist.nest.chick = chick.parent.data.contact$dist.nest
chick.parent.data.contact.sel$dist.nest.chick = chick.parent.data.contact.sel$dist.nest # can be removed when entire script has been rerun

# plot distance between parents and their chick in relation to age, and coloured according to sex of chick and parent:
windows(12,8)
ggplot(data = chick.parent.data.contact,
       aes(x = age.chick, y = distance.corr, color=sex.chick.parent))+ # use distance.corr here, to also include data where no data is available for the parent because it departed on migration while it did not 
  geom_line()+
  facet_wrap(~chick.parent, scales = "fixed")+
  ylim(0,100000)
# same but now only plotting chick-parent distances until 50 km
windows(12,8)
ggplot(data = chick.parent.data.contact[chick.parent.data.contact$distance<50000,],
       aes(x = age.chick, y = distance, color=sex.chick.parent))+
  geom_line()+
  facet_wrap(~chick.parent, scales = "fixed")

# plot distance of chick from nest location in relation to age:
windows(12,8)
ggplot(data = chick.parent.data.contact,
       aes(x = age.chick, y = dist.nest.chick, color=sex.chick))+
  geom_line()+
  facet_wrap(~chick.parent, scales = "fixed")
# same but now until 100 km:
windows(12,8)
ggplot(data = chick.parent.data.contact[chick.parent.data.contact$dist.nest.chick<100000,],
       aes(x = age.chick, y = dist.nest.chick, color=sex.chick)) +
  geom_line()+
  facet_wrap(~chick.parent, scales = "fixed")+
  ylim(0,100000)
# same but now until 25 km (cutting off the "serious" explorative flights):
windows(12,8)
ggplot(data = chick.parent.data.contact[chick.parent.data.contact$dist.nest.chick<100000,],
       aes(x = age.chick, y = dist.nest.chick, color=sex.chick)) +
  geom_line()+
  facet_wrap(~chick.parent, scales = "fixed")+
  ylim(0,25000)

# plot distance of parent to the nest 
chick.parent.data.contact$dist.nest.parent = round(distGeo(as.matrix(cbind(chick.parent.data.contact$longitude.parent, chick.parent.data.contact$latitude.parent)),
                                                             as.matrix(cbind(chick.parent.data.contact$lon.nest, chick.parent.data.contact$lat.nest))),0)
# plot data until 25 km:
windows(12,8)
ggplot(data = chick.parent.data.contact[chick.parent.data.contact$dist.nest.parent<100000,],
       aes(x = age.chick, y = dist.nest.parent/1000, color=sex.parent)) +
  geom_line()+
  facet_wrap(~chick.parent, scales = "fixed")+
  ylim(0,25)

# plot distance to nest during contact:
windows(12,8)
ggplot(data = chick.data.actual.contact,
       aes(x = age.chick, y = dist.nest.chick, color=sex.chick.parent)) +
  geom_point()+
  facet_wrap(~chick.parent, scales = "fixed")+
  ggtitle("Distance from nest during contact")+
  xlab("Age of chick (days)")+
  ylab("Distance to nest (m)")+
  ylim(0,25000)
# this was never more than 18 km (but because of the criterion of at least 130 joint GPS locations, the short contact between 6295 & 6288 during southward migration (at Slikken Flakkee) was excluded...)

aggregate(contact~age.chick, chick.parent.data.contact[chick.parent.data.contact$chick.parent=='6295-6288',], sum)

### FIGURE S4 ###
# Plot distance to the nest of chicks and parents and add dots at the moments where the chick was in contact with the parent
# For this plot, use the raw data, so that the departure on autumn migration is also visible. This is the file chick.parent.data.sel 
chick.parent.data.sel = chick.parent.data.sel[order(chick.parent.data.sel$birthyear, chick.parent.data.sel$birdID.chick),]
chick.parent.data.sel$dist.nest.parent = round(distGeo(as.matrix(cbind(chick.parent.data.sel$longitude.parent, chick.parent.data.sel$latitude.parent)),
                                                           as.matrix(cbind(chick.parent.data.sel$lon.nest, chick.parent.data.sel$lat.nest))),0)
chick.parent.data.sel$dist.nest.chick = round(distGeo(as.matrix(cbind(chick.parent.data.sel$longitude.chick, chick.parent.data.sel$latitude.chick)),
                                                       as.matrix(cbind(chick.parent.data.sel$lon.nest, chick.parent.data.sel$lat.nest))),0)

windows(16,8)
layout(matrix(1:16, nrow=4, byrow=T))
par(mar=c(1,1,0,0), oma=c(4,4,1,1))
panelnr=0
ymax=50000
for (i in unique(chick.parent.data.contact$chick.parent)) { # but only for the chick-parent pairs with enough data:
  panelnr=panelnr+1
  sex.parent.n = ifelse(chick.parent.data.sel$sex.parent[chick.parent.data.sel$chick.parent==i][1]=='f',1,2)
  plot(dist.nest.parent~age.chick, data = chick.parent.data.sel[chick.parent.data.sel$chick.parent==i,], xlim=c(35,140), ylim=c(0,ymax), type='l', xlab="", ylab="", xaxt='n', yaxt='n', col=c('orange','lightskyblue')[sex.parent.n])
  lines(dist.nest.chick~age.chick, data = chick.parent.data.sel[chick.parent.data.sel$chick.parent==i,])
  # overlay a dashed line for the parents again, so that the similar departure of 6295 and 6288 is visible as well as a similar long-distance flight by 6354-6298 (both >120d, so only overlay this part so that for the rest, the line of the chick is better visible:
  lines(dist.nest.parent~age.chick, data = chick.parent.data.sel[chick.parent.data.sel$chick.parent==i&chick.parent.data.sel$age.chick>120,], col=c('orange','lightskyblue')[sex.parent.n], lty='dashed')
  points(dist.nest.chick~age.chick, data = chick.parent.data.sel[chick.parent.data.sel$chick.parent==i&chick.parent.data.sel$contact==1,], col='red')
  if (panelnr %in% c(1,5,9,13)) axis(2, at=seq(0,ymax,5000), labels=seq(0,ymax,5000)/1000, las=1)
  if (panelnr %in% 13:16) axis(1, at=seq(40,140,10), las=1)
  text(35,0.95*ymax,i, adj=0)
}
mtext("Distance to the nest (km)",2,2,outer=T)
mtext("Chick age (days)",1,2,outer=T)
### END FIGURE S4 ###

max(chick.data.actual.contact$dist.nest.chick)/1000 # max distance to nest during contact was 17,3 km. 

contact.chickparentpair.age = table(chick.data.actual.contact$chick.parent, chick.data.actual.contact$age.chick)
contact.chickparentpair.age[,(dim(contact.chickparentpair.age)[2]-1):dim(contact.chickparentpair.age)[2]]
# at which age was the last contact per chick-parent pair?
max.age.contact.chkprnt = aggregate(age.chick~chick.parent, chick.data.actual.contact, max)
median(max.age.contact.chkprnt$age.chick)

# average distance to nest during contact per chick age pooled per 5 days. 
# for this, we use the dataset cut at chick age = 35-90 days
chick.parent.data.contact.sel = chick.parent.data.contact.sel[order(chick.parent.data.contact.sel$birthyear, chick.parent.data.contact.sel$birdID.chick, chick.parent.data.contact.sel$birdID.parent, chick.parent.data.contact.sel$age.chick),]
chick.parent.data.contact.sel$age5.chick = round((chick.parent.data.contact.sel$age.chick+2)/5,0)*5-2 # bin age into 5-day classes (36-40=38, 41-45=43 etc.). We add age 35 to the 36-40 class (=38)
chick.parent.data.contact.sel$age5.chick[chick.parent.data.contact.sel$age.chick==35]=38
unique(chick.parent.data.contact.sel[order(chick.parent.data.contact.sel$age.chick),c('age.chick','age5.chick')])
mean.distance.to.nest <- aggregate(dist.nest.chick~age5.chick, chick.parent.data.contact.sel, mean)

data.contacts <- aggregate(cbind(freq,contact)~birdID.chick+birdID.parent+chick.parent+age.chick+sex.chick+sex.parent+sex.chick.parent+yday+birthyear, chick.parent.data.contact.sel, sum)
data.contacts <- data.contacts[order(data.contacts$birthyear, data.contacts$birdID.chick, data.contacts$yday),]
data.contacts$prop.contact <- data.contacts$contact/data.contacts$freq
windows(12,8)
ggplot(data = data.contacts,
       aes(x = age.chick, y = prop.contact, color=sex.chick.parent)) +
  geom_point() +
  facet_wrap(~chick.parent, scales = "fixed")

# 
windows(12.8)
ggplot(data = data.contacts,
       aes(x = age.chick, y = prop.contact, color=sex.chick.parent)) +
  stat_summary(fun = mean, geom = "point", size = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
  facet_wrap(~factor(chick.parent, levels=c('6295-6289','6299-6284','6301-6285','6354-6298',
                                            '6304-6291','6315-6289','6374.1-6284.2','6385-6291',
                                            '6292-6287','6295-6288','6302-6067','6358.1-6066',
                                            '6296-656','6298.1-6118','6304-763','6381-6358'))) +
  labs(x = "Chick age", y = "Proportion of contact") +
  xlim(35,136)+
  theme_bw()
# is 6292 wel het kuiken van 6287? Aan de track te zien zat 6287 wel in dezelfde kolonie; hij kwam nog terug in de kolonie totdat 6292 besloot om te verhuizen naar de open vlakte. Het lijkt alsof 6287 toen zijn kind kwijt is geraakt. 6292 had ook nog een oudere broer of zus. Misschien had 6287 die onder zijn hoede? Of het is gewoon een slechte vader...

### Figure 2 ###
# pooling all data (irrespective of sex of the chick and its parent)

windows()
ggplot(data = data.contacts[data.contacts$age.chick<=90,],
       aes(x=age.chick, y = prop.contact)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
  labs(x = "Chick age", y = "Proportion of contact") +
  xlim(35,90)+
  theme_bw()
### End Figure 2 ###

# now manually calculate means and SE, until age 90:
# I used chatGPT to verify how to calculate 95% CI for proportion data. To avoid the 95% CI to get below 0 or above 1, the (1) Clopper-Pearson Exact Confidence Interval or the (2) Wilson Score Interval or (3) bootstrapping can be used.
# These are calculated as follows:
# (1) Clopper-Pearson Exact Confidence Interval
proportions = data.contacts$prop.contact
# Total number of observations and successes
n <- length(proportions)
x <- sum(proportions)  # Sum of proportions represents "successes"
# Exact binomial confidence interval
result <- binom.test(x = round(x), n = n, conf.level = 0.95)
print(result$conf.int)
# (2) Wilson Score Confidence Interval
result <- prop.test(x = round(x), n = n, conf.level = 0.95, correct = FALSE)
print(result$conf.int)
# (3) Bootstrapping
library(boot)
# Function to calculate the mean
mean_func <- function(data, indices) {
  mean(data[indices])
}
# Perform bootstrap
set.seed(123)  # For reproducibility
boot_result <- boot(data = proportions, statistic = mean_func, R = 10000)
# Calculate 95% CI
boot_ci <- boot.ci(boot_result, type = "perc")
print(boot_ci) # produces a narrower 95% CI than the other methods... 

# The first two methods produce nearly identical 95% CI, so I tend to opt for one of these methods. As Wilson Score is shorter, I choose this one :).
# Bootstrapping takes quite a long time...
calculate.Wilson.score.interval <- function(x) {
  n <- length(x)
  xsum <- sum(x)  # Sum of proportions represents "successes"
  result <- prop.test(x = round(xsum), n = n, conf.level = 0.95, correct = FALSE)
  result$conf.int
}

calculate.bootstrap.interval <- function(x) {
  boot_result <- boot(data = x, statistic = mean_func, R = 10000)
  boot_ci <- boot.ci(boot_result, type = "perc")
  boot_ci$percent
}

# the below code only works when sample size per age class is at least two, otherwise it produces an error. 
prop.contact.age = ddply(data.contacts[data.contacts$age.chick<100,], .(age.chick), summarize, 
                         N=length(prop.contact), mean = mean(prop.contact), 
                         CImin.Wilson = calculate.Wilson.score.interval(prop.contact)[1], CImax.Wilson = calculate.Wilson.score.interval(prop.contact)[2], 
                         CImin.boot = calculate.bootstrap.interval(prop.contact)[4], CImax.boot = calculate.bootstrap.interval(prop.contact)[5])
# warnings are produced by too low sample size to get reliable estimates of Wilson CI. 
# only show estimates based on at least 10 different chicks:
prop.contact.age.sel = prop.contact.age[prop.contact.age$N>=10,]
plotCI(prop.contact.age.sel$age.chick, prop.contact.age.sel$mean, li=prop.contact.age.sel$CImin.Wilson, ui=prop.contact.age.sel$CImax.Wilson, 
       sfrac=0, ylim=c(0,0.4))
plotCI(prop.contact.age.sel$age.chick+0.1, prop.contact.age.sel$mean, li=prop.contact.age.sel$CImin.boot, ui=prop.contact.age.sel$CImax.boot, 
       sfrac=0, add=T, col='red')
# as with the Wilson's CI, you would really conclude there is no significant decline in proportion of contact, while with the bootstrap CI there is. So this appears to be a more realistic CI. 

plotCI(prop.contact.age.sel$age.chick, prop.contact.age.sel$mean, li=prop.contact.age.sel$CImin.boot, ui=prop.contact.age.sel$CImax.boot, 
       sfrac=0, xlim=c(35,92), ylim=c(0,0.25), pch=21, pt.bg='grey', xlab='Chick age (days)', ylab='Proportion of contact')
# The model (with chickID as random effect) indicates that there is a significant effect of parent sex on the amount of contact. So perhaps also plot this difference?
prop.contact.age.sexp = ddply(data.contacts[data.contacts$age.chick<100,], .(age.chick, sex.parent), summarize, 
                              N=length(prop.contact), mean = mean(prop.contact), CImin.Wilson = calculate.Wilson.score.interval(prop.contact)[1], 
                         CImin.boot = calculate.bootstrap.interval(prop.contact)[4], CImax.boot = calculate.bootstrap.interval(prop.contact)[5])
prop.contact.age.sexp[prop.contact.age.sexp$age.chick>89,]
# only show estimates based on at least 6 different chicks:
prop.contact.age.mothers = prop.contact.age.sexp[prop.contact.age.sexp$sex.parent=='f' & prop.contact.age.sexp$N>5,]
prop.contact.age.fathers = prop.contact.age.sexp[prop.contact.age.sexp$sex.parent=='m' & prop.contact.age.sexp$N>5,]
plotCI(prop.contact.age.mothers$age.chick-0.07, prop.contact.age.mothers$mean, li=prop.contact.age.mothers$CImin.boot, ui=prop.contact.age.mothers$CImax.boot, sfrac=0, xlim=c(35,92), ylim=c(0,0.3), pch=21, pt.bg="lightcoral", col='coral4')
plotCI(prop.contact.age.fathers$age.chick+0.07, prop.contact.age.fathers$mean, li=prop.contact.age.fathers$CImin.boot, ui=prop.contact.age.fathers$CImax.boot, sfrac=0, pch=21, pt.bg="lightskyblue", col='lightskyblue4', add=T)
# plotting the two sexes separately in the same panel becomes quite messy... 

# Make population-level predictions on proportion contact based on the most parsimonious model  
# with weighted average over the three years, based on the number of chicks in the different years (7 in 2016, 4 in 2017 and 3 in 2018; 14 in total):
real.age = 35:136
z.age = (real.age-mean(chick.parent.data.contact.sel$age.chick))/sd(chick.parent.data.contact.sel$age.chick)
pred.age.sexp = expand.grid(sex.parent=c("f","m"), real.age = real.age)
pred.age.sexp$z.age = (real.age-mean(chick.parent.data.contact.sel$age.chick))/sd(chick.parent.data.contact.sel$age.chick)
summary(m.contact.parsim)
mcfix = fixef(m.contact.parsim) 
predicted.prop.pf = plogis(mcfix[1]+4/14*mcfix[4]+3/14*mcfix[5] + mcfix[3]*z.age)
predicted.prop.pm = plogis(mcfix[1]+4/14*mcfix[4]+3/14*mcfix[5] + mcfix[2] + mcfix[3]*z.age)

chick.parent.data.contact.sel$pred.contact = predict(m.contact.parsim, newdata=chick.parent.data.contact.sel)
pred.data = expand.grid(age.chick=30:100, sex.parent=c('f','m'), yearf=factor(2016:2018))
pred.data$z.age = (pred.data$age.chick-mean(pred.data$age.chick))/sd(pred.data$age.chick)
pred.data$pred.contact.pop = predict(m.contact.parsim, newdata=pred.data, re.form=NA, type='response')
pred.data[pred.data$age.chick==40,]
pred.data$pred.contact.weight = pred.data$pred.contact.pop*(7/14) # weight in 2016
pred.data$pred.contact.weight[pred.data$yearf==2017] = pred.data$pred.contact.pop[pred.data$yearf==2017]*(4/14) # weight in 2017
pred.data$pred.contact.weight[pred.data$yearf==2018] = pred.data$pred.contact.pop[pred.data$yearf==2018]*(3/14) # weight in 2018
pred.data$freq = 1
# sum up pred.weight to get the weighted prediction over the three years:
weighted.pred.pop = aggregate(cbind(pred.contact.weight, freq)~age.chick+sex.parent, pred.data, sum)
# averaged over males and females:
mean.pred.pop = aggregate(pred.contact.weight~age.chick, weighted.pred.pop, mean)
mean.pred.pop[mean.pred.pop$age.chick==40,] # 8.4%
mean.pred.pop[mean.pred.pop$age.chick==90,] # 2.6%
# calculate the mean begging contact probability:
pred.data$pred.begcontact.pop = predict(m.begging.contact.parsim, newdata=pred.data, re.form=NA, type='response')
pred.data$pred.begcontact.weight = pred.data$pred.begcontact.pop*(7/14) # weight in 2016
pred.data$pred.begcontact.weight[pred.data$yearf==2017] = pred.data$pred.begcontact.pop[pred.data$yearf==2017]*(4/14) # weight in 2017
pred.data$pred.begcontact.weight[pred.data$yearf==2018] = pred.data$pred.begcontact.pop[pred.data$yearf==2018]*(3/14) # weight in 2018
# sum up pred.weight to get the weighted prediction over the three years:
weighted.pred.begcontact.pop = aggregate(cbind(pred.begcontact.weight, freq)~age.chick+sex.parent, pred.data, sum)
# averaged over males and females:
mean.pred.begcontact.pop = aggregate(pred.begcontact.weight~age.chick, weighted.pred.begcontact.pop, mean)
mean.pred.begcontact.pop[mean.pred.begcontact.pop$age.chick==40,] # 0.3% 
mean.pred.begcontact.pop[mean.pred.begcontact.pop$age.chick==90,] # 0.06%
# % of contact time that the chick is begging:
(mean.pred.begcontact.pop$pred.begcontact.weight/mean.pred.pop$pred.contact.weight)[mean.pred.pop$age.chick==40] # 4.0%
(mean.pred.begcontact.pop$pred.begcontact.weight/mean.pred.pop$pred.contact.weight)[mean.pred.pop$age.chick==90] # 2.5%
# averaged over all ages
mean(mean.pred.begcontact.pop$pred.begcontact.weight/mean.pred.pop$pred.contact.weight) # 3.2%



windows()
plotCI(prop.contact.age.mothers$age.chick-0.07, prop.contact.age.mothers$mean, li=prop.contact.age.mothers$CImin.boot, ui=prop.contact.age.mothers$CImax.boot, sfrac=0, xlim=c(35,xmax), ylim=c(0,ymax), pch=21, pt.bg="lightcoral", col='coral4', xlab="", ylab='', xaxt='n', las=1)
lines(pred.contact.pop~age.chick, pred.data[pred.data$sex.parent=='f' & pred.data$yearf==2016,], col='lightcoral')
lines(pred.contact.pop~age.chick, pred.data[pred.data$sex.parent=='f' & pred.data$yearf==2017,], col='lightcoral')
lines(pred.contact.pop~age.chick, pred.data[pred.data$sex.parent=='f' & pred.data$yearf==2018,], col='lightcoral')
lines(pred.contact.weight~age.chick, weighted.pred.pop[weighted.pred.pop$sex.parent=='f',], col='lightcoral', lwd=2) 
lines(real.age, predicted.prop.pf, lwd=2, col='coral4')
# I had expected that the two weighted population-level predictions, either using the fixed effect estimates or the prediction function while ignoring the random effects, would give the same result. But they don't...
# As the lightcoral line (using the prediction function) fits the data much better, I think this one is correct. 
plotCI(prop.contact.age.fathers$age.chick+0.07, prop.contact.age.fathers$mean, li=prop.contact.age.fathers$CImin.boot, ui=prop.contact.age.fathers$CImax.boot, sfrac=0, xlim=c(35,92), ylim=c(0,0.3), pch=21, pt.bg="lightskyblue", col='lightskyblue4', xlab="", ylab="", xaxt='n', yaxt='n')
lines(pred.contact.pop~age.chick, pred.data[pred.data$sex.parent=='m' & pred.data$yearf==2016,], col='lightskyblue')
lines(pred.contact.pop~age.chick, pred.data[pred.data$sex.parent=='m' & pred.data$yearf==2017,], col='lightskyblue')
lines(pred.contact.pop~age.chick, pred.data[pred.data$sex.parent=='m' & pred.data$yearf==2018,], col='lightskyblue')
lines(pred.contact.weight~age.chick, weighted.pred.pop[weighted.pred.pop$sex.parent=='m',], col='lightskyblue', lwd=2) 
lines(real.age, predicted.prop.pm, lwd=2, col='lightskyblue4')

# Make the same calculations for the distance to the nest during contact:
calculate.mean.dist.km = function(x) round(mean(x)/1000,2)
mean.distance.chick.age.sexp = aggregate(dist.nest.chick~birdID.chick+age.chick+sex.parent, chick.data.actual.contact, calculate.mean.dist.km)
calculate.se <- function(x) {
  N = length(x)
  sd = sd(x)
  se = sd/sqrt(N)
  se
}
distance.contact.age.sexp = ddply(mean.distance.chick.age.sexp[mean.distance.chick.age.sexp$age.chick<100,], .(age.chick, sex.parent), summarize, 
                                  N=length(dist.nest.chick), mean = mean(dist.nest.chick), se = calculate.se(dist.nest.chick))
# and predictions from the most parsim model:
hist(chick.data.actual.contact$dist.nest.chick) # this is definitely not normally distributed; poisson distribution might be preferred. 
m.dist.nest.parsim = lmer(dist.nest.chick/1000~sex.parent+z.age+(z.age|birdID.chick), chick.data.actual.contact, na.action='na.fail', REML=T)
m.dist.nest.log.parsim = lmer(log.dist.nest.chick~sex.parent+z.age+(z.age|birdID.chick), chick.data.actual.contact, na.action='na.fail', REML=T)
m.dist.nest.parsim.gamma = glmer(dist.nest.chick.km~sex.parent+z.age+(z.age|birdID.chick), chick.data.actual.contact, na.action='na.fail', family=Gamma(link='log'))
pred.data$pred.dist.pop = predict(m.dist.nest.parsim.gamma, newdata=pred.data, re.form=NA, type='response')
# remove the year effect, as not relevant for the distance:
pred.data.dist = unique(pred.data[,c('age.chick','sex.parent','pred.dist.pop')])
plotCI(distance.contact.age.mothers$age.chick, distance.contact.age.mothers$mean, uiw=1.96*distance.contact.age.mothers$se, sfrac=0, xlab='', ylab='', pch=21, pt.bg='lightcoral', col='coral4', las=1, xlim=c(35,xmax), ylim=c(0,13))
lines(pred.dist.pop~age.chick, pred.data.dist[pred.data.dist$sex.parent=='f',], col='lightcoral', lwd=2) 
plotCI(distance.contact.age.fathers$age.chick, distance.contact.age.fathers$mean, uiw=1.96*distance.contact.age.fathers$se, sfrac=0, xlab='', ylab='', pch=21, pt.bg='lightskyblue', col='lightskyblue4', las=1, xlim=c(35,xmax), ylim=c(0,13), yaxt='n')
lines(pred.dist.pop~age.chick, pred.data.dist[pred.data.dist$sex.parent=='m',], col='lightskyblue', lwd=2) 


### FIGURE 2 ###
windows(12,8)
layout(matrix(1:4, ncol=2, byrow=T))
par(mar=c(1,1,0,0), oma=c(4,4,2,1), xaxs='i')
## (A) Overall probability of contact with the mother
ymax=0.3
xmax=91
plotCI(prop.contact.age.mothers$age.chick-0.07, prop.contact.age.mothers$mean, li=prop.contact.age.mothers$CImin.boot, ui=prop.contact.age.mothers$CImax.boot, sfrac=0, xlim=c(35,xmax), ylim=c(0,ymax), pch=21, pt.bg="lightcoral", col='coral4', xlab="", ylab='', xaxt='n', las=1)
lines(pred.contact.weight~age.chick, weighted.pred.pop[weighted.pred.pop$sex.parent=='f',], col='lightcoral') 
mtext("with mother",line=0.2)
mtext("Proportion of contact",2,3.5)
axis(1,at=seq(40,90,10),labels=F)
text(xmax-2,0.985*ymax,"(a)")
## (B) Overall probability of contact with the father
plotCI(prop.contact.age.fathers$age.chick+0.07, prop.contact.age.fathers$mean, li=prop.contact.age.fathers$CImin.boot, ui=prop.contact.age.fathers$CImax.boot, sfrac=0, xlim=c(35,92), ylim=c(0,0.3), pch=21, pt.bg="lightskyblue", col='lightskyblue4', xlab="", ylab="", xaxt='n', yaxt='n')
lines(pred.contact.weight~age.chick, weighted.pred.pop[weighted.pred.pop$sex.parent=='m',], col='lightskyblue') 
mtext("with father", line=0.2)
axis(1,at=seq(40,90,10),labels=F)
axis(2,at=seq(0,0.3,0.05),labels=F)
text(xmax-2,0.985*ymax,"(b)")
# only plot distance data based on at least three chick-parent pairs that still had (some) contact
distance.contact.age.fathers = distance.contact.age.sexp[distance.contact.age.sexp$sex.parent=='m' & distance.contact.age.sexp$N>2,]
distance.contact.age.mothers = distance.contact.age.sexp[distance.contact.age.sexp$sex.parent=='f' & distance.contact.age.sexp$N>2,]
## (C) Distance to the nest during contact with mother 
ymax=13
plotCI(distance.contact.age.mothers$age.chick, distance.contact.age.mothers$mean, uiw=1.96*distance.contact.age.mothers$se, sfrac=0, xlab='', ylab='', pch=21, pt.bg='lightcoral', col='coral4', las=1, xlim=c(35,xmax), ylim=c(0,ymax))
lines(pred.dist.pop~age.chick, pred.data.dist[pred.data.dist$sex.parent=='f',], col='lightcoral') 
mtext("Distance to nest during contact (km)",2,3.5)
text(xmax-2,0.985*ymax,"(c)")
## (D) Distance to the nest during contact with father 
plotCI(distance.contact.age.fathers$age.chick, distance.contact.age.fathers$mean, uiw=1.96*distance.contact.age.fathers$se, sfrac=0, xlab='', ylab='', pch=21, pt.bg='lightskyblue', col='lightskyblue4', las=1, xlim=c(35,xmax), ylim=c(0,13), yaxt='n')
lines(pred.dist.pop~age.chick, pred.data.dist[pred.data.dist$sex.parent=='m',], col='lightskyblue') 
axis(2,at=seq(0,12,2),labels=F)
mtext("Chick age (days)", 1, 2, outer=T)
text(xmax-2,0.985*ymax,"(d)")
### END FIGURE 2 ###



# now make separate panel for each chickID:
# first determine chick age at chick and parent departure date:
chick.age.departure = unique(chick.parent.data.contact.sel[,c('birdID.chick','birdID.parent','sex.parent','start_deployment','age_deployment','departure.datetime.chick','departure.datetime.parent')])
chick.age.departure$hatch_date = chick.age.departure$start_deployment - chick.age.departure$age_deployment
chick.age.departure$departure_day_chick = yday(chick.age.departure$departure.datetime.chick)
chick.age.departure$departure_day_parent = yday(chick.age.departure$departure.datetime.parent)
chick.age.departure$hatch_day = yday(chick.age.departure$hatch_date)
chick.age.departure$age_departure_chick = chick.age.departure$departure_day_chick - chick.age.departure$hatch_day
chick.age.departure$age_departure_parent = chick.age.departure$departure_day_parent - chick.age.departure$hatch_day

### FIGURE S1 - Individual variation in probability of contact ###
windows(12,8)
layout(matrix(1:16, nrow=4, byrow=T))
par(mar=c(1,1,0,0), oma=c(4,4,1,1))
panelnr = 0
for(i in unique(data.pred$birdID.chick)) {
  panelnr = panelnr+1
  ymax=0.7
  data.chick = prop.contact.age.pair[prop.contact.age.pair$birdID.chick==i,]
  if (data.chick$sex.chick[1]=='f') chickcol='coral4' else chickcol='lightskyblue4'
  pred.chick = data.pred[data.pred$birdID.chick==i,]
  plot(contact~age.chick, data.chick, pch=19, cex=0.6, type="n", xlim=c(35,140), ylim=c(0,ymax), xaxt='n', yaxt='n')
  # box(col=chickcol)
  # first plot white-filled points with zero-contact:
  points(contact~age.chick, data.chick[data.chick$sex.parent=='f'&data.chick$contact==0,], pch=21, bg="white", col='coral4')
  points(contact~age.chick, data.chick[data.chick$sex.parent=='m'&data.chick$contact==0,], pch=21, bg="white", col='lightskyblue4')
  # then plot colour-filled points with >0 contact:
  points(contact~age.chick, data.chick[data.chick$sex.parent=='f'&data.chick$contact>0,], pch=21, bg="lightcoral", col='coral4')
  points(contact~age.chick, data.chick[data.chick$sex.parent=='m'&data.chick$contact>0,], pch=21, bg="lightskyblue", col='lightskyblue4')
  lines(pred~age.chick, pred.chick[pred.chick$sex.parent=='f',], col='coral4', lwd=2)
  lines(pred~age.chick, pred.chick[pred.chick$sex.parent=='m',], col='lightskyblue4', lwd=2)
  # add vertical lines for chick age at departure date of chick and parent:
  age_departure_chick = chick.age.departure$age_departure_chick[chick.age.departure$birdID.chick==i][1]
  age_departure_father = chick.age.departure$age_departure_parent[chick.age.departure$birdID.chick==i&chick.age.departure$sex.parent=='m']
  age_departure_mother = chick.age.departure$age_departure_parent[chick.age.departure$birdID.chick==i&chick.age.departure$sex.parent=='f']
  lines(rep(age_departure_chick,2),c(0,ymax))
  if(length(age_departure_father)>0) lines(rep(age_departure_father,2),c(0,ymax),col='lightskyblue4',lty='dashed')
  if(length(age_departure_mother)>0) lines(rep(age_departure_mother,2),c(0,ymax),col='coral4', lty='dashed')
  if (panelnr %in% c(1,5,9,13)) axis(2, at=seq(0,ymax,0.1), las=1)
  if (panelnr %in% 11:14) axis(1, at=seq(40,140,10), las=1)
  # box(col=chickcol)
  text(40,0.95*ymax,paste(i," (",data.chick$birthyear[1],")",sep=""), adj=0, col=chickcol)
}
plot(1:2,1:2,type='n', xaxt='n', yaxt='n', bty='n') # make an empty plot to put the legend on top of it:
legend(1,1.7,legend=c("female","male"), text.col=c("coral4","lightskyblue4"), bty='n',cex=2, pch=21, pt.bg=c("lightcoral","lightskyblue"), col=c("coral4","lightskyblue4"))
mtext("Chick age (days)",1,2,outer=T)
mtext("Proportion of contact",2,2,outer=T)
# strong year effect in the amount of contact between chick and parent. In 2017, there was hardly any post-fledging parental care!
### END FIGURE S1 ###

### FIGURE S2 ###
## Plot individual variation in probability of begging at parent and foraging close to parent:
prop.contact.begging.foraging.age.pair = aggregate(cbind(contact.begging, contact.foraging)~age.chick+sex.chick+sex.parent+chick.parent+birdID.chick+birthyear, chick.parent.data.behav.beg, mean)
data.pred$pred.begging.contact = plogis(predict(m.begging.contact, newdata = data.pred))
data.pred$pred.foraging.contact = plogis(predict(m.foraging.contact.pars, newdata = data.pred))
windows(12,8)
layout(matrix(1:16, nrow=4, byrow=T))
par(mar=c(1,1,0,0), oma=c(4,4.5,1,1))
panelnr = 0
for(i in unique(data.pred$birdID.chick)) {
  panelnr = panelnr+1
  ymax=1.2*max(prop.contact.begging.foraging.age.pair$contact.begging)
  data.chick = prop.contact.begging.foraging.age.pair[prop.contact.begging.foraging.age.pair$birdID.chick==i,]
  if (data.chick$sex.chick[1]=='f') chickcol='coral4' else chickcol='lightskyblue4'
  pred.chick = data.pred[data.pred$birdID.chick==i,]
  plot(contact.begging~age.chick, data.chick, pch=19, cex=0.6, type="n", xlim=c(35,140), ylim=c(0,ymax), xaxt='n', yaxt='n')
  # first plot white-filled points with zero-contact:
  points(contact.begging~age.chick, data.chick[data.chick$sex.parent=='f'&data.chick$contact.begging==0,], pch=21, bg="white", col='coral4')
  points(contact.begging~age.chick, data.chick[data.chick$sex.parent=='m'&data.chick$contact.begging==0,], pch=21, bg="white", col='lightskyblue4')
  # then plot colour-filled points with >0 contact:
  points(contact.begging~age.chick, data.chick[data.chick$sex.parent=='f'&data.chick$contact.begging>0,], pch=21, bg="lightcoral", col='coral4')
  points(contact.begging~age.chick, data.chick[data.chick$sex.parent=='m'&data.chick$contact.begging>0,], pch=21, bg="lightskyblue", col='lightskyblue4')
  lines(pred.begging.contact~age.chick, pred.chick[pred.chick$sex.parent=='f',], col='coral4', lwd=2)
  lines(pred.begging.contact~age.chick, pred.chick[pred.chick$sex.parent=='m',], col='lightskyblue4', lwd=2)
  # add vertical lines for chick age at departure date of chick and parent:
  age_departure_chick = chick.age.departure$age_departure_chick[chick.age.departure$birdID.chick==i][1]
  age_departure_father = chick.age.departure$age_departure_parent[chick.age.departure$birdID.chick==i&chick.age.departure$sex.parent=='m']
  age_departure_mother = chick.age.departure$age_departure_parent[chick.age.departure$birdID.chick==i&chick.age.departure$sex.parent=='f']
  lines(rep(age_departure_chick,2),c(0,ymax))
  if(length(age_departure_father)>0) lines(rep(age_departure_father,2),c(0,ymax),col='lightskyblue4',lty='dashed')
  if(length(age_departure_mother)>0) lines(rep(age_departure_mother,2),c(0,ymax),col='coral4', lty='dashed')
  if (panelnr %in% c(1,5,9,13)) axis(2, at=seq(0,ymax,0.01), las=1)
  if (panelnr %in% 11:14) axis(1, at=seq(40,140,10), las=1)
  # box(col=chickcol)
  text(40,0.95*ymax,paste(i," (",data.chick$birthyear[1],")",sep=""), adj=0, col=chickcol)
}
plot(1:2,1:2,type='n', xaxt='n', yaxt='n', bty='n') # make an empty plot to put the legend on top of it:
legend(1,1.7,legend=c("female","male"), text.col=c("coral4","lightskyblue4"), bty='n',cex=2, pch=21, pt.bg=c("lightcoral","lightskyblue"), col=c("coral4","lightskyblue4"))
mtext("Chick age (days)",1,2,outer=T)
mtext("Proportion of begging at parent",2,2.5,outer=T)
# strong year effect in the amount of contact between chick and parent. In 2017, there was hardly any post-fledging parental care!
### END FIGURE S2 ###

### FIGURE S3 ###
windows(12,8)
layout(matrix(1:16, nrow=4, byrow=T))
par(mar=c(1,1,0,0), oma=c(4,4.5,1,1))
panelnr = 0
for(i in unique(data.pred$birdID.chick)) {
  panelnr = panelnr+1
  ymax=1.2*max(prop.contact.begging.foraging.age.pair$contact.foraging)
  data.chick = prop.contact.begging.foraging.age.pair[prop.contact.begging.foraging.age.pair$birdID.chick==i,]
  if (data.chick$sex.chick[1]=='f') chickcol='coral4' else chickcol='lightskyblue4'
  pred.chick = data.pred[data.pred$birdID.chick==i,]
  plot(contact.foraging~age.chick, data.chick, pch=19, cex=0.6, type="n", xlim=c(35,140), ylim=c(0,ymax), xaxt='n', yaxt='n')
  # first plot white-filled points with zero-contact:
  points(contact.foraging~age.chick, data.chick[data.chick$sex.parent=='f'&data.chick$contact.foraging==0,], pch=21, bg="white", col='coral4')
  points(contact.foraging~age.chick, data.chick[data.chick$sex.parent=='m'&data.chick$contact.foraging==0,], pch=21, bg="white", col='lightskyblue4')
  # then plot colour-filled points with >0 contact:
  points(contact.foraging~age.chick, data.chick[data.chick$sex.parent=='f'&data.chick$contact.foraging>0,], pch=21, bg="lightcoral", col='coral4')
  points(contact.foraging~age.chick, data.chick[data.chick$sex.parent=='m'&data.chick$contact.foraging>0,], pch=21, bg="lightskyblue", col='lightskyblue4')
  lines(pred.foraging.contact~age.chick, pred.chick[pred.chick$sex.parent=='f',], col='coral4', lwd=2)
  lines(pred.foraging.contact~age.chick, pred.chick[pred.chick$sex.parent=='m',], col='lightskyblue4', lwd=2)
  # add vertical lines for chick age at departure date of chick and parent:
  age_departure_chick = chick.age.departure$age_departure_chick[chick.age.departure$birdID.chick==i][1]
  age_departure_father = chick.age.departure$age_departure_parent[chick.age.departure$birdID.chick==i&chick.age.departure$sex.parent=='m']
  age_departure_mother = chick.age.departure$age_departure_parent[chick.age.departure$birdID.chick==i&chick.age.departure$sex.parent=='f']
  lines(rep(age_departure_chick,2),c(0,ymax))
  if(length(age_departure_father)>0) lines(rep(age_departure_father,2),c(0,ymax),col='lightskyblue4',lty='dashed')
  if(length(age_departure_mother)>0) lines(rep(age_departure_mother,2),c(0,ymax),col='coral4', lty='dashed')
  if (panelnr %in% c(1,5,9,13)) axis(2, at=seq(0,ymax,0.01), las=1)
  if (panelnr %in% 11:14) axis(1, at=seq(40,140,10), las=1)
  # box(col=chickcol)
  text(40,0.95*ymax,paste(i," (",data.chick$birthyear[1],")",sep=""), adj=0, col=chickcol)
}
plot(1:2,1:2,type='n', xaxt='n', yaxt='n', bty='n') # make an empty plot to put the legend on top of it:
legend(1,1.7,legend=c("female","male"), text.col=c("coral4","lightskyblue4"), bty='n',cex=2, pch=21, pt.bg=c("lightcoral","lightskyblue"), col=c("coral4","lightskyblue4"))
mtext("Chick age (days)",1,2,outer=T)
mtext("Proportion of foraging with parent",2,2.5,outer=T)
# strong year effect in the amount of contact between chick and parent. In 2017, there was hardly any post-fledging parental care!
### END FIGURE S4 ###


# Why does data sometimes stop earlier than when either chick or parent departs? 
data6296.check = chick.parent.data[chick.parent.data$birdID.chick==6296 & chick.parent.data$age.chick>80,] # this is because 656 (with logger 6282) was lagging behind with data download prior to its departure. 656 died during this autumn migration. Data was downloaded until halfway 28/08/2016.
data6292.check = chick.parent.data[chick.parent.data$birdID.chick==6292 & chick.parent.data$age.chick>100,] # this is because data of 6292 was only downloaded until 23/09/2016. 6292 died during migration and was never found back. 
data6358.1.check = chick.parent.data[chick.parent.data$birdID.chick==6358.1 & chick.parent.data$age.chick>100,][1:100,] # this is because data of 6066 (the father) was only downloaded until 9/8/2017. He died during the winter of 2017-2018(or at least never returned afterwards).
data6381.check = chick.parent.data[chick.parent.data$birdID.chick==6381 & chick.parent.data$age.chick>90,][1:1000,] # this is because the logger of 6358 (the father) started to malfunction halfway August 2018, and therefore sometimes had large gaps in the data, despite still being at the breeding grounds. Last downloaded data via the antenna network took place at 4/9/2018. 
load("data/processed/SMS.data.juvs.ads.RData")
smsdata.6358 <- sms.data.ad.list[[33]] # Also the sms data had huge gaps (of sometimes months), therefore, departure date of 6358 could not be determined either. 



# plot distance to nest of parents, irrespective of contact:
windows()
plot(dist.nest.parent~age.chick, chick.parent.data.behav.sel, type='n')
points(dist.nest.parent~age.chick, chick.parent.data.behav.sel[chick.parent.data.behav.sel$sex.parent=='f',], col='red')
points(dist.nest.parent~age.chick, chick.parent.data.behav.sel[chick.parent.data.behav.sel$sex.parent=='m',], col='blue')
# calculate means and SE on daily basis, first calculating daily mean distance to nest per parent:
mean.distance.parent.chickage = aggregate(dist.nest.parent~age.chick+birdID.parent+sex.parent, chick.parent.data.behav.sel, calculate.mean.dist.km)
# now calculate means over all chicks per chick age:
mean.distance.age = aggregate(dist.nest.parent~age.chick+sex.parent, mean.distance.parent.chickage, calculate.mean.rnd)
se.distance.age = aggregate(dist.nest.parent~age.chick+sex.parent, mean.distance.parent.chickage, calculate.se)
mean.se.distance.age = cbind(mean.distance.age, se.distance.age[,3])
names(mean.se.distance.age)[3:4]=c('mean.dist.nest.parent','se.dist.nest.parent')
mean.se.distance.age.fathers = mean.se.distance.age[mean.se.distance.age$sex.parent=='m',]
mean.se.distance.age.mothers = mean.se.distance.age[mean.se.distance.age$sex.parent=='f',]
windows(8,5)
par(mar=c(5,5,1,1))
plotCI(mean.se.distance.age.fathers$age.chick, mean.se.distance.age.fathers$mean.dist.nest.parent, uiw=mean.se.distance.age.fathers$se.dist.nest.parent, sfrac=0, xlab='Chick age (days)', ylab='Distance to nest of parent (km)', pch=21, pt.bg='lightskyblue', col='lightskyblue4', las=1, ylim=c(0,40))
plotCI(mean.se.distance.age.mothers$age.chick+0.4, mean.se.distance.age.mothers$mean.dist.nest.parent, uiw=mean.se.distance.age.mothers$se.dist.nest.parent, sfrac=0, pch=21, pt.bg='lightcoral', col='coral4', add=T)
legend("topleft", legend=c("Mother", "Father"), pch=21, col=c("coral4","lightskyblue4"), pt.bg=c("lightcoral","lightskyblue"))


tail(chick.parent.data.behav.sel[chick.parent.data.behav.sel$birdID.parent==6284.2 & chick.parent.data.behav.sel$dist.nest.parent>25000,])

# plot overall behaviours of chicks, irrespective of contact, including begging:
chick.parent.data.behav.beg <- chick.parent.data.behav[!is.na(chick.parent.data.behav$behaviour.beg.chick),]
xtab.behav.beg.age = table(chick.parent.data.behav.beg$behaviour.beg.chick, chick.parent.data.behav.beg$age5)
prop.behav.beg.age = as.matrix(prop.table(xtab.behav.beg.age, 2))
cols.behaviour = c('blue','orange','red','skyblue','gold','pink')

windows(width=15,height=8)
par(oma=c(0,0,0,10))
barp.data = barplot(prop.behav.beg.age, main='', col=cols.behaviour, las=1, xaxt='n' , cex.axis=1.2, border=NA, xlab="Chick age (days)", ylab="Proportion of time", cex.lab=1.3)
axis(1,at=barp.data,labels=colnames(prop.behav.beg.age), tick=F, cex.axis=1.2)
mtext('Age (days)', 1, 3, cex=1.2, outer=T)
mtext('Proportion of time spent', 2, 3, cex=1.2, outer=T)
legend(max(barp.data)+2, 0.4, rev(rownames(prop.behav.beg.age)), 
       pch=15, col=rev(cols.behaviour), bty='n', xpd=NA, cex=1.2)

chick.parent.data.behav.sel$freq = 1
dur.behav.contact.age.xtabs = xtabs(freq~behaviour.chick.contact+age.chick, data=chick.parent.data.behav.sel)
dur.behav.contact.age.prop = as.matrix(prop.table(dur.behav.contact.age.xtabs, 2))
rownames(dur.behav.contact.age.prop)
colnames(dur.behav.contact.age.prop)
round(rowSums(dur.behav.contact.age.prop),1)
cols.behaviour = c('blue','orange','red','skyblue','gold','pink')

windows(width=15,height=8)
par(oma=c(0,0,0,10))
barp.data = barplot(dur.behav.contact.age.prop[c("active1","resting1","flying1","active0","resting0","flying0"),], main='', col=cols.behaviour, las=1, xaxt='n' , cex.axis=1.2, border=NA, xlab="Chick age (days)", ylab="Proportion of time", cex.lab=1.3)
axis(1, barp.data[colnames(dur.behav.contact.age.prop)%in%seq(40,120,20)], seq(40,120,20), cex.axis=1.2)
mtext('Age (days)', 1, 3, cex=1.2, outer=T)
mtext('Proportion of time spent', 2, 3, cex=1.2, outer=T)
legend(130, 0.4, c('flying, no contact','resting, no contact','active, no contact','flying, contact','resting, contact','active, contact'), 
       pch=15, col=rev(cols.behaviour), bty='n', xpd=NA, cex=1.2)

# how does this figure change when plotting yday instead of chick age (which are highly correlated)?
chick.parent.data.behav.sel$yday_CEST <- yday(chick.parent.data.behav.sel$datetime.chick)
dur.behav.contact.yday.xtabs = xtabs(freq~behaviour.chick.contact+yday_CEST, data=chick.parent.data.behav.sel)
dur.behav.contact.yday.prop = as.matrix(prop.table(dur.behav.contact.yday.xtabs, 2))

windows(width=15,height=8)
par(oma=c(0,0,0,10))
barp.data = barplot(dur.behav.contact.yday.prop[c("active1","resting1","flying1","active0","resting0","flying0"),], main='', col=cols.behaviour, las=1, xaxt='n' , cex.axis=1.2, border=NA)
axis(1, barp.data[colnames(dur.behav.contact.yday.prop)%in%yday(ymd(c("2018-07-01","2018-08-01","2018-09-01","2018-10-01")))], c("Jul-01","Aug-01","Sep-01","Oct-01"), cex.axis=1.2)
mtext('Date', 1, 3, cex=1.3)
mtext('Proportion of time spent', 2, 3, cex=1.3)
legend(180, 0.4, c('flying, no contact','resting, no contact','active, no contact','flying, contact','resting, contact','active, contact'), 
       pch=15, col=rev(cols.behaviour), bty='n', xpd=NA, cex=1.2)

# manually calculate the population-level probs of active contact depending on the sex of chick and parent
macfix = fixef(m.active.contact.agernd.parsim)
# predicted contact for male chick + male parent
pac.mcmp = plogis(macfix[1]+macfix[2]+macfix[3]+macfix[4]*z.age+macfix[5])
# male chick + female parent
pac.mcfp = plogis(macfix[1]          +macfix[3]+macfix[4]*z.age)
# female chick + male parent
pac.fcmp = plogis(macfix[1]+macfix[2]          +macfix[4]*z.age)
# female chick + female parent
pac.fcfp = plogis(macfix[1]                    +macfix[4]*z.age)
# plot predicted population-level amount of contact:
windows()
plot(real.age, pac.fcfp, pch=19, cex=0.6, col='red', ylim=c(0,0.01), type='l', lwd=2)
lines(real.age, pac.mcfp, col='orange', lwd=2)
lines(real.age, pac.fcmp, col='green', lwd=2)
lines(real.age, pac.mcmp, col='blue', lwd=2)
legend("topright",legend=c("fc.fp","mc.fp","fc.mp","mc.mp"),col=c("red","orange","green",'blue'), lwd=2)
# female parents have more contact with male chicks, and male parents with female chicks. 
# overall, male parents have less active contact than female parents with the chick. 
# add the data points to this graph (1 point per chick-parent pair per age class, and then calculating the average of these points according to parent sex):
# order data on year and chickID and parentID:
chick.parent.data.behav.sel = chick.parent.data.behav.sel[order(chick.parent.data.behav.sel$birthyear, chick.parent.data.behav.sel$birdID.chick, chick.parent.data.behav.sel$birdID.parent),]
chick.parent.data.behav.sel$age5.chick = round(chick.parent.data.behav.sel$age.chick/5,0)*5
mean.prop.active.contact.age.pair = aggregate(active.contact~age.chick+sex.chick+sex.parent+chick.parent+birdID.chick+birthyear, chick.parent.data.behav.sel, mean)
mean.prop.active.contact.ageclass.pair = aggregate(active.contact~age5.chick+sex.chick+sex.parent+chick.parent+birdID.chick+birthyear, chick.parent.data.behav.sel, mean)
activecontact.min = min(mean.prop.active.contact.ageclass.pair$active.contact[mean.prop.active.contact.ageclass.pair$active.contact>0])
mean.prop.active.contact.ageclass.pair$active.contact.adj = mean.prop.active.contact.ageclass.pair$active.contact
mean.prop.active.contact.ageclass.pair$active.contact.adj[mean.prop.active.contact.ageclass.pair$active.contact.adj==0]=activecontact.min
mean.prop.active.contact.ageclass.sexpc = aggregate(active.contact.adj~age5.chick+sex.parent+sex.chick, mean.prop.active.contact.ageclass.pair, mean.logit)
points(active.contact.adj~age5.chick, mean.prop.active.contact.ageclass.sexpc[mean.prop.active.contact.ageclass.sexpc$sex.parent=='f'&
                                                                             mean.prop.active.contact.ageclass.sexpc$sex.chick=='f',], col='red')
points(active.contact.adj~age5.chick, mean.prop.active.contact.ageclass.sexpc[mean.prop.active.contact.ageclass.sexpc$sex.parent=='f'&
                                                                             mean.prop.active.contact.ageclass.sexpc$sex.chick=='m',], col='orange')
points(active.contact.adj~age5.chick, mean.prop.active.contact.ageclass.sexpc[mean.prop.active.contact.ageclass.sexpc$sex.parent=='m'&
                                                                             mean.prop.active.contact.ageclass.sexpc$sex.chick=='f',], col='green')
points(active.contact.adj~age5.chick, mean.prop.active.contact.ageclass.sexpc[mean.prop.active.contact.ageclass.sexpc$sex.parent=='m'&
                                                                             mean.prop.active.contact.ageclass.sexpc$sex.chick=='m',], col='blue')
# these means really don't fit the estimated lines... presumably because of the random effect... Or because for the only two chicks where we have data for both parents, the female chick has more active contact with her father and the male with his mother?

# now plot the individual fits of the model to each parent-chick pair:
# now predict for each chick-parent pair separately (population-level predictions with CI are not possible with glmer objects, it seems...)
data.pred = unique(chick.parent.data.behav.sel[,c('sex.parent','sex.chick','z.age','age.chick','birdID.chick')])
data.pred$pred = plogis(predict(m.active.contact.agernd.parsim, newdata = data.pred))
# now make separate panel for each chickID:
windows(12,8)
layout(matrix(1:16,nrow=4, byrow=T))
par(mar=c(1,1,0,0), oma=c(4,4,1,1))
for(i in unique(data.pred$birdID.chick)) {
  data.chick = mean.prop.active.contact.age.pair[mean.prop.active.contact.age.pair$birdID.chick==i,]
  pred.chick = data.pred[data.pred$birdID.chick==i,]
  plot(active.contact~age.chick, data.chick, pch=19, cex=0.6, type="n", xlim=c(40,140), ylim=c(0,0.04), xaxt='n', yaxt='n')
  if(data.chick$sex.chick[1]=='f') box(col='red') else box(col='blue')
  points(active.contact~age.chick, data.chick[data.chick$sex.parent=='f',], pch=19, col='red')
  points(active.contact~age.chick, data.chick[data.chick$sex.parent=='m',], pch=19, col='blue')
  lines(pred~age.chick, pred.chick[pred.chick$sex.parent=='f',], pch=19, col='red')
  lines(pred~age.chick, pred.chick[pred.chick$sex.parent=='m',], pch=19, col='blue')
  text(135,0.039,i)
  text(135,0.035,paste("(",data.chick$birthyear[1],")",sep=""))
}
# strong year effect on the amount of active contact!

# now plot only the population-level and chickID_level estimates in one figure:
windows()
plot(real.age, pc.fp, pch=19, cex=0.6, col='red', ylim=c(0,0.1), type='l', lwd=3, xlab="Chick age", ylab="Proportion of contact")
lines(real.age, pc.mp, col='blue', lwd=3)
for(i in unique(data.pred$birdID.chick)) {
  pred.chick = data.pred[data.pred$birdID.chick==i & data.pred$age.chick%in%40:130,]
  lines(pred~age.chick, pred.chick[pred.chick$sex.parent=='f',], pch=19, col='red', lty='dotted')
  lines(pred~age.chick, pred.chick[pred.chick$sex.parent=='m',], pch=19, col='blue', lty='dotted')
}  

# plot study map:
#windows()
#ggplot(schier_new84_sel)+
#  geom_sf(aes(fill=habitat))

habitat.active.contacts$habitat.col = 'green'
habitat.active.contacts$habitat.col[habitat.active.contacts$habitat.simple=='freshwater'] = 'lightblue'
habitat.active.contacts$habitat.col[habitat.active.contacts$habitat.simple=='marine'] = 'blue'
habitat.active.contacts$sexpr.col='blue'
habitat.active.contacts$sexpr.col[habitat.active.contacts$sex.parent=='f']='red'
habitat.active.contacts

# visually plot active contacts:
mapviewOptions(fgb = FALSE)
mapview(habitat.active.contacts["habitat.simple"], col.regions = habitat.active.contacts$habitat.col, map.types="Esri.WorldImagery")
mapview(habitat.active.contacts["sex.parent"], col.regions = habitat.active.contacts$sexpr.col, map.types="Esri.WorldImagery") # clear segregation of meeting places between chicks and their mothers and chicks and their fathers. But also clear that fathers also meet at the foraging grounds in the Wadden Sea! And that lots of meetings in marine habitat with mothers are very close to the shore of Schiermonnikoog, suggesting that these might actually be roosting (chick-gathering) sites... We might define the location of roosts as locations with a 100 m radius (this is rather arbitrary) where chicks are resting >4 hours per day for at least 5 (again arbitrary) days.  

# make this plot for each chick-parent combination separately
# calculate age at departure:
head(chick.parent.data.behav.sel)
chick.parent.data.behav.sel$departure.age <- yday(chick.parent.data.behav.sel$departure.datetime.chick)-yday(chick.parent.data.behav.sel$start_deployment)+chick.parent.data.behav.sel$age_deployment
dur.behav.contact.age.pair.xtabs = xtabs(freq~behaviour.chick.contact+age.chick+chick.parent, data=chick.parent.data.behav.sel)

windows(15,10)
layout(matrix(1:16,nrow=4,byrow=T))
par(mar=c(1,1,1,0), oma=c(3,3,1,15), las=1)
n=0
for (i in unique(chick.parent.data.behav.sel$chick.parent)) {
  n=n+1
  dur.behav.prop.pair.i = as.matrix(prop.table(dur.behav.contact.age.pair.xtabs[,,i], 2))
  barp.data = barplot(dur.behav.prop.pair.i[c("active1","resting1","flying1","active0","resting0","flying0"),], main=i, col=cols.behaviour, las=1, xaxt='n', yaxt='n', cex.axis=1.2, border=NA)
  departure.age = chick.parent.data.behav.sel$departure.age[chick.parent.data.behav.sel$chick.parent==i][1]
  if (!is.na(departure.age)) lines(c(barp.data[colnames(dur.behav.prop.pair.i)==departure.age], barp.data[colnames(dur.behav.prop.pair.i)==departure.age]), c(0,1), lwd=2)
  if (n%in%13:16) axis(1, barp.data[colnames(dur.behav.prop.pair.i)%in%seq(40,160,20)], seq(40,160,20), cex.axis=1.2) else
    axis(1, barp.data[colnames(dur.behav.prop.pair.i)%in%seq(40,160,20)], labels=F, cex.axis=1.2)
  if (n%in%c(1,5,9,13)) axis(2, at=seq(0,1,0.2), cex.axis=1.2) else  axis(2, at=seq(0,1,0.2), cex.axis=1.2, labels=F)
}
legend(170, 0.8, c('flying, no contact','resting, no contact','active, no contact','flying, contact','resting, contact','active, contact'), 
       pch=15, col=rev(cols.behaviour), bty='n', xpd=NA, cex=1.5)

# make the same plot but now with date on x-axis:
dur.behav.contact.yday.pair.xtabs = xtabs(freq~behaviour.chick.contact+yday_CEST+chick.parent, data=chick.parent.data.behav.sel)

windows(15,10)
layout(matrix(1:16,nrow=4,byrow=T))
par(mar=c(1,1,1,0), oma=c(3,5,1,15), las=1)
n=0
for (i in unique(chick.parent.data.behav.sel$chick.parent)) {
  n=n+1
  dur.behav.prop.pair.i = as.matrix(prop.table(dur.behav.contact.yday.pair.xtabs[,,i], 2))
  barp.data = barplot(dur.behav.prop.pair.i[c("active1","resting1","flying1","active0","resting0","flying0"),], main=i, col=cols.behaviour, las=1, xaxt='n', yaxt='n', cex.axis=1.2, border=NA)
  departure.yday = yday(chick.parent.data.behav.sel$departure.datetime.chick[chick.parent.data.behav.sel$chick.parent==i])[1]
  if (!is.na(departure.yday)) arrows(x0=barp.data[colnames(dur.behav.prop.pair.i)==departure.yday], y0=0.2, x1=barp.data[colnames(dur.behav.prop.pair.i)==departure.yday], y1=0, length=0.1, lwd=2)
  if (n==1) text(barp.data[colnames(dur.behav.prop.pair.i)==departure.yday],0.25,"departure chick", srt=90, las=1, adj=0)
  if (n%in%13:16) axis(1, barp.data[colnames(dur.behav.contact.yday.prop)%in%yday(ymd(c("2018-07-01","2018-08-01","2018-09-01","2018-10-01","2018-11-01")))], c("Jul-01","Aug-01","Sep-01","Oct-01","Nov-01"), cex.axis=1.2) else
    axis(1, barp.data[colnames(dur.behav.contact.yday.prop)%in%yday(ymd(c("2018-07-01","2018-08-01","2018-09-01","2018-10-01","2018-11-01")))], labels=F, cex.axis=1.2)
  if (n%in%c(1,5,9,13)) axis(2, at=seq(0,1,0.2), cex.axis=1.2) else  axis(2, at=seq(0,1,0.2), cex.axis=1.2, labels=F)
}
legend(190, 1.1, c('flying, no contact','resting, no contact','active, no contact','flying, contact','resting, contact','active, contact'), 
       pch=15, col=rev(cols.behaviour), bty='n', xpd=NA, cex=1.5)
mtext('Proportion of time spent', 2, 2.5, cex=1.3, outer=T, las=0)

# Now make a plot of only what the juveniles are doing during the contact moments
# pool age by 5 days, as otherwise the graphs become quite messy
chick.parent.data.behav.contact = chick.parent.data.behav.sel[chick.parent.data.behav.sel$contact==1,]
dur.behav.contact.age.pair.xtabs = xtabs(freq~behaviour.chick.contact+age5+chick.parent, data=chick.parent.data.behav.contact)

table(paste(chick.parent.data.behav.contact$behaviour.chick.contact, chick.parent.data.behav.contact$behaviour.parent))

windows(15,10)
layout(matrix(1:16,nrow=4,byrow=T))
par(mar=c(1,1,1,0), oma=c(3,3,1,15), las=1)
n=0
for (i in unique(chick.parent.data.behav.contact$chick.parent)) {
  n=n+1
  dur.behav.prop.pair.i = as.matrix(prop.table(dur.behav.contact.age.pair.xtabs[,,i], 2))
  #dur.behav.prop.pair.i[is.na(dur.behav.prop.pair.i)]=0
  barp.data = barplot(dur.behav.prop.pair.i[c("active1","resting1","flying1"),], main=i, col=cols.behaviour, las=1, xaxt='n', yaxt='n', cex.axis=1.2, border=NA)
  departure.age = chick.parent.data.behav.contact$departure.age[chick.parent.data.behav.contact$chick.parent==i][1]
  if (n%in%13:16) axis(1, barp.data[colnames(dur.behav.prop.pair.i)%in%seq(30,120,20)], seq(30,120,20), cex.axis=1.2) else
    axis(1, barp.data[colnames(dur.behav.prop.pair.i)%in%seq(30,120,20)], labels=F, cex.axis=1.2)
  if (n%in%c(1,5,9,13)) axis(2, at=seq(0,1,0.2), cex.axis=1.2) else  axis(2, at=seq(0,1,0.2), cex.axis=1.2, labels=F)
}
legend(25, 0.8, c('flying','resting','active'), 
       pch=15, col=rev(cols.behaviour[1:3]), bty='n', xpd=NA, cex=1.5, title="Behaviour during contact")

# now select data where both the chick and the parent have their behaviour classified:
chick.parent.data.behav.sel2 <- chick.parent.data.behav.sel[!is.na(chick.parent.data.behav.sel$behaviour.parent),]
chick.parent.data.behav.sel2$behaviour.parent.contact[chick.parent.data.behav.sel2$behaviour.parent.contact%in%c('foraging0','other0')]='active0'
chick.parent.data.behav.sel2$behaviour.parent.contact[chick.parent.data.behav.sel2$behaviour.parent.contact%in%c('foraging1','other1')]='active1'
chick.parent.data.behav.contact2 = chick.parent.data.behav.sel2[chick.parent.data.behav.sel2$contact==1,]
chick.parent.data.behav.contact2$behav.parent.chick.contact = paste(chick.parent.data.behav.contact2$behaviour.chick.contact, chick.parent.data.behav.contact2$behaviour.parent.contact)
dur.behav.contact2.age.pair.xtabs = xtabs(freq~behav.parent.chick.contact+age5+chick.parent, data=chick.parent.data.behav.contact2)

cols.behaviour.contact2 = rainbow(9)

windows(15,10)
layout(matrix(1:16,nrow=4,byrow=T))
par(mar=c(1,1,1,0), oma=c(3,3,1,18), las=1)
n=0
for (i in unique(chick.parent.data.behav.contact2$chick.parent)) {
  n=n+1
  dur.behav.prop.pair.i = as.matrix(prop.table(dur.behav.contact2.age.pair.xtabs[,,i], 2))
  #dur.behav.prop.pair.i[is.na(dur.behav.prop.pair.i)]=0
  barp.data = barplot(dur.behav.prop.pair.i, main=i, col=cols.behaviour.contact2, las=1, xaxt='n', yaxt='n', cex.axis=1.2, border=NA)
  departure.age = chick.parent.data.behav.contact$departure.age[chick.parent.data.behav.contact$chick.parent==i][1]
  if (n%in%13:16) axis(1, barp.data[colnames(dur.behav.prop.pair.i)%in%seq(30,120,20)], seq(30,120,20), cex.axis=1.2) else
    axis(1, barp.data[colnames(dur.behav.prop.pair.i)%in%seq(30,120,20)], labels=F, cex.axis=1.2)
  if (n%in%c(1,5,9,13)) axis(2, at=seq(0,1,0.2), cex.axis=1.2) else  axis(2, at=seq(0,1,0.2), cex.axis=1.2, labels=F)
}
legend(30, 1.5, rev(row.names(dur.behav.contact2.age.pair.xtabs[,,1])), 
       pch=15, col=rev(cols.behaviour.contact2), bty='n', xpd=NA, cex=1.5, title="Behaviour chick vs parent")

# now pooled across all individuals:
dur.behav.contact2.age.xtabs = xtabs(freq~behav.parent.chick.contact+age5, data=chick.parent.data.behav.contact2)
windows(10,5)
par(mar=c(1,1,1,0), oma=c(3,3,1,18), las=1)
dur.behav.contact2.age.prop = as.matrix(prop.table(dur.behav.contact2.age.xtabs, 2))
barp.data = barplot(dur.behav.contact2.age.prop, col=cols.behaviour.contact2, las=1, xaxt='n', yaxt='n', cex.axis=1.2, border=NA)
legend(30, 0.5, rev(row.names(dur.behav.contact2.age.xtabs)), 
       pch=15, col=rev(cols.behaviour.contact2), bty='n', xpd=NA, title="Behaviour chick vs parent")
# the amount of contact when both chick and parent are active increases with age. Now it becomes interesting to know if this is really begging, or foraging. We could look at the proportion of active-active contact in relation to habitat, or in relation to whether the chick is estimated to be begging or not... Even if the chick is following the parent to be fed, it gets the chance to see the habitat where the parent is foraging... And it can't stay there as it becomes high tide. 

# proportion of begging behaviour during active-active contact:
active.contact = chick.parent.data.behav.sel2[chick.parent.data.behav.sel2$behaviour.chick.contact=='active1' &
                                                chick.parent.data.behav.sel2$behaviour.parent.contact=='active1',]
dur.beg.active.contact.xtabs = xtabs(freq~behaviour.beg.chick+age5, data=active.contact)
windows(10,5)
par(mar=c(1,1,1,0), oma=c(3,3,1,15), las=1)
prop.beg.active.contact = as.matrix(prop.table(dur.beg.active.contact.xtabs, 2))
barp.data = barplot(prop.beg.active.contact, col=cols.behaviour.contact2[1:6], las=1, cex.axis=1.2, border=NA)
legend(25, 0.5, rev(row.names(prop.beg.active.contact)), 
       pch=15, col=rev(cols.behaviour.contact2[1:6]), bty='n', xpd=NA)
sample.sizes = colSums(dur.beg.active.contact.xtabs)
axis(3,at=barp.data,sample.sizes, tick=F, cex.axis=0.7, line=-1)
axis(3,at=max(barp.data)+0.8,"# contacts", tick=F, cex.axis=0.7, line=-1, xpd=NA, hadj=0)
# also determine number of different different individuals:
data.active.contact.chick = table(active.contact$age5, active.contact$birdID.chick)
data.active.contact.chick[data.active.contact.chick>1]=1
axis(3,at=barp.data,rowSums(data.active.contact.chick), tick=F, cex.axis=0.7, line=0)
axis(3,at=max(barp.data)+0.8,"# chicks", tick=F, cex.axis=0.7, line=0, xpd=NA, hadj=0)
text(25,0.63,"Behaviour of chick",xpd=NA,cex=1,adj=0)
text(25,0.57,"during contact when both",xpd=NA,cex=1,adj=0)
text(25,0.51,"parent and chick were active:",xpd=NA,cex=1,adj=0)

# plot the distribution of active1 (presumably begging or foraging) over the day:
diurnal.time.allocation <- table(chick.parent.data.behav.sel$behaviour.chick.contact, hour(chick.parent.data.behav.sel$datetime.chick))
windows()
barplot(diurnal.time.allocation[2,]) # this is the number of contact moments when the chick is active
barplot(diurnal.time.allocation[4,]) # this is the number of contact moments when the chick is flying
barplot(diurnal.time.allocation[6,]) # this is the number of contact moments when the chick is resting
# overall, contact moments most likely occur during the day:
diurnal.rhythm.contact <- table(chick.parent.data.behav.sel$contact, hour(chick.parent.data.behav.sel$datetime.chick))
windows()
barplot(diurnal.time.allocation[2,])
# does this differ per sex of the parent?
diurnal.rhythm.contact.sex <- table(paste(chick.parent.data.behav.sel$sex.parent, chick.parent.data.behav.sel$contact), hour(chick.parent.data.behav.sel$datetime.chick))
windows()
barplot(diurnal.rhythm.contact.sex[2,]) # rhythm of contact with female parent
barplot(diurnal.rhythm.contact.sex[4,]) # rhythm of contact with male parent
# females have more contact with the chick at night than males
# but this could be due to the fact that females are more likely to roost on Schiermonnikoog at night; what about the active contact moments?
diurnal.rhythm.active.contact.sex <- table(paste(chick.parent.data.behav.sel$sex.parent, chick.parent.data.behav.sel$behaviour.chick.contact), hour(chick.parent.data.behav.sel$datetime.chick))
windows()
barplot(diurnal.rhythm.active.contact.sex[2,]) # rhythm of active contact with female parent
barplot(diurnal.rhythm.active.contact.sex[8,]) # rhythm of active contact with male parent

nighttime.activity = chick.parent.data.behav.sel[chick.parent.data.behav.sel$behaviour.chick.contact=='active1' &
                                                   hour(chick.parent.data.behav.sel$datetime.chick)%in%0:2,]



# Figure 3
windows(7,7)
plot(departure.datetime.chick.2018~departure.datetime.parent.2018, departure.dates.chick.parent, xlab='Departure date parent', ylab='Departure date chick', pch=21, bg=c('white','grey')[sex.parent.n], cex=2, xlim=c(ymd_hms('2018-09-12 00:00:00'),ymd_hms('2018-10-12 00:00:00')), ylim=c(ymd_hms('2018-09-12 00:00:00'),ymd_hms('2018-10-12 00:00:00')), xaxt='n', yaxt='n') 
lines(c(ymd_hms('2018-09-01 00:00:00'),ymd_hms('2018-10-31 00:00:00')),c(ymd_hms('2018-09-01 00:00:00'),ymd_hms('2018-10-31 00:00:00')), lty='dashed') 
axis(1, at=c(ymd_hms('2018-09-20 00:00:00'),ymd_hms('2018-09-30 00:00:00'),ymd_hms('2018-10-10 00:00:00')), labels=c('Sep-20','Sep-30','Oct-10'))
axis(2, at=c(ymd_hms('2018-09-20 00:00:00'),ymd_hms('2018-09-30 00:00:00'),ymd_hms('2018-10-10 00:00:00')), labels=c('Sep-20','Sep-30','Oct-10'))
legend("bottomright", legend=c("mother","father"), pt.cex=2, cex=1.2, pch=21, pt.bg=c('white','grey'))
# End Figure 3 #


# Check age at certain dates for GPS-tagged chicks:
chick.parent.data.contact.sel$date = round(chick.parent.data.contact.sel$datetime.chick, 'day')
date.age.chick = unique(chick.parent.data.contact.sel[,c('birdID.chick','date','age.chick')])
tail(chick.parent.data.contact.sel[chick.parent.data.contact.sel$birdID.chick==6315 & chick.parent.data.contact.sel$date==ymd("2018-08-21"),])
