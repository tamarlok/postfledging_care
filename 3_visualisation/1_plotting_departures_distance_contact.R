## some numbers for reporting

# Create Table S1 with information on parents and chicks: 
parent_chick_info = parent_offspring_nest_data[,c('parentID','chickID','n_chicks','nth_chick','latitude','longitude')]
names(parent_chick_info)[c(1,2,5,6)] = c('birdID.parent','birdID.chick','lat.nest','lon.nest')
# add most recent deployment date + sex for parents
gps_refdata_sel = read.csv("data/raw/metadata.tagged.birds.csv")
deployment.mortality.date.sex.bird = aggregate(start_deployment~birdID+sex+mortality_date, gps_refdata_all, max)
deployment.mortality.date.sex.bird$mortality_date[deployment.mortality.date.sex.bird$mortality_date==ymd('2099-12-31')]=""
parent_chick_info = merge(parent_chick_info, deployment.mortality.date.sex.bird, by.x='birdID.parent', by.y='birdID')
# add chick info:
parent_chick_info = merge(parent_chick_info, deployment.mortality.date.sex.bird, by.x='birdID.chick', by.y='birdID')
# add age at deployment of chicks:
parent_chick_info = merge(parent_chick_info, chick.biometrics[,c('birdID','age_deployment')], by.x='birdID.chick', by.y='birdID')
# add the latest date with (some) joined data:
max.date.joined.data = aggregate(datetime.chick~chick.parent+birdID.parent+birdID.chick+age_deployment, chick.parent.data, max)
names(max.date.joined.data)[5] = 'last.date.joined.data'
max.date.joined.data$last.date.joined.data = floor_date(max.date.joined.data$last.date.joined.data, 'days')
parent_chick_info = merge(parent_chick_info, max.date.joined.data, all.x=T)
# add the number of days with nearly complete data used in the analysis:
# for overall contact:
ydays.used.pair = unique(chick.parent.data.contact.sel[,c('chick.parent','yday')])
ndays.pair = as.data.frame(table(ydays.used.pair$chick.parent))
names(ndays.pair) = c('chick.parent','ndays')
parent_chick_info = merge(parent_chick_info, ndays.pair, all.x=T)
# for behaviour-specific contact:
ydays.used.pair.behav = unique(st_drop_geometry(chick.parent.data.behav.sel[,c('chick.parent','yday')]))
ndays.pair.behav = as.data.frame(table(ydays.used.pair.behav$chick.parent))
names(ndays.pair.behav) = c('chick.parent','ndays.behav')
parent_chick_info = merge(parent_chick_info, ndays.pair.behav, all.x=T)
parent_chick_info
parent_chick_info$ndays[is.na(parent_chick_info$ndays)] = 0
parent_chick_info$ndays.behav[is.na(parent_chick_info$ndays.behav)] = 0
# due to malfunctioning of tag 6288, it didn't collect sufficient data to be included as a parent in 2017 and 2018:
mean(parent_chick_info$ndays[parent_chick_info$ndays>0])
mean(parent_chick_info$ndays.behav[parent_chick_info$ndays.behav>0])

write.csv(parent_chick_info[order(parent_chick_info$start_deployment.y),], 'output/TableS1 - Chick-parent pair info.csv')

# what is the maximum chick age when there was still contact between parent and chick in the selected data?
max.age.contact.sex = aggregate(age.chick~chick.parent+sex.chick+sex.parent, chick.parent.data.contact[chick.parent.data.contact$contact==1,], max) # here, contact is defined as a distance of less than 10m between chick and parent.
range(max.age.contact.sex$age.chick)
median(max.age.contact.sex$age.chick)

# what is the interval between last contact and departure day of chick and parent?
chick.parent.data.contact$days.until.chick.departure = date(chick.parent.data.contact$departure.datetime.chick)-date(chick.parent.data.contact$datetime.chick)
chick.parent.data.contact$days.until.chick.departure[chick.parent.data.contact$days.until.chick.departure>300]=NA
chick.parent.data.contact$days.until.parent.departure = date(chick.parent.data.contact$departure.datetime.parent)-date(chick.parent.data.contact$datetime.parent)

dates.last.contacts = unique(merge(chick.parent.data.contact, max.age.contact.sex)[,c('birdID.chick','birdID.parent','chick.parent','age.chick','days.until.chick.departure','days.until.parent.departure')])
median(na.omit(dates.last.contacts$days.until.chick.departure))
range(na.omit(dates.last.contacts$days.until.chick.departure))
median(na.omit(dates.last.contacts$days.until.parent.departure))
range(na.omit(dates.last.contacts$days.until.parent.departure))


# what was the age of chick departure? 
chick.age.departure.unique = unique(merge(departure.dates.chick.parent[,c('chickID','departure.datetime.chick')], chick.biometrics[,c('birdID','start_deployment','age_deployment')], by.x='chickID', by.y='birdID'))
chick.age.departure.unique$age_departure = yday(chick.age.departure.unique$departure.datetime.chick)-yday(chick.age.departure.unique$start_deployment)+chick.age.departure.unique$age_deployment
median(na.omit(chick.age.departure.unique$age_departure)) # 116 d
range(na.omit(chick.age.departure.unique$age_departure)) # 108 - 139 d

## Calculate first-year survival based on the chicks used in the analysis:
first.year.survival = unique(parent_chick_info[,c('birdID.chick','start_deployment.y','mortality_date.y')])
names(first.year.survival)[2:3] = c('start_deployment','mortality_date')
# is there a difference in first-year survival between GPS-tagged and colour-ringed juveniles?
first.year.survival$mortality_date = with_tz(first.year.survival$mortality_date, tz="UTC")
first.year.survival$survived.first.year = 1
first.year.survival$survived.first.year[first.year.survival$mortality_date-first.year.survival$start_deployment<365] = 0

N = length(first.year.survival$survived.first.year)
mean1 = mean(first.year.survival$survived.first.year)
se1 = sqrt(mean1*(1-mean1)/N)
round(c(mean1, se1),2)
# survival GPS-tagged: 0.53 +/- 0.13 SE
# survival colour-ringed (Lok et al. 2013): 0.56 +/- 0.04
mean2=0.56
se2=0.04
# use a two-sample t-test
t_value = (mean1 - mean2)/(sqrt(se1^2+se2^2))
# As sample size for mean2 is unknown (derived from large colour-ring dataset), we assume large sample sizes, and approximate the test with a normal distribution (Z-test):
p_value <- 2 * (1 - pnorm(abs(t_value)))
p_value


## Calculate the % of contact when using 10 vs 50 m contact distance, using the most parsim model with 10 m distance (which included age), at 40 and 90 d old.
pred.data = expand.grid(age.chick=30:100, sex.parent=c('f','m'), yearf=factor(2016:2018))
pred.data$z.age = (pred.data$age.chick-mean(pred.data$age.chick))/sd(pred.data$age.chick)
pred.data$freq = 1
# 10 m
pred.data$pred.contact.10.pop = predict(m.contact.parsim, newdata=pred.data, re.form=NA, type='response')
pred.data$pred.contact.10.weight = pred.data$pred.contact.10.pop*(7/14) # weight in 2016
pred.data$pred.contact.10.weight[pred.data$yearf==2017] = pred.data$pred.contact.10.pop[pred.data$yearf==2017]*(4/14) # weight in 2017
pred.data$pred.contact.10.weight[pred.data$yearf==2018] = pred.data$pred.contact.10.pop[pred.data$yearf==2018]*(3/14) # weight in 2018
# 50 m 
pred.data$pred.contact.50.pop = predict(m.contact50m.parsim, newdata=pred.data, re.form=NA, type='response')
pred.data$pred.contact.50.weight = pred.data$pred.contact.50.pop*(7/14) # weight in 2016
pred.data$pred.contact.50.weight[pred.data$yearf==2017] = pred.data$pred.contact.50.pop[pred.data$yearf==2017]*(4/14) # weight in 2017
pred.data$pred.contact.50.weight[pred.data$yearf==2018] = pred.data$pred.contact.50.pop[pred.data$yearf==2018]*(3/14) # weight in 2018
# sum up pred.weight to get the weighted prediction over the three years:
weighted.pred.pop = aggregate(cbind(pred.contact.10.weight, pred.contact.50.weight, freq)~age.chick+sex.parent, pred.data, sum)
# averaged over males and females:
# 10 m:
mean.pred.pop.10 = aggregate(pred.contact.10.weight~age.chick, weighted.pred.pop, mean)
mean.pred.pop.10[mean.pred.pop.10$age.chick==40,] # 8.0%
mean.pred.pop.10[mean.pred.pop.10$age.chick==90,] # 1.9%
# 50 m
mean.pred.pop.50 = aggregate(pred.contact.50.weight~age.chick, weighted.pred.pop, mean)
mean.pred.pop.50[mean.pred.pop.50$age.chick==40,] # 13.2%
mean.pred.pop.50[mean.pred.pop.50$age.chick==90,] # 4.2%

### Preparation for Figure 2 ###
data.contacts <- aggregate(cbind(freq,contact,contact50m)~birdID.chick+birdID.parent+chick.parent+age.chick+sex.chick+sex.parent+sex.chick.parent+yday+year, chick.parent.data.contact.sel, sum)
data.contacts <- data.contacts[order(data.contacts$year, data.contacts$birdID.chick, data.contacts$yday),]
data.contacts$prop.contact <- data.contacts$contact/data.contacts$freq
data.contacts$prop.contact.50m <- data.contacts$contact50m/data.contacts$freq

## also do this for behaviour-specific contacts:
data.contacts.behav <- aggregate(cbind(freq,contact.begging,contact50m.begging, contact.foraging, contact50m.foraging)~birdID.chick+birdID.parent+chick.parent+age.chick+sex.chick+sex.parent+sex.chick.parent+yday+year, chick.parent.data.behav.sel, sum)
data.contacts.behav <- data.contacts.behav[order(data.contacts.behav$year, data.contacts.behav$birdID.chick, data.contacts.behav$yday),]
data.contacts.behav$prop.begging.contact <- data.contacts.behav$contact.begging/data.contacts.behav$freq
data.contacts.behav$prop.begging.contact.50m <- data.contacts.behav$contact50m.begging/data.contacts.behav$freq
data.contacts.behav$prop.foraging.contact <- data.contacts.behav$contact.foraging/data.contacts.behav$freq
data.contacts.behav$prop.foraging.contact.50m <- data.contacts.behav$contact50m.foraging/data.contacts.behav$freq

# calculate mean and 95% CI for overall contact probability per chick age and parent sex:
prop.contact.age.sexp = ddply(data.contacts[data.contacts$age.chick<100,], .(age.chick, sex.parent), summarize, 
                              N=length(prop.contact), mean = mean(prop.contact), 
                              CImin.boot = calculate.bootstrap.interval(prop.contact)[4], CImax.boot = calculate.bootstrap.interval(prop.contact)[5])
# only show estimates based on at least 6 different chicks:
prop.contact.age.mothers = prop.contact.age.sexp[prop.contact.age.sexp$sex.parent=='f' & prop.contact.age.sexp$N>5,]
prop.contact.age.fathers = prop.contact.age.sexp[prop.contact.age.sexp$sex.parent=='m' & prop.contact.age.sexp$N>5,]

# Make population-level predictions on proportion contact based on the most parsimonious model  
# with weighted average over the three years, based on the number of chicks in the different years (7 in 2016, 4 in 2017 and 3 in 2018; 14 in total):
pred.data = expand.grid(age.chick=35:100, sex.parent=c('f','m'), yearf=factor(2016:2018))
pred.data$z.age = (pred.data$age.chick-mean(chick.parent.data.contact.sel$age.chick))/sd(chick.parent.data.contact.sel$age.chick)
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
mean.pred.pop[mean.pred.pop$age.chick==40,] # 8.8%
mean.pred.pop[mean.pred.pop$age.chick==90,] # 2.7%
# calculate the mean begging contact probability:
pred.data$pred.begcontact.pop = predict(m.begging.contact.parsim, newdata=pred.data, re.form=NA, type='response')
pred.data$pred.begcontact.weight = pred.data$pred.begcontact.pop*(7/14) # weight in 2016
pred.data$pred.begcontact.weight[pred.data$yearf==2017] = pred.data$pred.begcontact.pop[pred.data$yearf==2017]*(4/14) # weight in 2017
pred.data$pred.begcontact.weight[pred.data$yearf==2018] = pred.data$pred.begcontact.pop[pred.data$yearf==2018]*(3/14) # weight in 2018
# sum up pred.weight to get the weighted prediction over the three years:
weighted.pred.begcontact.pop = aggregate(cbind(pred.begcontact.weight, freq)~age.chick+sex.parent, pred.data, sum)
# averaged over males and females:
mean.pred.begcontact.pop = aggregate(pred.begcontact.weight~age.chick, weighted.pred.begcontact.pop, mean)
mean.pred.begcontact.pop[mean.pred.begcontact.pop$age.chick==40,] # 0.4% 
mean.pred.begcontact.pop[mean.pred.begcontact.pop$age.chick==90,] # 0.07%
# % of contact time that the chick is begging:
(mean.pred.begcontact.pop$pred.begcontact.weight/mean.pred.pop$pred.contact.weight)[mean.pred.pop$age.chick==40] # 4.7%
(mean.pred.begcontact.pop$pred.begcontact.weight/mean.pred.pop$pred.contact.weight)[mean.pred.pop$age.chick==90] # 2.5%
# averaged over all ages
mean(mean.pred.begcontact.pop$pred.begcontact.weight/mean.pred.pop$pred.contact.weight) # 3.4%

# calculate mean and predicted distance to the nest of the chick during contact
mean.distance.chick.age.sexp = aggregate(dist.nest.chick~birdID.chick+age.chick+sex.parent, chick.data.actual.contact, calculate.mean.dist.km)

distance.contact.age.sexp = ddply(mean.distance.chick.age.sexp[mean.distance.chick.age.sexp$age.chick<100,], .(age.chick, sex.parent), summarize, 
                                  N=length(dist.nest.chick), mean = mean(dist.nest.chick), se = calculate.se(dist.nest.chick))
# only plot distance data based on at least three chick-parent pairs that still had (some) contact
distance.contact.age.fathers = distance.contact.age.sexp[distance.contact.age.sexp$sex.parent=='m' & distance.contact.age.sexp$N>2,]
distance.contact.age.mothers = distance.contact.age.sexp[distance.contact.age.sexp$sex.parent=='f' & distance.contact.age.sexp$N>2,]
# model predictions for distance tot the nest during contact
pred.data$pred.dist.pop = predict(m.dist.nest.parsim.gamma, newdata=pred.data, re.form=NA, type='response')
# remove the year effect, as not relevant for the distance:
pred.data.dist = unique(pred.data[,c('age.chick','sex.parent','pred.dist.pop')])

# make histogram of the number of chick-parent pairs in the dataset per chick age (i.e. with sufficient data to calculate the proportion of contact on a given day), and the number of chick-parent pairs that had at least some contact during these days / chick ages (for calculation of the distance to the nest during these contacts). 
N.prop.contact = table(data.contacts$age.chick[data.contacts$age.chick<=91], data.contacts$sex.parent[data.contacts$age.chick<=91])
N.dist.contact = table(data.contacts$age.chick[data.contacts$age.chick<=91 & data.contacts$contact>0], data.contacts$sex.parent[data.contacts$age.chick<=91 & data.contacts$contact>0])

### FIGURE 2 ###
pdf("output/Fig2.pdf", width=12,height=8)
#windows(12,8)
layout(matrix(c(1,2,3,4,3,4,3,4,3,4,3,4,5,6,5,6,5,6,5,6,5,6), ncol=2, byrow=T))
par(mar=c(0,1,0,0), oma=c(5,4,2,1), xaxs='i',las=0)
### (A) Sample sizes of chicks with a mother
barplot(rbind(N.dist.contact[,'f'], N.prop.contact[,'f']-N.dist.contact[,'f']), space=0, col=c('coral4','lightcoral'), ylim=c(0,8), xaxt='n', yaxt='n')
axis(2, at=c(2,4,6,8), las=1, cex.axis=1.2)
mtext("with mother",line=0.4)
mtext("N",2,3.5)
### (B) Sample sizes of chicks with a father
barplot(rbind(N.dist.contact[,'m'], N.prop.contact[,'m']-N.dist.contact[,'m']), space=0, col=c('lightskyblue4','lightskyblue'), ylim=c(0,8), xaxt='n', yaxt='n')
mtext("with father", line=0.4)
## (A) Overall probability of contact with the mother
ymax=0.3
xmin=34.5
xmax=91.5
plotCI(prop.contact.age.mothers$age.chick-0.07, prop.contact.age.mothers$mean, li=prop.contact.age.mothers$CImin.boot, ui=prop.contact.age.mothers$CImax.boot, sfrac=0, xlim=c(xmin,xmax), ylim=c(0,ymax), pch=21, pt.bg="lightcoral", col='coral4', xlab="", ylab='', xaxt='n', las=1, cex=1.2, cex.axis=1.2)
lines(pred.contact.weight~age.chick, weighted.pred.pop[weighted.pred.pop$sex.parent=='f',], col='lightcoral') 
mtext("Proportion of contact",2,3.5)
axis(1,at=seq(40,90,10),labels=F)
text(xmax-2,0.98*ymax,"(a)", cex=2)
## (B) Overall probability of contact with the father
plotCI(prop.contact.age.fathers$age.chick+0.07, prop.contact.age.fathers$mean, li=prop.contact.age.fathers$CImin.boot, ui=prop.contact.age.fathers$CImax.boot, sfrac=0, xlim=c(xmin,xmax), ylim=c(0,0.3), pch=21, pt.bg="lightskyblue", col='lightskyblue4', xlab="", ylab="", xaxt='n', yaxt='n', cex=1.2)
lines(pred.contact.weight~age.chick, weighted.pred.pop[weighted.pred.pop$sex.parent=='m',], col='lightskyblue') 
axis(1,at=seq(40,90,10),labels=F)
axis(2,at=seq(0,0.3,0.05),labels=F)
text(xmax-2,0.98*ymax,"(b)", cex=2)
## (C) Distance to the nest during contact with mother 
ymax=13
plotCI(distance.contact.age.mothers$age.chick, distance.contact.age.mothers$mean, uiw=1.96*distance.contact.age.mothers$se, sfrac=0, xlab='', ylab='', pch=21, pt.bg='lightcoral', col='coral4', las=1, xlim=c(xmin,xmax), ylim=c(0,ymax), cex=1.2, cex.axis=1.2)
lines(pred.dist.pop~age.chick, pred.data.dist[pred.data.dist$sex.parent=='f',], col='lightcoral') 
mtext("Distance to nest during contact (km)",2,3.5)
text(xmax-2,0.98*ymax,"(c)", cex=2)
## (D) Distance to the nest during contact with father 
plotCI(distance.contact.age.fathers$age.chick, distance.contact.age.fathers$mean, uiw=1.96*distance.contact.age.fathers$se, sfrac=0, xlab='', ylab='', pch=21, pt.bg='lightskyblue', col='lightskyblue4', las=1, xlim=c(xmin,xmax), ylim=c(0,ymax), yaxt='n', cex=1.2, cex.axis=1.2)
lines(pred.dist.pop~age.chick, pred.data.dist[pred.data.dist$sex.parent=='m',], col='lightskyblue') 
axis(2,at=seq(0,12,2),labels=F)
mtext("Chick age (days)", 1, 3, outer=T)
text(xmax-2,0.98*ymax,"(d)", cex=2)
dev.off()
### END FIGURE 2 ###

### FIGURE 3 ###
pdf("output/Fig3.pdf", width=7, height=7)
# windows(7,7)
plot(departure.datetime.chick.2018~departure.datetime.parent.2018, departure.dates.chick.parent, xlab='Departure date parent', ylab='Departure date chick', pch=21, bg=c('white','grey')[sex.parent.n], cex=2, xlim=c(ymd_hms('2018-09-12 00:00:00'),ymd_hms('2018-10-12 00:00:00')), ylim=c(ymd_hms('2018-09-12 00:00:00'),ymd_hms('2018-10-12 00:00:00')), xaxt='n', yaxt='n', cex.lab=1.2) 
lines(c(ymd_hms('2018-09-01 00:00:00'),ymd_hms('2018-10-31 00:00:00')),c(ymd_hms('2018-09-01 00:00:00'),ymd_hms('2018-10-31 00:00:00')), lty='dashed') 
axis(1, at=c(ymd_hms('2018-09-20 00:00:00'),ymd_hms('2018-09-30 00:00:00'),ymd_hms('2018-10-10 00:00:00')), labels=c('Sep-20','Sep-30','Oct-10'))
axis(2, at=c(ymd_hms('2018-09-20 00:00:00'),ymd_hms('2018-09-30 00:00:00'),ymd_hms('2018-10-10 00:00:00')), labels=c('Sep-20','Sep-30','Oct-10'))
legend("bottomright", legend=c("mother","father"), pt.cex=2, cex=1.2, pch=21, pt.bg=c('white','grey'))
dev.off()
### END FIGURE 3 ###


### FIGURE 5 ###
# Plot distance to the nest of chicks and parents and add dots at the moments where the chick was in contact with the parent
# For this plot, use the raw data, so that the departure on autumn migration is also visible. This is the file chick.parent.data.departure 
chick.parent.data.contact = chick.parent.data.contact[order(chick.parent.data.contact$year, chick.parent.data.contact$birdID.chick),]

chick.parent.data.departure$dist.nest.parent = round(distGeo(as.matrix(cbind(chick.parent.data.departure$longitude.parent, chick.parent.data.departure$latitude.parent)),
                                                           as.matrix(cbind(chick.parent.data.departure$lon.nest, chick.parent.data.departure$lat.nest))),0)
chick.parent.data.departure$dist.nest.chick = round(distGeo(as.matrix(cbind(chick.parent.data.departure$longitude.chick, chick.parent.data.departure$latitude.chick)),
                                                       as.matrix(cbind(chick.parent.data.departure$lon.nest, chick.parent.data.departure$lat.nest))),0)

pdf("output/Fig5.pdf", width=16, height=8)
#windows(16,8)
layout(matrix(1:16, nrow=4, byrow=T))
par(mar=c(1,1,0,0), oma=c(4,4,1,1))
panelnr=0
ymax=50000
for (i in unique(chick.parent.data.contact$chick.parent)) { # but only for the chick-parent pairs with enough data:
  panelnr=panelnr+1
  sex.parent.n = ifelse(chick.parent.data.departure$sex.parent[chick.parent.data.departure$chick.parent==i][1]=='f',1,2)
  plot(dist.nest.parent~age.chick, data = chick.parent.data.departure[chick.parent.data.departure$chick.parent==i,], xlim=c(35,140), ylim=c(0,ymax), type='l', xlab="", ylab="", xaxt='n', yaxt='n', col=c('orange','lightskyblue')[sex.parent.n])
  lines(dist.nest.chick~age.chick, data = chick.parent.data.departure[chick.parent.data.departure$chick.parent==i,])
  # overlay a dashed line for the parents again, so that the similar departure of 6295 and 6288 is visible as well as a similar long-distance flight by 6354-6298 (both >120d, so only overlay this part so that for the rest, the line of the chick is better visible:
  lines(dist.nest.parent~age.chick, data = chick.parent.data.departure[chick.parent.data.departure$chick.parent==i&chick.parent.data.departure$age.chick>120,], col=c('orange','lightskyblue')[sex.parent.n], lty='dashed')
  points(dist.nest.chick~age.chick, data = chick.parent.data.departure[chick.parent.data.departure$chick.parent==i&chick.parent.data.departure$contact==1,], col='red')
  if (panelnr %in% c(1,5,9,13)) axis(2, at=seq(0,ymax,10000), labels=seq(0,ymax,10000)/1000, las=1, cex.axis=1.2)
  if (panelnr %in% 13:16) axis(1, at=seq(40,140,20), las=1, cex.axis=1.2)
  text(35,0.95*ymax, i, adj=0, cex=1.5)
}
mtext("Distance to the nest (km)",2,2,outer=T, cex=1.3)
mtext("Chick age (days)",1,2,outer=T, cex=1.3)
dev.off()
### END FIGURE 5 ###



# now make separate panel for each chickID:
# first determine chick age at chick and parent departure date:
chick.age.departure = unique(chick.parent.data.contact.sel[,c('birdID.chick','birdID.parent','sex.parent','start_deployment','age_deployment','departure.datetime.chick','departure.datetime.parent')])
chick.age.departure$hatch_date = chick.age.departure$start_deployment - chick.age.departure$age_deployment
chick.age.departure$departure_day_chick = yday(chick.age.departure$departure.datetime.chick)
chick.age.departure$departure_day_parent = yday(chick.age.departure$departure.datetime.parent)
chick.age.departure$hatch_day = yday(chick.age.departure$hatch_date)
chick.age.departure$age_departure_chick = chick.age.departure$departure_day_chick - chick.age.departure$hatch_day
chick.age.departure$age_departure_parent = chick.age.departure$departure_day_parent - chick.age.departure$hatch_day

# model predictions at chick-level for overall contact:
data.contacts$yearf <- as.factor(data.contacts$year)
data.contacts$z.age <- (data.contacts$age.chick-mean(chick.parent.data.contact.sel$age.chick))/sd(chick.parent.data.contact.sel$age.chick)
data.contacts$pred.contact <- predict(m.contact.parsim, newdata=data.contacts, type='response')

### FIGURE S2 - Individual variation in probability of contact ###
pdf("output/FigS2.pdf", width=12, height=8)
#windows(12,8)
layout(matrix(1:16, nrow=4, byrow=T))
par(mar=c(1,1,0,0), oma=c(4,4,1,1))
panelnr = 0
for(i in unique(data.contacts$birdID.chick)) {
  panelnr = panelnr+1
  ymax=0.7
  data.chick = data.contacts[data.contacts$birdID.chick==i,]
  if (data.chick$sex.chick[1]=='f') chickcol='coral4' else chickcol='lightskyblue4'
  plot(contact~age.chick, data.chick, pch=19, cex=0.6, type="n", xlim=c(35,140), ylim=c(0,ymax), xaxt='n', yaxt='n')
  # box(col=chickcol)
  # first plot white-filled points with zero-contact:
  points(prop.contact~age.chick, data.chick[data.chick$sex.parent=='f'&data.chick$prop.contact==0,], pch=21, bg="white", col='coral4')
  points(prop.contact~age.chick, data.chick[data.chick$sex.parent=='m'&data.chick$prop.contact==0,], pch=21, bg="white", col='lightskyblue4')
  # then plot colour-filled points with >0 contact:
  points(prop.contact~age.chick, data.chick[data.chick$sex.parent=='f'&data.chick$prop.contact>0,], pch=21, bg="lightcoral", col='coral4')
  points(prop.contact~age.chick, data.chick[data.chick$sex.parent=='m'&data.chick$prop.contact>0,], pch=21, bg="lightskyblue", col='lightskyblue4')
  lines(pred.contact~age.chick, data.chick[data.chick$sex.parent=='f',], col='coral4', lwd=2)
  lines(pred.contact~age.chick, data.chick[data.chick$sex.parent=='m',], col='lightskyblue4', lwd=2)
  # add vertical lines for chick age at departure date of chick and parent:
  age_departure_chick = chick.age.departure$age_departure_chick[chick.age.departure$birdID.chick==i][1]
  age_departure_father = chick.age.departure$age_departure_parent[chick.age.departure$birdID.chick==i&chick.age.departure$sex.parent=='m']
  age_departure_mother = chick.age.departure$age_departure_parent[chick.age.departure$birdID.chick==i&chick.age.departure$sex.parent=='f']
  lines(rep(age_departure_chick,2),c(0,ymax))
  if(length(age_departure_father)>0) lines(rep(age_departure_father,2),c(0,ymax),col='lightskyblue4',lty='dashed')
  if(length(age_departure_mother)>0) lines(rep(age_departure_mother,2),c(0,ymax),col='coral4', lty='dashed')
  if (panelnr %in% c(1,5,9,13)) axis(2, at=seq(0,ymax,0.1), las=1)
  if (panelnr %in% 11:14) axis(1, at=seq(40,140,20), las=1)
  # box(col=chickcol)
  text(40,0.95*ymax,paste(i," (",data.chick$year[1],")",sep=""), adj=0, col=chickcol)
}
plot(1:2,1:2,type='n', xaxt='n', yaxt='n', bty='n') # make an empty plot to put the legend on top of it:
legend(1,1.7,legend=c("female","male"), text.col=c("coral4","lightskyblue4"), bty='n',cex=2, pch=21, pt.bg=c("lightcoral","lightskyblue"), col=c("coral4","lightskyblue4"))
mtext("Chick age (days)",1,2,outer=T,cex=1.2)
mtext("Proportion of contact",2,2,outer=T,cex=1.2)
dev.off()
# strong year effect in the amount of contact between chick and parent. In 2017, there was hardly any post-fledging parental care!
### END FIGURE S2 ###

# model predictions at chick-level for behaviour-specific contact:
data.contacts.behav$yearf <- as.factor(data.contacts.behav$year)
data.contacts.behav$z.age <- (data.contacts.behav$age.chick-mean(chick.parent.data.behav.sel$age.chick))/sd(chick.parent.data.behav.sel$age.chick)
data.contacts.behav$pred.begging.contact <- predict(m.begging.contact.parsim, newdata=data.contacts.behav, type='response')

### FIGURE S3 ###
## Plot individual variation in probability of begging at parent and foraging close to parent:
pdf("output/FigS3.pdf", width=12, height=8)
layout(matrix(1:16, nrow=4, byrow=T))
par(mar=c(1,1,0,0), oma=c(4,4.5,1,1))
panelnr = 0
for(i in unique(data.contacts.behav$birdID.chick)) {
  panelnr = panelnr+1
  ymax=1.2*max(data.contacts.behav$prop.begging.contact)
  data.chick = data.contacts.behav[data.contacts.behav$birdID.chick==i,]
  if (data.chick$sex.chick[1]=='f') chickcol='coral4' else chickcol='lightskyblue4'
  plot(prop.begging.contact~age.chick, data.chick, pch=19, cex=0.6, type="n", xlim=c(35,140), ylim=c(0,ymax), xaxt='n', yaxt='n')
  # first plot white-filled points with zero-contact:
  points(prop.begging.contact~age.chick, data.chick[data.chick$sex.parent=='f'&data.chick$prop.begging.contact==0,], pch=21, bg="white", col='coral4')
  points(prop.begging.contact~age.chick, data.chick[data.chick$sex.parent=='m'&data.chick$prop.begging.contact==0,], pch=21, bg="white", col='lightskyblue4')
  # then plot colour-filled points with >0 contact:
  points(prop.begging.contact~age.chick, data.chick[data.chick$sex.parent=='f'&data.chick$prop.begging.contact>0,], pch=21, bg="lightcoral", col='coral4')
  points(prop.begging.contact~age.chick, data.chick[data.chick$sex.parent=='m'&data.chick$prop.begging.contact>0,], pch=21, bg="lightskyblue", col='lightskyblue4')
  lines(pred.begging.contact~age.chick, data.chick[data.chick$sex.parent=='f',], col='coral4', lwd=2)
  lines(pred.begging.contact~age.chick, data.chick[data.chick$sex.parent=='m',], col='lightskyblue4', lwd=2)
  # add vertical lines for chick age at departure date of chick and parent:
  age_departure_chick = chick.age.departure$age_departure_chick[chick.age.departure$birdID.chick==i][1]
  age_departure_father = chick.age.departure$age_departure_parent[chick.age.departure$birdID.chick==i&chick.age.departure$sex.parent=='m']
  age_departure_mother = chick.age.departure$age_departure_parent[chick.age.departure$birdID.chick==i&chick.age.departure$sex.parent=='f']
  lines(rep(age_departure_chick,2),c(0,ymax))
  if(length(age_departure_father)>0) lines(rep(age_departure_father,2),c(0,ymax),col='lightskyblue4',lty='dashed')
  if(length(age_departure_mother)>0) lines(rep(age_departure_mother,2),c(0,ymax),col='coral4', lty='dashed')
  if (panelnr %in% c(1,5,9,13)) axis(2, at=seq(0,ymax,0.01), las=1)
  if (panelnr %in% 11:14) axis(1, at=seq(40,140,20), las=1)
  # box(col=chickcol)
  text(40,0.95*ymax,paste(i," (",data.chick$year[1],")",sep=""), adj=0, col=chickcol)
}
plot(1:2,1:2,type='n', xaxt='n', yaxt='n', bty='n') # make an empty plot to put the legend on top of it:
legend(1,1.7,legend=c("female","male"), text.col=c("coral4","lightskyblue4"), bty='n',cex=2, pch=21, pt.bg=c("lightcoral","lightskyblue"), col=c("coral4","lightskyblue4"))
mtext("Chick age (days)",1,2,outer=T,cex=1.2)
mtext("Proportion of begging at parent",2,2.5,outer=T,cex=1.2)
dev.off()
### END FIGURE S3 ###

data.contacts.behav$pred.foraging.contact <- predict(m.foraging.contact.parsim, newdata=data.contacts.behav, type='response')

### FIGURE S4 ###
pdf("output/FigS4.pdf", width=12, height=8)
layout(matrix(1:16, nrow=4, byrow=T))
par(mar=c(1,1,0,0), oma=c(4,4.5,1,1))
panelnr = 0
for(i in unique(data.contacts.behav$birdID.chick)) {
  panelnr = panelnr+1
  ymax=1.2*max(data.contacts.behav$prop.foraging.contact)
  data.chick = data.contacts.behav[data.contacts.behav$birdID.chick==i,]
  if (data.chick$sex.chick[1]=='f') chickcol='coral4' else chickcol='lightskyblue4'
  plot(prop.foraging.contact~age.chick, data.chick, pch=19, cex=0.6, type="n", xlim=c(35,140), ylim=c(0,ymax), xaxt='n', yaxt='n')
  # first plot white-filled points with zero-contact:
  points(prop.foraging.contact~age.chick, data.chick[data.chick$sex.parent=='f'&data.chick$prop.foraging.contact==0,], pch=21, bg="white", col='coral4')
  points(prop.foraging.contact~age.chick, data.chick[data.chick$sex.parent=='m'&data.chick$prop.foraging.contact==0,], pch=21, bg="white", col='lightskyblue4')
  # then plot colour-filled points with >0 contact:
  points(prop.foraging.contact~age.chick, data.chick[data.chick$sex.parent=='f'&data.chick$prop.foraging.contact>0,], pch=21, bg="lightcoral", col='coral4')
  points(prop.foraging.contact~age.chick, data.chick[data.chick$sex.parent=='m'&data.chick$prop.foraging.contact>0,], pch=21, bg="lightskyblue", col='lightskyblue4')
  lines(pred.foraging.contact~age.chick, data.chick[data.chick$sex.parent=='f',], col='coral4', lwd=2)
  lines(pred.foraging.contact~age.chick, data.chick[data.chick$sex.parent=='m',], col='lightskyblue4', lwd=2)
  # add vertical lines for chick age at departure date of chick and parent:
  age_departure_chick = chick.age.departure$age_departure_chick[chick.age.departure$birdID.chick==i][1]
  age_departure_father = chick.age.departure$age_departure_parent[chick.age.departure$birdID.chick==i&chick.age.departure$sex.parent=='m']
  age_departure_mother = chick.age.departure$age_departure_parent[chick.age.departure$birdID.chick==i&chick.age.departure$sex.parent=='f']
  lines(rep(age_departure_chick,2),c(0,ymax))
  if(length(age_departure_father)>0) lines(rep(age_departure_father,2),c(0,ymax),col='lightskyblue4',lty='dashed')
  if(length(age_departure_mother)>0) lines(rep(age_departure_mother,2),c(0,ymax),col='coral4', lty='dashed')
  if (panelnr %in% c(1,5,9,13)) axis(2, at=seq(0,ymax,0.02), las=1)
  if (panelnr %in% 11:14) axis(1, at=seq(40,140,20), las=1)
  # box(col=chickcol)
  text(40,0.95*ymax,paste(i," (",data.chick$year[1],")",sep=""), adj=0, col=chickcol)
}
plot(1:2,1:2,type='n', xaxt='n', yaxt='n', bty='n') # make an empty plot to put the legend on top of it:
legend(1,1.7,legend=c("female","male"), text.col=c("coral4","lightskyblue4"), bty='n',cex=2, pch=21, pt.bg=c("lightcoral","lightskyblue"), col=c("coral4","lightskyblue4"))
mtext("Chick age (days)",1,2,outer=T,cex=1.2)
mtext("Proportion of foraging with parent",2,2.5,outer=T,cex=1.2)
dev.off()
### END FIGURE S4 ###

### Make a barplot with the behaviour of the chick and whether or not it was in contact with a parent. 
chick.parent.data.behav.sel$behaviour.chick.pooled = chick.parent.data.behav.sel$behaviour.chick
chick.parent.data.behav.sel$behaviour.chick.pooled[chick.parent.data.behav.sel$behaviour.chick=='foraging' & chick.parent.data.behav.sel$habitat.simple=='land'] = 'other'
chick.parent.data.behav.sel$behaviour.chick.pooled[chick.parent.data.behav.sel$behaviour.chick.pooled%in%c('sit','stand')] = 'rest'
chick.parent.data.behav.sel$behaviour.chick.contact = paste(chick.parent.data.behav.sel$contact, chick.parent.data.behav.sel$behaviour.chick.pooled, sep="_")
chick.parent.data.behav.sel$freq = 1
dur.behav.contact.age.xtabs = xtabs(freq~behaviour.chick.contact+age.chick, data=chick.parent.data.behav.sel)
dur.behav.contact.age.prop = as.matrix(prop.table(dur.behav.contact.age.xtabs, 2))
dur.behav.contact.age.prop = dur.behav.contact.age.prop[c('1_beg','1_foraging','1_rest','1_other','1_flying',
                             '0_flying','0_other','0_rest','0_foraging','0_beg'),]
rownames(dur.behav.contact.age.prop)
colnames(dur.behav.contact.age.prop)
round(rowSums(dur.behav.contact.age.prop),1)
cols.behaviour = c('red','blue','darkgreen','purple','orange', 
                   'gold','violet','lightgreen','skyblue','pink')

dur.behav.contact.age.prop[,1]

windows(width=20,height=10)
par(oma=c(0,0,0,10))
barp.data = barplot(dur.behav.contact.age.prop, main='', col=cols.behaviour, las=1, xaxt='n' , cex.axis=1.2, border=NA, xlab="Chick age (days)", ylab="Proportion of time", cex.lab=1.3)
axis(1, barp.data[colnames(dur.behav.contact.age.prop)%in%seq(40,120,20)], seq(40,120,20), cex.axis=1.2)
mtext('Age (days)', 1, 3, cex=1.2, outer=T)
mtext('Proportion of time spent', 2, 3, cex=1.2, outer=T)
legend(125, 0.7, rev(c('begging, contact','foraging, contact', 'resting, contact', 'other, contact', 'flying, contact', 'flying, no contact', 'other, no contact', 'resting, no contact', 'foraging, no contact', 'begging, no contact')), col = rev(cols.behaviour), 
       pch=15, bty='n', xpd=NA, cex=1.2)

# number of chick-parent pairs on which this time allocation is based on:
chick.parents.per.age.chick = unique(st_drop_geometry(chick.parent.data.behav.sel[,c('chick.parent','age.chick')]))
# add sample sizes to the plot at every 5 days:
N_age = colSums(table(chick.parents.per.age.chick))
N_age[6+seq(0,95,5)]
axis(3, barp.data[6+seq(0,95,5)], N_age[6+seq(0,95,5)], tick = F, cex.axis=1)
axis(3, barp.data[1], "N =", tick=F)
# this figure is currently not used in the manuscript, but gives a nice picture of changes in the time allocation of chicks during post-fledging. 
