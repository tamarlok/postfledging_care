# redo the analysis using 50 m distance between chick and parent as contact

# what if we use a broader definition of contact, where we can assume that birds can still see (and recognize) each other. As, to be able to follow a parent to the foraging grounds, a chick doens't have to be as close as 10 m from its parent. This might be possible up to 50-100 m. 
# check the distribution of distances between chicks and their parent(s) for this:
hist(chick.parent.data.sel$distance, xlim=c(0,500), breaks=200000) # 50m indeed seems to suitable cut-off point after which distance frequencies no longer show a strong decreasing trend. 
hist(chick.parent.data.sel$distance, xlim=c(0,1000), breaks=200000) 
# check how results change when using a distance of <50m as contact. 
chick.parent.data.contact.sel$contact50m = ifelse(chick.parent.data.contact.sel$distance<50,1,0)

### 1. TOTAL AMOUNT OF CONTACT
m.contact50m.chickagernd <- glmer(contact50m~sex.parent+sex.chick+z.age+yearf+(z.age|birdID.chick), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail')
summary(m.contact50m.chickagernd)
# based on summary, only parent sex and year have significant effecs:
m.contact50m.chickagernd.parsim <- glmer(contact50m~sex.parent+yearf+(z.age|birdID.chick), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail')
summary(m.contact50m.chickagernd.parsim) # very similar result compared to the 10m distance criterion for contact. 

model.sel.contact50m.chickagernd = dredge(m.contact50m.chickagernd, fixed="yearf")

# make predictions with 50m contact
pred.data = expand.grid(age.chick=30:100, sex.parent=c('f','m'), yearf=factor(2016:2018))
pred.data$z.age = (pred.data$age.chick-mean(pred.data$age.chick))/sd(pred.data$age.chick)
pred.data$pred.contact.pop = predict(m.contact50m.chickagernd.parsim, newdata=pred.data, re.form=NA, type='response')
pred.data$pred.contact.weight = pred.pop.unique$pred.contact.pop*(7/14) # weight in 2016
pred.data$pred.contact.weight[pred.data$yearf==2017] = pred.data$pred.contact.pop[pred.data$yearf==2017]*(4/14) # weight in 2017
pred.data$pred.contact.weight[pred.data$yearf==2018] = pred.data$pred.contact.pop[pred.data$yearf==2018]*(3/14) # weight in 2018
pred.data$freq = 1
# sum up pred.weight to get the weighted prediction over the three years:
weighted.pred.pop = aggregate(cbind(pred.contact.weight, freq)~age.chick+sex.parent, pred.data, sum)
# averaged over males and females:
mean.pred.pop = aggregate(pred.contact.weight~age.chick, weighted.pred.pop, mean)
mean.pred.pop[mean.pred.pop$age.chick==40,]
mean.pred.pop[mean.pred.pop$age.chick==90,]


### 2. AMOUNT OF BEGGING CONTACT
chick.parent.data.behav.beg$contact50m = ifelse(chick.parent.data.behav.beg$distance<50,1,0)
chick.parent.data.behav.beg$contact50m.begging = chick.parent.data.behav.beg$contact50m * chick.parent.data.behav.beg$begging

m.begging.contact50m <- glmer(contact50m.begging~sex.parent+sex.chick+z.age+yearf+(z.age|birdID.chick), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail')
summary(m.begging.contact50m)
# here, the chick sex effect becomes significant, with male chicks having less begging contact than female chicks. 
# based on this, it seems that only chick x age is not supported:
m.begging.contact50m.pars <- glmer(contact50m.begging~sex.parent+sex.chick+z.age+yearf+(z.age|birdID.chick), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail')
summary(m.begging.contact50m.pars)

### 3. AMOUNT OF FORAGING CONTACT
chick.parent.data.behav.beg$contact50m.foraging = chick.parent.data.behav.beg$contact50m * chick.parent.data.behav.beg$foraging

m.foraging.contact50m <- glmer(contact50m.foraging~sex.parent+sex.chick+z.age+yearf+(z.age|birdID.chick), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail')
summary(m.foraging.contact50m)
# based on this, it seems that only chick x age is not supported:
m.foraging.contact50m.pars <- glmer(contact50m.foraging~sex.parent+yearf+(z.age|birdID.chick), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail')
summary(m.foraging.contact50m.pars)

save.image("data/processed/results.analyses.0206.RData")