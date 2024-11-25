# redo the analysis using 50 m distance between chick and parent as contact

# what if we use a broader definition of contact, where we can assume that birds can still see (and recognize) each other. As, to be able to follow a parent to the foraging grounds, a chick doens't have to be as close as 10 m from its parent. This might be possible up to 50-100 m. 
# check the distribution of distances between chicks and their parent(s) for this:
hist(chick.parent.data.sel$distance, xlim=c(0,500), breaks=200000) # 50m indeed seems to suitable cut-off point after which distance frequencies no longer show a strong decreasing trend. 
hist(chick.parent.data.sel$distance, xlim=c(0,1000), breaks=200000) 
# check how results change when using a distance of <50m as contact. 
chick.parent.data.contact.sel$contact50m = ifelse(chick.parent.data.contact.sel$distance<50,1,0)

### 1. TOTAL AMOUNT OF CONTACT
m.contact50m.chickagernd <- glmer(contact50m~sex.parent*sex.chick+sex.parent*z.age+sex.chick*z.age+(z.age|birdID.chick), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail')
summary(m.contact50m.chickagernd)
# based on this summary, remove sex.parent*age and sex.chick*age:
model.sel.contact50m.chickagernd = dredge(m.contact50m.chickagernd)
m.contact50m.chickagernd.parsim <- glmer(contact50m~sex.parent*sex.chick+sex.parent*z.age+(z.age|birdID.chick), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail')
summary(m.contact50m.chickagernd.parsim) # very similar result compared to the 10m distance criterion for contact. 

### 2. BEHAVIOUR DURING CONTACT AND AMOUNT OF ACTIVE CONTACT 
chick.parent.data.behav.sel$contact50m = ifelse(chick.parent.data.behav.sel$distance<50,1,0)
chick.parent.data.behav.sel$active.contact.50m = 0
chick.parent.data.behav.sel$active.contact.50m[chick.parent.data.behav.sel$behaviour.parent!='resting'&chick.parent.data.behav.sel$behaviour.chick!='resting' & chick.parent.data.behav.sel$contact50m==1] = 1
table(chick.parent.data.behav.sel$active.contact.50m) # three times more cases (1593) than when using <10m as contact; still not many though

m.active.contact50m.chickagernd <- glmer(active.contact.50m~sex.parent*sex.chick+sex.parent*z.age+sex.chick*z.age+(z.age|birdID.chick), data=chick.parent.data.behav.sel, family='binomial', na.action='na.fail')
summary(m.active.contact50m.chickagernd)
# based on this, it seems that only chick x age is not supported:
m.active.contact50m.chickagernd.pars <- glmer(active.contact.50m~sex.parent*sex.chick+sex.parent*z.age+(z.age|birdID.chick), data=chick.parent.data.behav.sel, family='binomial', na.action='na.fail')
summary(m.active.contact50m.chickagernd.pars)
emmeans(m.active.contact50m.chickagernd.pars, pairwise~sex.chick*sex.parent) 
# female parents do not have more active contact with daughters than with sons (f f = m f)
# male parents do not have more active contact with daughters than with sons (f m = m m)
# female parents have more active contact with their daughters than male parents (f f >> f m)
# female parents have more active contact with their sons than male parents (m f >> m m)
# female parents tend to have more active contact with their daughters than fathers with their sons. (f f > m m)
# female parents tend to have more active contact with their sons than fathers have with their daughters. (m f > f m)
# very similar results as when using 10m as a contact moment. Except that here, the interaction between parent sex and chick age is significant (active contact decreasing more strong with age for fathers than for mothers...)  

# habitat during active contact
chick.parent.data.behav.sel$active.contact50 = ifelse(chick.parent.data.behav.sel$distance<=50 & chick.parent.data.behav.sel$behaviour.beg.chick!='resting' & chick.parent.data.behav.sel$behaviour.beg.parent!='resting',1,0)
chick.parent.data.behav.active.contact50 = chick.parent.data.behav.sel[which(chick.parent.data.behav.sel$active.contact50==1),]
dim(chick.parent.data.behav.active.contact50)
m.active.contact50.freshwater <- glmer(freshwater~sex.parent*sex.chick+(1|birdID.chick), data=chick.parent.data.behav.active.contact50, family='binomial', na.action='na.fail')
dredge(m.active.contact50.freshwater) # 
summary(m.active.contact50.freshwater) # males are more likely to have active contact with their chick in freshwater than females.
m.active.contact50.marine <- glmer(marine~sex.parent*sex.chick+(1|birdID.chick), data=chick.parent.data.behav.active.contact50, family='binomial', na.action='na.fail')
dredge(m.active.contact50.marine) 
summary(m.active.contact50.marine) # mothers are more likely to have active contact with their chick in marine habitat than fathers. 
m.active.contact50.land <- glmer(land~sex.parent*sex.chick+(1|birdID.chick), data=chick.parent.data.behav.active.contact50, family='binomial', na.action='na.fail')
dredge(m.active.contact50.land)
summary(m.active.contact50.land) # fathers are more likely to have active contact on land than mothers

# does contact take place during day or night?
active.contact50.hour.sex = table(chick.parent.data.behav.sel$active.contact50, chick.parent.data.behav.sel$sex.chick.parent, hour(chick.parent.data.behav.sel$datetime.chick))
calculate.prop.active.contact = function(x) round(x[2,]/colSums(x),4)
test = active.contact50.hour.sex[,,1]
calculate.prop.active.contact(test)
prop.active.contact50.hour.sex = apply(active.contact50.hour.sex,3,calculate.prop.active.contact)
windows()
plot(0:23,prop.active.contact50.hour.sex[1,], type='l',ylim=c(0,max(prop.active.contact50.hour.sex)), col='red', xlab= "Hour of the day", ylab="Proportion of the active contact within 50 m")
lines(0:23,prop.active.contact50.hour.sex[2,], col='orange')
lines(0:23,prop.active.contact50.hour.sex[3,], col='green')
lines(0:23,prop.active.contact50.hour.sex[4,], col='blue')
