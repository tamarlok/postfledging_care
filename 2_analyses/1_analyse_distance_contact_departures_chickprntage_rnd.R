# redo the analysis with chickprntage 

# (1) TOTAL AMOUNT OF CONTACT

m.contact.chkprntagernd <- glmer(contact~sex.parent+sex.chick+z.age+yearf+(z.age|chick.parent), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail') 
summary(m.contact.chkprntagernd)
model.sel.contact.chkprntagernd <- dredge(m.contact.chkprntagernd, fixed='yearf')

TableS5a.chkprntagernd = make.table.from.dredge.output(model.sel.contact.chkprntagernd)
write.csv(TableS5a.chkprntagernd, "output/TableS5a - chkprntagernd - Proportion of overall contact.csv")

# (2) BEHAVIOUR DURING CONTACT

# BEGGING
m.begging.chkprntagernd <- glmer(contact.begging~sex.parent+sex.chick+z.age+yearf+(z.age|chick.parent), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail') # convergence issues
modsel.begging.chkprntagernd <- dredge(m.begging.chkprntagernd, fixed='yearf') # three models with convergence issues, but not the full model. Perhaps models where z.age is not a fixed effect while a random slope is included. 
TableS5b.chkprntagernd = make.table.from.dredge.output(modsel.begging.chkprntagernd)
write.csv(TableS5b.chkprntagernd, "output/TableS5b - chkprntagernd - Proportion of begging contact.csv")

# FORAGING
m.foraging.chkprntagernd <- glmer(contact.foraging~sex.parent+sex.chick+z.age+yearf+(z.age|chick.parent), data=chick.parent.data.behav.beg, family='binomial', na.action='na.fail')
modsel.foraging.chkprntagernd <- dredge(m.foraging.chkprntagernd, fixed='yearf')
TableS5c.chkprntagernd = make.table.from.dredge.output(modsel.foraging.chkprntagernd)
write.csv(TableS5c.chkprntagernd, "output/TableS5c - chkprntagernd - Proportion of foraging contact.csv")

# DISTANCE TO NEST
m.dist.nest.chkprntagernd = lmer(dist.nest.chick/1000~sex.parent+sex.chick+z.age+(z.age|chick.parent), chick.data.actual.contact, na.action='na.fail', REML=F)
modsel.dist.nest.chkprntagernd = dredge(m.dist.nest.chkprntagernd)
TableS5d.chkprntagernd = make.table.from.dredge.output(modsel.dist.nest.chkprntagernd)
write.csv(TableS5d.chkprntagernd, "output/TableS5d - chkprntagernd - Distance to nest during contact.csv")

save.image("data/processed/results.analyses.1126.RData")
