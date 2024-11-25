# redo the analysis with chickprntage 

# (1) TOTAL AMOUNT OF CONTACT

model.sel.contact.chickprntagernd <- dredge(m.contact.chickprntagernd) # started running around 10:00. Finished around 11:00. Gave several warnings:
# 1x: Model failed to converge: degenerate Hessian with 1 negative eigenvalues 
# 2x: variance-covariance matrix computed from finite-difference Hessian is
# not positive definite or contains NA values: falling back to var-cov estimated from RX
# 1x: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
# unable to evaluate scaled gradient
# Interestingly, there was no warning for the full model, so apparently reduced (simpler) models caused convergence issues while the full model did not...
model.sel.contact.chickprntagernd # model with only chick age was best-supported.
m.contact.chickprntagernd.parsim <- glmer(contact~z.age+(z.age|chick.parent), data=chick.parent.data.contact.sel, family='binomial', na.action='na.fail')
summary(m.contact.chickprntagernd.parsim)

# (2) BEHAVIOUR DURING CONTACT AND AMOUNT OF ACTIVE CONTACT
modsel.active.contact.chickprntagernd = dredge(m.active.contact.chickprntagernd) # takes more than an hour
# 4x warning: Model failed to converge with max|grad| = 0.0175261 (tol = 0.002, component 1)
modsel.active.contact.chickprntagernd # again, only chick age is supported.
m.active.contact.pars <- glmer(active.contact~z.age+(z.age|chick.parent), data=chick.parent.data.behav.sel, family='binomial', na.action='na.fail')

# (3) HABITAT DURING (ACTIVE) CONTACT

# given that there is ACTIVE contact, in which habitat do these contacts take place, and does this depend on the chick and parent's sex?
chick.parent.data.behav.active.contact = chick.parent.data.behav.sel[chick.parent.data.behav.sel$active.contact==1,]
table(chick.parent.data.behav.active.contact$habitat.simple, chick.parent.data.behav.active.contact$sex.chick.parent)
# very limited data in freshwater. Model does not converge. 
m.active.contact.marine <- glmer(marine~sex.parent*sex.chick+(1|chick.parent), data=chick.parent.data.behav.active.contact, family='binomial', na.action='na.fail')
dredge(m.active.contact.marine) # no effect of sex of parent or chick on the probability of ACTIVE contact to take place in marine habitat. 
m.active.contact.land <- glmer(land~sex.parent*sex.chick+(1|chick.parent), data=chick.parent.data.behav.active.contact, family='binomial', na.action='na.fail')
dredge(m.active.contact.land) # no effect of sex of parent or chick on the probability of ACTIVE contact to take place on land.
# So despite the overall numbers suggesting some habitat- and sex-related differences, these are not significant. 
