random.props = rbinom(100,10,0.5)/10
mean(random.props)
mean.logit <- function(x) mean(qlogis(x))
plogis(mean.logit(random.props))
# very similar values for 'normal' mean and mean calculated via logit. This might be because the values are around 0.5, so far away from 0 and 1.
# what if we choose a value close to 1.
random.props = rbinom(100,1000,0.05)/1000
min(random.props)
mean(random.props)
plogis(mean.logit(random.props))
# still very similar. 

windows()
plot(contact~contact.adj, prop.contact.age.pair)
mean(prop.contact.age.pair$contact)
mean(prop.contact.age.pair$contact.adj)
plogis(mean.logit(prop.contact.age.pair$contact.adj))
# perhaps the difference comes from the fact that the distribution is not normal, but very skewed towards 0:
hist(prop.contact.age.pair$contact.adj)

random.values = rlnorm(1000, meanlog=0, sdlog=1)
hist(random.values)
# make it proportional data:
random.values = random.values / (max(random.values)+1)
hist(random.values)
min(random.values)
max(random.values)
mean(random.values)
plogis(mean.logit(random.values))
# yes, there the difference occurs... 
# but then, what is the correct way of calculating mean proportions? 