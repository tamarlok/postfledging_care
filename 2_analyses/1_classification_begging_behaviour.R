# load annotated begging data:
annotated.begging.data = read.csv("data/raw/annotated.begging.data.csv", header=T)

# combine annotated begging behaviour with previously annotated behaviours from Lok et al. 2023 Anim Biotelem available at https://doi.org/10.25850/nioz/7b.b.yd
acc.soaring <- read.csv("data/raw/visual_annotated_passive_flight_data.csv") 
acc.video <- read.csv("data/raw/video_annotated_data.csv") 
behaviours <- read.csv("data/raw/behaviours.csv") # load list of behaviours distinguished during video annotation
acc.video <- acc.video[,1:10] # remove the video information columns, not needed for analysis
acc.annotated <- rbind(acc.video, acc.soaring) # combine visual and video annotated data
acc.annotated$date.time <- ymd_hms(acc.annotated$date.time)
acc.annotated$behaviour <- as.character(behaviours$behaviour[match(acc.annotated$behaviour.index, behaviours$behaviour.index)]) # add column with behaviour names
acc.annotated$Index = acc.annotated$Index+1 # make the Index in this datafile start at 1
acc.annotated$annotation.method = 'video'

# combine the two datasets:
acc.annotated.comb = rbind(annotated.begging.data, acc.annotated[,names(annotated.begging.data)])
acc.annotated.comb$behaviour[acc.annotated.comb$behaviour=='shake_feathers']='stand-shake-feathers'
acc.annotated.comb$behaviour[acc.annotated.comb$behaviour=='preen_tag']='stand-preen'
acc.annotated.comb$behaviour[acc.annotated.comb$behaviour=='beg_flap']='beg'
table(acc.annotated.comb$behaviour)

# pool certain behaviours:
acc.annotated.comb$behaviour.pooled = acc.annotated.comb$behaviour
acc.annotated.comb$behaviour.pooled[acc.annotated.comb$behaviour=='sit-alert']='sit'
acc.annotated.comb$behaviour.pooled[acc.annotated.comb$behaviour%in%c('stand-alert','stand-other','stand-rest','stand-shake-feathers','stand-preen','drink')] = 'stand'
table(acc.annotated.comb$behaviour.pooled)

behaviour.pooled = names(table(acc.annotated.comb$behaviour.pooled))
behaviour.pooled2 = c('forage','walk','fly','rest','beg') # only used to determine the length of the arrays to calculate pooled sensitivity and precision

# assign id's
names(acc.annotated.comb)[1:2] = c('birdID','date_time')
acc.annotated.comb$obs.id <- paste(acc.annotated.comb$birdID, as.numeric(acc.annotated.comb$date_time), sep = ".")
acc.annotated.comb$segment.id <- paste(acc.annotated.comb$obs.id, ".a", sep = "")
acc.annotated.comb$ind.id <- paste(acc.annotated.comb$obs.id, ".", acc.annotated.comb$Index, sep = "")

# run the fixed segmentation models with different segment lengths 10 times
nsim <- 10
seg.lengths <- c(0.4,0.8,1.0,1.6) # segment length in seconds
sensitivity.fixed.seglength.nsim <- array(NA, c(nsim,length(behaviour.pooled),length(seg.lengths)), dimnames = list(1:nsim, behaviour.pooled, seg.lengths))
precision.fixed.seglength.nsim <- array(NA, c(nsim,length(behaviour.pooled),length(seg.lengths)), dimnames = list(1:nsim, behaviour.pooled, seg.lengths))
sensitivity.fixed.seglength.behav.pooled.nsim <- array(NA, c(nsim,length(behaviour.pooled2),length(seg.lengths)), dimnames = list(1:nsim, behaviour.pooled2, seg.lengths))
precision.fixed.seglength.behav.pooled.nsim <- array(NA, c(nsim,length(behaviour.pooled2),length(seg.lengths)), dimnames = list(1:nsim, behaviour.pooled2, seg.lengths))

df1.seg.length <- list(length(seg.lengths))
df2.seg.length <- list(length(seg.lengths))

for (j in 1:length(seg.lengths)) { 
  dfs <- create.fixed.segments(segment.length=seg.lengths[j], data=acc.annotated.comb, annotated.data=T)
  df1.seg.length[[j]] <- dfs[[1]]
  df2.seg.length[[j]] <- dfs[[2]]
}

for (j in 1:length(seg.lengths)) { 
  print(seg.lengths[j])
  for (i in 1:nsim) {
    RF.model.output <- RF.model(df1.seg.length[[j]], df2.seg.length[[j]], stand=4)
    performance.stats <- calculate.performance(RF.model.output[[2]])
    print(performance.stats[[5]])
    sensitivity.fixed.seglength.nsim[i,,j] <- performance.stats[[1]]
    precision.fixed.seglength.nsim[i,,j] <- performance.stats[[3]]
    # calculate performance when behaviours are pooled even further (into forage, walk, fly, rest or beg)
    performance.stats.pooled <- calculate.performance.pooled(RF.model.output[[2]])
    print(performance.stats.pooled[[5]])
    sensitivity.fixed.seglength.behav.pooled.nsim[i,,j] <- performance.stats.pooled[[1]]
    precision.fixed.seglength.behav.pooled.nsim[i,,j] <- performance.stats.pooled[[3]]
    print(i)
    print(precision.fixed.seglength.nsim[i,,j])
  }
}

### calculate mean, lcl and ucl over the N simulations
mean.CRI.sensitivity.fixed.seglength <- calculate.mean.CRI(sensitivity.fixed.seglength.nsim)
mean.CRI.precision.fixed.seglength <- calculate.mean.CRI(precision.fixed.seglength.nsim)
F.measure.fixed.seglength.nsim <- calculate.F.measure(sensitivity.fixed.seglength.nsim, precision.fixed.seglength.nsim)
mean.CRI.Fmeasure.fixed.seglength <- calculate.mean.CRI(F.measure.fixed.seglength.nsim)
mean.CRI.Fmeasure.fixed.seglength[,,'mean']

# also for the pooled behaviours
mean.CRI.sensitivity.pooled.seglength <- calculate.mean.CRI(sensitivity.fixed.seglength.behav.pooled.nsim)
mean.CRI.precision.pooled.seglength <- calculate.mean.CRI(precision.fixed.seglength.behav.pooled.nsim)
F.measure.pooled.seglength.nsim <- calculate.F.measure(sensitivity.fixed.seglength.behav.pooled.nsim, precision.fixed.seglength.behav.pooled.nsim)
mean.CRI.Fmeasure.pooled.seglength <- calculate.mean.CRI(F.measure.pooled.seglength.nsim)
round(mean.CRI.Fmeasure.pooled.seglength[,,'mean'],2)
write.csv(round(mean.CRI.Fmeasure.pooled.seglength[,,'mean'],2), "output/Fmeasures.pooled.behaviours.csv")

# train best-supported RF model (with segment length 1.6 s) on all annotated data:
RF.model.with.begging <- RF.model.start(df1.seg.length[[4]], stand=4) # the 4th df1 is the dataset with the 4th segment length (=1.6 s)
RF.model.with.begging$importance

keep(mean.CRI.sensitivity.fixed.seglength, mean.CRI.precision.fixed.seglength, mean.CRI.Fmeasure.fixed.seglength, 
     mean.CRI.sensitivity.pooled.seglength, mean.CRI.precision.pooled.seglength, mean.CRI.Fmeasure.pooled.seglength,
     acc.annotated.comb, dfs, RF.model.with.begging, sure=T)
