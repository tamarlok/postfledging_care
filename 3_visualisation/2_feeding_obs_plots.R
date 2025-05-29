# years in which feeding observations took place:
table(year(feeding.obs.pf.chick.uni$Date)) # 1999-2024

# Prepare data for plotting:
n.feedings.chickage.schier <- table(feeding.obs.pf.chick.uni$ChickAge, feeding.obs.pf.chick.uni$schier)
n.feedings.chickage.schier  # only observations on the Oosterkwelder of Schiermonnikoog for chicks until 57 days old. 
n.feedings.chickage <- table(feeding.obs.pf.chick.uni$ChickAge10) 
chick.age=as.numeric(names(n.feedings.chickage))
n.obs=as.numeric(n.feedings.chickage)

# first combine the ring location and feeding location into one column, specifying group=colony vs group=feeding
colony.data = feeding.obs.pf.chick.uni[,c('RingLongitude','RingLatitude')]
names(colony.data)=c('Longitude','Latitude')
colony.data$group = 'colony'
feeding.data = feeding.obs.pf.chick.uni[,c('Longitude','Latitude')]
feeding.data$group = 'feeding'
colony.feeding.data = rbind(feeding.data, colony.data)

### FIGURE 1A & 1B ###
windows(10,11)
layout(1:2)
par(mar=c(1,4,0,0),oma=c(3,0,1,1))
# (A) Number of feedings in relation to chick age
plot(chick.age, n.obs, xlab="Chick age (days)", ylab="# observed feeding events", las=1, cex=2, xaxt='n',xlim=c(35,135), ylim=c(0,55))
axis(1,at=seq(40,130,10),labels=F)
sum(n.obs)
text(36,55*0.95,"(a)")
# add the estimated relationship:
chick.age.pred = data.frame(ChickAge10=seq(40,130,10))
chick.age.pred$n.obs.pred = predict(m.nfeeds.age10, newdata = chick.age.pred, type='response')
lines(n.obs.pred~ChickAge10, chick.age.pred)
# (B) Distance from colony of feedings in relation to chick age
boxdata = boxplot(distance.from.colony~ChickAge10, feeding.obs.pf.chick.uni, xlab="", ylab="Distance from colony (km)", ylim=c(0,43), las=1)
# add estimated relationship:
chick.age.pred$ChickAge = chick.age.pred$ChickAge10
chick.age.pred$ChickAgeRel = chick.age.pred$ChickAge10/10-3 # ranges from 1 to 10, to plot on the boxplot
chick.age.pred$dist.pred = predict(m.feeddist.parsim, newdata = chick.age.pred, type='response')
lines(dist.pred~ChickAgeRel, chick.age.pred)
text(0.6,43*0.95,"(b)")
axis(1,9,120)
mtext("Chick age (days)",1,2.5)
dev.off()
# Adding sample size is not needed, as this is reflected by the top panel showing the number of feeding observations!

### FIGURE 1C ###
# Map of colony and feeding locations:
source("C:/Users/Tamar Lok/Documents/R/Load_API_keys_for_using_maps.R")
NL_stamen_terrain <- get_stadiamap(c(left = 3, bottom = 50.7, right = 7, top = 53.7), maptype="stamen_terrain") 
#windows(8,8)
Fig1C <- ggmap(NL_stamen_terrain) +
  geom_point(data=colony.feeding.data, aes(x=Longitude, y=Latitude, shape=group, color=group), size=4) +
  geom_segment(data = feeding.obs.pf.chick.uni, 
               aes(x = RingLongitude, y = RingLatitude, xend = Longitude, yend = Latitude),
               linewidth = 0.6                                  # Set arrow thickness
  ) +
  scale_shape_manual(values=c(16,17))+
  scale_color_manual(values=c('orange','darkgreen'))+
  labs(x="Longitude (°E)", y = "Latitude (°N)")+
  theme_minimal()+
  theme(legend.title=element_blank(),
        legend.text = element_text(size=14),
        axis.title=element_text(size=16))

### combine figures 1A & 1B with 1C: 
#windows(13,7)
# panel A
Fig1A <- function() {
  par(mar=c(1,10,1,3),oma=c(0,0,1,0))
  plot(chick.age, n.obs, xlab="", ylab="# observed feeding events", las=1, cex=2, xaxt='n',xlim=c(35,135), ylim=c(0,55), cex.lab=1.3)
  lines(n.obs.pred~ChickAge10, chick.age.pred)
  axis(1,at=seq(40,130,10),labels=F)
  text(36,55*0.95,"(a)",cex=1.2)
}
# panel B
Fig1B <- function() {
  par(mar=c(10,10,1,3),oma=c(0,0,0,0))
  boxdata = boxplot(distance.from.colony~ChickAge10, feeding.obs.pf.chick.uni, xlab="", ylab="Distance from colony (km)", ylim=c(0,43), cex.lab=1.3, las=1)
  lines(dist.pred~ChickAgeRel, chick.age.pred)
  text(0.6,43*0.95,"(b)",cex=1.2)
  mtext("Chick age (days)",1,2.5,cex=1.2)
}

# Convert base R plots to grob:
pdf("output/Fig1.pdf", width=12, height=7)
grid.newpage()
grid.echo(Fig1A)
Fig1A_grob <- grid.grab()

grid.newpage()
grid.echo(Fig1B)
Fig1B_grob <- grid.grab()

left_column <- plot_grid(Fig1A_grob, Fig1B_grob, ncol = 1, rel_heights = c(1, 1.25))

Fig1 <- plot_grid(left_column, Fig1C,
                  ncol = 2,
                  rel_widths = c(1, 1.2))
Fig1
dev.off()

### END FIGURE 1 ###

### FIGURE S1 ###
# number of observed feeding events in relation to chick and parent sex
pdf("output/FigS1.pdf", width=8, height=4)
layout(matrix(1:2,ncol=2))
par(mar=c(1,3,0,0), oma=c(4,2,1,1))

# (A) number of feedings in relation to chick age, coloured by chick sex
freq.age.sex = table(feeding.obs.chicksex.known$SexChickSel, feeding.obs.chicksex.known$ChickAge10)
plot(as.numeric(colnames(freq.age.sex)), freq.age.sex[1,], xlim=c(40,130), ylim=c(0,max(freq.age.sex)), col='red', xlab="", ylab="", las=1, cex=1.5)
lines(as.numeric(colnames(freq.age.sex)), freq.age.sex[1,], col='red')
points(as.numeric(colnames(freq.age.sex)), freq.age.sex[2,], col='blue', cex=1.5)
lines(as.numeric(colnames(freq.age.sex)), freq.age.sex[2,], col='blue')
legend("topright", bty="n", legend=c("female chick","male chick"), col=c("red","blue"), pch=21)
mtext("Number of observed feeding events",2,3, cex=1.2)

# (B) number of feedings in relation to chick age, coloured by parent sex
freq.age.parentsex = table(feeding.obs.parentsex.known$SexParentSel, feeding.obs.parentsex.known$ChickAge10)
plot(as.numeric(colnames(freq.age.parentsex)), freq.age.parentsex[1,], xlim=c(40,130), ylim=c(0,max(freq.age.parentsex[!is.na(freq.age.parentsex)])), col='red', xlab=")", ylab="", las=1, cex=1.5)
lines(as.numeric(colnames(freq.age.parentsex)), freq.age.parentsex[1,], col='red')
points(as.numeric(colnames(freq.age.parentsex)), freq.age.parentsex[2,], col='blue', cex=1.5)
lines(as.numeric(colnames(freq.age.parentsex)), freq.age.parentsex[2,], col='blue')
legend("topright", bty="n", legend=c("female parent","male parent"), col=c("red","blue"), pch=21)
mtext("Chick age (days)",1,2, cex=1.2,outer=T)
dev.off()
### END FIGURE S1 ###