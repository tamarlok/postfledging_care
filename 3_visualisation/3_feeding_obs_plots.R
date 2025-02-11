# Prepare data for plotting:
n.feedings.chickage.schier <- table(feeding.obs.pf.chick.uni$ChickAge, feeding.obs.pf.chick.uni$schier)
n.feedings.chickage.schier  # only observations on the Oosterkwelder of Schiermonnikoog for chicks until 57 days old. 
n.feedings.chickage <- table(feeding.obs.pf.chick.uni$ChickAge10) 
chick.age=as.numeric(names(n.feedings.chickage))
n.obs=as.numeric(n.feedings.chickage)

### FIGURE 1 ###
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
chick.age.pred$dist.pred.log = exp(predict(m.feeddistlog.parsim, newdata = chick.age.pred))
lines(dist.pred~ChickAgeRel, chick.age.pred)
lines(dist.pred.log~ChickAgeRel, chick.age.pred, col='blue')
text(0.6,43*0.95,"(b)")
axis(1,9,120)
mtext("Chick age (days)",1,2.5)
### END FIGURE 1 ###
# Adding sample size is not needed, as this is reflected by the top panel showing the number of feeding observations!

# Does the number of feedings per age classes differ depending on a chick's sex or the parent's sex?

# Despite the sex effects not being significant, here's the plot:
# make figure of # observed feeding events in relation to chick sex, parent sex and chick x parent sex
windows(8,4)
layout(matrix(1:2,ncol=2))
par(mar=c(1,3,0,0), oma=c(4,2,1,1))

# (A) number of feedings in relation to chick age, coloured by chick sex
freq.age.sex = table(feeding.obs.chicksex.known$SexChickSel, feeding.obs.chicksex.known$ChickAge10)
plot(as.numeric(colnames(freq.age.sex)), freq.age.sex[1,], xlim=c(40,130), ylim=c(0,max(freq.age.sex)), col='red', xlab="", ylab="", las=1, cex=2)
lines(as.numeric(colnames(freq.age.sex)), freq.age.sex[1,], col='red')
points(as.numeric(colnames(freq.age.sex)), freq.age.sex[2,], col='blue', cex=2)
lines(as.numeric(colnames(freq.age.sex)), freq.age.sex[2,], col='blue')
legend("topright", bty="n", legend=c("female chick","male chick"), col=c("red","blue"), pch=21)
mtext("Number of observed feeding events",2,3)

# (B) number of feedings in relation to chick age, coloured by parent sex
freq.age.parentsex = table(feeding.obs.parentsex.known$SexParentSel, feeding.obs.parentsex.known$ChickAge10)
plot(as.numeric(colnames(freq.age.parentsex)), freq.age.parentsex[1,], xlim=c(40,130), ylim=c(0,max(freq.age.parentsex[!is.na(freq.age.parentsex)])), col='red', xlab=")", ylab="", las=1, cex=2)
lines(as.numeric(colnames(freq.age.parentsex)), freq.age.parentsex[1,], col='red')
points(as.numeric(colnames(freq.age.parentsex)), freq.age.parentsex[2,], col='blue', cex=2)
lines(as.numeric(colnames(freq.age.parentsex)), freq.age.parentsex[2,], col='blue')
legend("topright", bty="n", legend=c("female parent","male parent"), col=c("red","blue"), pch=21)


# Map of feeding observations and colony locations:
# plot colonies and feeding locations
NL_stamen_terrain <- get_stadiamap(c(left = 3, bottom = 50.7, right = 7.5, top = 53.7), maptype="stamen_terrain") 
ggmap(NL_stamen_terrain)
library(png)
library(grid)
img <- readPNG("data/raw/NL_map_grey.png")
img_adj <- rasterGrob(img, interpolate=T)

ggmap(NL_stamen_terrain) +
  coord_cartesian() +
  annotation_custom(
    img_adj,
    xmin = 3, xmax = 7.5,  # Set the longitude range
    ymin = 50.7, ymax = 53.7     # Set the latitude range
  ) +
  labs(title = "Map with Image Overlay")

# this code I used for the density dependent survival paper:
NL <- GetMap.bbox(c(maxlong, minlong), c(maxlat, minlat), destfile = "NL_roadmap1.png", maptype = "roadmap", NEWMAP=F)
### replace roadmap picture with greyscale picture, then run the next
img = readPNG("NL_roadmap1.png")
NL$myTile = img
# but it doesnt work anymore as it used RGoogleMaps

# first combine the ring location and feeding location into one column, specifying group=colony vs group=feeding
colony.data = feeding.obs.pf.chick.uni[,c('RingLongitude','RingLatitude')]
names(colony.data)=c('Longitude','Latitude')
colony.data$group = 'colony'
feeding.data = feeding.obs.pf.chick.uni[,c('Longitude','Latitude')]
feeding.data$group = 'feeding'
colony.feeding.data = rbind(feeding.data, colony.data)

windows(8,8)
ggmap(NL_stamen_terrain) +
  geom_point(data=colony.feeding.data, aes(x=Longitude, y=Latitude, shape=group, color=group), size=4) +
  geom_segment(data = feeding.obs.pf.chick.uni, 
    aes(x = RingLongitude, y = RingLatitude, xend = Longitude, yend = Latitude),
    size = 1                                  # Set arrow thickness
  ) +
scale_shape_manual(values=c(16,17))+
  scale_color_manual(values=c('orange','darkgreen'))+
  scale_x_continuous(breaks=3:7, labels = function(x) paste0(x, "°E"), limits=c(3,7)) +
  #scale_y_continuous(breaks=51:53, labels = function(y) paste0(y, "°N"))+
  theme_minimal()#+
  #theme(axis.ticks.x = element_blank(),
  #      axis.ticks.y = element_blank())

# Somehow I don;t get the axes right, with ggmap the functions scale_(x|y)_continuous together with the theme options axis.ticks are not working correctly.
# So I leave it as:
windows(8,8)
ggmap(NL_stamen_terrain) +
  geom_point(data=colony.feeding.data, aes(x=Longitude, y=Latitude, shape=group, color=group), size=4) +
  geom_segment(data = feeding.obs.pf.chick.uni, 
               aes(x = RingLongitude, y = RingLatitude, xend = Longitude, yend = Latitude),
               size = 1                                  # Set arrow thickness
  ) +
  scale_shape_manual(values=c(16,17))+
  scale_color_manual(values=c('orange','darkgreen'))+
  labs(x="Longitude (°E)", y = "Latitude (°N)")+
  theme_minimal()+
  theme(legend.title=element_blank())

table(year(feeding.obs.pf.chick.uni$Date)) # 1999-2024
