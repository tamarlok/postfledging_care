# register_google(key="AIzaSyBYq_g9r-ewGGPUozcATuscTMIJ0DkNjYc") # free usage until 21/April/2025 (or when 288 euros of 'free credit' are all spent)
#register_stadiamaps(key="fc371c8d-3c09-4534-b284-f273cb7781d7")
# map the overall distributions of parents and their chicks during post-fledging.
# overlay these maps with the distributions of contact, potentially distinguishing overall, begging and foraging contact.

# the package OpenStreetMap is preferred over ggmap as explained here: https://www.linkedin.com/pulse/plot-over-openstreetmap-ggplot2-abel-tortosa-andreu/
# schiermap <- openmap(c(53.525,6.1),c(53.325,6.4),type="bing")
# However, plotting in multiple panels is only possible with ggmap, not when the map is downloaded with openstreetmap. 
#schiermap <- get_map(c(left = 6.1, bottom = 53.325, right = 6.4, top = 53.525), source='google', maptype='satellite', zoom=11) # zoom=12 zooms too much...
#ggmap(schiermap)
#keep(schiermap, sure=T)
#save.image("data/raw/schiermap.RData")
load("data/processed/results.analyses.0117.RData")
load("data/raw/schiermap.RData")
source("functions.r")

chick.parent.data.contact.sel$freq=1
ggmap(schiermap)

# alternative maps:
schiermap_stamen_terrain <- get_stadiamap(c(left = 5, bottom = 53, right = 7.5, top = 53.8), maptype="stamen_terrain") # requires an API-key but it's for free (in contrast to Google Maps API key, where you can use max maps worth up to $200/month)
windows(16,8)
ggmap(schiermap_stamen_terrain)


map_ggmap = ggmap(schiermap)
p = map_ggmap + 
  geom_density_2d_filled(data = chick.parent.data.contact.sel, aes( x = longitude.chick, y = latitude.chick), alpha=0.6, bins=6) +
  #theme(legend.position = c(1.1,0.8)) +
  #scale_fill_gradient(low='yellow', high='red') + 
  scale_x_continuous("Longitude (°E)", limits=c(6.1,6.4)) +
  scale_y_continuous("Latitude (°N)", limits=c(53.325,53.525)) #+
#inset_element(map.NL, left=1, bottom=0, right=1.2, top=0.4)
p

# I don't get the density plot right, so I make the location plot manually... 
# I could cut-off the plotting based on a minimum number of visits to a certain square...
# round coordinates such that they become squares on the map, i.e. round longitude to 2 decimals, and latitude to 2.5 decimals.
chick.parent.data.contact.sel$lat.rnd <- round_to_nearest(chick.parent.data.contact.sel$latitude.chick,0.005)
chick.parent.data.contact.sel$lon.rnd <- round_to_nearest(chick.parent.data.contact.sel$longitude.chick,0.0085)
head(chick.parent.data.contact.sel)
df.chick.per.coord <- aggregate(freq~lat.rnd+lon.rnd, chick.parent.data.contact.sel, sum)
# remove locations that bird visited less than ten times
#df.chick.per.coord = df.chick.per.coord[df.chick.per.coord$freq>=10,]
p = map_ggmap + 
  geom_tile(data = df.chick.per.coord, aes( x = lon.rnd, y = lat.rnd, fill=freq)) +
  theme(legend.position = c(1.2,0.85)) +
  scale_fill_gradient(low = "yellow", high = "red", trans="log", name="density") #+ 
  #scale_x_continuous("Longitude (°E)", limits=c(6.1,6.4)) +
  #scale_y_continuous("Latitude (°N)", limits=c(53.325,53.525)) #+
p

# now make three panels, one for the chick locations, one for the parent locations, and one for the contact locations.
# first make a plot of all the data across chick-parent pairs combined
chick.parent.data.contact.sel$lat.rnd.parent <- round_to_nearest(chick.parent.data.contact.sel$latitude.parent,0.005)
chick.parent.data.contact.sel$lon.rnd.parent <- round_to_nearest(chick.parent.data.contact.sel$longitude.parent,0.0085)
df.parent.per.coord <- aggregate(freq~lat.rnd.parent+lon.rnd.parent, chick.parent.data.contact.sel, sum)
#df.parent.per.coord = df.parent.per.coord[df.parent.per.coord$freq>=10,]
# contact locations (using the location of the chick, which is by definition less than 10m away from the parent's location)
df.contact.per.coord <- aggregate(freq~lat.rnd+lon.rnd, chick.parent.data.contact.sel[chick.parent.data.contact.sel$contact==1,], sum)
# here, we do not remove freq<10, as contacts are already quite rare.
# combine the three panels into one df, to be able to use facet_wrap for plotting the three panels (otherwise we'd need to use the libraries patchwork or gridExtra)
df.chick.per.coord$group = 'chick locations'
df.parent.per.coord$group = 'parent locations'
df.contact.per.coord$group = 'contact locations'
names(df.parent.per.coord)[1:2]=c('lat.rnd','lon.rnd')
df.combined <- rbind(df.chick.per.coord, df.parent.per.coord, df.contact.per.coord)
df.combined$group <- factor(df.combined$group, levels=c('chick locations','parent locations','contact locations'))
windows(16,8)
p = map_ggmap + 
  geom_tile(data = df.combined, aes( x = lon.rnd, y = lat.rnd, fill=freq)) +
  facet_wrap(~ group, ncol=3) +
  theme(legend.position = c(1.2,0.85)) +
  scale_fill_gradient(low = "yellow", high = "red", trans="log", name="density") +
  labs(x="Longitude (°E)",y="Latitude (°N)")
  #scale_x_continuous("Longitude (°E)", limits=c(6.1,6.4)) +
  #scale_y_continuous("Latitude (°N)", limits=c(53.325,53.525))
p

# make the same map for only female parents:
df.chick.per.coord.female.parents <- aggregate(freq~lat.rnd+lon.rnd, chick.parent.data.contact.sel[chick.parent.data.contact.sel$sex.parent=='f',], sum)
df.parent.per.coord.female.parents <- aggregate(freq~lat.rnd.parent+lon.rnd.parent, chick.parent.data.contact.sel[chick.parent.data.contact.sel$sex.parent=='f',], sum)
df.contact.per.coord.female.parents <- aggregate(freq~lat.rnd+lon.rnd, chick.parent.data.contact.sel[chick.parent.data.contact.sel$contact==1&chick.parent.data.contact.sel$sex.parent=='f',], sum)
df.chick.per.coord.female.parents$group = 'chick locations'
df.parent.per.coord.female.parents$group = 'mother locations'
df.contact.per.coord.female.parents$group = 'contact locations'
names(df.parent.per.coord.female.parents)[1:2]=c('lat.rnd','lon.rnd')
df.combined.female.parents <- rbind(df.chick.per.coord.female.parents, df.parent.per.coord.female.parents, df.contact.per.coord.female.parents)
df.combined.female.parents$group <- factor(df.combined.female.parents$group, levels=c('chick locations','mother locations','contact locations'))
windows(16,8)
p = map_ggmap + 
  geom_tile(data = df.combined.female.parents, aes( x = lon.rnd, y = lat.rnd, fill=freq)) +
  facet_wrap(~ group, ncol=3) +
  theme(legend.position = c(1.2,0.85)) +
  scale_fill_gradient(low = "yellow", high = "red", trans="log", name="density") +  
  scale_x_continuous(name="", breaks=seq(6.1,6.5,0.1), labels=paste(seq(6.1,6.5,0.1),"°",sep=""), limits=c(6.05,6.45)) +
  scale_y_continuous(name="", breaks=seq(53.3,53.5,0.1), labels=paste(format(seq(53.3,53.5,0.1),00.00),"°",sep=""), limits=c(53.3, 53.55))
p

# make the same map for only fathers: 
df.chick.per.coord.male.parents <- aggregate(freq~lat.rnd+lon.rnd, chick.parent.data.contact.sel[chick.parent.data.contact.sel$sex.parent=='m',], sum)
df.parent.per.coord.male.parents <- aggregate(freq~lat.rnd.parent+lon.rnd.parent, chick.parent.data.contact.sel[chick.parent.data.contact.sel$sex.parent=='m',], sum)
df.contact.per.coord.male.parents <- aggregate(freq~lat.rnd+lon.rnd, chick.parent.data.contact.sel[chick.parent.data.contact.sel$contact==1&chick.parent.data.contact.sel$sex.parent=='m',], sum)
df.chick.per.coord.male.parents$group = 'chick locations'
df.parent.per.coord.male.parents$group = 'father locations'
df.contact.per.coord.male.parents$group = 'contact locations'
names(df.parent.per.coord.male.parents)[1:2]=c('lat.rnd','lon.rnd')
df.combined.male.parents <- rbind(df.chick.per.coord.male.parents, df.parent.per.coord.male.parents, df.contact.per.coord.male.parents)
df.combined.male.parents$group <- factor(df.combined.male.parents$group, levels=c('chick locations','father locations','contact locations'))
windows(16,8)
p = map_ggmap + 
  geom_tile(data = df.combined.male.parents, aes( x = lon.rnd, y = lat.rnd, fill=freq)) +
  facet_wrap(~ group, ncol=3) +
  theme(legend.position = c(1.2,0.85)) +
  scale_fill_gradient(low = "yellow", high = "red", trans="log", name="density") +  
  scale_x_continuous(name="", breaks=seq(6.1,6.5,0.1), labels=paste(seq(6.1,6.5,0.1),"°",sep=""), limits=c(6.05,6.45)) +
  scale_y_continuous(name="", breaks=seq(53.3,53.5,0.1), labels=paste(format(seq(53.3,53.5,0.1),00.00),"°",sep=""), limits=c(53.3, 53.55))
p

windows(16,8)
ggmap(schiermap_stamen_terrain)+
  geom_tile(data = df.combined.male.parents, aes( x = lon.rnd, y = lat.rnd, fill=freq)) +
  facet_wrap(~ group, ncol=3) +
  theme(legend.position = c(1.2,0.85)) +
  scale_fill_gradient(low = "yellow", high = "red", trans="log", name="density") +  
  #labs(x="Longitude (°E)",y="Latitude (°N)")+
  scale_x_continuous(name="", breaks=seq(6.1,6.5,0.1), labels=paste(seq(6.1,6.5,0.1),"°",sep=""), limits=c(6.05,6.45)) +
  scale_y_continuous(name="", breaks=seq(53.3,53.5,0.1), labels=paste(format(seq(53.3,53.5,0.1),00.00),"°",sep=""), limits=c(53.3, 53.55))
  
## now make the three panels for each individual chick-parent pair
chick.parent.pairs <- levels(chick.parent.data.contact.sel$chick.parent)
chick.parent.pair = chick.parent.pairs[2]
# create general function for this, where you can fill in the desired chick-parent pair:
plot.locations.chick.parent.pair <- function(chick.parent.pair){
  chickID = chick.parent.data.contact.sel$birdID.chick[chick.parent.data.contact.sel$chick.parent==chick.parent.pair][1]
  parentID = chick.parent.data.contact.sel$birdID.parent[chick.parent.data.contact.sel$chick.parent==chick.parent.pair][1]
  df.chick.per.coord <- aggregate(freq~lat.rnd+lon.rnd, chick.parent.data.contact.sel[chick.parent.data.contact.sel$chick.parent==chick.parent.pair,], sum)
  df.parent.per.coord <- aggregate(freq~lat.rnd.parent+lon.rnd.parent, chick.parent.data.contact.sel[chick.parent.data.contact.sel$chick.parent==chick.parent.pair,], sum)
  df.contact.per.coord <- aggregate(freq~lat.rnd+lon.rnd, chick.parent.data.contact.sel[chick.parent.data.contact.sel$contact==1&chick.parent.data.contact.sel$chick.parent==chick.parent.pair,], sum)
  df.chick.per.coord$group = 'locations chick'
  df.parent.per.coord$group = 'locations parent'
  df.contact.per.coord$group = 'locations contact'
  names(df.parent.per.coord)[1:2]=c('lat.rnd','lon.rnd')
  df.combined <- rbind(df.chick.per.coord, df.parent.per.coord, df.contact.per.coord)
  df.combined$group = factor(df.combined$group, levels=unique(df.combined$group))
  p = map_ggmap + 
    geom_tile(data = df.combined, aes( x = lon.rnd, y = lat.rnd, fill=freq)) +
    facet_wrap(~ group, ncol=3) +
    scale_fill_gradient(low = "yellow", high = "red", trans="log", name="density", labels=c(1,7,55,403,2980)) +  
    labs(title=chick.parent.pair) +
    scale_x_continuous(name="", breaks=seq(6.1,6.5,0.1), labels=paste(seq(6.1,6.5,0.1),"°",sep=""), limits=c(6.05,6.45)) +
    scale_y_continuous(name="", breaks=seq(53.3,53.5,0.1), labels=paste(format(seq(53.3,53.5,0.1),00.00),"°",sep=""), limits=c(53.3, 53.55)) +
    theme_minimal()+
    theme(axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          strip.text = element_text(size = 14),
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          legend.text = element_text(size=12, colour='black'),
          legend.title = element_text(size=14, colour='black'),
          legend.position = c(1.2,0.85))
  png(paste("output/map.locations.",chick.parent.pair,".png", sep=""), width=1600, height=600)
  print(p)
  dev.off()
  #p
  #ggsave(paste("output/map.locations.2",chick.parent.pair,".png", sep=""), device='png', width=1200, height=450, units='px')
}

for (i in chick.parent.pairs) plot.locations.chick.parent.pair(i)

plot.locations.chick.parent.pair("6292-6287")

