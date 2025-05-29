# https://cran.r-project.org/web/packages/move/vignettes/browseMovebank.html

loginStored <- movebankLogin(username="xxx", password="xxx") # fill in your Movebank credentials

# access to the relevant Movebank studies will be granted to researchers upon reasonable request. Please contact the data managers of these studies for this. 
spoonbill_metawad_ID <- getMovebankID("SPOONBILL_METAWAD - Eurasian Spoonbills (Platalea leucorodia, Threskiornithidae) breeding on Schiermonnikoog, The Netherlands", login=loginStored) # contains adult data
spoonbill_spinoza_ID <- getMovebankID("SPOONBILL_SPINOZA - Eurasian Spoonbills (Platalea leucorodia, Threskiornithidae) born on Schiermonnikoog, The Netherlands", login=loginStored) # contains juvenile data

# get reference data from the two studies:
refdata_adults <- getMovebankReferenceTable(spoonbill_metawad_ID,login=loginStored)
refdata_juvs <- getMovebankReferenceTable(spoonbill_spinoza_ID,login=loginStored)
# contains separate reference data for GPS and accelerometer data

# import parent-offspring relationships to only download data of those individuals:
parent_offspring_data <- read.csv("data/raw/parent.offspring.data.csv")
parents <- unique(parent_offspring_data$parentID)
chicks <- unique(parent_offspring_data$chickID)

# download data of chicks and their parents

# first download gps data:
gps.data.list <- list()

for (i in 1:length(parents)) {
  print(parents[i])
  animalID = refdata_adults$animal_id[refdata_adults$animal_local_identifier==parents[i]][1] # there are 2 entries per bird in the refdata, one for the GPS data and one for the ACC data
  gps.data = getMovebankLocationData(spoonbill_metawad_ID, animalName=animalID, login=loginStored)
  gps.data.list[[i]] <- gps.data[,c(11,12,3,4,5,15:19,21,22,24)]
}

for (i in 1:length(parents)) print(gps.data.list[[i]]$individual.local.identifier[1])

for (i in 1:length(chicks)) {
  print(chicks[i])
  animalID = refdata_juvs$animal_id[refdata_juvs$animal_local_identifier==chicks[i]]
  gps.data = getMovebankLocationData(spoonbill_spinoza_ID, animalName=animalID, login=loginStored)
  # add the chick data to the same gps.data and acc.data lists, therefore start counting at first empty list entry after all parents data:
  gps.data.list[[i+length(parents)]] <- gps.data[,c(11,12,3,4,5,15:19,21,22,24)]
}

for (i in 1:length(gps.data.list)) print(gps.data.list[[i]]$individual.local.identifier[1])

save.image("data/processed/gps.data.from.movebank.RData")

rm(gps.data.list) # so that ACC data is saved separately (to save memory)
### (2) download ACC data (as this takes much longer, and is not needed for calculating distance between parent and chick and departure dates on autumn migration):
acc.data.list <- list()

for (i in 1:length(parents)) {
  print(parents[i])
  animalID = refdata_adults$animal_id[refdata_adults$animal_local_identifier==parents[i]][1] # there are 2 entries per bird in the refdata, one for the GPS data and one for the ACC data
  acc.data = getMovebankNonLocationData(spoonbill_metawad_ID, animalName=animalID, login=loginStored)
  acc.data$start_timestamp = ymd_hms(acc.data$start_timestamp)
  acc.data.list[[i]] <- acc.data[,c(6,7,12,16,13:15)]
}

for (i in 1:length(chicks)) {
  print(chicks[i])
  animalID = refdata_juvs$animal_id[refdata_juvs$animal_local_identifier==chicks[i]]
  acc.data = getMovebankNonLocationData(spoonbill_spinoza_ID, animalName=animalID, login=loginStored)
  acc.data$start_timestamp = ymd_hms(acc.data$start_timestamp)
  # add the chick data to the same gps.data and acc.data lists, therefore start counting at first empty list entry after all parents data:
  acc.data.list[[i+length(parents)]] <- acc.data[,c(6,7,12,16,13:15)]
}

for (i in 1:length(acc.data.list)) print(paste(i, acc.data.list[[i]]$individual_local_identifier[1]))

save.image("data/processed/acc.data.from.movebank.RData")
