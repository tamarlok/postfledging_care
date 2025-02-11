rm(list=ls())
# remote tmux session: postcare
#setwd("/export/lv6/user/tlok/Tamar/postfledging_care") # ada
#setwd("/export/data01/user/tlok/Tamar/postfledging_care") # ymga
#setwd("C:/Users/Tamar Lok/Documents/Analyses/22. Post-fledging parental care in spoonbills/postfledging_care")

source('functions.R')

packages <- c("RODBC", "move", "dplyr", "lubridate","reshape2","reshape","caret","randomForest", "plyr", "moments", "MuMIn", "lme4", "geosphere", "ggplot2", "gdata", "sf", "mapview","terra","plotrix","emmeans","DHARMa","broom.mixed","OpenStreetMap","ggplot2","ggmap")
ipak(packages)

Sys.setenv(TZ="GMT")
set.seed(3) # to be able to reproduce random processes
options(digits=10)

#source("1_data_preparations/0_load_tracking_data_from_uvabits_database_and_export_to_movebank.r")
#save.image("data/processed/GPS.ACC.data.juvs.RData")
#load("data/processed/GPS.ACC.data.juvs.RData")
#keep(sms.data.ad.list, sms.data.juv.list, sure=T)
#save.image("data/processed/SMS.data.juvs.ads.RData")
#load("data/processed/SMS.data.juvs.ads.RData")

source("1_data_preparations/1_load_data_from_Movebank.r") # this loads and saves both GPS and ACC data, and also a separate GPS data file. 
#save.image("data/processed/gps.data.from.movebank.0327.RData")
source("1_data_preparations/2_behavioral_classification.r")
#save.image("data/processed/gps.behav.data.0327.RData")
source("2_analyses/4_classification_begging_behaviour.r")
#save.image("data/processed/gps.behav.data.0327.RData")
source("1_data_preparations/2_behavioral_classification_with_begging.r") # using the best-performing model derived from the script 4_classification_begging_behaviour.r
#save.image("data/processed/gps.behav.beg.data.1203.RData")
#load("data/processed/gps.behav.beg.data.1203.RData")
load("data/processed/gps.data.from.movebank.0327.RData")
source("1_data_preparations/3_determine_departure_dates_and_distance_parent_offspring.r")
#save.image("data/processed/chick.parent.data.1008.RData")
load("data/processed/chick.parent.data.1008.RData")
source("1_data_preparations/4_linking_behaviour_and_habitat.r")
#save.image("data/processed/chick.parent.behav.habitat.data.1203.RData")
load("data/processed/chick.parent.behav.habitat.data.1203.RData")

source("2_analyses/1_analyse_distance_contact_departures.r")
#save.image("data/processed/results.analyses.0117.RData")
load("data/processed/results.analyses.0117.RData")

source("3_visualisation/1_plotting_departures_distance_contact.r")
source("3_visualisation/2_maps_of_spatial_distribution_overall_and_during_contact.r")

source("2_analyses/3_feeding_obs_analysis.r")
source("3_visualisation/3_feeding_obs_plots.r")