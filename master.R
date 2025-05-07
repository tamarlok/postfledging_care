rm(list=ls())
#setwd("C:/Users/Tamar Lok/Documents/Analyses/22. Post-fledging parental care in spoonbills/postfledging_care")

source('functions.R')

packages <- c("RODBC", "move", "dplyr", "lubridate","reshape2","reshape","caret","randomForest", "plyr", "moments", "MuMIn", "lme4", "geosphere", "gdata", "sf", "mapview","terra","plotrix","emmeans","DHARMa","broom.mixed","OpenStreetMap","ggplot2","ggmap","boot","parallel","car","cowplot","grid","gridGraphics")
ipak(packages)

Sys.setenv(TZ="GMT")
set.seed(3) # to be able to reproduce random processes
options(digits=10)

# source("1_data_preparations/0_preparing_annotated_begging_data.r")

source("1_data_preparations/1_load_data_from_Movebank.r") # this loads and saves GPS and ACC data in two separate files
#save.image("data/processed/gps.data.from.movebank.0416.RData")
#save.image("data/processed/acc.data.from.movebank.0327.RData")
load("data/processed/gps.data.from.movebank.0416.RData")
source("1_data_preparations/2_determine_departure_dates_and_distance_parent_offspring.r")
#save.image("data/processed/chick.parent.data.sel.0418.RData")
source("2_analyses/1_classification_begging_behaviour.r")
#save.image("data/processed/model.classification.begging.1202.RData")
load("data/processed/acc.data.from.movebank.0327.RData")
load("data/processed/model.classification.begging.1202.RData")
load("data/processed/chick.parent.data.sel.0418.RData")
source("1_data_preparations/3_behavioral_classification_with_begging.r")
#save.image("data/processed/gps.behav.beg.data.1203.RData")
load("data/processed/gps.behav.beg.data.1203.RData")
load("data/processed/chick.parent.data.sel.0418.RData")
source("1_data_preparations/4_linking_behaviour_and_habitat.r")
#save.image("data/processed/chick.parent.behav.habitat.data.0418.RData")
load("data/processed/chick.parent.behav.habitat.data.0418.RData")

source("2_analyses/2_analyse_distance_contact_departures.r")
# save.image('data/processed/modelsel.results.0418.RData')
source("2_analyses/4_analyse_contact_with_other_parents.r")

source("2_analyses/3_feeding_obs_analysis.r")

load('data/processed/modelsel.results.0418.RData')

source("3_visualisation/1_plotting_departures_distance_contact.r")
source("3_visualisation/2_maps_of_spatial_distribution_overall_and_during_contact.r")


source("3_visualisation/3_feeding_obs_plots.r")