rm(list=ls())

source('functions.R')

packages <- c("RODBC", "move", "dplyr", "lubridate","reshape2","reshape","caret","randomForest", "plyr", "moments", "MuMIn", "lme4", "geosphere", "gdata", "sf", "mapview","terra","plotrix","emmeans","DHARMa","broom.mixed","OpenStreetMap","ggplot2","ggmap","boot","car","cowplot","grid","gridGraphics")
ipak(packages)

Sys.setenv(TZ="GMT")
set.seed(3) # to be able to reproduce random processes
options(digits=10)

# DATA PROCESSING
# source("1_data_preparations/0_importing_feeding_observations_and_sex_info_from_colourring_database.r")
# source("1_data_preparations/0_preparing_annotated_begging_data.r")
source("1_data_preparations/1_load_data_from_Movebank.r") # this loads and saves raw GPS and ACC data from Movebank in two separate files
load("data/processed/gps.data.from.movebank.RData")
source("1_data_preparations/2_process_gps_data.r")
source("2_analyses/1_classification_begging_behaviour.r")
#save.image("data/processed/model.classification.begging.RData")
load("data/processed/acc.data.from.movebank.RData")
load("data/processed/model.classification.begging.RData")
source("1_data_preparations/3_process_acc_data.r")

source("1_data_preparations/4_determine_departure_dates_and_distance_parent_offspring.r")
#save.image("data/processed/chick.parent.contact.data.RData")
source("1_data_preparations/5_linking_behaviour_and_habitat.r")
# save.image("data/processed/chick.parent.behav.habitat.data.RData")

# DATA ANALYSES
source("2_analyses/2_analyse_distance_contact_departures.r")
source("2_analyses/3_feeding_obs_analysis.r")
# save.image('data/processed/modelsel.results.RData')

# VISUALISATION OF RESULTS
source("3_visualisation/1_plotting_departures_distance_contact.r")
source("3_visualisation/2_feeding_obs_plots.r")