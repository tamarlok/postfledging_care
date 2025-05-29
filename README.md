Reference Information
=====================

* Author: 			Tamar Lok
* Institutes:			EcoMoves, University of Groningen, NIOZ Royal Netherlands Institute for Sea Research 
* Study area:			The Netherlands, with specific focus on Schiermonnikoog and surroundings (53°29’N, 6°15’E).
* Study period:			June 2016 - December 2019
* Persistent Identifier:	not yet available
* Date of Issue:		2025-05-14

This readme.md file briefly describes the contents of this repository, which has the aim to process, 
analyse and plot the data that produce the results reported in the paper.  

- - -

Accompanying manuscript
=====================

* Paper Title: Eurasian Spoonbill chicks receive parental care up to several months after fledging, but not into migration
* Preprint Identifier: https://doi.org/10.1101/2025.05.15.654226
* Authors: Tamar Lok, P. de Goeij, E. Rakhimberdiev, T. Piersma, W. Vansteelant

- - -

Licensing
==========================

Code in this repository is licensed under the MIT License (see LICENSE).

Data files are licensed under the CC-BY 4.0 License (see DATA_LICENSE).

- - -

Methodological Information
==========================

* Methods of data collection: see article for details
* Methods of data processing: see article for details and R-scripts in the folder '1_data_preparation'. These scripts import the raw tracking (GPS and accelerometer) data from Movebank, along with additional files published in this repository, and process the data for analysis.

- - -

Project Structure
===================

```
├── README.md
├── data/raw				<- Data used in the analyses
├── master.R			<- Code that calls the different scripts for data preparation, analyses and visualisation
├── functions.R		<- Code for the functions used in the different scripts
├── 1_data_preparation	<- Code to import and process data
├── 2_analyses			<- Code to reproduce analyses 
└── 3_visualisation	<- Code for visualisation of results
```

- - -

Data files
==========
The raw tracking data generated and analyzed in this study are stored at Movebank (www.movebank.org) in the study “SPOONBILL_METAWAD - Eurasian Spoonbills (Platalea leucorodia, Threskiornithidae) breeding on Schiermonnikoog, The Netherlands” (Movebank study ID 2596955604) and "SPOONBILL_SPINOZA – Eurasian Spoonbills (Platalea leucorodia, Threskiornithidae) born on Schiermonnikoog, The Netherlands" (Movebank study ID 2818737964) and will be made available upon reasonable request. The processed tracking data and other datafiles required to run the scripts in the folders '2_analyses' and '3_visualisation' are available in the folder 'data/raw' and are described below in alphabetical order. 

Details for: annotated.begging.data.csv
--------------------------
* Description: a comma-delimited file containing the behaviour-annotated accelerometer data, either from video footage or based on graphic inspection, of juvenile 6381. 

* Format: .csv

* Size: 1,055 kB

* Variables:
	* BirdID: unique identifier of bird
	* date.time: the date and time with resolution up to whole seconds.
	* Index: numbered column with the order in which accelerometer measurements were taken
	* x: acceleration in surge-direction
	* y: acceleration in sway-direction
	* z: acceleration in heave-direction
	* speed_2d: speed in m/s measured by the GPS device
	* behaviour: behaviour determined from video or from graphical inspection, distinguishing begging for food at the parent (beg), shaking feathers 
	* annotation.method: video = annotation through video footage, graphical = estimation of behaviour (only done for begging) based on the visual pattern in the accelerometer data and the confirmed proximity to the parent (based on field observation). 

Details for: chick.biometrics.csv
--------------------------
* Description: a comma-delimited file containing the biometric measurements and mass of the tracker for each GPS-tagged chick 

* Format: .csv

* Size: 1 kB

* Variables: 
	* birdID: unique identifier of bird
	* start_deployment: date at which the bird received its tracker and biometric measurements were taken
	* bodymass: mass of the bird in grams
	* P8: length of the 8th primary feather in mm
	* tarsus: length of the tarsus in mm
	* tracker_mass: mass of the tracker in grams 

Details for: gps.behav.data.csv
--------------------------
* Description: a comma-delimited file containing the locations, timestamps and behaviours estimated from the raw acceleration data of GPS-tagged parents and their chicks. This datafile is somewhat smaller than gps.data.csv (see below) because acceleration samples were sometimes missing for gps timestamps.

* Format: .csv

* Size: 47,313 kB

* Variables:
	* BirdID: unique identifier of bird
	* datetime_CEST: the date and time with resolution up to whole seconds in CEST timezone.
	* latitude: latitude in decimal degrees as measured by the GPS tag
	* longitude: longitude in decimal degrees as measured by the GPS tag
	* altitude: altitude in decimal degrees as measured by the GPS tag
	* ground.speed: speed in m/s at ground level as measured by the GPS tag
	* sex: sex of the bird
	* ageclass: age of the bird at tagging
	* behaviour: behaviour classified from the raw acceleration data from Movebank using the script 1_data_preparations/3_process_acc_data.r
	* odba: overall dynamic body acceleration measured over the three axes during the 1.6 s 20 Hz acceleration samples

Details for: gps.data.csv
--------------------------
* Description: a comma-delimited file containing the 10-minute resolution locations and timestamps of GPS-tagged parents and their chicks. 

* Format: .csv

* Size: 49,145 kB

* Variables:
	* BirdID: unique identifier of bird
	* datetime_CEST: the date and time with resolution up to whole seconds in CEST timezone.
	* latitude: latitude in decimal degrees as measured by the GPS tag
	* longitude: longitude in decimal degrees as measured by the GPS tag
	* altitude: altitude in decimal degrees as measured by the GPS tag
	* ground.speed: speed in m/s at ground level as measured by the GPS tag
	* sex: sex of the bird
	* ageclass: age of the bird at tagging

Details for: feeding.observations.with.sex.info.csv
--------------------------
* Description: a comma-delimited file containing the information on the feeding observations of colour-ringed chicks born in The Netherlands

* Format: .csv

* Size: 29 kB

* Variables:
	* Chick.code: colour-ring code of the chick
	* Parent.code: colour-ring code of the parent (black if parent was unringed)
	* Date: date when chick was observed being fed by the parent
	* SexParentSel: selected sex of the parent (which is the molecularly determined sex if available, otherwise the sex determined by observation, as described in the manuscript)
	* SexParentMol: molecularly determined sex of the parent
	* SexChickSel: selected sex of the chick (which is the molecularly determined sex if available, otherwise the sex determined by observation, as described in the manuscript)
	* SexChickMol: molecularly determined sex of the chick
	* ChickAge: age of the chick at the time of the feeding observation as estimated from its 8th primary length at ringing
	* Location: location of feeding observation
	* Latitude: latitude of the feeding observation location
	* Longitude: longitude of the feeding observation location
	* RingLocation: location where the chick was ringed (and born)
	* RingLatitude: latitude of the location where the chick was ringed (and born)
	* RingLongitude: longitude of the location where the chick was ringed (and born)
	* rnd: column with a random number to randomly select one observation per chick for the analysis

Details for: metadata.tagged.birds.csv
--------------------------
* Description: a comma-delimited file containing the 10-minute resolution locations and timestamps of GPS-tagged parents and their chicks. 

* Format: .csv

* Size: 3 kB

* Variables:
	* BirdID: unique identifier of bird
	* tagID: unique identifier of the GPS tag
	* colourcode: unique combination of (coded) colour-rings attached to the tibia of the bird
	* sex: sex of the bird
	* ageclass: age of the bird at tagging
	* start_deployment: date when the bird received its GPS tag
	* end_deployment: date when the GPS tag was removed from the bird
	* mortality_date: date at which the bird died

Details for: parent.offspring.data.csv
--------------------------
* Description: a comma-delimited file containing the information of GPS-tagged parents, their GPS-tagged chick and potentially other siblings 

* Format: .csv

* Size: 1 kB

* Variables:
	* parentID: unique identifier of the parent
	* chickID: unique identifier of the chick
	* year: year in which the chick was born
	* n_chicks: total number of chicks that survived until fledging
	* nth_chick: the chick order of the GPS-tagged chick (chickID) relative to its siblings 







