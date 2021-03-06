# Landscape-level drivers of flying insect biomass
This repository (R project) contains all scripts necessary to run the ecological/statistical analyses from the study "Landscape-level drivers of flying insect biomass" (Svenningsen et al.).

The data were collected in June 2018 as part of the citizen science project InsectMobile ("Insektmobilen") at the Natural History Museum of Denmark and at iDiv in Germany during June-July 2018.

## Description of the sub directories for data processing ##

* **raw-data**: contains all the raw data used to generate the full datasets for statistical analyses, e.g. metadata from the laboratory and sampling event data.
* **cleaned-data**: contains tables generated by the scripts used in the analysis, e.g. the proportional land cover and land use data for each buffer zone.
* **covariate-data**: contains all the raw data files for the environmental data, e.g. land cover, land use, coordinates, and potential stops on the routes.
* **reports**: a step-wise list of scripts (01_, 02_ etc.) used for processing and analysing the data
* **plots**: figures generated by the scripts and other plots not used in the manuscript

## Statistical analyses and modelling ##
Landscape-level effects on flying insect biomass was modelled with linear mixed-effects models at the buffer size with the most pronounced effect size.
