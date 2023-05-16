# Malaria data downloaded by Andy and TJ;
# Raw data found in AmazonMalaria/Data/Raw/Disease_Data/ folder

# Scripts for processing
### Brazil: AmazonMalaria/R/1_DataProcessing/Brazil_Disease_v3.R
### Colombia: AmazonMalaria/R/1_DataProcessing/Colombia_Disease.R
### Peru: AmazonMalaria/R/1_DataProcessing/Peru_Disease_v2.R

################################################################################
#                                                                              #
# Purpose: The purpose of this script is to stitch all Malaria disease count   #
# data together to get a usable data frame to merge with the rest of the data  #
#                                                                              #
################################################################################


################################################################################
#                                                                              #
#                                    Annual                                    #
#                                                                              #
################################################################################

# read in data
brazil <- readRDS("~/network-storage/AmazonMalaria/Data/1_DataProcessing/Disease/Brazil/Brazil_Disease/annual/brazil_autoctones_malaria_annual_incidence.rds")

colombia.malaria_asociada <- read.csv("~/network-storage/AmazonMalaria/Data/1_DataProcessing/Disease/Colombia/Colombia_Disease_v4/malaria_asociada.csv")
colombia.malaria_complicada <- read.csv("~/network-storage/AmazonMalaria/Data/1_DataProcessing/Disease/Colombia/Colombia_Disease_v4/malaria_complicada.csv")
colombia.malaria_malarie <- read.csv("~/network-storage/AmazonMalaria/Data/1_DataProcessing/Disease/Colombia/Colombia_Disease_v4/malaria_malarie.csv")
colombia.malaria_mortalidad <- read.csv("~/network-storage/AmazonMalaria/Data/1_DataProcessing/Disease/Colombia/Colombia_Disease_v4/malaria_mortalidad.csv")
colombia.malaria_pfal <- read.csv("~/network-storage/AmazonMalaria/Data/1_DataProcessing/Disease/Colombia/Colombia_Disease_v4/malaria_pfal.csv")
colombia.malaria_vivax <- read.csv("~/network-storage/AmazonMalaria/Data/1_DataProcessing/Disease/Colombia/Colombia_Disease_v4/malaria_vivax.csv")
colombia.malaria <- read.csv("~/network-storage/AmazonMalaria/Data/1_DataProcessing/Disease/Colombia/Colombia_Disease_v4/malaria.csv")

peru.malaria_all <- read.csv("~/network-storage/AmazonMalaria/Data/Raw/Disease_Data/Peru/Malaria_all_2016_2022_v2.csv", encoding='latin1')
peru.pfal_all <- read.csv("~/network-storage/AmazonMalaria/Data/Raw/Disease_Data/Peru/Pfal_all_2016_2022_v2.csv", encoding='latin1')
peru.pviv_all <- read.csv("~/network-storage/AmazonMalaria/Data/Raw/Disease_Data/Peru/Pviv_all_2016_2022_v2.csv", encoding='latin1')

