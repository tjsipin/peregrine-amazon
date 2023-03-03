library(dplyr)
# Pulls together all variables into one data set using neighborhoods of zones with population > 1 person/km^2
## Variables
### Average Radiance
### FLDAS (humidity)
### Harmonized Night Time Lights (harmonized between AvgRad and StableLights)
### Land Surface Temperature
### NDVI / EVI
### Primary Production (unnecessary due to high correlation with other variables ?)
### Precipitation
### Stable Lights
### Standing Water Occurrence


cleanerFunc <- function(data, country, var){
  cleaned <- data %>%
    select(-c(system.index, .geo)) %>%
    mutate(Year = substring(date, 1, 4) %>% as.integer(),
           Month = substring(date, 6, 7) %>% as.integer(),
           Day = substring(date, 9, 10) %>% as.integer(),
           Country = country)
  if(length(unique(cleaned$Month)) == 1){
    cleaned_v2 <- cleaned %>%
      select(-date, -Month, -Day) %>%
      group_by(Year, MuniCode, Country) %>%
      summarise_all(mean) %>%
      ungroup()
  } else {
    cleaned_v2 <- cleaned %>%
      select(-date, -Day) %>%
      group_by(Year, Month, MuniCode, Country) %>%
      summarise_all(mean) %>%
      ungroup()
  }
  return(cleaned_v2)
}

## Need function for precip since daily ##

cleanerFunc_precip <- function(data, country){
  cleaned <- data %>%
    dplyr::select(-c(system.index, .geo)) %>%
    mutate(Year = substring(date, 1, 4) %>% as.integer(),
           Month = substring(date, 6, 7) %>% as.integer(),
           Day = substring(date, 9, 10) %>% as.integer(),
           Country = country)

  cleaned_v2 <- cleaned %>%
    dplyr::select(-date) %>%
    group_by(Year, Month, MuniCode, Country) %>%
    summarise_all(sum) %>%
    ungroup()

  return(cleaned_v2)
}

##### Neighborhood CSVs ####

# AvgRad <- read.csv("~/peregrine_amazon/data/colombia/neighborhood_colombia_zones_AvgRad.csv") %>%
#   cleanerFunc()
# FLDAS <- read.csv("~/peregrine_amazon/data/colombia/neighborhood_colombia_zones_FLDAS.csv") %>%
#   cleanerFunc()
# HNTL <- read.csv("~/peregrine_amazon/data/colombia/neighborhood_colombia_zones_HNTL.csv") %>%
#   cleanerFunc()
# LST <- read.csv("~/peregrine_amazon/data/colombia/neighborhood_colombia_zones_LST.csv") %>%
#   cleanerFunc()
# modis <- read.csv("~/peregrine_amazon/data/colombia/neighborhood_colombia_zones_modis_veg.csv") %>%
#   cleanerFunc()
# PP <- read.csv("~/peregrine_amazon/data/colombia/neighborhood_colombia_zones_PP.csv") %>%
#   cleanerFunc()
# Precip <- read.csv("~/peregrine_amazon/data/colombia/neighborhood/neighborhood_colombia_zones_Precip.csv") %>%
#   cleanerFunc()
# StableLights <- read.csv("~/peregrine_amazon/data/colombia/neighborhood/neighborhood_colombia_zones_StableLights.csv") %>%
#   cleanerFunc()
# SWOccurrence <- read.csv("~/peregrine_amazon/data/colombia/neighborhood/neighborhood_colombia_zones_SWOccurrence.csv") %>%
#   cleanerFunc()
# Incidence <- read.csv("~/peregrine_amazon/data/colombia/Incidence_v4/colombia_monthly_incidence_wide.csv") %>%
#   rename(MuniCode = Muni_Code,
#          Total_Population = Population) %>%
#   filter(Year != 2021)

#### Full CSVs ####

##################################################################
##                           Colombia                           ##
##################################################################

raw_colombia_files <- list.files("~/peregrine_amazon/data/colombia/raw", full.names = F) # no population


cleanCSVfunc_colombia <- function(file_name){
  data <- read.csv(file = paste0("~/peregrine_amazon/data/colombia/raw/", file_name))
  country = "Colombia"
  data_str <- file_name
  cleaned <- if(grepl("precip", file_name, ignore.case = T)){
    cleanerFunc_precip(data, country)
  } else {
    cleanerFunc(data, country)
  }
  if("mean" %in% names(cleaned)){
    colnames(cleaned)[colnames(cleaned) == "mean"] <- data_str # rename after
  }
  write.csv(cleaned, paste0("~/peregrine_amazon/data/colombia/processed/", gsub("_raw.csv", "_processed.csv", data_str)))
}

for(file in raw_colombia_files){
  print(file)
  if(grepl("Population", file, ignore.case = T)) {next}
  cleanCSVfunc_colombia(file)
}



##################################################################
##                             Peru                             ##
##################################################################


raw_peru_files <- list.files("~/peregrine_amazon/data/peru/raw", full.names = F)


cleanCSVfunc_peru <- function(file_name){
  data <- read.csv(file = paste0("~/peregrine_amazon/data/peru/raw/", file_name))
  country = "Peru"
  data_str <- file_name
  cleaned <- if(grepl("precip", file_name, ignore.case = T)){
    cleanerFunc_precip(data, country)
  } else {
    cleanerFunc(data, country)
  }
  if("mean" %in% names(cleaned)){
    colnames(cleaned)[colnames(cleaned) == "mean"] <- data_str # rename after
  }
  write.csv(cleaned, paste0("~/peregrine_amazon/data/peru/processed/", gsub("_raw.csv", "_processed.csv", data_str)))
}

for(file in raw_peru_files){
  print(file)
  if(grepl("Population", file, ignore.case = T)) {next}
  cleanCSVfunc_peru(file)
}



##################################################################
##                            Brazil                            ##
##################################################################


raw_brazil_files <- list.files("~/peregrine_amazon/data/brazil/raw", full.names = F)[-7] # remove population

cleanCSVfunc_brazil <- function(file_name){
  data <- read.csv(file = paste0("~/peregrine_amazon/data/brazil/raw/", file_name))
  country = "Brazil"
  data_str <- file_name
  cleaned <- if(grepl("precip", file_name, ignore.case = T)){
    cleanerFunc_precip(data, country)
  } else {
    cleanerFunc(data, country)
  }
  if("mean" %in% names(cleaned)){
    colnames(cleaned)[colnames(cleaned) == "mean"] <- data_str # rename var
  }
  write.csv(cleaned, paste0("~/peregrine_amazon/data/brazil/processed/", gsub("_raw.csv", "_processed.csv", data_str)))
}

for(file in raw_brazil_files){
  print(file)
  if(grepl("Population", file, ignore.case = T)) {next}
  cleanCSVfunc_brazil(file)
}

#################################################################
##                       Stitch together                       ##
#################################################################

# Colombia

processed_colombia_files <- list.files("~/peregrine_amazon/data/colombia/processed/", full.names = T)
full_colombia_env_vars <- data.frame(NA)
for(file in processed_colombia_files){
  print(file)
  data <- read.csv(file)
  full_colombia_env_vars = full_join(full_colombia_env_vars, data)
}



read.csv(processed_colombia_files[3]) %>% names


processed_peru_files <- list.files("~/peregrine_amazon/data/peru/processed/", full.names = T)

read.csv(processed_peru_files[1]) %>% names()

processed_colombia_files <- list.files("~/peregrine_amazon/data/colombia/processed/", full.names = T)

read.csv(processed_colombia_files[3]) %>% names()
