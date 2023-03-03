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

############################################################################
############################################################################
###                                                                      ###
###                                NOTE:                                 ###
###                                                                      ###
############################################################################
############################################################################

# Download files listed in the **Auxiliary files** sub-section in the [README] to stitch
# each variable together


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

#### Full CSVs ####

# Reminder: downlaod raw files in **Auxiliary files** section in the [README] and place corresponding files into
# its corresponding directory

##################################################################
##                           Colombia                           ##
##################################################################


raw_colombia_files <- list.files("~/peregrine_amazon/data/colombia/raw", full.names = F)


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

cleanCSVfunc_colombia_toRDS <- function(file_name){ # save space
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
  saveRDS(cleaned, paste0("~/peregrine_amazon/data/colombia/processed/", gsub("_raw.csv", "_processed", data_str)))
}



for(file in raw_colombia_files){
  print(file)
  if(grepl("Population", file, ignore.case = T)) {next}
  cleanCSVfunc_colombia_toRDS(file)
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

cleanCSVfunc_peru_toRDS <- function(file_name){ # save space
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
  saveRDS(cleaned, paste0("~/peregrine_amazon/data/peru/processed/", gsub("_raw.csv", "_processed", data_str)))
}

for(file in raw_peru_files){
  print(file)
  if(grepl("Population", file, ignore.case = T)) {next}
  cleanCSVfunc_peru_toRDS(file)
}



##################################################################
##                            Brazil                            ##
##################################################################


raw_brazil_files <- list.files("~/peregrine_amazon/data/brazil/raw", full.names = F) # remove population

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

cleanCSVfunc_brazil_toRDS <- function(file_name){ # save space
  data <- read.csv(file = paste0("~/peregrine_amazon/data/brazil/raw/", file_name))
  country = "Brazil"
  data_str <- file_name
  cleaned <- if(grepl("precip", file_name, ignore.case = T)){
    cleanerFunc_precip(data, country)
  } else {
    cleanerFunc(data, country)
  }
  if("mean" %in% names(cleaned)){
    colnames(cleaned)[colnames(cleaned) == "mean"] <- data_str # rename after
  }
  saveRDS(cleaned, paste0("~/peregrine_amazon/data/brazil/processed/", gsub("_raw.csv", "_processed", data_str)))
}



for(file in raw_brazil_files){
  print(file)
  if(grepl("Population", file, ignore.case = T)) {next}
  cleanCSVfunc_brazil_toRDS(file)
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
