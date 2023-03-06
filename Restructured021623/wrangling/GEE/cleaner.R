library(dplyr)
library(tidyr)
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
      group_by(Year, MuniCode, Month, Country) %>%
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
    dplyr::select(-date, -Day) %>%
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
cleanCSVfunc_colombia_toRDS(raw_colombia_files[1])
clean_colombia_veg <- readRDS("~/peregrine_amazon/data/colombia/processed/full_colombia_modis_veg_processed")
clean_colombia_hntl <- readRDS("~/peregrine_amazon/data/colombia/processed/full_colombia_HNTL_processed")
##
file_name = raw_colombia_files[1]
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

if("Month" %in% names(cleaned)){
  saveRDS(cleaned, paste0("~/peregrine_amazon/data/colombia/processed/", gsub("_raw.csv", "_processed_monthly", data_str)))
  annual <- cleaned %>%
    group_by(Year, MuniCode, Country) %>%
    select(-Month) %>%
    summarise_all(mean, na.rm = ifelse(sum(complete.cases(.)) > 10, T, F)) %>%
    ungroup()
  saveRDS(annual, paste0("~/peregrine_amazon/data/colombia/processed/", gsub("_raw.csv", "_processed_annual", data_str)))
} else {
  saveRDS(cleaned, paste0("~/peregrine_amazon/data/colombia/processed/", gsub("_raw.csv", "_processed_annual", data_str)))
}






##

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

  if("Month" %in% names(cleaned)){
    saveRDS(cleaned, paste0("~/peregrine_amazon/data/colombia/processed/", gsub("_raw.csv", "_processed_monthly", data_str)))
    annual <- cleaned %>%
      group_by(Year, MuniCode, Country) %>%
      select(-Month) %>%
      summarise_all(mean, na.rm = ifelse(sum(complete.cases(.)) > 10, T, F)) %>%
      ungroup()
    saveRDS(annual, paste0("~/peregrine_amazon/data/colombia/processed/", gsub("_raw.csv", "_processed_annual", data_str)))
  } else {
    saveRDS(cleaned, paste0("~/peregrine_amazon/data/colombia/processed/", gsub("_raw.csv", "_processed_annual", data_str)))
  }
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

  if("Month" %in% names(cleaned)){
    saveRDS(cleaned, paste0("~/peregrine_amazon/data/peru/processed/", gsub("_raw.csv", "_processed_monthly", data_str)))
    annual <- cleaned %>%
      group_by(Year, MuniCode, Country) %>%
      select(-Month) %>%
      summarise_all(mean, na.rm = ifelse(sum(complete.cases(.)) > 10, T, F)) %>%
      ungroup()
    saveRDS(annual, paste0("~/peregrine_amazon/data/peru/processed/", gsub("_raw.csv", "_processed_annual", data_str)))
  } else {
    saveRDS(cleaned, paste0("~/peregrine_amazon/data/peru/processed/", gsub("_raw.csv", "_processed_annual", data_str)))
  }
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

  if("Month" %in% names(cleaned)){
    saveRDS(cleaned, paste0("~/peregrine_amazon/data/brazil/processed/", gsub("_raw.csv", "_processed_monthly", data_str)))
    annual <- cleaned %>%
      group_by(Year, MuniCode, Country) %>%
      select(-Month) %>%
      summarise_all(mean, na.rm = ifelse(sum(complete.cases(.)) > 10, T, F)) %>%
      ungroup()
    saveRDS(annual, paste0("~/peregrine_amazon/data/brazil/processed/", gsub("_raw.csv", "_processed_annual", data_str)))
  } else {
    saveRDS(cleaned, paste0("~/peregrine_amazon/data/brazil/processed/", gsub("_raw.csv", "_processed_annual", data_str)))
  }
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

## use to filter to munis of interest;
## future task might be to filter colombia shapefile starting from GEE
colombia_munis_filter <- read.csv("~/MacDonald-REU-Summer-22-1/models/data/aad.csv") %>%
  filter(Country == 'Colombia') %>%
  filter(!is.na(pland_forest)) %>%
  select(Code) %>%
  unique()

processed_colombia_files <- list.files("~/peregrine_amazon/data/colombia/processed/", full.names = T, pattern = "_annual")

sapply(sapply(processed_colombia_files, readRDS), names)

full_colombia_env_vars <- data.frame(MuniCode = NA, Year = NA, Country = NA, stringsAsFactors = F)
for(file in processed_colombia_files){
  print(file)
  data <- readRDS(file)
  full_colombia_env_vars = full_join(full_colombia_env_vars, data, by = c('MuniCode', 'Year', 'Country'))
}

full_colombia_env_vars_v2 <- full_colombia_env_vars %>%
  filter(Year > 2000) %>%
  select(-Day) %>%
  rename(Code = MuniCode,
         HNTL = full_colombia_HNTL_raw.csv,
         LST_Day = LST_Day_1km,
         LST_Night = LST_Night_1km,
         Precip = full_colombia_Precip_raw.csv,
         StableLights = full_colombia_StableLights_raw.csv,
         AvgRad = full_colombia_AvgRad_raw.csv) %>%
  filter(Code %in% colombia_munis_filter$Code) %>%
  select(Code, Year, Country, NDVI, EVI, LST_Day, LST_Night,
         HNTL, StableLights, AvgRad, Precip,
         Evap_tavg, Qair_f_tavg, SoilMoi00_10cm_tavg, Wind_f_tavg,
         Tair_f_tavg, everything())

full_colombia_env_vars_v2 %>% summary()


## PERU ##

peru_munis_filter <- read.csv("~/MacDonald-REU-Summer-22-1/models/data/aad.csv") %>%
  filter(Country == 'Peru') %>%
  filter(!is.na(pland_forest)) %>%
  select(Code) %>%
  unique()

processed_peru_files <- list.files("~/peregrine_amazon/data/peru/processed/", full.names = T, pattern = "_annual")

sapply(sapply(processed_peru_files, readRDS), names)

full_peru_env_vars <- data.frame(MuniCode = NA, Year = NA, Country = NA, stringsAsFactors = F)
for(file in processed_peru_files){
  print(file)
  data <- readRDS(file)
  full_peru_env_vars = full_join(full_peru_env_vars, data, by = c('MuniCode', 'Year', 'Country'))
}

full_peru_env_vars_v2 <- full_peru_env_vars %>%
  filter(Year > 2000) %>%
  select(-Day) %>%
  rename(Code = MuniCode,
         HNTL = full_peru_HNTL_raw.csv,
         LST_Day = LST_Day_1km,
         LST_Night = LST_Night_1km,
         Precip = `full_peru_precip_20-21_raw.csv`,
         AvgRad = full_peru_AvgRad_raw.csv) %>%
  filter(Code %in% peru_munis_filter$Code) %>%
  mutate(StableLights = NA) %>%
  select(Code, Year, Country, NDVI, EVI, LST_Day, LST_Night,
         HNTL, StableLights, AvgRad, Precip,
         Evap_tavg, Qair_f_tavg, SoilMoi00_10cm_tavg, Wind_f_tavg,
         Tair_f_tavg, everything())


## Brazil ##


## use to filter to munis of interest;
## future task might be to filter brazil shapefile starting from GEE
brazil_munis_filter <- read.csv("~/MacDonald-REU-Summer-22-1/models/data/aad.csv") %>%
  filter(Country == 'Brazil') %>%
  filter(!is.na(pland_forest)) %>%
  select(Code) %>%
  unique()

processed_brazil_files <- list.files("~/peregrine_amazon/data/brazil/processed/", full.names = T, pattern = "_annual")

sapply(sapply(processed_brazil_files, readRDS), summary)
sapply(sapply(processed_brazil_files, readRDS), dim)

full_brazil_env_vars <- data.frame(MuniCode = NA, Year = NA, Country = NA, stringsAsFactors = F)
for(file in processed_brazil_files){
  print(file)
  data <- readRDS(file)
  full_brazil_env_vars = full_join(full_brazil_env_vars, data, by = c('MuniCode', 'Year', 'Country'))
}

full_brazil_env_vars_v2 <- full_brazil_env_vars %>%
  filter(Year > 2000) %>%
  select(-Day) %>%
  rename(Code = MuniCode,
         HNTL = full_brazil_HNTL_raw.csv,
         LST_Day = LST_Day_1km,
         LST_Night = LST_Night_1km,
         Precip = `full_brazil_precip_20-21_raw.csv`,
         AvgRad = full_brazil_AvgRad_raw.csv) %>%
  filter(Code %in% brazil_munis_filter$Code) %>%
  mutate(StableLights = NA) %>%
  select(Code, Year, Country, NDVI, EVI, LST_Day, LST_Night,
         HNTL, StableLights, AvgRad, Precip,
         Evap_tavg, Qair_f_tavg, SoilMoi00_10cm_tavg, Wind_f_tavg,
         Tair_f_tavg, everything())

full_brazil_env_vars_v2 %>% summary()

full_env_vars_updated <- full_colombia_env_vars_v2 %>%
  rbind(full_peru_env_vars_v2) %>%
  rbind(full_brazil_env_vars_v2) %>%
  group_by(Country, Year) %>%
  arrange(Code) %>%
  # group_by(Code, .add=T) %>%
  # mutate(n = sum(complete.cases(.)/n())) %>%
  ungroup()

## aad to merge ##
aad <- read.csv("~/MacDonald-REU-Summer-22-1/models/data/aad.csv")

aad_v2 <- aad %>%
  full_join(full_env_vars_updated, by = c("Code", "Year", "Country",
                                          'NDVI', 'EVI', 'LST_Day', 'LST_Night',
                                          'StableLights', 'AvgRad', 'Precip')) %>% # Use fill()
  mutate(Country = as.factor(Country)) %>%
  filter(Code %in% intersect(aad$Code, full_env_vars_updated$Code))

aad_v2 %>% summary()

aad_v2 %>% filter(Country == "Colombia") %>% View("colombia")
aad_v2 %>% filter(Country == "Brazil") %>% View("brazil")
aad_v2 %>% filter(Country == "Peru") %>% View("peru")

aad_v3 <- aad_v2 %>%
  group_by(Country) %>%
  arrange(Code, Year)

aad_v3 %>% filter(Country == "Colombia") %>% View("colombia")
aad_v2 %>% filter(Country == "Brazil") %>% View("brazil")
aad_v2 %>% filter(Country == "Peru") %>% View("peru")


## Missing

naniar::gg_miss_var(aad_v2 %>%
                      select(-c(Chikungunya:Zika, OptTemp_Obs:Malaria_OptTemp)),
                    facet = Year)
