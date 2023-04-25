library(dplyr)
library(tidyr)
library(stringr)
library(sf)
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


t0 <- proc.time()
cleanerFunc <- function(data, country){
  cleaned <- data %>%
    dplyr::select(-c(system.index, .geo)) %>%
    mutate(Year = substring(date, 1, 4) %>% as.integer(),
           Month = substring(date, 6, 7) %>% as.integer(),
           Day = substring(date, 9, 10) %>% as.integer(),
           Country = country) %>%
    filter(Year >= 2000)
  if(length(unique(cleaned$Month)) == 1){
    cleaned_v2 <- cleaned %>%
      dplyr::select(-date, -Month, -Day) %>%
      group_by(Year, MuniCode, Country) %>%
      summarise_all(mean) %>%
      ungroup()
  } else {
    cleaned_v2 <- cleaned %>%
      dplyr::select(-date, -Day) %>%
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
           Country = country) %>%
    group_by(Year, MuniCode, Country) %>%
    rename(Precip = mean) %>%
    ungroup()

  cleaned_v2 <- cleaned %>%
    dplyr::select(-date, -Day) %>%
    group_by(Year, Month, MuniCode, Country) %>%
    summarise(Precip = sum(Precip)) %>%
    ungroup()

  return(cleaned_v2)
}

cleanerFunc_LST_Day <- function(data, country){

  cleaned <- data %>%
    dplyr::select(-c(.geo)) %>%
    mutate(Year = substring(date, 1, 4) %>% as.integer(),
           Month = substring(date, 6, 7) %>% as.integer(),
           Country = country) %>%
    group_by(Year, MuniCode, Country) %>%
    mutate(max_LST_Day = max(LST_Day_1km),
           min_LST_Day = min(LST_Day_1km))

  cleaned_v2 <- cleaned %>%
    dplyr::select(-date) %>%
    group_by(Year, Month, MuniCode, Country) %>%
    summarise_all(mean) %>%
    ungroup()

}

cleanerFunc_LST_Night <- function(data, country){

  cleaned <- data %>%
    dplyr::select(-c(.geo)) %>%
    mutate(Year = substring(date, 1, 4) %>% as.integer(),
           Month = substring(date, 6, 7) %>% as.integer(),
           Country = country) %>%
    group_by(Year, MuniCode, Country) %>%
    mutate(max_LST_Night = max(LST_Night_1km),
           min_LST_Night = min(LST_Night_1km))

  cleaned_v2 <- cleaned %>%
    dplyr::select(-date) %>%
    group_by(Year, Month, MuniCode, Country) %>%
    summarise_all(mean) %>%
    ungroup()

}

#### Full CSVs ####

# Reminder: downlaod raw files in **Auxiliary files** section in the [README] and place corresponding files into
# their corresponding directories

##################################################################
##                           Colombia                           ##
##################################################################


raw_colombia_files <- list.files("~/peregrine_amazon/data/colombia/raw/final/", full.names = F)


##

cleanCSVfunc_colombia_toRDS <- function(file_name){ # save space
  data <- read.csv(file = paste0("~/peregrine_amazon/data/colombia/raw/final/", file_name))
  country = "Colombia"
  data_str <- file_name
  if(grepl("precip", file_name, ignore.case = T)){
    cleaned <- cleanerFunc_precip(data, country)
    print('precip')
  } else if (grepl("LST_Day", file_name, ignore.case = T)) {
    cleaned <- cleanerFunc_LST_Day(data, country)
    print('lst')
  } else if (grepl("LST_Night", file_name, ignore.case = T)) {
    cleaned <- cleanerFunc_LST_Night(data, country)
    print('lst')
  }
  else {
    cleaned <- cleanerFunc(data, country)
    print('else')
  }

  if("mean" %in% names(cleaned)){
    colnames(cleaned)[colnames(cleaned) == "mean"] <- data_str # rename after
  }





  if("Month" %in% names(cleaned)){
    saveRDS(cleaned, paste0("~/peregrine_amazon/data/colombia/processed/final/monthly/", gsub("_raw.csv", "_processed_monthly", data_str)))
    annual <- if(grepl("precip", file_name, ignore.case = T)) {
      cleaned %>%
        group_by(Year, MuniCode, Country) %>%
        dplyr::select(-Month) %>%
        summarise(
          Precip = sum(Precip)) %>%
        ungroup()
    } else {
      cleaned %>%
        group_by(Year, MuniCode, Country) %>%
        dplyr::select(-Month) %>%
        summarise_all(mean, na.rm = ifelse(sum(complete.cases(.)) > 10, T, F)) %>%
        ungroup()
    }

    saveRDS(annual, paste0("~/peregrine_amazon/data/colombia/processed/final/annual/", gsub("_raw.csv", "_processed_annual", data_str)))
  } else {
    saveRDS(cleaned, paste0("~/peregrine_amazon/data/colombia/processed/final/annual/", gsub("_raw.csv", "_processed_annual", data_str)))
  }
}



for(file in raw_colombia_files){
  print(file)
  cleanCSVfunc_colombia_toRDS(file)
}





##################################################################
##                             Peru                             ##
##################################################################


raw_peru_files <- list.files("~/peregrine_amazon/data/peru/raw/final/", full.names = F)

cleanCSVfunc_peru_toRDS <- function(file_name){ # save space
  data <- read.csv(file = paste0("~/peregrine_amazon/data/peru/raw/final/", file_name))
  country = "Peru"
  data_str <- file_name
  if(grepl("precip", file_name, ignore.case = T)){
    cleaned <- cleanerFunc_precip(data, country)
    print('precip')
  } else if (grepl("LST_Day", file_name, ignore.case = T)) {
    cleaned <- cleanerFunc_LST_Day(data, country)
    print('lst')
  } else if (grepl("LST_Night", file_name, ignore.case = T)) {
    cleaned <- cleanerFunc_LST_Night(data, country)
    print('lst')
  }
  else {
    cleaned <- cleanerFunc(data, country)
    print('else')
  }

  if("mean" %in% names(cleaned)){
    colnames(cleaned)[colnames(cleaned) == "mean"] <- data_str # rename after
  }



  if("Month" %in% names(cleaned)){
    saveRDS(cleaned, paste0("~/peregrine_amazon/data/peru/processed/final/monthly/", gsub("_raw.csv", "_processed_monthly", data_str)))
    annual <- if(grepl("precip", file_name, ignore.case = T)) {
      cleaned %>%
        group_by(Year, MuniCode, Country) %>%
        dplyr::select(-Month) %>%
        summarise(
          Precip = sum(Precip)) %>%
        ungroup()
    } else {
      cleaned %>%
        group_by(Year, MuniCode, Country) %>%
        dplyr::select(-Month) %>%
        summarise_all(mean, na.rm = ifelse(sum(complete.cases(.)) > 10, T, F)) %>%
        ungroup()
    }

    saveRDS(annual, paste0("~/peregrine_amazon/data/peru/processed/final/annual/", gsub("_raw.csv", "_processed_annual", data_str)))
  } else {
    saveRDS(cleaned, paste0("~/peregrine_amazon/data/peru/processed/final/annual/", gsub("_raw.csv", "_processed_annual", data_str)))
  }
}


for(file in raw_peru_files){
  print(file)
  cleanCSVfunc_peru_toRDS(file)
}




##################################################################
##                            Brazil                            ##
##################################################################


raw_brazil_files <- list.files("~/peregrine_amazon/data/brazil/raw/final/", full.names = F) # remove population


### DONE ###

# # Combine LST_Day .csv and LST_Night .csv
# brazil_LST_Day_raw <- read.csv(paste0("~/peregrine_amazon/data/brazil/raw/final/", raw_brazil_files[4])) %>%
#   select(-system.index, -.geo)
#
# brazil_LST_Night_raw <- read.csv(paste0("~/peregrine_amazon/data/brazil/raw/final/", raw_brazil_files[5])) %>%
#   select(-system.index, -.geo)
#
# brazil_LST_combined <- brazil_LST_Day_raw %>%
#   full_join(brazil_LST_Night_raw, by = c("MuniCode", "date")) %>%
#   select(MuniCode, date, everything()) %>%
#   # add dummy columns for processing step later
#   mutate(.geo = 0,
#          system.index = 0)
#
# write_csv(brazil_LST_combined, "~/peregrine_amazon/data/brazil/raw/final/full_brazil_LST_20_raw.csv")
# file.remove("~/peregrine_amazon/data/brazil/raw/final/full_brazil_LST_Day_20_raw.csv")
# file.remove("~/peregrine_amazon/data/brazil/raw/final/full_brazil_LST_Night_20_raw.csv")

##############

raw_brazil_files <- list.files("~/peregrine_amazon/data/brazil/raw/final/", full.names = F) # remove population

cleanCSVfunc_brazil_toRDS <- function(file_name){ # save space
  data <- read.csv(file = paste0("~/peregrine_amazon/data/brazil/raw/final/", file_name))
  country = "Brazil"
  data_str <- file_name
  if(grepl("precip", file_name, ignore.case = T)){
    cleaned <- cleanerFunc_precip(data, country)
    print('precip')
  } else if (grepl("LST_Day", file_name, ignore.case = T)) {
    cleaned <- cleanerFunc_LST_Day(data, country)
    print('lst')
  } else if (grepl("LST_Night", file_name, ignore.case = T)) {
    cleaned <- cleanerFunc_LST_Night(data, country)
    print('lst')
  }
  else {
    cleaned <- cleanerFunc(data, country)
    print('else')
  }

  if("mean" %in% names(cleaned)){
    colnames(cleaned)[colnames(cleaned) == "mean"] <- data_str # rename after
  }



  if("Month" %in% names(cleaned)){
    saveRDS(cleaned, paste0("~/peregrine_amazon/data/brazil/processed/final/monthly/", gsub("_raw.csv", "_processed_monthly", data_str)))
    annual <- if(grepl("precip", file_name, ignore.case = T)) {
      cleaned %>%
        group_by(Year, MuniCode, Country) %>%
        dplyr::select(-Month) %>%
        summarise(
          Precip = sum(Precip)) %>%
        ungroup()
    } else {
      cleaned %>%
        group_by(Year, MuniCode, Country) %>%
        dplyr::select(-Month) %>%
        summarise_all(mean, na.rm = ifelse(sum(complete.cases(.)) > 10, T, F)) %>%
        ungroup()
    }

    saveRDS(annual, paste0("~/peregrine_amazon/data/brazil/processed/final/annual/", gsub("_raw.csv", "_processed_annual", data_str)))
  } else {
    saveRDS(cleaned, paste0("~/peregrine_amazon/data/brazil/processed/final/annual/", gsub("_raw.csv", "_processed_annual", data_str)))
  }
}



for(file in raw_brazil_files){
  print(file)
  cleanCSVfunc_brazil_toRDS(file)
}

proc.time() - t0
#################################################################
##                       Stitch together                       ##
#################################################################

## HNTL ##
### Requires special algorithm
### Contains all municipios

HNTL_IC <- list.files("~/peregrine_amazon/data/HNTL_raw/", full.names = T)

HNTL <- data.frame()
for(file in HNTL_IC){
  data <- read.csv(file) %>%
    rename(HNTL = mean) %>%
    mutate(Year = substr(system.index, 19, 22) %>%
             as.integer()) %>%
    select(-system.index, -.geo)
  HNTL <- HNTL %>%
    rbind(data)
}

# saveRDS(HNTL, "~/peregrine_amazon/data/wrangling/HNTL.rds")


# Colombia

## use to filter to munis of interest;
## future task might be to filter colombia shapefile starting from GEE
colombia_munis_filter <- read.csv("~/MacDonald-REU-Summer-22-1/models/data/aad.csv") %>%
  filter(Country == 'Colombia') %>%
  filter(!is.na(pland_forest)) %>%
  dplyr::select(Code) %>%
  unique()


# move all final files to ~/peregrine_amazon/data/colombia/processed/annual folder

processed_colombia_files <- list.files("~/peregrine_amazon/data/colombia/processed/final/annual/", full.names = T, pattern = "_annual")

sapply(sapply(processed_colombia_files, readRDS), names)

full_colombia_env_vars <- data.frame(MuniCode = NA, Year = NA, Country = NA, stringsAsFactors = F)
for(file in processed_colombia_files){
  print(file)
  data <- readRDS(file) %>%
    filter(Year >= 2000)
  full_colombia_env_vars = full_join(full_colombia_env_vars, data, by = c('MuniCode', 'Year', 'Country'))
}

## This is the final product:
## Use full_precip_v2

full_colombia_env_vars_v2 <- full_colombia_env_vars %>%
  filter(Year != 2022) %>%
  # dplyr::select(-Day) %>%
  rename(Code = MuniCode,
         StableLights = full_colombia_StableLights_raw.csv,
         AvgRad = full_colombia_AvgRad_raw.csv,
         LST_Day = LST_Day_1km,
         LST_Night = LST_Night_1km) %>%
  group_by(Code) %>%
  arrange(Year, .by_group = T) %>%
  ungroup() %>%
  filter(Code %in% colombia_munis_filter$Code) %>%
  dplyr::select(Code, Year, Country, Population, NDVI, EVI,
                LST_Day, max_LST_Day, min_LST_Day,
                LST_Night, max_LST_Night, min_LST_Night,
                StableLights, AvgRad, Precip,
                Evap_tavg, Qair_f_tavg, SoilMoi00_10cm_tavg, Wind_f_tavg,
                Tair_f_tavg, everything(), -contains('.x'), -contains('.y'))



## PERU ##

peru_munis_filter <- read.csv("~/MacDonald-REU-Summer-22-1/models/data/aad.csv") %>%
  filter(Country == 'Peru') %>%
  filter(!is.na(pland_forest)) %>%
  dplyr::select(Code) %>%
  unique()

processed_peru_files <- list.files("~/peregrine_amazon/data/peru/processed/final/annual/", full.names = T, pattern = "_annual")

full_peru_env_vars <- data.frame(MuniCode = NA, Year = NA, Country = NA, stringsAsFactors = F)
for(file in processed_peru_files){
  print(file)
  data <- readRDS(file) %>%
    filter(Year >= 2000)
  full_peru_env_vars = full_join(full_peru_env_vars, data, by = c('MuniCode', 'Year', 'Country'))
}

## This is the final product:
## Use full_precip_v2

full_peru_env_vars_v2 <- full_peru_env_vars %>%
  filter(Year != 2022) %>%
  # dplyr::select(-Day) %>%
  rename(Code = MuniCode,
         AvgRad = full_peru_AvgRad_raw.csv,
         LST_Day = LST_Day_1km,
         LST_Night = LST_Night_1km) %>%
  mutate(StableLights = NA,
         Country = "Peru") %>%
  group_by(Code) %>%
  arrange(Year, .by_group = T) %>%
  ungroup() %>%
  filter(Code %in% peru_munis_filter$Code) %>%
  dplyr::select(Code, Year, Country, Population, NDVI, EVI,
                LST_Day, min_LST_Day, max_LST_Day,
                LST_Night, min_LST_Night, max_LST_Night,
                StableLights, AvgRad, Precip,
                Evap_tavg, Qair_f_tavg, SoilMoi00_10cm_tavg, Wind_f_tavg,
                Tair_f_tavg, everything(), -contains('.x'), -contains('.y'))



## Brazil ##


## use to filter to munis of interest;
## future task might be to filter brazil shapefile starting from GEE
brazil_munis_filter <- read.csv("~/MacDonald-REU-Summer-22-1/models/data/aad.csv") %>%
  filter(Country == 'Brazil') %>%
  filter(!is.na(pland_forest)) %>%
  dplyr::select(Code) %>%
  unique()

processed_brazil_files <- list.files("~/peregrine_amazon/data/brazil/processed/final/annual/", full.names = T, pattern = "_annual")

sapply(sapply(processed_brazil_files, readRDS), summary)
sapply(sapply(processed_brazil_files, readRDS), dim)

full_brazil_env_vars <- data.frame(MuniCode = NA, Year = NA, Country = NA, stringsAsFactors = F)
for(file in processed_brazil_files){
  print(file)
  data <- readRDS(file)
  full_brazil_env_vars = full_join(full_brazil_env_vars, data, by = c('MuniCode', 'Year', 'Country'))
}

full_brazil_env_vars_v2 <- full_brazil_env_vars %>%
  filter(Year != 2022) %>%
  rename(Code = MuniCode,
         AvgRad = full_brazil_AvgRad_raw.csv,
         LST_Day = LST_Day_1km,
         LST_Night = LST_Night_1km) %>%
  mutate(StableLights = NA) %>%
  group_by(Code) %>%
  arrange(Year, .by_group = T) %>%
  ungroup() %>%
  filter(Code %in% brazil_munis_filter$Code) %>%
  dplyr::select(Code, Year, Country, Population, NDVI, EVI,
                LST_Day, min_LST_Day, max_LST_Day,
                LST_Night, min_LST_Night, max_LST_Night,
                StableLights, AvgRad, Precip,
                Evap_tavg, Qair_f_tavg, SoilMoi00_10cm_tavg, Wind_f_tavg,
                Tair_f_tavg, everything(), -contains('.x'), -contains('.y'))


## aad to merge ##
aad <- read.csv("~/MacDonald-REU-Summer-22-1/models/data/aad.csv") %>%
  select(-c(Muni_TotalArea, Chikungunya:Zika, Precip, Population, LST_Day, LST_Night, AvgRad, SWOccurrence, pland_forest:Nonveg_Riverlakeocean)) # add in updated LandscapeMetrics_v4

old_colombia <- aad %>%
  filter(Country == "Colombia")

old_brazil <- aad %>%
  filter(Country == "Brazil")

old_peru <- aad %>%
  filter(Country == "Peru")

# Combine Colombia
colombia_combined <- full_colombia_env_vars_v2 %>%
  left_join(old_colombia, by = c("Code", "Year", "Country",
                                 'NDVI', 'EVI',
                                 'StableLights')) %>% # Use fill()
  group_by(Country) %>%
  arrange(Code, Year) %>%
  group_by(Code, Year, Country) %>%
  fill(everything(), .direction = 'up') %>%
  unique() %>%
  filter(row_number() == 1) %>%
  ungroup()

# Combine Brazil
brazil_combined <- full_brazil_env_vars_v2 %>%
  full_join(old_brazil, by = c("Code", "Year", "Country",
                               'NDVI', 'EVI',
                               'StableLights')) %>% # Use fill()
  group_by(Country) %>%
  arrange(Code, Year) %>%
  group_by(Code, Year, Country) %>%
  fill(everything(), .direction = 'downup') %>%
  unique() %>%
  ungroup()

# Combine Peru
peru_combined <- full_peru_env_vars_v2 %>%
  full_join(old_peru, by = c("Code", "Year", "Country",
                             'NDVI', 'EVI',
                             'StableLights')) %>% # Use fill()
  group_by(Country) %>%
  arrange(Code, Year) %>%
  group_by(Code, Year, Country) %>%
  fill(everything(), .direction = 'up') %>%
  unique() %>%
  filter(row_number() == 1) %>%
  ungroup()

aad_v2 <- colombia_combined %>%
  rbind(brazil_combined) %>%
  rbind(peru_combined) %>%
  select(-Name)

# saveRDS(aad_v2, "~/peregrine_amazon/data/wrangling/aad_v2.rds")
# aad_v2 <- readRDS("~/peregrine_amazon/data/wrangling/aad_v2.rds")

##         ##
## Disease ##
##         ##

# 2001-2006 Brazil
old_brazil_CL <- read.csv("~/MacDonald-REU-Summer-22-1/models/data/aad.csv") %>%
  filter(Country=='Brazil',
         !is.na(Cutaneous.Leishmaniasis),
         Year < 2007) %>%
  select(Code, CL=Cutaneous.Leishmaniasis, Year)

brazil_incidence <- readRDS("~/peregrine_amazon/data/brazil/disease/use/brazil_CL_incidence_annual.rds") %>% # 2007-2020 Brazil
  select(-Cases, -Population) %>%
  filter(!is.na(CL)) %>%
  rbind(old_brazil_CL)

colombia_incidence <- readRDS("~/peregrine_amazon/data/colombia/disease/use/colombia_CL_incidence_annual.rds") %>%
  select(-Name, -Country, -Population)

# 2010-2015 Peru
old_peru_CL <- read.csv("~/MacDonald-REU-Summer-22-1/models/data/aad.csv") %>%
  filter(Country=='Peru',
         !is.na(Cutaneous.Leishmaniasis),
         Year < 2016) %>%
  select(Code, CL=Cutaneous.Leishmaniasis, Year)

peru_incidence <- readRDS("~/peregrine_amazon/data/peru/disease/peru_cutleish_df_annual_2016_2020.rds") %>% # 2016-2020 Peru
  select(-Country, -Name, -Population) %>%
  filter(Year >= 2016) %>%
  rbind(old_peru_CL) %>%
  filter(Code %in% as.integer(read_sf("~/peregrine_amazon/data/peru/shapefiles/Final_Peru_Amazonian_Municipios_Projected.shp")$IDDIST)) #filter out municipalities not in final munis

all_incidence <- brazil_incidence %>%
  rbind(colombia_incidence) %>%
  rbind(peru_incidence)

# saveRDS(all_incidence, "~/peregrine_amazon/data/wrangling/all_incidence.rds")

# precipitation superlatives
brazil_precip_super <- readRDS("~/peregrine_amazon/data/brazil/processed/final/monthly/full_brazil_precip_00_daily_processed_monthly")
colombia_precip_super <- readRDS("~/peregrine_amazon/data/colombia/processed/final/monthly/full_colombia_precip_00_daily_processed_monthly")
peru_precip_super <- readRDS("~/peregrine_amazon/data/peru/processed/final/monthly/full_peru_precip_00_daily_processed_monthly")


all_precip_super <- brazil_precip_super %>%
  rbind(colombia_precip_super) %>%
  rbind(peru_precip_super) %>%
  group_by(Year, MuniCode, Country) %>%
  filter(Precip == max(Precip) | Precip == min(Precip)) %>%
  arrange(Precip, .by_group = T) %>%
  summarise(min_Precip = min(Precip),
            max_Precip = max(Precip),
            min_Precip_Month = Month[1],
            max_Precip_Month = Month[2]) %>%
  ungroup() %>%
  rename(Code = MuniCode)

# saveRDS(all_precip_super, "~/peregrine_amazon/data/wrangling/all_precip_super.rds")

# Standing water occurrence
SWO <- read.csv("~/peregrine_amazon/data/SWOccurrence/SWOccurrence.csv") %>%
  select(-c(system.index, .geo, max_extent, transition))

# create names dictionary
aad_names <- aad %>%
  select(Name, Code, Country) %>%
  unique() %>%
  mutate(Name = str_to_sentence(Name, locale='es'))

# saveRDS(aad_names, "~/peregrine_amazon/data/wrangling/aad_names.rds")
aad_names <- readRDS("~/peregrine_amazon/data/wrangling/aad_names.rds")

#### completed in grit server ####

aad_v2 <- readRDS("~/peregrine_amazon/data/wrangling/aad_v2.rds")
HNTL <- readRDS("~/peregrine_amazon/data/wrangling/HNTL.rds")
all_incidence <- readRDS("~/peregrine_amazon/data/wrangling/all_incidence.rds")
all_precip_super <- readRDS("~/peregrine_amazon/data/wrangling/all_precip_super.rds")
LandscapeMetrics_v4_classmetrics <- read.csv("~/peregrine_amazon/data/wrangling/LandscapeMetrics_v4_classmetrics.csv") %>%
  select(-c(X.NA.)) %>%
  mutate(Muni_ID = ifelse(Country=='Peru', substr(Muni_ID, 3, 8), Muni_ID) %>%
           as.integer())
LandscapeMetrics_v4_adjacency_matrix <- read.csv("~/peregrine_amazon/data/wrangling/adjacencymatrix_v2.csv") %>%
  mutate(Muni_ID = ifelse(Country=='Peru', substr(Muni_ID, 3, 8), Muni_ID) %>%
           as.integer())
SWO <- read.csv("~/peregrine_amazon/data/SWOccurrence/SWOccurrence.csv") %>%
  select(-c(system.index, .geo, max_extent, transition))

aad_v3 <- aad_v2 %>%
  right_join(aad_names, by = c("Code", "Country")) %>% # join with names dictionary, keeping only rows from dictionary
  left_join(HNTL, by = c("Code" = "MuniCode", "Year")) %>% # join with HNTL
  full_join(all_incidence, by = c("Code", "Year")) %>% #clean this 4/19
  full_join(all_precip_super, by = c("Code", "Year", "Country")) %>%
  full_join(LandscapeMetrics_v4_classmetrics, by = c("Code"="Muni_ID", "Country", "Year")) %>% # merge with LandscapeMetrics_v4 dfs
  full_join(LandscapeMetrics_v4_adjacency_matrix, by = c("Code"="Muni_ID", "Country", "Year")) %>%
  full_join(SWO, by=c('Code'='MuniCode')) %>%
  mutate(Code = as.integer(Code)) %>%
  rename(SWOccurrence = occurrence,
         SWSeasonality = seasonality,
         SWRecurrence = recurrence,
         SWChange_abs = change_abs,
         SWChange_norm = change_norm) %>%
  # group_by(Code, Name, Country) %>%
  # fill(CL, .direction = 'up') %>%
  # unique() %>%
  # ungroup() %>%
  # mutate(CL = ifelse(!is.na(CL), CL, Cutaneous.Leishmaniasis)) %>%
  select(Code, Name, Country, Year, CL, NDVI, EVI,
         LST_Day, min_LST_Day, max_LST_Day,
         LST_Night, min_LST_Night, max_LST_Night,
         HNTL, StableLights, AvgRad, Precip,
         Evap_tavg, Qair_f_tavg, SoilMoi00_10cm_tavg, Wind_f_tavg,
         Tair_f_tavg, everything())

# saveRDS(aad_v3, "~/peregrine_amazon/data/annual/aad_2021_full.rds")

aad_v3 <- readRDS("~/peregrine_amazon/data/annual/aad_2021_full.rds") # dim: 156256    488

# merge with spatial folds data set from folds_wrangling.R
folds <- readRDS("~/peregrine_amazon/Restructured021623/binary_classification/random_forest/model_data/st/1/spatial_leave_location_out_cv_k3_hclust_data_splits_dict.rds")

aad_v4 <- aad_v3 %>%
  full_join(folds, by = c("Code", "Year")) %>%
  # add feature-engineered variables
  # Proportion of land covered by forest
  rename(forest_density = pland_forest) %>%
  mutate(
    # Combines these two measures of fragmentation into a single measure
    forest_fragmentation = enn_mn_forest * area_mn_forest,
    # Change in proportion of land covered by forest from the previous year
    land_use_change = forest_density - lag(forest_density),
    # Change in total edge of forest from the previous year
    edge_loss = te_forest - lag(te_forest)
  ) %>%
  # filter(!is.na(CL)) %>%
  mutate(Country = as.factor(Country)) %>%
  filter(!is.na(forest_density)) %>% # subset to rows that have forest variables recorded
  unique()

# saveRDS(aad_v4, "~/peregrine_amazon/data/wrangling/aad_v4.rds") #dim: 47744   492

# Get lat, long, and other forest variables
## Get geometries of municipalities
brazil_shp <- read_sf("~/peregrine_amazon/data/brazil/shapefiles/Final_Brazil_Amazonian_Municipios_Projected.shp") %>%
  select(Code = codigo_ibg, geometry)
colombia_shp <- read_sf("~/peregrine_amazon/data/colombia/shapefiles/Final_Colombia_Amazonian_Municipios_Projected.shp") %>%
  select(Code = MPIOS, geometry) %>%
  st_transform(st_crs(brazil_shp))
peru_shp <- read_sf("~/peregrine_amazon/data/peru/shapefiles/Final_Peru_Amazonian_Municipios_Projected.shp") %>%
  select(Code = IDDIST, geometry)

all_shp <- brazil_shp %>%
  rbind(colombia_shp) %>%
  rbind(peru_shp) %>%
  mutate(Code = as.integer(Code))

aad_v5 <- aad_v4 %>%
  left_join(all_shp, by = 'Code') %>%
  mutate(centroids = st_centroid(geometry),
         long = st_coordinates(centroids)[,1],
         lat = st_coordinates(centroids)[,2]) %>%
  select(-centroids) %>%
  filter(!is.na(CL)) %>%
  mutate(Country = as.factor(Country)) %>%
  st_set_geometry(.$geometry)

saveRDS(aad_v5, "~/peregrine_amazon/data/annual/aad_2021_forests_geom.rds")  # dim: 26213   495


aad_v6 <- aad_v5 %>%
  st_drop_geometry()

saveRDS(aad_v6, "~/peregrine_amazon/data/annual/aad_2021_forests.rds")  # dim: 26213   494
write.csv(aad_v6, "~/peregrine_amazon/data/annual/aad_2021_forests.csv")


## Missing

naniar::miss_var_summary(aad_v6)

naniar::miss_var_summary(aad_v6 %>% filter(is.na(Forest_Forest)))

naniar::gg_miss_var(aad_v6 %>%
                      select(Country, Year, CL))

naniar::gg_miss_var(aad_v5 %>%
                      dplyr::select(c(1:10)),
                    facet = Year)

naniar::gg_miss_var(aad_v5 %>%
                      dplyr::select(c(Year, 11:20)),
                    facet = Year)

naniar::gg_miss_var(aad_v5 %>%
                      dplyr::select(c(Year, 21:30)),
                    facet = Year)

naniar::gg_miss_var(aad_v5 %>%
                      dplyr::select(c(Year, 31:40)),
                    facet = Year)

naniar::gg_miss_var(aad_v5 %>%
                      dplyr::select(c(Year, 41:50)),
                    facet = Year)
# missing population

naniar::gg_miss_var(aad_v5 %>%
                      dplyr::select(c(Year, 51:60)),
                    facet = Year)

# missing some SWOccurrence (and all of 2020-2021)

naniar::gg_miss_var(aad_v5 %>%
                      dplyr::select(c(Year, 61:70)),
                    facet = Year)

naniar::gg_miss_var(aad_v5 %>%
                      dplyr::select(c(Year, 71:80)),
                    facet = Year)

naniar::gg_miss_var(aad_v5 %>%
                      dplyr::select(c(Year, 81:91)),
                    facet = Year)

naniar::gg_miss_var(aad_v5 %>%
                      dplyr::select(c(Year, 92:102)),
                    facet = Year)

