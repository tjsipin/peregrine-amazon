usethis::edit_r_environ(scope = 'project')
path.expand("~")
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

cleanerFunc <- function(data){
  cleaned <- data %>%
    select(-c(system.index)) %>%
    mutate(Year = substring(date, 1, 4) %>% as.integer(),
           Month = substring(date, 6, 7) %>% as.integer(),
           Day = substring(date, 9, 10) %>% as.integer())
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

cleanerFunc_precip <- function(data){
  cleaned <- data %>%
    dplyr::select(-c(system.index, .geo)) %>%
    mutate(Year = substring(date, 1, 4) %>% as.integer(),
           Month = substring(date, 6, 7) %>% as.integer(),
           Day = substring(date, 9, 10) %>% as.integer())

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


FLDAS <- read.csv("~/peregrine_amazon/data/colombia/full/full_colombia_FLDAS.csv") %>%
  cleanerFunc()
HNTL <- read.csv("~/peregrine_amazon/data/colombia/full/full_colombia_HNTL.csv") %>%
  cleanerFunc()

Precip <- read.csv("~/peregrine_amazon/data/colombia/full/full_colombia_Precip (1).csv") %>%
  cleanerFunc_precip()


data <- read.csv("~/peregrine_amazon/data/monthly_df.csv") %>%
  filter(Country == 'Colombia')

full_data <- data %>%
  full_join(FLDAS %>% rename(Code = MuniCode), by = c('Code', 'Month', 'Year', 'Country'))

full_data_v2 <- full_data %>%
  full_join(HNTL %>% rename(Code = MuniCode,
                            HNTL = mean),
            by = c('Code', 'Year', 'Country')) %>%
  filter(Year > 1999) %>%
  select(-c(contains('geo')))

full_data_v3 <- full_data_v2[,c(1:30, 99, 71:98, 31:70)] # contains all munis in Colombia (even outside the Amazon)

write.csv(full_data_v3, "~/peregrine_amazon/data/full_colombia_monthly_df.csv")


# Each file has its own number of municipios captured. There should only be about 110.
# Find the intersection of the municipios present in each file and keep only those in the next stage.
# The next stage consists of finding the intersection of the municipios present in this data set and the
# municipios present in the LandscapeMetrics_v4 data set.

colombia <- AvgRad %>%
  full_join(FLDAS, by = c('MuniCode', 'Month', 'Year', 'Country'))
colombia <- colombia %>%
  full_join(HNTL, by = c('MuniCode', 'Year', 'Country'))
colombia <- colombia %>%
  full_join(LST, by = c('MuniCode', 'Month', 'Year', 'Country'))
colombia <- colombia %>%
  full_join(modis, by = c('MuniCode', 'Month', 'Year', 'Country')) ## limit
colombia <- colombia %>%
  full_join(PP, by = c('MuniCode', 'Month', 'Year', 'Country'))
colombia <- colombia %>%
  full_join(Precip, by = c('MuniCode', 'Month', 'Year', 'Country'))
colombia <- colombia %>%
  full_join(StableLights, by = c('MuniCode', 'Year', 'Country'))
colombia <- colombia %>%
  full_join(SWOccurrence, by = c('MuniCode', 'Year', 'Country'))
colombia <- colombia %>%
  full_join(Incidence, by = c('MuniCode', 'Year', 'Month', 'Country'))

population <- colombia %>%
  select(MuniCode, Year, Month, Country, contains('Population_per')) %>%
  group_by(MuniCode, Year, Month, Country) %>%
  tidyr::pivot_longer(cols = contains('Population')) %>%
  group_by(MuniCode, Year, Month, Country) %>%
  filter(!is.na(value)) %>%
  summarise(Population_density = mean(value))

colombia <- colombia %>%
  select(-contains('Population_per')) %>%
  full_join(population, by = c('MuniCode', 'Year', 'Month', 'Country'))

colombia <- colombia %>%
  select(Year, Month, MuniCode, Muni_Name, Country, Total_Population, Population_density,
         HNTL, EVI, NDVI, Gpp, PsnNet, Psn_QC, precipitation, avg_rad,
         stable_lights, occurrence, everything())

# write.csv(colombia, '~/peregrine_amazon/data/colombia/colombia_monthly.csv')

colombia <- read.csv('~/peregrine_amazon/data/colombia/colombia_monthly.csv')







######################################
### Comparisons ###

# StableLights_full <- read.csv("~/peregrine_amazon/data/colombia/full/full_colombia_StableLights.csv") %>%
#   cleanerFunc()
#
# StableLights_old <- read.csv("~/peregrine_amazon/data/monthly_df.csv") %>%
#   select(Code, StableLights, Month, Year, Country) %>%
#   filter(Year %in% StableLights_full$Year,
#          Country == 'Colombia')
#
# StableLights_old_v2 <- StableLights_old %>%
#   filter(!is.na(StableLights)) %>%
#   filter(Code %in% StableLights_full$MuniCode) %>%
#   rename(MuniCode = Code)
#
# StableLights_compare <- StableLights_full %>%
#   full_join(StableLights_old_v2, by = c('Country', 'Year', 'MuniCode')) %>%
#   mutate(diff = mean - StableLights) %>%
#   filter(!is.na(diff))
#
Precip_old <- read.csv("~/peregrine_amazon/data/monthly_df.csv") %>%
  select(Code, Precip, Month, Year, Country)
#
# Precip_new <- read.csv("~/peregrine_amazon/data/colombia/full/full_colombia_Precip.csv") %>%
#   cleanerFunc()
#

Precip_new <- Precip

Precip_new %>% filter(MuniCode == 5001) %>% View()

Precip_old_v2 <- Precip_old %>%
  filter(Year %in% Precip_new$Year,
         Country == 'Colombia') %>%
  dplyr::rename(MuniCode = Code) %>%
  filter(MuniCode %in% Precip_new$MuniCode)

Precip_compare <- Precip_old_v2 %>%
  full_join(Precip_new, by = c('Country', 'Year', 'Month', 'MuniCode')) %>%
  mutate(diff = mean - Precip) %>%
  filter(!is.na(diff))


Precip_compare %>% summary()
