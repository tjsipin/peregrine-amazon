library(dplyr)
library(tidyverse)
library(sf)
# install.packages("xtable")
library("xtable") # for making html tables
# install.packages('bannerCommenter')
library(bannerCommenter)


# Tasks:
  # get existing annual data and sum up monthly incidence for 2020
  # add new 2020 data, as well as new data (FLDAS) for every year

annual_v1 <- read.csv("~/MacDonald-REU-Summer-22-1/models/data/aad.csv")

colombia_annual_incidence_v4 <- read.csv("~/peregrine_amazon/data/colombia/Incidence_v4/colombia_annual_incidence.csv") %>%
  pivot_wider(id_cols = c("Muni_Code", "Muni_Name", "Year", "Country", "Population"),
              names_from = "Disease",
              values_from = "Incidence")

colombia <- annual_v1 %>%
  filter(Country == 'Colombia')

old_mun_colombia <- read_sf("~/peregrine_amazon/data/colombia/shapefiles/Final_Colombia_Amazonian_Municipios_Projected.shp")

colombia <- read_sf("~/peregrine_amazon/data/colombia/shapefiles/MGN_MPIO_POLITICO_2018_EPSG3857.shp") %>%
  dplyr::rename(Code = MPIO_CCNCT) %>%
  dplyr::filter(Code %in% old_mun_colombia$MPIOS) %>%
  mutate(Code = as.numeric(Code))

write_sf(colombia, "~/peregrine_amazon/data/colombia/shapefiles/Final_Colombia_Amazonian_Municipios_Projected_v2.shp")

old_mun_peru <- read_sf("~/peregrine_amazon/data/peru/shapefiles/Final_Peru_Amazonian_Municipios_Projected.shp") %>%
  rename(Code = IDDIST) %>%
  mutate(Code = as.numeric(Code))

old_mun_brazil <- read_sf("~/peregrine_amazon/data/brazil/shapefiles/Final_Brazil_Amazonian_Municipios_Projected.shp") %>%
  rename(Code = codigo_ibg) %>%
  mutate(Code = as.numeric(Code))

old_muns <- c(old_mun_colombia$MPIOS, old_mun_peru$IDDIST, old_mun_brazil$codigo_ibg)

annual_v2 <- annual_v1 %>%
  filter(Code %in% old_muns)

saveRDS(annual_v2, "~/peregrine_amazon/Restructured021623/data/annual/aad_v2")

missing_cases <- annual_v2 %>%
  group_by(Year, Country) %>%
  summarise(missing_CL = is.na(Cutaneous.Leishmaniasis) %>% sum(),
            missing_malaria = is.na(Malaria) %>% sum()) %>%
  ungroup() %>%
  pivot_wider(id_cols = 'Year',
              names_from = 'Country',
              values_from = c('missing_CL'))

saveRDS(missing_cases, "~/peregrine_amazon/Restructured021623/documents/supplementary/missing_cases_v1")


#     Year Brazil Colombia  Peru

# 1   2000    935      149   494
# 2   2001      0      149   494
# 3   2002      0      149   494
# 4   2003      0      149   494
# 5   2004      0      149   494
# 6   2005      0      149   494
# 7   2006      0      149   494
# 8   2007      0        0   494
# 9   2008      0        0   494
# 10  2009      0        0   494
# 11  2010      0        0     0
# 12  2011      0        0     0
# 13  2012      0        0     0
# 14  2013      0        0     0
# 15  2014      0        0     0
# 16  2015      0        0     0
# 17  2016      0        0     0
# 18  2017      0        0     0
# 19  2018      0      149     0
# 20  2019    935      149     0

#### See if we can get the data for CL for
#### Brazil in 2000, 2019, 2020;
#### Colombia in 2000-2006, 2018-2020;
#### Peru for 2000-2009, 2020

#### It seems like there is a hard limit for reporting data by country.
#### May need to work with what we have.
#### This might mean going by country or only 2010 onward if we use all countries.


####################
####################
###              ###
###  RANKING CL  ###
###              ###
####################
####################

rank_aad <- annual_v2 %>%
  select(Code, Year, Country, Cutaneous.Leishmaniasis, Muni_TotalArea) %>%
  group_by(Year, Country) %>%
  mutate(CL_percentile_byCountryYear = percent_rank(Cutaneous.Leishmaniasis),
         Area_percentile_byCountryYear = percent_rank(Muni_TotalArea)) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(CL_percentile_byYear = percent_rank(Cutaneous.Leishmaniasis),
         Area_percentile_byYear = percent_rank(Muni_TotalArea)) %>%
  ungroup()

saveRDS(rank_aad, "~/peregrine_amazon/Restructured021623/documents/supplementary/rank_aad")


#################################################################
##                      Simulating Points                      ##
#################################################################

simulating_n_points <- rank_aad %>%
  group_by(Year, Country, Code) %>%
  mutate(n_points = (floor(3*Area_percentile_byCountryYear + 2))) %>%
  ungroup() %>%
  filter(!is.na(n_points))

simulating_n_points_long <- simulating_n_points[rep(seq_len(nrow(simulating_n_points)), simulating_n_points$n_points),]

# epsilon
epsilon_v2 = 0.5

simulating_n_points_v2 <- simulating_n_points_long %>%
  filter(!is.na(CL_percentile_byCountryYear)) %>%
  group_by(Country, Year) %>%
  mutate(occurrence_error = abs(CL_percentile_byCountryYear - rnorm(1, mean = 0, sd = 0.1))) %>%
  mutate(occurrence = rbinom(1, 1, occurrence_error), .after = CL_percentile_byCountryYear) %>%
  ungroup()

epsilon_v3 = 0.9

simulating_n_points_v3 <- simulating_n_points_long %>%
  filter(!is.na(CL_percentile_byCountryYear)) %>%
  mutate(occurrence_error = abs(CL_percentile_byCountryYear - rnorm(1, mean = 0, sd = 0.1))) %>%
  mutate(occurrence = rbinom(1, 1, occurrence_error), .after = CL_percentile_byCountryYear)

# having some trouble with this code; due to seed, not random #


occurrence_vector <- rbinom(nrow(simulating_n_points_long), size = 1, prob = abs(simulating_n_points_long$CL_percentile_byCountryYear - rnorm(1, mean = 0.0001, sd = 0.00000001)))

simulating_n_points_v4 <- simulating_n_points_long %>%
  cbind(occurrence_vector) %>%
  select(Code, Year, Country, Cutaneous.Leishmaniasis, CL_percentile_byCountryYear, occurrence_vector, everything())





#################################################################
##             Merge n_points data with shapefiles             ##
#################################################################

colombia_points <- simulating_n_points_v4 %>%
  filter(Country == 'Colombia') %>%
  select(Code, Year, n_points)

colombia_w_points <- colombia %>%
  full_join(colombia_points, by = 'Code')

brazil_points <- simulating_n_points_v4 %>%
  filter(Country == 'Brazil') %>%
  select(Code, Year, n_points)

brazil_w_points <- old_mun_brazil %>%
  full_join(brazil_points, by = 'Code')

peru_points <- simulating_n_points_v4 %>%
  filter(Country == 'Peru') %>%
  select(Code, Year, n_points)

peru_w_points <- old_mun_peru %>%
  full_join(peru_points, by = 'Code')

write_sf(colombia_w_points, "~/peregrine_amazon/Restructured021623/data/shapefiles/colombia_w_points.shp")
write_sf(brazil_w_points, "~/peregrine_amazon/Restructured021623/data/shapefiles/brazil_w_points.shp")
write_sf(peru_w_points, "~/peregrine_amazon/Restructured021623/data/shapefiles/peru_w_points.shp")


colombia_points_v2 <- simulating_n_points %>%
  full_join(colombia) %>%
  filter(Country == 'Colombia',
         Year == 2000) %>%
  select(Code, n_points, geometry)

write_sf(colombia_points_v2, "~/peregrine_amazon/Restructured021623/data/shapefiles/colombia_w_points_v2.shp") # test on this first in GEE
## Get other countries later
