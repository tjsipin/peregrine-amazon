##################################################################
##                           Packages                           ##
##################################################################
library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(tidyr)
library(stringr)

# existing data
aad <- readRDS("~/peregrine_amazon/Restructured021623/data/annual/incidence") %>%
  select(-c(Chikungunya, Dengue:Zika, OptTemp_Obs:Malaria_OptTemp)) %>%
  select(Code, Year, Country, Name, has_CL, Cutaneous.Leishmaniasis, everything())

old_aad <- read.csv("~/MacDonald-REU-Summer-22-1/models/data/aad.csv")

##################################################################
##                            Brazil                            ##
##################################################################

brazil_c_leish_cleaner_func <- function(data, year){
  if("Ign.Em.Branco" %in% names(data)) {
    data <- data %>%
      select(-Ign.Em.Branco)
  }
  res <- data %>%
    mutate(Code = str_sub(Município.Infecção., start = 1, end = 6),
           Muni_Name = str_sub(Município.Infecção., start = 8, -1)) %>%
    select(-Total) %>%
    gather(Month, Cases, Jan:Dez) %>%
    mutate(Year=year, #Add year column
           Cases=gsub("-", 0, Cases), #Substitute dashes for 0
           Cases=as.numeric(Cases)) %>%
    select(-Cases, everything(), -Município.Infecção.) %>%
    filter(!is.na(Cases), # Gets rid of extra rows (with text)
           Code != "Total") # Gets rid of "Total" row


}
years <- 2007:2021
brazil_cutleish_df <- data.frame()

for (year in years){
  print(year)
  raw_data <- read.csv(paste0("~/peregrine_amazon/data/brazil/disease/brazil_cleish_", year, ".csv"), skip = 3, encoding = 'latin1', sep = ';')
  cleaned_data <- brazil_c_leish_cleaner_func(raw_data, year)
  print(names(cleaned_data))
  brazil_cutleish_df <- rbind(brazil_cleish_df, cleaned_data)
}

# Population
brazil_population <- readRDS("~/peregrine_amazon/data/brazil/processed/full_brazil_Population")

# Get names
brazil_names <- aad %>%
  filter(Country == "Brazil") %>%
  select(Name, Code)

brazil_population_v2 <- brazil_population %>%
  full_join(aad %>% select(Name, Code), by = "Code")

# brazil_cutleish_df_yearly <- brazil_cutleish_df %>%
#   group_by(Code, Muni_Name, Year) %>%
#   summarise(Cases = sum(Cases)) %>%
#   ungroup() %>%
#   rename(Name = Muni_Name) %>%
#   filter(Code %in% intersect(aad$Code, Code)) %>%
#   full_join(aad %>% select(Code, Country) %>% mutate(Code = as.character(Code)), by = "Code") %>%
#   full_join(brazil_population_v2, by = c("Name", "Year")) %>%
#   mutate(CL = Cases / Population * 1000,
#          Country.x = as.factor(Country.x),
#          Country.y = as.factor(Country.y)) %>%
#   select(-Cases)

brazil_cutleish_df_yearly <- brazil_cutleish_df %>%
  group_by(Code, Muni_Name, Year) %>%
  summarise(Cases = sum(Cases)) %>%
  ungroup() %>%
  rename(Name = Muni_Name) %>%
  # mutate(Code = as.integer(Code)) %>%
  select(-Code) %>%
  inner_join(brazil_population_v2, by = c("Name", "Year")) %>%
  mutate(CL = Cases / Population * 1000) %>%
  select(-Cases) %>%
  unique()

## Ready to merge with all other countries ^^





##################################################################
##                             Peru                             ##
##################################################################

peru_raw <- read.csv("~/peregrine_amazon/data/peru/disease/Cut_Leish_all_2016_2022_v2.csv", encoding = 'latin1')

peru_selected <- peru_raw[, c(3, 4, 8, 10, 11, 12, 13)]

peru_selected_v2 <- peru_selected %>%
  rename(Year = Año,
         Week = Semana,
         Code = Ubigeo,
         Accumulated_Incidence = Incidencia.acumulada.,
         Weekly_Incidence = Incidencia.semanal) %>%
  group_by(Year, Code) %>%
  mutate(Max_Incidence = max(Accumulated_Incidence)) %>%
  arrange(Week, .by_group = T) %>%
  mutate(max_week = max(Week)) %>%
  ungroup()

peru_old_muni <- read_sf("~/peregrine_amazon/data/peru/shapefiles/Final_Peru_Amazonian_Municipios_Projected.shp")


# Disease reporting in incidence already, no need to convert from cases and population
peru_cutleish_df <- peru_selected_v2 %>%
  filter(Week == 53) %>%
  select(-Week, -max_week, -Weekly_Incidence) %>%
  filter(Code %in% intersect(Code, peru_old_muni$IDDIST)) %>%
  rename(Name = Distrito,
         CL = Accumulated_Incidence) %>%
  select(-Max_Incidence, -Departamento)

# Peru population
peru_population <- readRDS("~/peregrine_amazon/data/peru/processed/full_peru_Population")

peru_cutleish_df_yearly <- peru_cutleish_df %>%
  inner_join(peru_population, by = c('Code', 'Year')) %>%
  mutate(CL = CL / 100) # needed to be in-line with the scaling of the original data set


peru_diff_check <- aad %>%
  filter(Country == 'Peru') %>%
  select(Code, Year, Cutaneous.Leishmaniasis, Population, Name) %>%
  mutate(Cutaneous.Leishmaniasis_ = Cutaneous.Leishmaniasis * Population) %>%
  full_join(peru_cutleish_df_yearly,
            by = c("Code", "Year")) %>%
  mutate(diff = Cutaneous.Leishmaniasis - CL,
         diff3 = log(diff + 1))

summary(peru_diff_check$diff)

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# -7.403  -0.004   0.036   0.063   0.131   4.306    9756



ggplot(peru_diff_check) +
  geom_density(aes(diff)) # This looks acceptable, but it's odd how there are differing reportings for each municipio


##################################################################
##                           Colombia                           ##
##################################################################

full_colombia_annual <- read.csv("~/peregrine_amazon/data/colombia/Incidence_v4/colombia_annual_incidence.csv")
colombia_population <- readRDS("~/peregrine_amazon/data/colombia/processed/full_colombia_Population")

colombia_cutleish_df_yearly <- full_colombia_annual %>%
  rename(Code = Muni_Code,
         Name = Muni_Name) %>%
  filter(Disease == "Cutaneous Leishmaniasis",
         Code %in% intersect(aad_new$Code, full_colombia_annual$Muni_Code)) %>%
  select(Code, Name, Year, Country, Cases) %>%
  rename(CL_cases = Cases) %>%
  mutate(Country = 'Colombia') %>%
  inner_join(colombia_population %>%
               select(Year, Code, Population)) %>%
  mutate(CL = CL_cases/Population * 1000) %>% # fits aad
  select(-CL_cases)




##################################################################
##                      Bring all together                      ##
##################################################################

full_CL <- brazil_cutleish_df_yearly %>%
  rbind(peru_cutleish_df_yearly) %>%
  rbind(colombia_cutleish_df_yearly)

brazil_CL_2019_2020 <- brazil_cutleish_df_yearly %>%
  filter(Year %in% c(2019, 2020))

peru_CL_2020 <- peru_cutleish_df_yearly %>%
  filter(Year == 2020)

brazil_CL_2019_2020_df <- data.frame(
  names(aad),
  NA. = NA
) %>% pivot_wider(names_from = names.aad.,
                  values_from = NA.) %>%
  select(-c(Cutaneous.Leishmaniasis)) %>%
  mutate(CL = NA) %>%
  select(Code, Year, Country, Name, Population, CL, everything()) %>%
  full_join(brazil_CL_2019_2020) %>%
  mutate(Population = ifelse(Year == 2019, NA, Population)) %>%
  unique() # needed otherwise there are duplicates

aad_2020_brazil <- aad %>%
  rename(CL = Cutaneous.Leishmaniasis) %>%
  filter(Country == "Brazil") %>%
  filter(Code %in% brazil_CL_2019_2020$Code) %>%
  select(Code, Year, Name, Country, has_CL, CL, Population, everything()) %>%
  full_join(brazil_CL_2019_2020 %>%
              # Use as a marker for fill() function later since 2019 has environmental vars, but not CL
              mutate(Population = ifelse(Year == 2019, NA, Population)),
            by = c("Code", "Year", "Name", "Country", "CL", "Population")) %>%
  group_by(Country, Code) %>%
  arrange(Year,.by_group = T) %>%
  ungroup() %>%
  group_by(Code, Year) %>%
  fill(CL, .direction = "up") %>% # fill in missing CL for 2019
  unique() %>%
  filter(row_number() == 1) %>% # keep the first "full" one in the muni-year group
  ungroup() %>%
  mutate(Name = str_to_sentence(Name)) # give all muni names the same case format



aad_2020_peru <- aad %>%
  rename(CL = Cutaneous.Leishmaniasis) %>%
  filter(Country == "Peru") %>%
  filter(Code %in% peru_CL_2020$Code) %>%
  select(Code, Year, Name, Country, has_CL, CL, Population, everything()) %>%
  full_join(peru_CL_2020,
            by = c("Code", "Year", "Country", "CL", "Population", "Name")) %>%
  group_by(Country, Code) %>%
  arrange(Year,.by_group = T) %>%
  ungroup() %>%
  mutate(Name = str_to_sentence(Name)) # give all muni names the same case format


# Need to get environmental values for new Colombia

