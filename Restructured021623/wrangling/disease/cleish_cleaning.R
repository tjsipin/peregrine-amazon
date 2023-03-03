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

brazil_cutleish_df_yearly <- brazil_cutleish_df %>%
  # mutate(Muni_Name = toupper(Muni_Name)) %>%
  group_by(Code, Muni_Name, Year) %>%
  summarise(Cases = sum(Cases)) %>%
  ungroup()

brazil_diff_check <- old_aad %>%
  # filter(Country == 'Brazil') %>%
  filter(Name %in% brazil_cutleish_df_yearly$Muni_Name,
         Year > 2006) %>%
  select(Code, Year, Cutaneous.Leishmaniasis, Population, Name) %>% # Name = District name, not muni
  mutate(Code = as.character(Code)) %>%
  rename(Muni_Name = Name) %>%
  full_join(brazil_cutleish_df_yearly,
            by = c("Year", "Muni_Name")) %>%
  mutate(CL = Cases/Population * 1000,
         Cutaneous.Leishmaniasis_ = Cutaneous.Leishmaniasis * Population,
         diff = Cutaneous.Leishmaniasis - CL)

brazil_diff_check$diff %>% summary() # acceptable

brazil_diff_check %>%
  filter(diff > 1)

brazil_diff_check %>% filter(!is.na(Muni_Name.x)) %>% head()

brazil_diff_check %>% filter(!is.na(Muni_Name.y)) %>% head()

brazil_diff_check_v2 <- old_aad %>%
  mutate(Code = Code %>% as.character() %>% str_sub(start = 1, end = -2)) %>%
  filter(Code %in% brazil_cutleish_df_yearly$Code,
         Year > 2006) %>%
  select(Code, Year, Cutaneous.Leishmaniasis, Population, Name) %>% # Name = District name, not muni
  mutate(Code = as.character(Code)) %>%
  rename(Muni_Name = Name) %>%
  full_join(brazil_cutleish_df_yearly,
            by = c("Year", "Code")) %>%
  mutate(CL = Cases/Population * 1000,
         Cutaneous.Leishmaniasis_ = Cutaneous.Leishmaniasis * Population/1000,
         diff = Cutaneous.Leishmaniasis - CL,
         cases_diff = Cases - Cutaneous.Leishmaniasis_) %>%
  filter(Year < 2019)

brazil_diff_check_v2$diff %>% summary()

#### Merge Brazil disease data
brazil_2021_disease <- aad %>%
  full_join(brazil_cutleish_df_yearly %>%
              rename(Name = Muni_Name) %>%
              filter(Name %in% aad$Name,
                     Year > 2018) %>%
              mutate(Country = "Brazil"),
            by = c("Year", "Name", "Country")) %>%
  select(-Code.y) %>%
  rename(Code = Code.x) %>%
  mutate(Cutaneous.Leishmaniasis = case_when(Year < 2019 ~ Cutaneous.Leishmaniasis,
                                             Year >= 2019 ~ Cases/Population * 1000)) %>%
  select(-Cases)

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

peru_cutleish_df <- peru_selected_v2 %>%
  filter(Week == 53) %>%
  select(-Week, -max_week, -Weekly_Incidence) %>%
  filter(Code %in% peru_old_muni$IDDIST)


sapply(sapply(peru_raw, FUN = unique), FUN = length)
peru_cutleish_df$Code %>% unique() %>% length() # 187

###### Check to see how many unique Peru codes in annual data

peru_aad_check <- aad %>%
  filter(!is.na(Cutaneous.Leishmaniasis),
         Country == 'Peru')

peru_aad_check$Code %>% unique() %>% length() #494

##### Need to find where other munis went


peru_cutleish_df_v2 <- peru_selected_v2 %>%
  filter(Week == 53) %>%
  select(-Week, -max_week, -Incidence, -Weekly_Incidence) %>%
  filter(Code %in% peru_aad_check$Code) %>%
  mutate(Distri)

## still 187


old_aad_peru_check <- old_aad %>%
  filter(!is.na(Cutaneous.Leishmaniasis),
         Country == "Peru")

old_aad_peru_check$Code %>% unique() %>% length() #1873

peru_diff_check <- aad %>%
  filter(Country == 'Peru') %>%
  # filter(Code == 100101,
  #        Year %in% c(2016:2019)) %>%
  select(Code, Year, Cutaneous.Leishmaniasis, Population, Name) %>%
  mutate(Cutaneous.Leishmaniasis_ = Cutaneous.Leishmaniasis * Population) %>%
  full_join(peru_cutleish_df %>%
              # filter(Code == 100101) %>%
              mutate(CL = Accumulated_Incidence / 100,
                     CL2 = Max_Incidence / 100),
            by = c("Code", "Year")) %>%
  mutate(diff = Cutaneous.Leishmaniasis - CL,
         diff2 = Cutaneous.Leishmaniasis - CL2,
         diff3 = log(diff + 1))

summary(peru_diff_check$diff)

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# -7.403  -0.004   0.036   0.063   0.131   4.306    9756



ggplot(peru_diff_check) +
  geom_density(aes(diff)) # This looks acceptable, but it's odd how there are differing reportings for each municipio

###### Merge Peru disease data
peru_cutleish_df_v3 <- peru_selected_v2 %>%
  filter(Week == 53) %>%
  select(-Week, -max_week, -Weekly_Incidence) %>%
  filter(Code %in% peru_old_muni$IDDIST) %>%
  mutate(Distrito = str_to_sentence(Distrito, locale = "es")) %>%
  rename(Name = Distrito)


new_disease_2019_2022 <- brazil_2021_disease %>%
  select(Name, Year, Code, Cutaneous.Leishmaniasis, Country) %>%
  full_join(peru_cutleish_df_v3 %>%
              select(-c(Departamento, Accumulated_Incidence)) %>%
              rename(Cutaneous.Leishmaniasis = Max_Incidence) %>%
              mutate(Country = "Peru",
                     Cutaneous.Leishmaniasis = Cutaneous.Leishmaniasis / 100) %>%
              filter(Year > 2019),
            by = c("Code", "Year", "Country", "Name")) %>%
  mutate(Cutaneous.Leishmaniasis = case_when(!is.na(Cutaneous.Leishmaniasis.x) ~ Cutaneous.Leishmaniasis.x,
                                             is.na(Cutaneous.Leishmaniasis.x) ~ Cutaneous.Leishmaniasis.y),
         Name = str_to_sentence(Name)) %>%
  select(-c(Cutaneous.Leishmaniasis.x, Cutaneous.Leishmaniasis.y))

new_aad <- aad %>%
  mutate(Name = str_to_sentence(Name)) %>%
  full_join(new_disease_2019_2022,
            by = c("Name", "Code", "Year", 'Country')) %>%
  group_by(Code, Country) %>%
  arrange(Code, Year) %>%
  ungroup() %>%
  mutate(Cutaneous.Leishmaniasis = case_when(!is.na(Cutaneous.Leishmaniasis.x) ~ Cutaneous.Leishmaniasis.x,
                                             is.na(Cutaneous.Leishmaniasis.x) ~ Cutaneous.Leishmaniasis.y),
         Name = str_to_sentence(Name)) %>%
  select(-c(Cutaneous.Leishmaniasis.x, Cutaneous.Leishmaniasis.y)) %>%
  select(Name, Code, Year, Country, Cutaneous.Leishmaniasis, everything()) %>%
  filter(Country != "Colombia")


##################################################################
##                           Colombia                           ##
##################################################################

full_colombia_annual <- read.csv("~/peregrine_amazon/data/colombia/Incidence_v4/colombia_annual_incidence.csv")
colombia_population <- readRDS("~/peregrine_amazon/data/colombia/processed/full_colombia_Population")

full_colombia_annual_wide <- full_colombia_annual %>%
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

new_aad_v2 <- new_aad %>%
  # filter(!(Code %in% full_colombia_annual_wide$Code)) %>%
  rename(CL = Cutaneous.Leishmaniasis) %>%
  full_join(full_colombia_annual_wide)

saveRDS(new_aad, "~/peregrine_amazon/data/disease/")