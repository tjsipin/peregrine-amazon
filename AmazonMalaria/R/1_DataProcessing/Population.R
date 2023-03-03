#Combine Population data

#Population Data comes from WorldPop Global Project Population Data: Estimated Residential Population per 
  #100x100m Grid Square, as accessed on google earth engine. More information can be found at the WorldPop website
  #and on google earth engine. 
#For each county, for each year, subset the country-year population raster (100m) by the countries projected 
  #municipality/district shapefiles (found in the Data/Raw/Municipios/Municipios_Projected folder). 
  #Used reduce by regions function on GEE to reduce world pop raster per country-year by each municipio/district,
    #Reducing by sum for scale=100. (Sum as each pixel is estimate number of people per that pixel).
    #Wrote table for population of municipios for each country-year to google drive, and downloaded files into 
      #WorldPop_GEE folder under each countries folder in Population. 
  #This script will combine population data across the years, adding year column and rounding population to nearest person.
    #Then saving that csv per country.

#Packages
library(tidyverse)
options(scipen = 999)

#Peru
years<-2000:2021
peru_df<-data.frame()
for (y in years){
  peru_pop<-read_csv(paste0("Data/1_DataProcessing/Population/Peru/WorldPop_GEE/Peru_Dist_Pop", y, ".csv")) %>% 
    dplyr::select(IDDIST,sum) %>% dplyr::mutate(Year=y, Population=round(sum)) %>% dplyr::select(-sum)
  peru_df<-rbind(peru_df, peru_pop)
}
write_csv(peru_df, "Data/1_DataProcessing/Population/Peru/Peru_population.csv")

#Colombia
years<-2000:2021
colombia_df<-data.frame()
for (y in years){
  colombia_pop<-read_csv(paste0("Data/1_DataProcessing/Population/Colombia/WorldPop_GEE/Colombia_Muni_Pop", y, ".csv")) %>% 
    dplyr::select(MPIOS,sum) %>% dplyr::mutate(Year=y, Population=round(sum)) %>% dplyr::select(-sum) %>% 
    group_by(MPIOS, Year) %>% summarize(Population=sum(Population)) %>% ungroup() #Deal with repeated MPIOS
  colombia_df<-rbind(colombia_df, colombia_pop)
}
write_csv(colombia_df, "Data/1_DataProcessing/Population/Colombia/Colombia_population.csv")

#Know that Two MPIOS are repeated twice because they have two separate geometries, but no other identifier
#MPIOS are 19809 and 	25839
#Only problem for Colombia
#Going to combine there population sizes as the cases are combined.

#Brazil
years<-2000:2021
brazil_df<-data.frame()
for (y in years){
  brazil_pop<-read_csv(paste0("Data/1_DataProcessing/Population/Brazil/WorldPop_GEE/Brazil_Muni_Pop", y, ".csv")) %>% 
    dplyr::select(codigo_ibg,sum) %>% dplyr::mutate(Year=y, Population=round(sum)) %>% dplyr::select(-sum)
  brazil_df<-rbind(brazil_df, brazil_pop)
}
write_csv(brazil_df, "Data/1_DataProcessing/Population/Brazil/Brazil_population.csv")