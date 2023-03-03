#Clean Google Earth Engine and Ministry Socioeconomic Data and Summarize Monthly/Annually

#Harmonized DMSP Lights: https://figshare.com/articles/Harmonization_of_DMSP_and_VIIRS_nighttime_light_data_from_1992-2018_at_the_global_scale/9828827/2
#DMSP Stable Lights and VIIRS Avg Rad Google Earth Engine datasets, reduced by districts/municipios GEE

#Packages
####################################################
library(tidyverse) #Data wrangling
library(sf) #shapefiles
options(scipen = 999) #no scientific notation

#Read in Shapefiles to have district/municipios names
####################################################
peru<-sf::read_sf("Data/Raw/Municipios/Municipios_Projected/Peru_Municipios_projected.shp") %>% 
  dplyr::select(IDDIST, NOMBDIST) %>% dplyr::rename(Code=IDDIST, Name=NOMBDIST) %>% 
  sf::st_set_geometry(NULL)

colombia<-sf::read_sf("Data/Raw/Municipios/Municipios_Projected/Colombia_Municipios_projected.shp") %>% 
  dplyr::select(MPIOS, NOMBRE_MPI) %>% dplyr::rename(Code=MPIOS, Name=NOMBRE_MPI) %>% 
  sf::st_set_geometry(NULL) %>% distinct() #Remove 2 MPIOS duplicates

brazil<-sf::read_sf("Data/Raw/Municipios/Municipios_Projected/Brazil_Municipios_projected.shp")%>% 
  dplyr::select(codigo_ibg, nome) %>% dplyr::rename(Code=codigo_ibg, Name=nome) %>% #Select needed columns/rename
  sf::st_set_geometry(NULL) #Remove geometry

#Nighttime Lights Datasets: Old Code for DMSP and VIIRS
####################################################
#Read in GEE processed harmonized csvs
  #Colombia
  colombia_nl<-read_csv("Data/1_DataProcessing/Socioeconomic/NightLights/Harmonized/GEE_Harmonized_2008_2018/Colombia_Municipios_HarmonizedDMSP_Table.csv") %>% 
      dplyr::mutate(Year=2000:2018) %>% dplyr::select(imageId, Year, everything()) %>% 
      dplyr::select(-"system:index", -".geo", -"imageId") %>% 
      gather(key="Code", value ="Value", -Year)%>% dplyr::rename(StableLights=Value)
  colombia_nl_cross<-crossing(Year=unique(colombia_nl$Year), Code=unique(colombia$Code))
  colombia_nl_full<-full_join(colombia_nl, colombia_nl_cross, by=c("Year", "Code")) %>%
      right_join(colombia, by="Code") %>%
      dplyr::mutate(Country="Colombia") %>%
      dplyr::select(Code, Name, Country, Year, StableLights) %>%
      group_by(Code, Name, Country, Year) %>% summarize(StableLights=mean(StableLights, na.rm=TRUE)) %>% ungroup()

  #Peru
  peru_nl<-read_csv("Data/1_DataProcessing/Socioeconomic/NightLights/Harmonized/GEE_Harmonized_2008_2018/Peru_Districts_HarmonizedDMSP_Table.csv")%>% 
      dplyr::mutate(Year=2000:2018) %>% dplyr::select(imageId, Year, everything()) %>% 
      dplyr::select(-"system:index", -".geo", -"imageId") %>% 
      gather(key="Code", value ="Value", -Year)%>% dplyr::rename(StableLights=Value)
  peru_nl_cross<-crossing(Year=unique(peru_nl$Year), Code=unique(peru$Code))
  peru_nl_full<-full_join(peru_nl, peru_nl_cross, by=c("Year", "Code")) %>%
      right_join(peru, by="Code") %>%
      dplyr::mutate(Country="Peru") %>%
      dplyr::select(Code, Name, Country, Year, StableLights) %>%
      group_by(Code, Name, Country, Year) %>% summarize(StableLights=mean(StableLights, na.rm=TRUE)) %>% ungroup()
    
  #Brazil
  brazil_nl<-read_csv("Data/1_DataProcessing/Socioeconomic/NightLights/Harmonized/GEE_Harmonized_2008_2018/Brazil_Municipios_HarmonizedDMSP_Table.csv")%>% 
      dplyr::mutate(Year=2000:2018) %>% dplyr::select(imageId, Year, everything()) %>% 
      dplyr::select(-"system:index", -".geo", -"imageId") %>% 
      gather(key="Code", value ="Value", -Year)%>% dplyr::rename(StableLights=Value)
  brazil_nl_cross<-crossing(Year=unique(brazil_nl$Year), Code=unique(brazil$Code))
  brazil_nl_full<-full_join(brazil_nl, brazil_nl_cross, by=c("Year", "Code")) %>%
      right_join(brazil, by="Code") %>%
      dplyr::mutate(Country="Brazil") %>%
      dplyr::select(Code, Name, Country, Year, StableLights) %>%
      group_by(Code, Name, Country, Year) %>% summarize(StableLights=mean(StableLights, na.rm=TRUE)) %>% ungroup()
    
  #Combine Data from 3 Countries and Save
  nl_df<-rbind(colombia_nl_full, peru_nl_full, brazil_nl_full)
  write_csv(nl_df, "Data/1_DataProcessing/Socioeconomic/NightLights/Harmonized/nightlights_annual_df.csv")

  
  

# #Nighttime Lights Datasets: Old Code for DMSP and VIIRS
# ####################################################
# #DMSP 2000-2013 Annual
#   #Colombia
#   colombia_dmsp<-read_csv("Data/1_DataProcessing/Socioeconomic/NightLights/DMSPStableLights/Colombia_Municipios_StableLights_Table.csv") %>% 
#     dplyr::select(-"system:index", -".geo") %>% dplyr::select(imageId, everything()) %>% 
#     gather(key="Code", value ="Value", -imageId)%>% dplyr::rename(StableLights=Value)
#   colombia_dmsp_cross<-crossing(imageId=unique(colombia_dmsp$imageId), Code=unique(colombia$Code))
#   colombia_dmsp_full<-full_join(colombia_dmsp, colombia_dmsp_cross, by=c("imageId", "Code")) %>% 
#     right_join(colombia, by="Code") %>%
#     dplyr::mutate(Year=substr(imageId, 4, 7),
#                   Country="Colombia") %>% 
#     dplyr::select(-imageId) %>% dplyr::select(Code, Name, Country, Year, StableLights) %>% 
#     group_by(Code, Name, Country, Year) %>% summarize(StableLights=mean(StableLights, na.rm=TRUE)) %>% ungroup()
# 
#   #Peru
#   peru_dmsp<-read_csv("Data/1_DataProcessing/Socioeconomic/NightLights/DMSPStableLights/Peru_Districts_StableLights_Table.csv") %>% 
#     dplyr::select(-"system:index", -".geo") %>% dplyr::select(imageId, everything()) %>% 
#     gather(key="Code", value ="Value", -imageId)%>% dplyr::rename(StableLights=Value)
#   peru_dmsp_cross<-crossing(imageId=unique(peru_dmsp$imageId), Code=unique(peru$Code))
#   peru_dmsp_full<-full_join(peru_dmsp, peru_dmsp_cross, by=c("imageId", "Code")) %>% 
#     right_join(peru, by="Code") %>%
#     dplyr::mutate(Year=substr(imageId, 4, 7),
#                   Country="Peru") %>% 
#     dplyr::select(-imageId) %>% dplyr::select(Code, Name, Country, Year, StableLights) %>% 
#     group_by(Code, Name, Country, Year) %>% summarize(StableLights=mean(StableLights, na.rm=TRUE)) %>% ungroup()
#   
#   #Brazil
#   brazil_dmsp<-read_csv("Data/1_DataProcessing/Socioeconomic/NightLights/DMSPStableLights/Brazil_Municipios_StableLights_Table.csv") %>% 
#     dplyr::select(-"system:index", -".geo") %>% dplyr::select(imageId, everything()) %>% 
#     gather(key="Code", value ="Value", -imageId)%>% dplyr::rename(StableLights=Value)
#   brazil_dmsp_cross<-crossing(imageId=unique(brazil_dmsp$imageId), Code=unique(brazil$Code))
#   brazil_dmsp_full<-full_join(brazil_dmsp, brazil_dmsp_cross, by=c("imageId", "Code")) %>% 
#     right_join(brazil, by="Code") %>%
#     dplyr::mutate(Year=substr(imageId, 4, 7),
#                   Country="Brazil") %>% 
#     dplyr::select(-imageId) %>% dplyr::select(Code, Name, Country, Year, StableLights) %>% 
#     group_by(Code, Name, Country, Year) %>% summarize(StableLights=mean(StableLights, na.rm=TRUE)) %>% ungroup()
#   
#   #Combine Data from 3 Countries and Save
#   dmsp_df<-rbind(colombia_dmsp_full, peru_dmsp_full, brazil_dmsp_full) %>% 
#     mutate(Year=as.numeric(Year))
#   write_csv(dmsp_df, "Data/1_DataProcessing/Socioeconomic/NightLights/DMSPStableLights/dmsp_annual_df.csv")
# 
#   
# #VIIRS 2014-2019 Monthly
#   #Peru
#   peru_viirs_14_16<-read_csv("Data/1_DataProcessing/Socioeconomic/NightLights/VIIRSAvgRad/Peru_Districts_AvgRad_Table_2014_2016.csv")
#   peru_viirs_17_19<-read_csv("Data/1_DataProcessing/Socioeconomic/NightLights/VIIRSAvgRad/Peru_Districts_AvgRad_Table_2017_2019.csv")
#   peru_viirs<-rbind(peru_viirs_14_16,peru_viirs_17_19)%>% 
#     dplyr::select(-"system:index", -".geo") %>% dplyr::select(imageId, everything()) %>% 
#     gather(key="Code", value ="Value", -imageId)%>% dplyr::rename(AvgRad=Value)
#   peru_viirs_cross<-crossing(imageId=unique(peru_viirs$imageId), Code=unique(peru$Code))
#   peru_viirs_full<-full_join(peru_viirs, peru_viirs_cross, by=c("imageId", "Code")) %>% 
#     right_join(peru, by="Code") %>%
#     dplyr::mutate(Year=substr(imageId, 1, 4),
#                   Month=substr(imageId, 5, 6),
#                   Country="Peru") %>% 
#     dplyr::select(-imageId) %>% dplyr::select(Code, Name, Country, Year, Month, AvgRad) %>% 
#     group_by(Code, Name, Country, Year, Month) %>% summarize(AvgRad=mean(AvgRad, na.rm=TRUE)) %>% ungroup() #Just in case
#   
#   #Colombia
#   colombia_viirs<-read_csv("Data/1_DataProcessing/Socioeconomic/NightLights/VIIRSAvgRad/Colombia_Municipios_AvgRad_Table.csv")%>% 
#     dplyr::select(-"system:index", -".geo") %>% dplyr::select(imageId, everything()) %>% 
#     gather(key="Code", value ="Value", -imageId)%>% dplyr::rename(AvgRad=Value)
#   colombia_viirs_cross<-crossing(imageId=unique(colombia_viirs$imageId), Code=unique(colombia$Code))
#   colombia_viirs_full<-full_join(colombia_viirs, colombia_viirs_cross, by=c("imageId", "Code"))%>% 
#     right_join(colombia, by="Code")%>%
#     dplyr::mutate(Year=substr(imageId, 1, 4),
#                   Month=substr(imageId, 5, 6),
#                   Country="Colombia") %>% 
#     dplyr::select(-imageId) %>% dplyr::select(Code, Name, Country, Year, Month, AvgRad) %>% 
#     group_by(Code, Name, Country, Year, Month) %>% summarize(AvgRad=mean(AvgRad, na.rm=TRUE)) %>% ungroup() #Just in case
#   
#   #Brazil
#   brazil_viirs<-read_csv("Data/1_DataProcessing/Socioeconomic/NightLights/VIIRSAvgRad/Brazil_Municipios_AvgRad_Table.csv")%>% 
#     dplyr::select(-"system:index", -".geo") %>% dplyr::select(imageId, everything()) %>% 
#     gather(key="Code", value ="Value", -imageId)%>% dplyr::rename(AvgRad=Value)
#   brazil_viirs_cross<-crossing(imageId=unique(brazil_viirs$imageId), Code=unique(brazil$Code))
#   brazil_viirs_full<-full_join(brazil_viirs, brazil_viirs_cross, by=c("imageId", "Code")) %>% 
#     right_join(brazil, by="Code") %>%
#     dplyr::mutate(Year=substr(imageId, 1, 4),
#                   Month=substr(imageId, 5, 6),
#                   Country="Brazil") %>% 
#     dplyr::select(-imageId) %>% dplyr::select(Code, Name, Country, Year, Month, AvgRad) %>% 
#     group_by(Code, Name, Country, Year, Month) %>% summarize(AvgRad=mean(AvgRad, na.rm=TRUE)) %>% ungroup() #Just in case
#   
#   #Combine Data from 3 Countries and Save
#   viirs_df<-rbind(colombia_viirs_full, peru_viirs_full, brazil_viirs_full) %>% 
#     mutate(Year=as.numeric(Year))
#   write_csv(viirs_df, "Data/1_DataProcessing/Socioeconomic/NightLights/VIIRSAvgRad/viirs_monthly_df.csv")
#   viirs_df_annual<-viirs_df %>% 
#     group_by(Code, Name, Country, Year) %>% summarize(AvgRad=mean(AvgRad, na.rm=TRUE)) %>% ungroup() #Just in case
#   write_csv(viirs_df_annual, "Data/1_DataProcessing/Socioeconomic/NightLights/VIIRSAvgRad/viirs_annual_df.csv")
#   