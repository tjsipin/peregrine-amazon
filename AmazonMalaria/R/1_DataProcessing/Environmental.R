#Clean Google Earth Engine Environmental Data and Summarize Monthly/Annually

#Data provided and reduced by districts/municipios on Google Earth engine
  #MODIS11A2 Terra Land Surface Temperature & Emissivity Data Global 1 km LST_Day_1km and LST_Night_1km
  #MOD13Q1 Terra Vegetation Indices 16-Day Global 250 m NDVI and EVI
  #CHIRPS Daily Precipitation 0.05 arc degrees precipitation
  #JRC Global Surface Water 30m resolution 1984-Present occurrence


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

#Read in environmental datasets created on Google Earth Engine and join with district names
####################################################

#MOD11A2 LST Day and Night Temperature
  #2000-2019, but 2000 is fairly incomplete
  #LST Function
  wrangle_LST<-function(filefolder){
    files <- list.files(path = paste0("Data/1_DataProcessing/Environmental/Temp/MOD11A2Temp/", filefolder, "/"),
                                         full.names = TRUE)
    df<-data.frame()
    for(i in files){
      df_i<-read_csv(i)  %>% 
        dplyr::select(-"system:index", -".geo") %>% dplyr::select(imageId, everything()) %>% 
        gather(key="Code", value ="Value", -imageId)
      df<-rbind(df, df_i)
    }#end for loop
    return(df)
  }  #end function
  #Apply LST Function to Countries Day and Night
    #Colombia
      colombia_lst_day<-wrangle_LST(filefolder="Colombia_Day") %>% dplyr::rename(LST_Day=Value)
      colombia_lst_night<-wrangle_LST(filefolder="Colombia_Night")%>% dplyr::rename(LST_Night=Value)
      colombia_lst<-full_join(colombia_lst_day, colombia_lst_night, by=c("imageId", "Code"))
      colombia_lst_cross<-crossing(imageId=unique(colombia_lst$imageId), Code=unique(colombia$Code))
      colombia_lst_full<-full_join(colombia_lst, colombia_lst_cross, by=c("imageId", "Code")) %>% 
        group_by(imageId, Code) %>% summarize(LST_Day=mean(LST_Day), LST_Night=mean(LST_Night)) %>% ungroup() %>% 
        right_join(colombia, by="Code")%>%  #summarized values between two duplicate Codes just in case (19809 and 25839)
        separate(imageId, c("Year", "Month", "Day")) %>% 
        dplyr::mutate(Country="Colombia")
    #Peru
      peru_lst_day<-wrangle_LST(filefolder="Peru_Day") %>% dplyr::rename(LST_Day=Value)
      peru_lst_night<-wrangle_LST(filefolder="Peru_Night") %>% dplyr::rename(LST_Night=Value) 
      peru_lst<-full_join(peru_lst_day, peru_lst_night, by=c("imageId", "Code"))
      peru_lst_cross<-crossing(imageId=unique(peru_lst$imageId), Code=unique(peru$Code))
      peru_lst_full<-full_join(peru_lst, peru_lst_cross, by=c("imageId", "Code")) %>% 
        right_join(peru, by="Code") %>%
        separate(imageId, c("Year", "Month", "Day")) %>% 
        dplyr::mutate(Country="Peru")
    #Brazil
      brazil_lst_day<-wrangle_LST(filefolder="Brazil_Day") %>% dplyr::rename(LST_Day=Value)
      brazil_lst_night<-wrangle_LST(filefolder="Brazil_Night") %>% dplyr::rename(LST_Night=Value) 
      brazil_lst<-full_join(brazil_lst_day, brazil_lst_night, by=c("imageId", "Code"))
      brazil_lst_cross<-crossing(imageId=unique(brazil_lst$imageId), Code=unique(brazil$Code))
      brazil_lst_full<-full_join(brazil_lst, brazil_lst_cross, by=c("imageId", "Code")) %>% 
        right_join(brazil, by="Code") %>%
        separate(imageId, c("Year", "Month", "Day")) %>% 
        dplyr::mutate(Country="Brazil")
      
      #Combine Data from 3 Countries and Save
      lst_df<-rbind(colombia_lst_full, peru_lst_full, brazil_lst_full) %>% 
        select(Code, Name, Country, everything())
      write_csv(lst_df, "Data/1_DataProcessing/Environmental/Temp/MOD11A2Temp/lst_df.csv")
      
      
#MOD13Q1 Veg NDVI and EVI
    #2000-2019, but 2000 is fairly incomplete
    #Veg Function
    wrangle_veg<-function(filefolder){
      files <- list.files(path = paste0("Data/1_DataProcessing/Environmental/Veg/MOD13Q1Veg/", filefolder, "/"),
                          full.names = TRUE)
      df<-data.frame()
      for(i in files){
        df_i<-read_csv(i)  %>% 
          dplyr::select(-"system:index", -".geo") %>% dplyr::select(imageId, everything()) %>% 
          gather(key="Code", value ="Value", -imageId)
        df<-rbind(df, df_i)
      }#end for loop
      return(df)
    }  #end function
  #Apply Veg Function to Countries NDVI and EVI
    #Colombia
    colombia_veg_ndvi<-wrangle_veg(filefolder="Colombia_NDVI") %>% dplyr::rename(NDVI=Value)
    colombia_veg_evi<-wrangle_veg(filefolder="Colombia_EVI")%>% dplyr::rename(EVI=Value)
    colombia_veg<-full_join(colombia_veg_ndvi, colombia_veg_evi, by=c("imageId", "Code"))
    colombia_veg_cross<-crossing(imageId=unique(colombia_veg$imageId), Code=unique(colombia$Code))
    colombia_veg_full<-full_join(colombia_veg, colombia_veg_cross, by=c("imageId", "Code")) %>% 
      group_by(imageId, Code) %>% summarize(NDVI=mean(NDVI), EVI=mean(EVI)) %>% ungroup() %>% 
      right_join(colombia, by="Code")%>%  #summarized values between two duplicate Codes just in case (19809 and 25839)
      separate(imageId, c("Year", "Month", "Day")) %>% 
      dplyr::mutate(Country="Colombia")
    #Peru
    peru_veg_ndvi<-wrangle_veg(filefolder="Peru_NDVI") %>% dplyr::rename(NDVI=Value)
    peru_veg_evi<-wrangle_veg(filefolder="Peru_EVI") %>% dplyr::rename(EVI=Value) 
    peru_veg<-full_join(peru_veg_ndvi, peru_veg_evi, by=c("imageId", "Code"))
    peru_veg_cross<-crossing(imageId=unique(peru_veg$imageId), Code=unique(peru$Code))
    peru_veg_full<-full_join(peru_veg, peru_veg_cross, by=c("imageId", "Code")) %>% 
      right_join(peru, by="Code") %>%
      separate(imageId, c("Year", "Month", "Day")) %>% 
      dplyr::mutate(Country="Peru")
    #Brazil
    brazil_veg_ndvi<-wrangle_veg(filefolder="Brazil_NDVI") %>% dplyr::rename(NDVI=Value)
    brazil_veg_evi<-wrangle_veg(filefolder="Brazil_EVI") %>% dplyr::rename(EVI=Value) 
    brazil_veg<-full_join(brazil_veg_ndvi, brazil_veg_evi, by=c("imageId", "Code"))
    brazil_veg_cross<-crossing(imageId=unique(brazil_veg$imageId), Code=unique(brazil$Code))
    brazil_veg_full<-full_join(brazil_veg, brazil_veg_cross, by=c("imageId", "Code")) %>% 
      right_join(brazil, by="Code") %>%
      separate(imageId, c("Year", "Month", "Day")) %>% 
      dplyr::mutate(Country="Brazil")
    
    #Combine Data from 3 Countries and Save
    veg_df<-rbind(colombia_veg_full, peru_veg_full, brazil_veg_full) %>% 
      select(Code, Name, Country, everything())
    write_csv(veg_df, "Data/1_DataProcessing/Environmental/Veg/MOD13Q1Veg/veg_df.csv")
    
    
#CHIRPS Precip
  #2000-2019, but 2000 is fairly incomplete
    #Precip Function
    wrangle_precip<-function(filefolder){
      files <- list.files(path = paste0("Data/1_DataProcessing/Environmental/Precip/CHIRPSPrecip/", filefolder, "/"),
                          full.names = TRUE)
      df<-data.frame()
      for(i in files){
        df_i<-read_csv(i)  %>% 
          dplyr::select(-"system:index", -".geo") %>% dplyr::select(imageId, everything()) %>% 
          gather(key="Code", value ="Value", -imageId)
        df<-rbind(df, df_i)
      }#end for loop
      return(df)
    }  #end function
    #Apply Precip Function to Countries
    #Colombia
    colombia_precip<-wrangle_precip(filefolder="Colombia") %>% dplyr::rename(Precip=Value)
    colombia_precip_cross<-crossing(imageId=unique(colombia_precip$imageId), Code=unique(colombia$Code))
    colombia_precip_full<-full_join(colombia_precip, colombia_precip_cross, by=c("imageId", "Code")) %>% 
      group_by(imageId, Code) %>% summarize(Precip=mean(Precip)) %>% ungroup() %>% 
      right_join(colombia, by="Code")%>%  #summarized values between two duplicate Codes just in case (19809 and 25839)
      dplyr::mutate(Year=substr(imageId, 1, 4),
                    Month=substr(imageId, 5, 6),
                    Day=substr(imageId, 7, 8)) %>% #Get Year, Month, and Day from ImageID
      dplyr::select(Year, Month, Day, everything()) %>% dplyr::select(-imageId) %>% 
      dplyr::mutate(Country="Colombia")
    #Peru
    peru_precip<-wrangle_precip(filefolder="Peru") %>% dplyr::rename(Precip=Value)
    peru_precip_cross<-crossing(imageId=unique(peru_precip$imageId), Code=unique(peru$Code))
    peru_precip_full<-full_join(peru_precip, peru_precip_cross, by=c("imageId", "Code")) %>% 
      right_join(peru, by="Code") %>%
      dplyr::mutate(Year=substr(imageId, 1, 4),
                    Month=substr(imageId, 5, 6),
                    Day=substr(imageId, 7, 8)) %>% #Get Year, Month, and Day from ImageID
      dplyr::select(Year, Month, Day, everything()) %>% 
      dplyr::filter(imageId!=20129858) %>% dplyr::select(-imageId) %>%  #remove werid ImageID rows
      dplyr::mutate(Country="Peru")
    #Brazil
    brazil_precip<-wrangle_precip(filefolder="Brazil") %>% dplyr::rename(Precip=Value)
    brazil_precip_cross<-crossing(imageId=unique(brazil_precip$imageId), Code=unique(brazil$Code))
    brazil_precip_full<-full_join(brazil_precip, brazil_precip_cross, by=c("imageId", "Code")) %>% 
      right_join(brazil, by="Code") %>%
      dplyr::mutate(Year=substr(imageId, 1, 4),
                    Month=substr(imageId, 5, 6),
                    Day=substr(imageId, 7, 8)) %>% #Get Year, Month, and Day from ImageID
      dplyr::select(Year, Month, Day, everything()) %>% dplyr::select(-imageId) %>% 
      dplyr::mutate(Country="Brazil")
    
    #Combine Data from 3 Countries and Save
    precip_df<-rbind(colombia_precip_full, peru_precip_full, brazil_precip_full) %>% 
      select(Code, Name, Country, everything())
    write_csv(precip_df, "Data/1_DataProcessing/Environmental/Precip/CHIRPSPrecip/precip_df.csv")    


#JRC Global Surface Water
  colombia_occ<-read_csv("Data/1_DataProcessing/Environmental/SurfaceWater/JRCSurfaceWater/Colombia_Municipios_Occurrence_Table.csv") %>% 
    dplyr::select(MPIOS, mean) %>% dplyr::rename(Code=MPIOS, SWOccurrence=mean) %>% 
    group_by(Code) %>% summarize(SWOccurrence=mean(SWOccurrence)) %>% 
    right_join(colombia, by="Code") %>%  #summarized values between two duplicate Codes (19809 and 25839)
    dplyr::mutate(Country="Colombia")  
  peru_occ<-read_csv("Data/1_DataProcessing/Environmental/SurfaceWater/JRCSurfaceWater/Peru_Districts_Occurrence_Table.csv") %>% 
    dplyr::select(IDDIST, mean) %>% dplyr::rename(Code=IDDIST, SWOccurrence=mean) %>% 
    right_join(peru, by="Code") %>% 
    dplyr::mutate(Country="Peru")
  brazil_occ<-read_csv("Data/1_DataProcessing/Environmental/SurfaceWater/JRCSurfaceWater/Brazil_Municipios_Occurrence_Table.csv") %>% 
    dplyr::select(codigo_ibg, mean) %>% dplyr::rename(Code=codigo_ibg, SWOccurrence=mean) %>% 
    dplyr::mutate(Code=as.character(Code)) %>% 
    right_join(brazil, by="Code") %>% 
    dplyr::mutate(Country="Brazil")

  #Combine Data from 3 Countries and Save
  occ_df<-rbind(colombia_occ, peru_occ, brazil_occ) %>% 
    select(Code, Name, Country, everything())
  write_csv(occ_df, "Data/1_DataProcessing/Environmental/SurfaceWater/JRCSurfaceWater/occ_df.csv")


######################################################################################
#Summarize Environmental Variables Annually and Monthly per Municipio/District
  #Group by Country and Code for both, year for annually, and year and month for monthly
  
#Read in csvs from above if not already read-in
  # lst_df<-read_csv("Data/1_DataProcessing/Environmental/Temp/MOD11A2Temp/lst_df.csv", guess_max = min(100000, Inf))
  # veg_df<-read_csv("Data/1_DataProcessing/Environmental/Veg/MOD13Q1Veg/veg_df.csv", guess_max = min(100000, Inf))
  # # precip_df<-read_csv("Data/1_DataProcessing/Environmental/Precip/CHIRPSPrecip/precip_df.csv", guess_max = min(100000, Inf))
  ## occ_df<-read_csv("Data/1_DataProcessing/Environmental/SurfaceWater/JRCSurfaceWater/occ_df.csv")#Do not need to summarize

  #LST
  lst_df_annual<-lst_df %>% 
    group_by(Code, Name, Country, Year) %>% 
    summarize(LST_Day=mean(LST_Day, na.rm=TRUE),
              LST_Night=mean(LST_Night, na.rm = TRUE)) %>% 
    ungroup()
  lst_df_monthly<-lst_df %>% 
    group_by(Code, Name, Country, Year, Month) %>% 
    summarize(LST_Day=mean(LST_Day, na.rm=TRUE),
              LST_Night=mean(LST_Night, na.rm = TRUE)) %>% 
    ungroup()
  
  #VEG
  veg_df_annual<-veg_df %>% 
    group_by(Code, Name, Country, Year) %>% 
    summarize(NDVI=mean(NDVI, na.rm=TRUE),
              EVI=mean(EVI, na.rm = TRUE)) %>% 
    ungroup()
  veg_df_monthly<-veg_df %>% 
    group_by(Code, Name, Country, Year, Month) %>% 
    summarize(NDVI=mean(NDVI, na.rm=TRUE),
              EVI=mean(EVI, na.rm = TRUE)) %>% 
    ungroup()
  #PRECIP
  precip_df_annual<-precip_df %>% 
    group_by(Code, Name, Country, Year) %>% 
    summarize(Precip=sum(Precip, na.rm=TRUE)) %>% 
    ungroup()
  precip_df_monthly<-precip_df %>% 
    group_by(Code, Name, Country, Year, Month) %>% 
    summarize(Precip=sum(Precip, na.rm=TRUE)) %>% 
    ungroup()
  #OCC-Leave as is
  
  #Write final summarized csvs
  write_csv(lst_df_annual, "Data/1_DataProcessing/Environmental/Temp/MOD11A2Temp/lst_df_annual.csv")
  write_csv(lst_df_monthly, "Data/1_DataProcessing/Environmental/Temp/MOD11A2Temp/lst_df_monthly.csv")
  write_csv(veg_df_annual, "Data/1_DataProcessing/Environmental/Veg/MOD13Q1Veg/veg_df_annual.csv")
  write_csv(veg_df_monthly, "Data/1_DataProcessing/Environmental/Veg/MOD13Q1Veg/veg_df_monthly.csv")
  write_csv(precip_df_annual, "Data/1_DataProcessing/Environmental/Precip/CHIRPSPrecip/precip_df_annual.csv")
  write_csv(precip_df_monthly, "Data/1_DataProcessing/Environmental/Precip/CHIRPSPrecip/precip_df_monthly.csv")
  # Occ already written as occ.csv in surface water folder
  
  
######################################################################################
  #Calculate Optimal Temperature by Disease Annually and Monthly per Municipio/District
  #Group by Country and Code for both, year for annually, and year and month for monthly
  
  #Read in Temp csv from above if not already read-in; used guess_max so keeps code as character
  # lst_df<-read_csv("Data/1_DataProcessing/Environmental/Temp/MOD11A2Temp/lst_df.csv", guess_max = min(100000, Inf))
  

  
  #Optimal Temps for Diseases: (defined from Mordecai et al 2019 paper Table 2)
    # Dengue_Albopictus: 25-28 degrees C [Greater than/equal to 25 & less than (not equal to) 28 since Table Range was 25.4-27.6]
    # Dengue_Aegypti: 28-30 degrees C [Greater than/equal to 28 & less than (not equal to) 30 since Table Range was 28.4-29.8]
    # Chikungunya_Albopictus: 25-28 degrees C [Same application as Dengue_Alb] 
    # Chikungunya_Aegypti: 28-30 degrees C [Same application as Dengue_Aeg]
    # Zika: 28-30 degrees C [Greater than (not equal to) 28 & less than (not equal to) 30 since Table Range was 28.2-29.6]
    # Malaria: 24-27 degrees C [Greater than/equal to 24 & less than/ equal to 27 since Table Range was 23.9-27.0]
    
  #No optimal temp data for Leishmaniasis (Cut, Muc, or Visc) or Yellow Fever
    
  #Optimal Temp based on LST_Day Daily Value
  opttemp_df<-lst_df %>% 
    dplyr::select(-LST_Night) %>% #only using Daily Temp Avgs
    dplyr::mutate(OptTemp_Obs=ifelse(!is.na(LST_Day), 1, NA),
                  Dengue_Alb=ifelse(LST_Day>=25 & LST_Day<28, 1, 0),
                  Dengue_Aeg=ifelse(LST_Day>=28 & LST_Day<30, 1, 0),
                  Chik_Alb=ifelse(LST_Day>=25 & LST_Day<28, 1, 0),
                  Chik_Aeg=ifelse(LST_Day>=28 & LST_Day<30, 1, 0),
                  Zika=ifelse(LST_Day>28 & LST_Day<30, 1, 0),
                  Malaria=ifelse(LST_Day>=24 & LST_Day<=27, 1, 0))
  
  #LST
  opttemp_df_annual<-opttemp_df %>% 
    group_by(Code, Name, Country, Year) %>% 
    summarize(OptTemp_Obs=sum(OptTemp_Obs, na.rm=TRUE),
              Dengue_Alb_OptTemp=sum(Dengue_Alb, na.rm=TRUE),
              Dengue_Aeg_OptTemp=sum(Dengue_Aeg, na.rm=TRUE),
              Chik_Alb_OptTemp=sum(Chik_Alb, na.rm=TRUE),
              Chik_Aeg_OptTemp=sum(Chik_Aeg, na.rm=TRUE),
              Zika_OptTemp=sum(Zika, na.rm=TRUE),
              Malaria_OptTemp=sum(Malaria, na.rm=TRUE)) %>% 
    ungroup()
  opttemp_df_monthly<-opttemp_df %>% 
    group_by(Code, Name, Country, Year, Month) %>% 
    summarize(OptTemp_Obs=sum(OptTemp_Obs, na.rm=TRUE),
              Dengue_Alb_OptTemp=sum(Dengue_Alb, na.rm=TRUE),
              Dengue_Aeg_OptTemp=sum(Dengue_Aeg, na.rm=TRUE),
              Chik_Alb_OptTemp=sum(Chik_Alb, na.rm=TRUE),
              Chik_Aeg_OptTemp=sum(Chik_Aeg, na.rm=TRUE),
              Zika_OptTemp=sum(Zika, na.rm=TRUE),
              Malaria_OptTemp=sum(Malaria, na.rm=TRUE)) %>% 
    ungroup()
  
  #Write final summarized csvs
  write_csv(opttemp_df_annual, "Data/1_DataProcessing/Environmental/Temp/MOD11A2Temp/opttemp_df_annual.csv")
  write_csv(opttemp_df_monthly, "Data/1_DataProcessing/Environmental/Temp/MOD11A2Temp/opttemp_df_monthly.csv") 
  
  
  