#Combine Incidence, Environmental, Landscape, & Socioeconomic Data, both annually and monthly

#Data sourced from outputs of other Data_Processing R Scripts

#######################
#Read in Packages
library(tidyverse)
library(purrr)
options(scipen = 999)

#######################
#Read in Dataframes
  #Incidence 
    #(Include guess_max in read_csv so that certain columns not guessed as logical when they are numeric)
    #Change Months from Names to Character Numbers to Match with Environmental
    annual_incidence_wide<-read_csv("Data/1_DataProcessing/Disease/annual_incidence_wide.csv", guess_max = min(100000, Inf))
    months_df<-data.frame(MonthNum=c("01","02","03","04","05","06","07","08","09","10","11","12",
                                     "01","02","03","04","05","06","07","08","09","10","11","12"),
                          Month=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                                  "01","02","03","04","05","06","07","08","09","10","11","12"),
                          stringsAsFactors = FALSE)
    monthly_incidence_wide<-read_csv("Data/1_DataProcessing/Disease/monthly_incidence_wide.csv", guess_max = min(1000000, Inf))%>% 
      full_join(months_df, by="Month") %>% dplyr::select(-Month) %>% dplyr::rename(Month=MonthNum) %>% 
      select(Code, Name, Country, Year, Month, everything())
  
  #Environmental
    lst_df_annual<-read_csv("Data/1_DataProcessing/Environmental/Temp/MOD11A2Temp/lst_df_annual.csv")
    lst_df_monthly<-read_csv("Data/1_DataProcessing/Environmental/Temp/MOD11A2Temp/lst_df_monthly.csv")
    opttemp_df_annual<-read_csv("Data/1_DataProcessing/Environmental/Temp/MOD11A2Temp/opttemp_df_annual.csv")
    opttemp_df_monthly<-read_csv("Data/1_DataProcessing/Environmental/Temp/MOD11A2Temp/opttemp_df_monthly.csv") 
    veg_df_annual<-read_csv("Data/1_DataProcessing/Environmental/Veg/MOD13Q1Veg/veg_df_annual.csv")
    veg_df_monthly<-read_csv("Data/1_DataProcessing/Environmental/Veg/MOD13Q1Veg/veg_df_monthly.csv")
    precip_df_annual<-read_csv("Data/1_DataProcessing/Environmental/Precip/CHIRPSPrecip/precip_df_annual.csv")
    precip_df_monthly<-read_csv("Data/1_DataProcessing/Environmental/Precip/CHIRPSPrecip/precip_df_monthly.csv")
    occ_df<-read_csv("Data/1_DataProcessing/Environmental/SurfaceWater/JRCSurfaceWater/occ_df.csv")
    
  #Population (re-add for all possible years/combos)
    colombia_pop_df<-read_csv("Data/1_DataProcessing/Population/Colombia/Colombia_population.csv") %>% 
      dplyr::rename(Code=MPIOS) %>% filter(Year!=2020) %>% dplyr::mutate(Country="Colombia")
    peru_pop_df<-read_csv("Data/1_DataProcessing/Population/Peru/Peru_population.csv") %>% 
      dplyr::rename(Code=IDDIST) %>% filter(Year!=2020) %>% dplyr::mutate(Country="Peru")
    brazil_pop_df<-read_csv("Data/1_DataProcessing/Population/Brazil/Brazil_population.csv") %>% 
      dplyr::rename(Code=codigo_ibg) %>% filter(Year!=2020) %>% dplyr::mutate(Country="Brazil")
    pop_df<-rbind(colombia_pop_df, peru_pop_df, brazil_pop_df)
    
  #Landscape Metrics
    classmetrics_df<-read_csv("Data/1_DataProcessing/LandscapeMetrics/classmetrics_df.csv", guess_max = min(1000000, Inf))
    adjacency_df<-read_csv("Data/1_DataProcessing/LandscapeMetrics/adjacency_df.csv", guess_max = min(1000000, Inf))
    
  #Socioeconomic
    nightlights_df<-read_csv("Data/1_DataProcessing/Socioeconomic/NightLights/Harmonized/nightlights_annual_df.csv", guess_max = min(1000000, Inf))
    census_df<-read_csv("Data/1_DataProcessing/Socioeconomic/IPUMS/Census/socioeconomic_ipums_census.csv", guess_max = min(1000000, Inf))
    # dmsp_annual_df<-read_csv("Data/1_DataProcessing/Socioeconomic/NightLights/DMSPStableLights/dmsp_annual_df.csv")
    # viirs_annual_df<-read_csv("Data/1_DataProcessing/Socioeconomic/NightLights/VIIRSAvgRad/viirs_annual_df.csv")
    # viirs_monthly_df<-read_csv("Data/1_DataProcessing/Socioeconomic/NightLights/VIIRSAvgRad/viirs_monthly_df.csv")
    
#Combine Dataframes
  #Annual
  annual_df<-list(annual_incidence_wide, lst_df_annual, opttemp_df_annual, 
                  veg_df_annual, precip_df_annual, nightlights_df) %>%
    purrr::reduce(full_join, by=c("Code", "Name", "Country", "Year")) %>% #Combine
    full_join(occ_df, by=c("Code", "Name", "Country")) %>% #Add SW
    full_join(census_df, by=c("Code", "Name", "Country")) %>% #Add socioeconomic census
    full_join(classmetrics_df, by=c("Code", "Country", "Year")) %>% #Add classmetrics
    full_join(adjacency_df, by=c("Code", "Country", "Year")) %>% #Add adjacency matrix
    dplyr::select(-Population) %>% #Remove to replace with complete population #'s for each year
    full_join(pop_df, by=c("Code", "Country", "Year")) %>% #Add complete pop
    dplyr::select(Code, Name, Country, Year, Population, everything()) %>% #Organize
    arrange(Code, Year)
  
  write_csv(annual_df, "Data/1_DataProcessing/DF/annual_df.csv")
  
  #Monthly
  monthly_df<-list(monthly_incidence_wide, lst_df_monthly, opttemp_df_monthly,
                   veg_df_monthly, precip_df_monthly) %>% 
    purrr::reduce(full_join, by=c("Code", "Name", "Country", "Year", "Month")) %>% #Combine
    full_join(nightlights_df, by=c("Code", "Name", "Country", "Year")) %>% #Add nightlights (annual only)
    full_join(occ_df, by=c("Code", "Name", "Country")) %>% #Add SW
    full_join(census_df, by=c("Code", "Name", "Country")) %>% #Add socioeconomic census
    full_join(classmetrics_df, by=c("Code", "Country", "Year")) %>% #Add classmetrics
    full_join(adjacency_df, by=c("Code", "Country", "Year")) %>% #Add adjacency matrix
    dplyr::select(-Population) %>% #Remove to replace with complete population #'s for each year
    full_join(pop_df, by=c("Code", "Country", "Year")) %>% #Add complete pop
    dplyr::select(Code, Name, Country, Year, Month, Population, everything()) %>% #Organize
    arrange(Code, Year, Month) %>% 
    dplyr::mutate(Month=as.numeric(Month))
  
  write_csv(monthly_df, "Data/1_DataProcessing/DF/monthly_df.csv")
  