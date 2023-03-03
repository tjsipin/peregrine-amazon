#Calculate Incidence Per County

#The script calculated incidence per municipio per country. 
  #It uses the disease data calculated in the Peru, Colombia, and Brazil Disease R Scripts (In Country_Disease folders per disease)
  #And the population data calculated in the Population R script (one csv per country)
#It will calculate annual and monthly incidence data for each municipio for each country


#Load packages
################################
library (tidyverse)
options(scipen = 999)


#Load in Data
################################
#Population data
##Unique population per municipio-year
peru_pop<-read_csv("Data/1_DataProcessing/Population/Peru/Peru_population.csv")
colombia_pop<-read_csv("Data/1_DataProcessing/Population/Colombia/WorldPop_GEE/colombia_population.csv")
brazil_pop<-read_csv("Data/1_DataProcessing/Population/Brazil/Brazil_population.csv")


#Disease data
  #Peru
  ##Includes Disease Type which is not replicated by week-yr
  peru_chik<-read_csv("Data/1_DataProcessing/Disease/Peru/Peru_Disease/chikungunya.csv")
  peru_cutleish<-read_csv("Data/1_DataProcessing/Disease/Peru/Peru_Disease/cutaneous_leishmaniasis.csv")
  peru_mucleish<-read_csv("Data/1_DataProcessing/Disease/Peru/Peru_Disease/mucosal_leishmaniasis.csv")
  peru_dengue<-read_csv("Data/1_DataProcessing/Disease/Peru/Peru_Disease/dengue.csv")
  peru_malariapfal<-read_csv("Data/1_DataProcessing/Disease/Peru/Peru_Disease/pfal_malaria.csv")
  peru_zika<-read_csv("Data/1_DataProcessing/Disease/Peru/Peru_Disease/zika.csv")
  
  #Colombia
  colombia_chik<-read_csv("Data/1_DataProcessing/Disease/Colombia/Colombia_Disease/chikungunya.csv") %>% dplyr::select(-Muni)
  colombia_cutleish<-read_csv("Data/1_DataProcessing/Disease/Colombia/Colombia_Disease/cutaneous_leishmaniasis.csv") %>% dplyr::select(-Muni)
  colombia_mucleish<-read_csv("Data/1_DataProcessing/Disease/Colombia/Colombia_Disease/mucosal_leishmaniasis.csv") %>% dplyr::select(-Muni)
  colombia_viscleish<-read_csv("Data/1_DataProcessing/Disease/Colombia/Colombia_Disease/visceral_leishmaniasis.csv") %>% 
    dplyr::mutate(Muni_Code=str_pad(as.character(Muni_Code), 5, side="left", pad=0))%>% dplyr::select(-Muni)
  colombia_dengue<-read_csv("Data/1_DataProcessing/Disease/Colombia/Colombia_Disease/dengue.csv") %>% dplyr::select(-Muni)
  colombia_denguegrave<-read_csv("Data/1_DataProcessing/Disease/Colombia/Colombia_Disease/dengue_grave.csv") %>% dplyr::select(-Muni)
  colombia_malaria<-read_csv("Data/1_DataProcessing/Disease/Colombia/Colombia_Disease/malaria.csv") %>% dplyr::select(-Muni)
  colombia_malariapfal<-read_csv("Data/1_DataProcessing/Disease/Colombia/Colombia_Disease/malaria_pfal.csv") %>% dplyr::select(-Muni)
  colombia_malariavivax<-read_csv("Data/1_DataProcessing/Disease/Colombia/Colombia_Disease/malaria_vivax.csv") %>% dplyr::select(-Muni)
  colombia_malariaasociada<-read_csv("Data/1_DataProcessing/Disease/Colombia/Colombia_Disease/malaria_asociada.csv") %>% dplyr::select(-Muni)
  colombia_malariacomplicada<-read_csv("Data/1_DataProcessing/Disease/Colombia/Colombia_Disease/malaria_complicada.csv") %>% dplyr::select(-Muni)
  colombia_malariamalarie<-read_csv("Data/1_DataProcessing/Disease/Colombia/Colombia_Disease/malaria_malarie.csv") %>% dplyr::select(-Muni)
  colombia_malariamortalidad<-read_csv("Data/1_DataProcessing/Disease/Colombia/Colombia_Disease/malaria_mortalidad.csv") %>% dplyr::select(-Muni)
  colombia_yellowfever<-read_csv("Data/1_DataProcessing/Disease/Colombia/Colombia_Disease/yellowfever.csv") %>% 
    dplyr::mutate(Muni_Code=str_pad(as.character(Muni_Code), 5, side="left", pad=0))%>% dplyr::select(-Muni)
  colombia_zika<-read_csv("Data/1_DataProcessing/Disease/Colombia/Colombia_Disease/zika.csv") %>% dplyr::select(-Muni)
  
  #Brazil
  brazil_dengue<-read_csv("Data/1_DataProcessing/Disease/Brazil/Brazil_Disease/dengue.csv")
  brazil_malaria<-read_csv("Data/1_DataProcessing/Disease/Brazil/Brazil_Disease/malaria.csv")
  brazil_yellowfever<-read_csv("Data/1_DataProcessing/Disease/Brazil/Brazil_Disease/yellowfever.csv")
  brazil_viscleish<-read_csv("Data/1_DataProcessing/Disease/Brazil/Brazil_Disease/visceral_leishmaniasis.csv")
  brazil_cutleish_0106<-read_csv("Data/1_DataProcessing/Disease/Brazil/Brazil_Disease/cutaneous_leishmaniasis_2001_2006.csv")
  brazil_cutleish_0718<-read_csv("Data/1_DataProcessing/Disease/Brazil/Brazil_Disease/cutaneous_leishmaniasis_2007_2018.csv")

#Combine Colombia Data for Dengue and Malaria  
################################
##Dengue
  colombia_dengue<-colombia_dengue %>% dplyr::rename(Dengue=Cases)
  colombia_denguegrave<-colombia_denguegrave%>% dplyr::rename(DengueGrave=Cases)
  colombia_dengue_all<-full_join(colombia_dengue, colombia_denguegrave, 
                                 by=c("Muni_Code", "Muni_Name", "Year", "Week", "Week_Yr")) %>% 
    dplyr::mutate(Cases=Dengue+DengueGrave) %>% #Adding together because reported separated, no NA values b/c balanced same amount
    dplyr::select(-Dengue, -DengueGrave)#Remove separate case columns
  colombia_dengue_all[is.na(colombia_dengue_all)]<-0#Replace NA in dataframe with 0
##Malaria
  colombia_malaria<-colombia_malaria %>% dplyr::rename(Malaria=Cases)
  colombia_malariapfal<-colombia_malariapfal%>% dplyr::rename(MalariaPfal=Cases) 
  colombia_malariavivax<-colombia_malariavivax%>% dplyr::rename(MalariaVivax=Cases) 
  colombia_malariaasociada<-colombia_malariaasociada%>% dplyr::rename(MalariaAsociada=Cases) 
  colombia_malariacomplicada<-colombia_malariacomplicada%>% dplyr::rename(MalariaComplicada=Cases) 
  colombia_malariamalarie<-colombia_malariamalarie%>% dplyr::rename(MalariaMalarie=Cases) 
  colombia_malariamortalidad<-colombia_malariamortalidad  %>% dplyr::rename(MalariaMortalidad=Cases)
  colombia_malaria_combine<-list(colombia_malaria, colombia_malariapfal,colombia_malariavivax,
                             colombia_malariaasociada,colombia_malariacomplicada,colombia_malariamalarie,
                             colombia_malariamortalidad) %>% 
                        purrr::reduce(full_join, by=c("Muni_Code", "Muni_Name", "Year", "Week", "Week_Yr"))
  colombia_malaria_combine[is.na(colombia_malaria_combine)]<-0#Replace NA in dataframe with 0 to add together columns
  colombia_malaria_all<-colombia_malaria_combine %>% 
    dplyr::mutate(Cases=Malaria + MalariaPfal + MalariaVivax + MalariaAsociada + 
                    MalariaComplicada + MalariaMalarie + MalariaMortalidad) %>% 
    dplyr::select(Muni_Code, Muni_Name, Year, Week, Week_Yr, Cases)
    
  ##########################################################################  
  
#Calculate Annual Incidence (per country per disease)
################################
##Peru
  #Function to calculate annual incidence per disease for Peru
  Calc_Peru_Annual_Incidence<-function(disease){
    result<-disease%>% 
      dplyr::select(-District) %>% #Use District_Name as better formatted
      group_by(Dist_Code, District_Name, Year) %>% #Group by District and Year
      summarize(Cases=sum(Cases, na.rm=TRUE)) %>% #Annual Cases- will sum across weekyr and diagnosis type
      ungroup() %>% #ungroup
      left_join(peru_pop, by=c("Dist_Code"="IDDIST", "Year")) %>%#Left join for population (pop has 2020)
      dplyr::mutate(Incidence=Cases/Population)#Calculate Annual Incidence
    return(result)
  }
  
  #Apply function by disease
  peru_chik_annual_incidence<-Calc_Peru_Annual_Incidence(disease=peru_chik) %>% dplyr::mutate(Disease="Chikungunya")
  peru_cutleish_annual_incidence<-Calc_Peru_Annual_Incidence(disease=peru_cutleish) %>% dplyr::mutate(Disease="Cutaneous Leishmaniasis")
  peru_mucleish_annual_incidence<-Calc_Peru_Annual_Incidence(disease=peru_mucleish) %>% dplyr::mutate(Disease="Mucosal Leishmaniasis")
  peru_dengue_annual_incidence<-Calc_Peru_Annual_Incidence(disease=peru_dengue) %>% dplyr::mutate(Disease="Dengue")
  peru_malaria_annual_incidence<-Calc_Peru_Annual_Incidence(disease=peru_malariapfal) %>% dplyr::mutate(Disease="Malaria") #Naming Malaria to see across countries
  peru_zika_annual_incidence<-Calc_Peru_Annual_Incidence(disease=peru_zika) %>% dplyr::mutate(Disease="Zika")
  
  #Combine Peru across diseases
  peru_annual_incidence<-rbind(peru_chik_annual_incidence,peru_cutleish_annual_incidence,peru_mucleish_annual_incidence,
                               peru_dengue_annual_incidence, peru_malaria_annual_incidence, peru_zika_annual_incidence)

  #Save Peru Annual Incidence as csv
  write_csv(peru_annual_incidence, "Data/1_DataProcessing/Incidence/peru_annual_incidence.csv")
  
##Colombia
  #Function to calculate annual incidence per disease for Colombia
  Calc_Colombia_Annual_Incidence<-function(disease){
    result<-disease%>% 
      # dplyr::select(-Muni) %>% #(removed Muni from all Colombia df above, since combining diseases)
      group_by(Muni_Code, Muni_Name, Year) %>% #Group by Muni and Year
      summarize(Cases=sum(Cases, na.rm=TRUE)) %>% #Annual Cases-sum across weeks
      ungroup() %>% #ungroup
      left_join(colombia_pop, by=c("Muni_Code"="MPIOS", "Year")) %>%#Left join for population (pop has 2020)
      dplyr::mutate(Incidence=Cases/Population)#Calculate Annual Incidence
    return(result)
  }
  
  #Apply function by disease
  colombia_chik_annual_incidence<-Calc_Colombia_Annual_Incidence(disease=colombia_chik)%>% dplyr::mutate(Disease="Chikungunya")
  colombia_cutleish_annual_incidence<-Calc_Colombia_Annual_Incidence(disease=colombia_cutleish)%>% dplyr::mutate(Disease="Cutaneous Leishmaniasis")
  colombia_mucleish_annual_incidence<-Calc_Colombia_Annual_Incidence(disease=colombia_mucleish)%>% dplyr::mutate(Disease="Mucosal Leishmaniasis")
  colombia_viscleish_annual_incidence<-Calc_Colombia_Annual_Incidence(disease=colombia_viscleish)%>% dplyr::mutate(Disease="Visceral Leishmaniasis")
  colombia_dengue_annual_incidence<-Calc_Colombia_Annual_Incidence(disease=colombia_dengue_all)%>% dplyr::mutate(Disease="Dengue")
  colombia_malaria_annual_incidence<-Calc_Colombia_Annual_Incidence(disease=colombia_malaria_all)%>% dplyr::mutate(Disease="Malaria")
  colombia_yellowfever_annual_incidence<-Calc_Colombia_Annual_Incidence(disease=colombia_yellowfever)%>% dplyr::mutate(Disease="Yellow Fever")
  colombia_zika_annual_incidence<-Calc_Colombia_Annual_Incidence(disease=colombia_zika)%>% dplyr::mutate(Disease="Zika")

  #Combine Colombia across diseases
  colombia_annual_incidence<-rbind(colombia_chik_annual_incidence, colombia_cutleish_annual_incidence,
                                   colombia_mucleish_annual_incidence, colombia_viscleish_annual_incidence, 
                                   colombia_dengue_annual_incidence,colombia_malaria_annual_incidence, 
                                   colombia_yellowfever_annual_incidence, colombia_zika_annual_incidence)
  
  #Save Colombia Annual Incidence as csv
  write_csv(colombia_annual_incidence, "Data/1_DataProcessing/Incidence/colombia_annual_incidence.csv")
  
  
##Brazil
  #Function to calculate annual incidence per disease for Brazil
  Calc_Brazil_Annual_Incidence<-function(disease){
    result<-disease%>% 
      dplyr::select(-Muni) %>% #Use Muni_Name as better formatted
      group_by(Muni_Code, Muni_Name, codigo_ibg, Year) %>% #Group by Muni/codigo and Year
      summarize(Cases=sum(Cases, na.rm=TRUE)) %>% #Annual Cases-sum across weeks
      ungroup() %>% #ungroup
      left_join(brazil_pop, by=c("codigo_ibg", "Year")) %>%#Left join for population (pop has 2020)
      dplyr::mutate(Incidence=Cases/Population)#Calculate Annual Incidence
    return(result)
  }
  
  #Apply function by disease
  brazil_dengue_annual_incidence<-Calc_Brazil_Annual_Incidence(disease=brazil_dengue)%>% dplyr::mutate(Disease="Dengue")
  brazil_malaria_annual_incidence<-Calc_Brazil_Annual_Incidence(disease=brazil_malaria)%>% dplyr::mutate(Disease="Malaria")
  brazil_yellowfever_annual_incidence<-Calc_Brazil_Annual_Incidence(disease=brazil_yellowfever)%>% dplyr::mutate(Disease="Yellow Fever")
  brazil_viscleish_annual_incidence<-Calc_Brazil_Annual_Incidence(disease=brazil_viscleish)%>% dplyr::mutate(Disease="Visceral Leishmaniasis")
  brazil_cutleish_0106_annual_incidence<-Calc_Brazil_Annual_Incidence(disease=brazil_cutleish_0106)
  brazil_cutleish_0718_annual_incidence<-Calc_Brazil_Annual_Incidence(disease=brazil_cutleish_0718)
  brazil_cutleish_annual_incidence<-rbind(brazil_cutleish_0106_annual_incidence,brazil_cutleish_0718_annual_incidence) %>% 
    arrange(Muni_Code, Year)%>% dplyr::mutate(Disease="Cutaneous Leishmaniasis")
  
  #Combine Brazil across diseases
  brazil_annual_incidence<-rbind(brazil_dengue_annual_incidence, brazil_malaria_annual_incidence,
                                 brazil_yellowfever_annual_incidence,brazil_viscleish_annual_incidence,
                                 brazil_cutleish_annual_incidence)
  
  #Save Brazil Annual Incidence as csv
  write_csv(brazil_annual_incidence, "Data/1_DataProcessing/Incidence/brazil_annual_incidence.csv")
  
  
  
#Combine data across countries, and put in wide format for incidence (format/rename to rbind)
###########################
  peru_annual_incidence_format<-peru_annual_incidence %>% 
    dplyr::rename(Code=Dist_Code, Name=District_Name) %>% 
    dplyr::mutate(Country="Peru")
  colombia_annual_incidence_format<-colombia_annual_incidence %>% 
    dplyr::rename(Code=Muni_Code, Name=Muni_Name) %>% 
    dplyr::mutate(Country="Colombia")
  brazil_annual_incidence_format<-brazil_annual_incidence %>% 
    dplyr::rename(Code=codigo_ibg, Name=Muni_Name) %>% 
    dplyr::select(-Muni_Code) %>% #Muni_code is codigo_ibg with last character cut
    dplyr::mutate(Country="Brazil")
  annual_incidence<-rbind(peru_annual_incidence_format, colombia_annual_incidence_format, brazil_annual_incidence_format)

  write_csv(annual_incidence, "Data/1_DataProcessing/Disease/annual_incidence.csv")#Write csv

  #Wide format for case data
  annual_cases_wide<-annual_incidence %>% 
    dplyr::select(-Incidence) %>% #Only Have Cases go wide by Disease, Code/Name/Year/Population will be the same
    spread(Disease, Cases)  
  write_csv(annual_cases_wide, "Data/1_DataProcessing/Disease/annual_cases_wide.csv")#Write csv
  
  #Wide format for incidence data
  annual_incidence_wide<-annual_incidence %>% 
    dplyr::select(-Cases) %>% #Only Have Incidence go wide by Disease, Code/Name/Year/Population will be the same
    spread(Disease, Incidence)
  write_csv(annual_incidence_wide, "Data/1_DataProcessing/Disease/annual_incidence_wide.csv")#Write csv
  
  
 ########################################################################## 
  
  #Calculate Monthly Incidence (per country per disease)
  ################################
  ##Peru
  #Function to calculate monthly incidence per disease for Peru
  Calc_Peru_Monthly_Incidence<-function(disease){
    result<-disease%>% 
      dplyr::select(-District) %>% #Use District_Name as better formatted
      dplyr::mutate(Month=substr(Week_Yr, 6, 7)) %>% 
      group_by(Dist_Code, District_Name, Year, Month) %>% #Group by District and Year/Month
      summarize(Cases=sum(Cases, na.rm=TRUE)) %>% #Monthly Cases- will sum across weekyr and diagnosis type
      ungroup() %>% #ungroup
      left_join(peru_pop, by=c("Dist_Code"="IDDIST", "Year")) %>%#Left join for population (pop has 2020)
      dplyr::mutate(Incidence=Cases/Population)#Calculate Monthly Incidence
    return(result)
  }
  
  #Apply function by disease
  peru_chik_monthly_incidence<-Calc_Peru_Monthly_Incidence(disease=peru_chik) %>% dplyr::mutate(Disease="Chikungunya")
  peru_cutleish_monthly_incidence<-Calc_Peru_Monthly_Incidence(disease=peru_cutleish) %>% dplyr::mutate(Disease="Cutaneous Leishmaniasis")
  peru_mucleish_monthly_incidence<-Calc_Peru_Monthly_Incidence(disease=peru_mucleish) %>% dplyr::mutate(Disease="Mucosal Leishmaniasis")
  peru_dengue_monthly_incidence<-Calc_Peru_Monthly_Incidence(disease=peru_dengue) %>% dplyr::mutate(Disease="Dengue")
  peru_malaria_monthly_incidence<-Calc_Peru_Monthly_Incidence(disease=peru_malariapfal) %>% dplyr::mutate(Disease="Malaria") #Naming Malaria to see across countries
  peru_zika_monthly_incidence<-Calc_Peru_Monthly_Incidence(disease=peru_zika) %>% dplyr::mutate(Disease="Zika")
  
  #Combine Peru across diseases
  peru_monthly_incidence<-rbind(peru_chik_monthly_incidence,peru_cutleish_monthly_incidence,peru_mucleish_monthly_incidence,
                               peru_dengue_monthly_incidence, peru_malaria_monthly_incidence, peru_zika_monthly_incidence)
  
  #Save Peru Monthly Incidence as csv
  write_csv(peru_monthly_incidence, "Data/1_DataProcessing/Incidence/peru_monthly_incidence.csv")
  
  ##Colombia
  #Function to calculate monthly incidence per disease for Colombia
  Calc_Colombia_Monthly_Incidence<-function(disease){
    result<-disease%>% 
      # dplyr::select(-Muni) %>% #(removed Muni from all Colombia df above, since combining diseases)
      dplyr::mutate(Month=substr(Week_Yr, 6, 7)) %>% 
      group_by(Muni_Code, Muni_Name, Year, Month) %>% #Group by Muni and Year/Month
      summarize(Cases=sum(Cases, na.rm=TRUE)) %>% #Monthly Cases-sum across weeks
      ungroup() %>% #ungroup
      left_join(colombia_pop, by=c("Muni_Code"="MPIOS", "Year")) %>%#Left join for population (pop has 2020)
      dplyr::mutate(Incidence=Cases/Population)#Calculate Monthly Incidence
    return(result)
  }
  
  #Apply function by disease
  colombia_chik_monthly_incidence<-Calc_Colombia_Monthly_Incidence(disease=colombia_chik)%>% dplyr::mutate(Disease="Chikungunya")
  colombia_cutleish_monthly_incidence<-Calc_Colombia_Monthly_Incidence(disease=colombia_cutleish)%>% dplyr::mutate(Disease="Cutaneous Leishmaniasis")
  colombia_mucleish_monthly_incidence<-Calc_Colombia_Monthly_Incidence(disease=colombia_mucleish)%>% dplyr::mutate(Disease="Mucosal Leishmaniasis")
  colombia_viscleish_monthly_incidence<-Calc_Colombia_Monthly_Incidence(disease=colombia_viscleish)%>% dplyr::mutate(Disease="Visceral Leishmaniasis")
  colombia_dengue_monthly_incidence<-Calc_Colombia_Monthly_Incidence(disease=colombia_dengue_all)%>% dplyr::mutate(Disease="Dengue")
  colombia_malaria_monthly_incidence<-Calc_Colombia_Monthly_Incidence(disease=colombia_malaria_all)%>% dplyr::mutate(Disease="Malaria")
  colombia_yellowfever_monthly_incidence<-Calc_Colombia_Monthly_Incidence(disease=colombia_yellowfever)%>% dplyr::mutate(Disease="Yellow Fever")
  colombia_zika_monthly_incidence<-Calc_Colombia_Monthly_Incidence(disease=colombia_zika)%>% dplyr::mutate(Disease="Zika")
  
  #Combine Colombia across diseases
  colombia_monthly_incidence<-rbind(colombia_chik_monthly_incidence, colombia_cutleish_monthly_incidence,
                                   colombia_mucleish_monthly_incidence, colombia_viscleish_monthly_incidence, 
                                   colombia_dengue_monthly_incidence,colombia_malaria_monthly_incidence, 
                                   colombia_yellowfever_monthly_incidence, colombia_zika_monthly_incidence)
  
  #Save Colombia Monthly Incidence as csv
  write_csv(colombia_monthly_incidence, "Data/1_DataProcessing/Incidence/colombia_monthly_incidence.csv")
  
  
  ##Brazil
  #Function to calculate monthly incidence per disease for Brazil
  Calc_Brazil_Monthly_Incidence<-function(disease){
    result<-disease%>% 
      dplyr::select(-Muni) %>% #Use Muni_Name as better formatted
      group_by(Muni_Code, Muni_Name, codigo_ibg, Year, Month) %>% #Group by Muni/codigo and Year/Month
      summarize(Cases=sum(Cases, na.rm=TRUE)) %>% #Monthly Cases-sum across weeks
      ungroup() %>% #ungroup
      left_join(brazil_pop, by=c("codigo_ibg", "Year")) %>%#Left join for population (pop has 2020)
      dplyr::mutate(Incidence=Cases/Population)#Calculate Monthly Incidence
    return(result)
  }
  
  #Apply function by disease
  brazil_dengue_monthly_incidence<-Calc_Brazil_Monthly_Incidence(disease=brazil_dengue)%>% dplyr::mutate(Disease="Dengue")
  brazil_malaria_monthly_incidence<-Calc_Brazil_Monthly_Incidence(disease=brazil_malaria)%>% dplyr::mutate(Disease="Malaria")
  brazil_yellowfever_monthly_incidence<-Calc_Brazil_Monthly_Incidence(disease=brazil_yellowfever)%>% dplyr::mutate(Disease="Yellow Fever")
  brazil_viscleish_monthly_incidence<-Calc_Brazil_Monthly_Incidence(disease=brazil_viscleish)%>% dplyr::mutate(Disease="Visceral Leishmaniasis")
  # brazil_cutleish_0106_annual_incidence<-Calc_Brazil_Annual_Incidence(disease=brazil_cutleish_0106) #Remove- no monthly cutleish 2001-2006, only annual
  brazil_cutleish_0718_monthly_incidence<-Calc_Brazil_Monthly_Incidence(disease=brazil_cutleish_0718)%>% dplyr::mutate(Disease="Cutaneous Leishmaniasis")
  
  #Combine Brazil across diseases
  brazil_monthly_incidence<-rbind(brazil_dengue_monthly_incidence, brazil_malaria_monthly_incidence,
                                 brazil_yellowfever_monthly_incidence,brazil_viscleish_monthly_incidence,
                                 brazil_cutleish_0718_monthly_incidence)
  
  #Save Brazil Monthly Incidence as csv
  write_csv(brazil_monthly_incidence, "Data/1_DataProcessing/Incidence/brazil_monthly_incidence.csv")
  
  
  
  #Combine data across countries, and put in wide format for incidence (format/rename to rbind)
  ###########################
  peru_monthly_incidence_format<-peru_monthly_incidence %>% 
    dplyr::rename(Code=Dist_Code, Name=District_Name) %>% 
    dplyr::mutate(Country="Peru")
  colombia_monthly_incidence_format<-colombia_monthly_incidence %>% 
    dplyr::rename(Code=Muni_Code, Name=Muni_Name) %>% 
    dplyr::mutate(Country="Colombia")
  brazil_monthly_incidence_format<-brazil_monthly_incidence %>% 
    dplyr::rename(Code=codigo_ibg, Name=Muni_Name) %>% 
    dplyr::select(-Muni_Code) %>% #Muni_code is codigo_ibg with last character cut
    dplyr::mutate(Country="Brazil")
  monthly_incidence<-rbind(peru_monthly_incidence_format, colombia_monthly_incidence_format, brazil_monthly_incidence_format)
  
  write_csv(monthly_incidence, "Data/1_DataProcessing/Disease/monthly_incidence.csv")#Write csv
  
  #Wide format for case data
  monthly_cases_wide<-monthly_incidence %>% 
    dplyr::select(-Incidence) %>% #Only Have Cases go wide by Disease, Code/Name/Year/Population will be the same
    spread(Disease, Cases)  
  write_csv(monthly_cases_wide, "Data/1_DataProcessing/Disease/monthly_cases_wide.csv")#Write csv
  
  #Wide format for incidence data
  monthly_incidence_wide<-monthly_incidence %>% 
    dplyr::select(-Cases) %>% #Only Have Incidence go wide by Disease, Code/Name/Year/Population will be the same
    spread(Disease, Incidence)
  write_csv(monthly_incidence_wide, "Data/1_DataProcessing/Disease/monthly_incidence_wide.csv")#Write csv
  