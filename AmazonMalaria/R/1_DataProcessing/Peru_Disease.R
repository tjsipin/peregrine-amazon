#Clean Peru Disease/Health Data

#Data Provided by Andy MacDonald from the Peruvian Ministry of Health


#Packages
####################################################
library(tidyverse)
library(lubridate)
library(sf)

#Read in Original Data
####################################################
chikungunya<-read_csv("Data/1_DataProcessing/Disease/Peru/MinistryofHealth/1TablaCasosSE_data_Chik_all.csv")
dengue<-read_csv("Data/1_DataProcessing/Disease/Peru/MinistryofHealth/1TablaCasosSE_data_Dengue_all.csv")
cutaneous_leishmaniasis<-read_csv("Data/1_DataProcessing/Disease/Peru/MinistryofHealth/1TablaCasosSE_data_Leish_Cut.csv")
mucosal_leishmaniasis<-read_csv("Data/1_DataProcessing/Disease/Peru/MinistryofHealth/1TablaCasosSE_data_Leish_Muc.csv")
pfal_malaria<-read_csv("Data/1_DataProcessing/Disease/Peru/MinistryofHealth/1TablaCasosSE_data_Pfal.csv")     #Only doing Pfal Malaria as Pviv Malaria csv containts same Pfal data
zika<-read_csv("Data/1_DataProcessing/Disease/Peru/MinistryofHealth/1TablaCasosSE_data_Zika_all.csv")

#Read in Peru Shapefile to have nice district names (make district ID list)
####################################################
peru<-sf::read_sf("Data/Raw/Municipios/Municipios_Projected/Peru_Municipios_projected.shp") %>% 
  dplyr::select(IDDIST, NOMBDIST) %>% dplyr::rename(Dist_Code=IDDIST, District_Name=NOMBDIST) %>% 
  sf::st_set_geometry(NULL)

#Create directory to save all formatted csvs
####################################################
dir.create("Data/1_DataProcessing/Disease/Peru/Peru_Disease/")

#Create function to wrangle data for each disease
####################################################
clean_disease_data<-function(data, name){
  #Select needed columns and rename
  data<-data[,c(3,15,19,21,31,33)]
  colnames(data)<-  c("Dist_Code","District","Population","Year","Week","Diagnosis_Type")
  #Calculate cases per grouping and week-yr
  data_cases<-data %>% 
    filter(District!="Importados") %>% #Remove imported cases
    dplyr::mutate(Cases=1) %>% #Make Cases Column (check all confirmed cases first)
    group_by(Dist_Code, District, Population, Year, Week, Diagnosis_Type) %>% 
    summarise(Cases=sum(Cases)) %>% #Summarize cases by grouping
    ungroup() %>% 
    dplyr::select(-Population) %>% #Remove population since we do not have complete data per year
    filter(Year!=2020) %>%  #Remove 2020 since not complete yet
    dplyr::mutate(Dist_Code=str_pad(Dist_Code, 6, side=c("left"), pad=0)) %>% #Format Dist_Code
    na.omit()#get rid of NA Dist_Code, etc. (double check-should be none)
  #Create balanced panel of weeks, years, and district codes to add cases to
  #53 weeks(some report week 53), # years (removed 2020 data)
  #Use total Peru district codes from peru district shapefile
  uniq_Dist_Code<-unique(peru$Dist_Code) #All possible districts
  uniq_Year<-sort(unique(data_cases$Year))
  df_bal<-data.frame()
  for (i in 1:length(uniq_Dist_Code)){
    list_week <- 1:53 #53 weeks
    list_year<-rep(uniq_Year[1]:uniq_Year[length(uniq_Year)],length(list_week)) %>% sort() #Repeat first year to last year for each week and then sort numerically
    list_district <- rep(uniq_Dist_Code[i],length(list_week)*length(uniq_Year)) #Number years times 53 weeks
    district_df <- data.frame(Dist_Code = list_district, Year = list_year, Week=list_week, stringsAsFactors = FALSE) #combine list in dataframe
    df_bal<-rbind(df_bal, district_df)#Bind each district
  }
  #Join Distinct ID Information to Balanced Dataframe and add Week-Yr Column
  data_all<-data_cases %>% 
    dplyr::select(Dist_Code, District) %>% 
    dplyr::distinct()%>% 
    full_join(df_bal, by=c("Dist_Code")) %>% #Join first with dataset to get all ID values
    full_join(data_cases, by=c("Dist_Code", "District", "Year", "Week")) %>%  #Join with cases to get case numbers and diagnosis type
    dplyr::mutate(Week_Yr=lubridate::make_date(Year)+weeks(Week))
  #Balance dataframe by filling NA Cases with 0 (for added Years/Weeks) and NA Diagnosis with None
  data_all$Cases[is.na(data_all$Cases)] <- 0
  data_all$Diagnosis_Type[is.na(data_all$Diagnosis_Type)] <- "None"
  #Join data with Peru shapefile district list and keep matches
  data_all<-data_all %>% inner_join(peru, by="Dist_Code")
  
  #Write Csv
  write_csv(data_all, paste0("Data/1_DataProcessing/Disease/Peru/Peru_Disease/", name, ".csv"))
}#End function

#Apply function
###########################
clean_disease_data(data=chikungunya, name="chikungunya")
clean_disease_data(data=dengue, name="dengue")
clean_disease_data(data=cutaneous_leishmaniasis, name="cutaneous_leishmaniasis")
clean_disease_data(data=mucosal_leishmaniasis, name="mucosal_leishmaniasis")
clean_disease_data(data=pfal_malaria, name="pfal_malaria")
clean_disease_data(data=zika, name="zika")

#Confirmed and Probable cases reported, not combined and not balanced (could have both for district-week-year)