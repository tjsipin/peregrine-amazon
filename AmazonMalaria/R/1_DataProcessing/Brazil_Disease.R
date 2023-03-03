# Edited by TJ Sipin Feb 24, 2023
  # Get full 2020-21 data

#Clean Brazil Disease/Health Data

#Data downloaded from the Brazilian Ministry of Health here:
  #https://datasus.saude.gov.br/informacoes-de-saude-tabnet/

#Data download information:
  #Went to the epidemiology drop down, and went to the notifiable disease 2001-2006, and 2007, onwards links
  #Downloaded data for the following diseases: dengue, cutaneous and visceral leishmanesis, yellow fever, and malaria
  #For each disease downloaded monthly data for each year available, except for cutaneous leish. 
    #2001-2006 where only years and not months available
  #For dengue and both leish. downloaded by municipality of residence for the cases, while 
    #for yellow fever and malaria used municipality of infection.
    #Did this as the website said to use these to calculate incidence for the specific disease
    #(Analyze the incidence by infection site vs residence)
  #There was no yellow fever 2012 data- said no records available, possible no cases that year
    #Made assumption that there were no cases, and filled in zeros
  #Did not download 2020 malaria data, would not be complete
  #Merged with muni shapefile at end to only keep munis with spatial data (remove ignorado, no muni cases)
  
#Will clean up each disease separately, since some have slightly different format


#Packages
####################################################
library(tidyverse)
library(sf)

#Create directory to save all formatted csvs
####################################################
dir.create("~/AmazonMalaria/Data/1_DataProcessing/Disease/Brazil/Brazil_Disease/")

#Read in Brazil Shapefile to subset districts (make district ID list)
####################################################
brazil<-sf::read_sf("~/AmazonMalaria/Data/Raw/Municipios/Municipios_Projected_v4/municipios_2010_EPSG3857.shp") %>% 
  dplyr::select(codigo_ibg, nome) %>% dplyr::rename(Muni_Name=nome) %>% #Select needed columns/rename
  dplyr::mutate(Muni_Code=str_sub(codigo_ibg, start=1, end=6)) %>% #Make column with just 6 digit muni code (match with disease data)
  dplyr::select(Muni_Code, everything()) %>% #Move muni_code column to front (still unique)
  sf::st_set_geometry(NULL) #Remove geometry

#Clean-up disease data for each disease
####################################################
#Dengue
########################
#Monthly csvs 2001-2017, with data available for all months, first 4 lines text
#2012-2017 have extra column Ign included that needs to be dealt with
years<-2001:2017
dengue_df<-data.frame()
for(y in years){
  year<-y #Define year
  skipnum<-ifelse(year<=2006, 4, 3) #Define how many rows to skip with read_csv (4 for 2001-2006, 3 for 2007 onwards)
  dengue_raw<-read_csv(file=paste0("Y:/AmazonMalaria/Data/1_DataProcessing/Disease/Brazil/MinistryofHealth/dengue/dengue_",
                                   year, ".csv"), skip=skipnum)[1] #Read in raw data, skipping first 3-4 rows, and only selecting first column
  #Set separation colnames based on years (2012-2017 has extra column)
  if(year<2012){
    dengue<-dengue_raw %>% separate(1, into=c("Municipio", "Jan", "Feb", "Mar", "Apr", "May",
                         "Jun", "Jul", "Aug", "Sep", "Oct", 
                         "Nov", "Dec", "Total"), sep=";") #Separate by ; into columns-Municipios and Months
  }else if (year>=2012){
    dengue<-dengue_raw %>% separate(1, into=c("Municipio", "Ign", "Jan", "Feb", "Mar", "Apr", "May",
                         "Jun", "Jul", "Aug", "Sep", "Oct", 
                         "Nov", "Dec", "Total"), sep=";") %>% #Separate by ; into columns-Municipios and Months
      dplyr::select(-Ign) #Remove Ign (only 2012-2017)
  }
    
  dengue<-dengue %>% na.omit() %>% #Omit rows with NA for months(extra text rows)
    dplyr::select(-Total) %>% dplyr::filter(Municipio!="Total") %>% #Remove total columns
    dplyr::mutate(Muni_Code=str_sub(Municipio, start=1, end=6),
                  Muni=str_sub(Municipio, start=7, end=-1)) %>% #Make separate Muni Code and Name columns
    dplyr::select(-Municipio) %>% dplyr:: select(Muni_Code, Muni, everything()) %>% #move those first
    gather(Month, Cases, Jan:Dec) %>% #Gather month and cases into long format
    mutate(Year=year, #Add year column
           Cases=gsub("-", 0, Cases), #Substitute dashes for 0
           Cases=as.numeric(Cases)) %>%  #Make cases numeric
    dplyr::select(-Cases, everything())#Move cases to the end
  dengue_df<-rbind(dengue_df, dengue)
}

#Create balanced panel of months, years, and muni codes to add cases to
  #12 months by name, use years defined above, and total brazil muni from shapefile
  uniq_Muni_Code<-unique(brazil$Muni_Code) #All possible muni
  uniq_Year<-years
  df_bal<-data.frame()
  for (i in 1:length(uniq_Muni_Code)){
    list_month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    list_year<-rep(uniq_Year[1]:uniq_Year[length(uniq_Year)],length(list_month)) %>% sort() #Repeat first year to last year for each month and then sort numerically
    list_muni <- rep(uniq_Muni_Code[i],length(list_month)*length(uniq_Year)) #Number years times 53 weeks
    muni_df <- data.frame(Muni_Code = list_muni, Year = list_year, Month=list_month, stringsAsFactors = FALSE) #combine list in dataframe
    df_bal<-rbind(df_bal, muni_df)#Bind each muni
  }
#Join Muni ID Information to Balanced Dataframe
  data_all<-dengue_df %>% 
    dplyr::select(Muni_Code, Muni) %>% 
    dplyr::distinct()%>% 
    full_join(df_bal, by=c("Muni_Code")) %>% #Join first with dataset to get all ID values
    full_join(dengue_df, by=c("Muni_Code", "Muni", "Year", "Month"))  #Join with cases to get case numbers and diagnosis type
#Balance dataframe by filling NA Cases with 0 (for added Years/Months) and NA Diagnosis with None
  data_all$Cases[is.na(data_all$Cases)] <- 0
#Join data with Brazil shapefile district list and keep matches
  dengue_final<-data_all %>% inner_join(brazil, by="Muni_Code") #Remove countries without defined municipios
#Write Csv
  write_csv(dengue_final, "~/AmazonMalaria/Data/1_DataProcessing/Disease/Brazil/Brazil_Disease/dengue.csv")


#Visceral Leishmaniasis
########################
#Monthly csvs 2001-2018, with data available for all months, first 3 lines text for all years, no ign columns
years<-2001:2018
vis_leish_df<-data.frame()
for(y in years){
  year<-y #Define year
  vis_leish_raw<-read_csv(file=paste0("Y:/AmazonMalaria/Data/1_DataProcessing/Disease/Brazil/MinistryofHealth/visceral_leishmaniasis/visceral_leishmaniasis_",
                                   year, ".csv"), skip=3)[1] #Read in raw data, skipping first 3 rows, and only selecting first column
  vis_leish<-vis_leish_raw %>% 
    separate(1, into=c("Municipio", "Jan", "Feb", "Mar", "Apr", "May",
                       "Jun", "Jul", "Aug", "Sep", "Oct", 
                       "Nov", "Dec", "Total"), sep=";") %>% #Separate by ; into columns-Municipios and Months
    na.omit() %>% #Omit rows with NA for months(extra text rows)
    dplyr::select(-Total) %>% dplyr::filter(Municipio!="Total") %>% #Remove total columns
    dplyr::mutate(Muni_Code=str_sub(Municipio, start=1, end=6),
                  Muni=str_sub(Municipio, start=7, end=-1)) %>% #Make separate Muni Code and Name columns
    dplyr::select(-Municipio) %>% dplyr:: select(Muni_Code, Muni, everything()) %>% #move those first
    gather(Month, Cases, Jan:Dec) %>% #Gather month and cases into long format
    mutate(Year=year, #Add year column
           Cases=gsub("-", 0, Cases), #Substitute dashes for 0
           Cases=as.numeric(Cases)) %>%  #Make cases numeric
    dplyr::select(-Cases, everything())#Move cases to the end
  vis_leish_df<-rbind(vis_leish_df, vis_leish)
}

#Create balanced panel of months, years, and muni codes to add cases to
#12 months by name, use years defined above, and total brazil muni from shapefile
  uniq_Muni_Code<-unique(brazil$Muni_Code) #All possible muni
  uniq_Year<-years
  df_bal<-data.frame()
  for (i in 1:length(uniq_Muni_Code)){
    list_month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    list_year<-rep(uniq_Year[1]:uniq_Year[length(uniq_Year)],length(list_month)) %>% sort() #Repeat first year to last year for each month and then sort numerically
    list_muni <- rep(uniq_Muni_Code[i],length(list_month)*length(uniq_Year)) #Number years times 53 weeks
    muni_df <- data.frame(Muni_Code = list_muni, Year = list_year, Month=list_month, stringsAsFactors = FALSE) #combine list in dataframe
    df_bal<-rbind(df_bal, muni_df)#Bind each muni
  }
#Join Muni ID Information to Balanced Dataframe
data_all<-vis_leish_df %>% 
  dplyr::select(Muni_Code, Muni) %>% 
  dplyr::distinct()%>% 
  full_join(df_bal, by=c("Muni_Code")) %>% #Join first with dataset to get all ID values
  full_join(vis_leish_df, by=c("Muni_Code", "Muni", "Year", "Month"))  #Join with cases to get case numbers and diagnosis type
#Balance dataframe by filling NA Cases with 0 (for added Years/Months) and NA Diagnosis with None
data_all$Cases[is.na(data_all$Cases)] <- 0
#Join data with Brazil shapefile district list and keep matches
vis_leish_final<-data_all %>% inner_join(brazil, by="Muni_Code") #Remove countries without defined municipios
#Write Csv
write_csv(vis_leish_final, "~/AmazonMalaria/Data/1_DataProcessing/Disease/Brazil/Brazil_Disease/visceral_leishmaniasis.csv")


#Cutaneous Leishmaniasis
########################
#Yearly csvs 2001-2006, with data available for each year, first 3 lines text, no Ign column 
#Monthly csvs 2007-2018, with data available for all months, first 3 lines text for all years, no Ign columns
#Clean-up yearly first, then monthly, save as two different data frames

#Yearly
  cut_leish_raw_01_06<-read_csv(file="Y:/AmazonMalaria/Data/1_DataProcessing/Disease/Brazil/MinistryofHealth/cutaneous_leishmaniasis/cutaneous_leishmaniasis_2001_2006.csv", 
                        skip=3)[1] #Read in raw data, skipping first 3 rows, and only selecting first column
  cut_leish_01_06<-cut_leish_raw_01_06 %>% 
    separate(1, into=c("Municipio", "2001", "2002", "2003", "2004", 
                       "2005", "2006", "Total"), sep=";") %>% #Separate by ; into columns-Municipios and Months
    na.omit() %>% #Omit rows with NA for months(extra text rows)
    dplyr::select(-Total) %>% dplyr::filter(Municipio!="Total")%>% #Remove total columns
    dplyr::mutate(Muni_Code=str_sub(Municipio, start=1, end=6),
                  Muni=str_sub(Municipio, start=7, end=-1)) %>% #Make separate Muni Code and Name columns
    dplyr::select(-Municipio) %>% dplyr:: select(Muni_Code, Muni, everything()) %>% #move those first
    gather(Year, Cases, "2001":"2006") %>% #Gather month and cases into long format
    mutate(Year=as.numeric(Year), #Make year numeric
           Cases=gsub("-", 0, Cases), #Substitute dashes for 0
           Cases=as.numeric(Cases)) %>%  #Make cases numeric
    dplyr::select(-Cases, everything())#Move cases to the end

  
  #Create balanced panel of years, and muni codes to add cases to
  #Use years defined above, and total brazil muni from shapefile
  uniq_Muni_Code<-unique(brazil$Muni_Code) #All possible muni
  df_bal<-data.frame()
  for (i in 1:length(uniq_Muni_Code)){
    list_year<-2001:2006
    list_muni <- rep(uniq_Muni_Code[i],length(list_year)) #Muni for each year
    muni_df <- data.frame(Muni_Code = list_muni, Year = list_year, stringsAsFactors = FALSE) #combine list in dataframe
    df_bal<-rbind(df_bal, muni_df)#Bind each muni
  }
  #Join Muni ID Information to Balanced Dataframe
  data_all<-cut_leish_01_06 %>% 
    dplyr::select(Muni_Code, Muni) %>% 
    dplyr::distinct()%>% 
    full_join(df_bal, by=c("Muni_Code")) %>% #Join first with dataset to get all ID values
    full_join(cut_leish_01_06, by=c("Muni_Code", "Muni", "Year"))  #Join with cases to get case numbers and diagnosis type
  #Balance dataframe by filling NA Cases with 0 (for added Years) and NA Diagnosis with None
  data_all$Cases[is.na(data_all$Cases)] <- 0
  #Join data with Brazil shapefile district list and keep matches
  cut_leish_final<-data_all %>% inner_join(brazil, by="Muni_Code") #Remove countries without defined municipios
#Write Csv
  write_csv(cut_leish_final, "~/AmazonMalaria/Data/1_DataProcessing/Disease/Brazil/Brazil_Disease/cutaneous_leishmaniasis_2001_2006.csv")


#Monthly
years<-2007:2018
cut_leish_df<-data.frame()
for(y in years){
  year<-y #Define year
  cut_leish_raw<-read_csv(file=paste0("Y:/AmazonMalaria/Data/1_DataProcessing/Disease/Brazil/MinistryofHealth/cutaneous_leishmaniasis/cutaneous_leishmaniasis_",
                                      year, ".csv"), skip=3)[1] #Read in raw data, skipping first 3 rows, and only selecting first column
  cut_leish<-cut_leish_raw %>% 
    separate(1, into=c("Municipio", "Jan", "Feb", "Mar", "Apr", "May",
                       "Jun", "Jul", "Aug", "Sep", "Oct", 
                       "Nov", "Dec", "Total"), sep=";") %>% #Separate by ; into columns-Municipios and Months
    na.omit() %>% #Omit rows with NA for months(extra text rows)
    dplyr::select(-Total) %>% dplyr::filter(Municipio!="Total") %>% #Remove total columns
    dplyr::mutate(Muni_Code=str_sub(Municipio, start=1, end=6),
                  Muni=str_sub(Municipio, start=7, end=-1)) %>% #Make separate Muni Code and Name columns
    dplyr::select(-Municipio) %>% dplyr:: select(Muni_Code, Muni, everything()) %>% #move those first
    gather(Month, Cases, Jan:Dec) %>% #Gather month and cases into long format
    mutate(Year=year, #Add year column
           Cases=gsub("-", 0, Cases), #Substitute dashes for 0
           Cases=as.numeric(Cases)) %>%  #Make cases numeric
    dplyr::select(-Cases, everything())#Move cases to the end
  cut_leish_df<-rbind(cut_leish_df, cut_leish)
}

#Create balanced panel of months, years, and muni codes to add cases to
#12 months by name, use years defined above, and total brazil muni from shapefile
  uniq_Muni_Code<-unique(brazil$Muni_Code) #All possible muni
  uniq_Year<-years
  df_bal<-data.frame()
  for (i in 1:length(uniq_Muni_Code)){
    list_month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    list_year<-rep(uniq_Year[1]:uniq_Year[length(uniq_Year)],length(list_month)) %>% sort() #Repeat first year to last year for each month and then sort numerically
    list_muni <- rep(uniq_Muni_Code[i],length(list_month)*length(uniq_Year)) #Number years times 53 weeks
    muni_df <- data.frame(Muni_Code = list_muni, Year = list_year, Month=list_month, stringsAsFactors = FALSE) #combine list in dataframe
    df_bal<-rbind(df_bal, muni_df)#Bind each muni
  }
#Join Muni ID Information to Balanced Dataframe
  data_all<-cut_leish_df %>% 
    dplyr::select(Muni_Code, Muni) %>% 
    dplyr::distinct()%>% 
    full_join(df_bal, by=c("Muni_Code")) %>% #Join first with dataset to get all ID values
    full_join(cut_leish_df, by=c("Muni_Code", "Muni", "Year", "Month"))  #Join with cases to get case numbers and diagnosis type
#Balance dataframe by filling NA Cases with 0 (for added Years/Months) and NA Diagnosis with None
  data_all$Cases[is.na(data_all$Cases)] <- 0
#Join data with Brazil shapefile district list and keep matches
  cut_leish_final<-data_all %>% inner_join(brazil, by="Muni_Code") #Remove countries without defined municipios
#Write Csv
  write_csv(cut_leish_final, "~/AmazonMalaria/Data/1_DataProcessing/Disease/Brazil/Brazil_Disease/cutaneous_leishmaniasis_2007_2018.csv")


#Malaria
########################
#Monthly csvs 2001-2018, with data available for all months, first 3 lines text for all years
  #Ign column for years 2007-2011, all other years do not have Ign column (Ign/Em Branco) (removed if no month information)
years<-2001:2019
malaria_df<-data.frame()
for(y in years){
  year<-y #Define year
  malaria_raw<-read_csv(file=paste0("Y:/AmazonMalaria/Data/1_DataProcessing/Disease/Brazil/MinistryofHealth/malaria/malaria_",
                                   year, ".csv"), skip=3)[1] #Read in raw data, skipping first 3 rows, and only selecting first column
  #Set separation colnames based on years (2012-2017 has extra column)
  if(year<2007|year>2011){
    malaria<-malaria_raw %>% separate(1, into=c("Municipio", "Jan", "Feb", "Mar", "Apr", "May",
                                              "Jun", "Jul", "Aug", "Sep", "Oct", 
                                              "Nov", "Dec", "Total"), sep=";") #Separate by ; into columns-Municipios and Months
  }else if (year>=2007&year<=2011){
    malaria<-malaria_raw %>% separate(1, into=c("Municipio", "Ign", "Jan", "Feb", "Mar", "Apr", "May",
                                              "Jun", "Jul", "Aug", "Sep", "Oct", 
                                              "Nov", "Dec", "Total"), sep=";") %>% #Separate by ; into columns-Municipios and Months
      dplyr::select(-Ign) #Remove Ign (only 2012-2017)
  }
  
  malaria<-malaria %>% na.omit() %>% #Omit rows with NA for months(extra text rows)
    dplyr::select(-Total) %>% dplyr::filter(Municipio!="Total") %>% #Remove total columns
    dplyr::mutate(Muni_Code=str_sub(Municipio, start=1, end=6),
                  Muni=str_sub(Municipio, start=7, end=-1)) %>% #Make separate Muni Code and Name columns
    dplyr::select(-Municipio) %>% dplyr:: select(Muni_Code, Muni, everything()) %>% #move those first
    gather(Month, Cases, Jan:Dec) %>% #Gather month and cases into long format
    mutate(Year=year, #Add year column
           Cases=gsub("-", 0, Cases), #Substitute dashes for 0
           Cases=as.numeric(Cases)) %>%  #Make cases numeric
    dplyr::select(-Cases, everything())#Move cases to the end
  malaria_df<-rbind(malaria_df, malaria)
}

#Create balanced panel of months, years, and muni codes to add cases to
#12 months by name, use years defined above, and total brazil muni from shapefile
  uniq_Muni_Code<-unique(brazil$Muni_Code) #All possible muni
  uniq_Year<-years
  df_bal<-data.frame()
  for (i in 1:length(uniq_Muni_Code)){
    list_month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    list_year<-rep(uniq_Year[1]:uniq_Year[length(uniq_Year)],length(list_month)) %>% sort() #Repeat first year to last year for each month and then sort numerically
    list_muni <- rep(uniq_Muni_Code[i],length(list_month)*length(uniq_Year)) #Number years times 53 weeks
    muni_df <- data.frame(Muni_Code = list_muni, Year = list_year, Month=list_month, stringsAsFactors = FALSE) #combine list in dataframe
    df_bal<-rbind(df_bal, muni_df)#Bind each muni
  }
#Join Muni ID Information to Balanced Dataframe
data_all<-malaria_df %>% 
  dplyr::select(Muni_Code, Muni) %>% 
  dplyr::distinct()%>% 
  full_join(df_bal, by=c("Muni_Code")) %>% #Join first with dataset to get all ID values
  full_join(malaria_df, by=c("Muni_Code", "Muni", "Year", "Month"))  #Join with cases to get case numbers and diagnosis type
#Balance dataframe by filling NA Cases with 0 (for added Years/Months) and NA Diagnosis with None
data_all$Cases[is.na(data_all$Cases)] <- 0
#Join data with Brazil shapefile district list and keep matches
malaria_final<-data_all %>% inner_join(brazil, by="Muni_Code") #Remove countries without defined municipios
  #For malaria, there are quite a few rows removed because they do not report municipio- and have quite a few cases
#Write Csv
write_csv(malaria_final, "~/AmazonMalaria/Data/1_DataProcessing/Disease/Brazil/Brazil_Disease/malaria.csv")


#Yellow Fever
########################
#Monthly csvs 2001-2016, no records found for 2012
  #First 3 lines text for all years, and no IGN columns
  #Data not available for all months, different months per year
    #2001: Jan, Feb, Mar, May, Jun, Jul, Aug, Nov, Total
    #2002: Jan, Feb, Mar, May, Dec, Total 
    #2003: Jan, Feb, Mar, Apr, Aug, Dec, Total 
    #2004: Mar, Jun, Nov, Total 
    #2005: Jan, Jun, Aug, Dec, Total
    #2006: Feb, Mar, Total
    #2007: Feb, Mar, Apr, Jun, Sep, Dec, Total
    #2008: Jan, Feb, Mar, Apr, May, Jun, Dec, Total
    #2009: Jan, Feb, Mar, Apr, May, Jun, Aug, Sep, Dec, Total
    #2010: Jan, Feb, Jun, Total
    #2011: Jan, May, Total
    #2012: No Records
    #2013: Mar, Aug, Total
    #2014: May, Total
    #2015: Jan, Feb, Mar, May, Jul, Dec, Total
    #2016: Jan, Mar, Apr, May, Jul, Dec, Total
    

years<-c(2001:2011, 2013:2016)
yellowfever_df<-data.frame()
for(y in years){
  year<-y #Define year
  yellowfever_raw<-read_csv(file=paste0("Y:/AmazonMalaria/Data/1_DataProcessing/Disease/Brazil/MinistryofHealth/yellowfever/yellowfever_",
                                    year, ".csv"), skip=3)[1] #Read in raw data, skipping first 3 rows, and only selecting first column
  #Set separation colnames based on years (2012-2017 has extra column)
  if(year==2001){  #2001: Jan, Feb, Mar, May, Jun, Jul, Aug, Nov, Total
    yellowfever<-yellowfever_raw %>% separate(1, into=c("Municipio", "Jan", "Feb", "Mar", "May","Jun", "Jul", "Aug", 
                                                        "Nov", "Total"), 
                                              sep=";") %>%  #Separate by ; into columns-Municipios and Months
      na.omit() %>% #Omit rows with NA for months(extra text rows)
      dplyr::select(-Total) %>% dplyr::filter(Municipio!="Total") %>% #Remove total columns
      gather(Month, Cases, Jan:Nov) #Gather month and cases into long format
  }else if (year==2002){  #2002: Jan, Feb, Mar, May, Dec, Total 
    yellowfever<-yellowfever_raw %>% separate(1, into=c("Municipio", "Jan", "Feb", "Mar", "May","Dec", "Total"), 
                                              sep=";") %>% #Separate by ; into columns-Municipios and Months
      na.omit() %>% #Omit rows with NA for months(extra text rows)
      dplyr::select(-Total) %>% dplyr::filter(Municipio!="Total") %>% #Remove total columns
      gather(Month, Cases, Jan:Dec) #Gather month and cases into long format
  }else if (year==2003){  #2003: Jan, Feb, Mar, Apr, Aug, Dec, Total 
    yellowfever<-yellowfever_raw %>% separate(1, into=c("Municipio", "Jan", "Feb", "Mar", "Apr", "Aug", "Dec", "Total"), 
                                              sep=";") %>% #Separate by ; into columns-Municipios and Months
      na.omit() %>% #Omit rows with NA for months(extra text rows)
      dplyr::select(-Total) %>% dplyr::filter(Municipio!="Total") %>% #Remove total columns
      gather(Month, Cases, Jan:Dec) #Gather month and cases into long format
  }else if (year==2004){  #2004: Mar, Jun, Nov, Total 
    yellowfever<-yellowfever_raw %>% separate(1, into=c("Municipio", "Mar", "Jun", "Nov", "Total"), 
                                              sep=";") %>% #Separate by ; into columns-Municipios and Months
      na.omit() %>% #Omit rows with NA for months(extra text rows)
      dplyr::select(-Total) %>% dplyr::filter(Municipio!="Total") %>% #Remove total columns
      gather(Month, Cases, Mar:Nov) #Gather month and cases into long format
  }else if (year==2005){  #2005: Jan, Jun, Aug, Dec, Total
    yellowfever<-yellowfever_raw %>% separate(1, into=c("Municipio", "Jan", "Jun",  "Aug", "Dec", "Total"), 
                                              sep=";") %>% #Separate by ; into columns-Municipios and Months
      na.omit() %>% #Omit rows with NA for months(extra text rows)
      dplyr::select(-Total) %>% dplyr::filter(Municipio!="Total") %>% #Remove total columns
      gather(Month, Cases, Jan:Dec) #Gather month and cases into long format
  }else if (year==2006){  #2006: Feb, Mar, Total
    yellowfever<-yellowfever_raw %>% separate(1, into=c("Municipio", "Feb", "Mar", "Total"), 
                                              sep=";") %>% #Separate by ; into columns-Municipios and Months
      na.omit() %>% #Omit rows with NA for months(extra text rows)
      dplyr::select(-Total) %>% dplyr::filter(Municipio!="Total") %>% #Remove total columns
      gather(Month, Cases, Feb:Mar) #Gather month and cases into long format
  }else if (year==2007){  #2007: Feb, Mar, Apr, Jun, Sep, Dec, Total
    yellowfever<-yellowfever_raw %>% separate(1, into=c("Municipio", "Feb", "Mar", "Apr", "Jun", "Sep", "Dec", "Total"), 
                                              sep=";") %>% #Separate by ; into columns-Municipios and Months
      na.omit() %>% #Omit rows with NA for months(extra text rows)
      dplyr::select(-Total) %>% dplyr::filter(Municipio!="Total") %>% #Remove total columns
      gather(Month, Cases, Feb:Dec) #Gather month and cases into long format
  }else if (year==2008){  #2008: Jan, Feb, Mar, Apr, May, Jun, Dec, Total
    yellowfever<-yellowfever_raw %>% separate(1, into=c("Municipio", "Jan", "Feb", "Mar", "Apr", "May","Jun", 
                                                        "Dec", "Total"), 
                                              sep=";") %>% #Separate by ; into columns-Municipios and Months
      na.omit() %>% #Omit rows with NA for months(extra text rows)
      dplyr::select(-Total) %>% dplyr::filter(Municipio!="Total") %>% #Remove total columns
      gather(Month, Cases, Jan:Dec) #Gather month and cases into long format
  }else if (year==2009){  #2009: Jan, Feb, Mar, Apr, May, Jun, Aug, Sep, Dec, Total
    yellowfever<-yellowfever_raw %>% separate(1, into=c("Municipio", "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                        "Aug", "Sep", "Dec", "Total"), 
                                              sep=";") %>% #Separate by ; into columns-Municipios and Months
      na.omit() %>% #Omit rows with NA for months(extra text rows)
      dplyr::select(-Total) %>% dplyr::filter(Municipio!="Total") %>% #Remove total columns
      gather(Month, Cases, Jan:Dec) #Gather month and cases into long format
  }else if (year==2010){  #2010: Jan, Feb, Jun, Total
    yellowfever<-yellowfever_raw %>% separate(1, into=c("Municipio", "Jan", "Feb", "Jun", "Total"), 
                                              sep=";") %>% #Separate by ; into columns-Municipios and Months
      na.omit() %>% #Omit rows with NA for months(extra text rows)
      dplyr::select(-Total) %>% dplyr::filter(Municipio!="Total") %>% #Remove total columns
      gather(Month, Cases, Jan:Jun) #Gather month and cases into long format
  }else if (year==2011){  #2011: Jan, May, Total
    yellowfever<-yellowfever_raw %>% separate(1, into=c("Municipio", "Jan", "May","Total"), 
                                              sep=";") %>% #Separate by ; into columns-Municipios and Months
      na.omit() %>% #Omit rows with NA for months(extra text rows)
      dplyr::select(-Total) %>% dplyr::filter(Municipio!="Total") %>% #Remove total columns
      gather(Month, Cases, Jan:May) #Gather month and cases into long format
  }else if (year==2013){  #2013: Mar, Aug, Total
    yellowfever<-yellowfever_raw %>% separate(1, into=c("Municipio",  "Mar", "Aug", "Total"), 
                                              sep=";") %>% #Separate by ; into columns-Municipios and Months
      na.omit() %>% #Omit rows with NA for months(extra text rows)
      dplyr::select(-Total) %>% dplyr::filter(Municipio!="Total") %>% #Remove total columns
      gather(Month, Cases, Mar:Aug) #Gather month and cases into long format
  }else if (year==2014){  #2014: May, Total
    yellowfever<-yellowfever_raw %>% separate(1, into=c("Municipio", "May", "Total"), 
                                              sep=";") %>% #Separate by ; into columns-Municipios and Months
      na.omit() %>% #Omit rows with NA for months(extra text rows)
      dplyr::select(-Total) %>% dplyr::filter(Municipio!="Total") %>% #Remove total columns
      gather(Month, Cases, May) #Gather month and cases into long format
  }else if (year==2015){ #2015: Jan, Feb, Mar, May, Jul, Dec, Total
    yellowfever<-yellowfever_raw %>% 
      separate(1, into=c("Municipio", "Jan", "Feb", "Mar", "May","Jul", "Dec", "Total"), 
                                              sep=";") %>% #Separate by ; into columns-Municipios and Months
      na.omit() %>% #Omit rows with NA for months(extra text rows)
      dplyr::select(-Total) %>% dplyr::filter(Municipio!="Total") %>% #Remove total columns
      gather(Month, Cases, Jan:Dec) #Gather month and cases into long format
  }else if (year==2016){  #2016: Jan, Mar, Apr, May, Jul, Dec, Total
    yellowfever<-yellowfever_raw %>% separate(1, into=c("Municipio", "Jan", "Mar", "Apr", "May", "Jul", "Dec", "Total"), 
                                              sep=";") %>% #Separate by ; into columns-Municipios and Months
    na.omit() %>% #Omit rows with NA for months(extra text rows)
    dplyr::select(-Total) %>% dplyr::filter(Municipio!="Total") %>% #Remove total columns
    gather(Month, Cases, Jan:Dec) #Gather month and cases into long format
  }
  
  #Rest of formatting
  yellowfever<-yellowfever %>% 
    dplyr::mutate(Muni_Code=str_sub(Municipio, start=1, end=6),
                  Muni=str_sub(Municipio, start=7, end=-1)) %>% #Make separate Muni Code and Name columns
    dplyr::select(-Municipio) %>% dplyr:: select(Muni_Code, Muni, everything()) %>% 
    mutate(Year=year, #Add year column
           Cases=gsub("-", 0, Cases), #Substitute dashes for 0
           Cases=as.numeric(Cases)) %>%  #Make cases numeric
    dplyr::select(-Cases, everything())#Move cases to the end
  #Combine with other years
 yellowfever_df<-rbind(yellowfever_df, yellowfever)
}

#Create balanced panel of months, years, and muni codes to add cases to
#12 months by name, use years defined above, and total brazil muni from shapefile
  uniq_Muni_Code<-unique(brazil$Muni_Code) #All possible muni
  uniq_Year<-2001:2016 #2012 no records, but assuming no cases that year
  df_bal<-data.frame()
  for (i in 1:length(uniq_Muni_Code)){
    list_month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    list_year<-rep(uniq_Year[1]:uniq_Year[length(uniq_Year)],length(list_month)) %>% sort() #Repeat first year to last year for each month and then sort numerically
    list_muni <- rep(uniq_Muni_Code[i],length(list_month)*length(uniq_Year)) #Number years times 53 weeks
    muni_df <- data.frame(Muni_Code = list_muni, Year = list_year, Month=list_month, stringsAsFactors = FALSE) #combine list in dataframe
    df_bal<-rbind(df_bal, muni_df)#Bind each muni
  }
#Join Muni ID Information to Balanced Dataframe
data_all<-yellowfever_df %>% 
  dplyr::select(Muni_Code, Muni) %>% 
  dplyr::distinct()%>% 
  full_join(df_bal, by=c("Muni_Code")) %>% #Join first with dataset to get all ID values
  full_join(yellowfever_df, by=c("Muni_Code", "Muni", "Year", "Month"))  #Join with cases to get case numbers and diagnosis type
#Balance dataframe by filling NA Cases with 0 (for added Years/Months) and NA Diagnosis with None
  data_all$Cases[is.na(data_all$Cases)] <- 0
#Join data with Brazil shapefile district list and keep matches
  yellowfever_final<-data_all %>% inner_join(brazil, by="Muni_Code") #Remove countries without defined municipio
#Write Csv
  write_csv(yellowfever_final, "~/AmazonMalaria/Data/1_DataProcessing/Disease/Brazil/Brazil_Disease/yellowfever.csv")

