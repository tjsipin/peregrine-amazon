#Clean Colombia Disease/Health Data

#Data Provided by Andy MacDonald from the Colombian Surveillance Data

#Saved Colombia 2007-2017 excel files as single sheet csv files
#For 2007-2016 only saved the rutinaria_year sheet, which has the case counts per municipio week and year
#Those files should be able to be read in together and rbinded
#2017 in different format- took the eventos_municipios pivot table, had all the codes/etc. repeat, and the paste special on a new sheet
#Saved this new sheet, also saved sheet with the code/code names to be attached, since only codes on the other sheet

#Packages
####################################################
library(tidyverse)
library(lubridate)
library(sf)
library(stringi)

#Create directory to save all formatted csvs
####################################################
dir.create("~/AmazonMalaria/Data/1_DataProcessing/Disease/Colombia/Colombia_Disease/")

#Read in Colombia Shapefile to subset districts (make district ID list)
####################################################
colombia<-sf::read_sf("~/AmazonMalaria/Data/Raw/Municipios/Municipios_Projected_v4/MGN_MPIO_POLITICO_2018_EPSG3857.shp") %>% 
  dplyr::select(MPIO_CCNCT, MPIO_CNMBR) %>% dplyr::rename(Muni_Code=MPIO_CCNCT, Muni_Name=MPIO_CNMBR) %>% 
  sf::st_set_geometry(NULL) %>% distinct() #Remove 2 MPIOS duplicates

#Read in Original Data
####################################################
#Read in 2007 (similar to 2008-2016)
rutinaria_2007<-read_csv("~/AmazonMalaria/Data/1_DataProcessing/Disease/Colombia/Surveillance_data/rutinaria_2007.csv")

#Read in and rbind 2008-2016 (all same columns/headers)
#(except made semana lowercase first letter for 2012 and 2015)
filenames <- list.files(path="~/AmazonMalaria/Data/1_DataProcessing/Disease/Colombia/Surveillance_data/",
                        pattern="*.csv", full.names = TRUE)
filenames_08_16<-filenames[2:10] #Remove 2007, and 2017 files
rutinaria_08_16 <- do.call("rbind",lapply(filenames_08_16,FUN=function(files){ read_csv(files)}))

#Read in 2017 (different format) with codes
rutinaria_2017<-read_csv("~/AmazonMalaria/Data/1_DataProcessing/Disease/Colombia/Surveillance_data/rutinaria_2017.csv")
rutinaria_2017_codes<-read_csv("~/AmazonMalaria/Data/1_DataProcessing/Disease/Colombia/Surveillance_data/rutinaria_2017_codes.csv")

#Read in 2018 (simiar to 2008-2016) 
rutinaria_2018 <- read_csv("~/AmazonMalaria/Data/1_DataProcessing/Disease/Colombia/Surveillance_data/rutinaria_2018.csv")

#Read in 2019 (similar to 2017)
rutinaria_2019 <- read_csv("~/AmazonMalaria/Data/1_DataProcessing/Disease/Colombia/Surveillance_data/rutinaria_2019.csv",
                           skip = 3)

#Read in 2020
rutinaria_2020 <- read_csv("~/AmazonMalaria/Data/1_DataProcessing/Disease/Colombia/Surveillance_data/rutinaria_2020.csv",
                           skip = 5)

#Read in 2021
rutinaria_2021 <- read_csv("~/AmazonMalaria/Data/1_DataProcessing/Disease/Colombia/Surveillance_data/rutinaria_2021.csv",
                           skip = 2)


#Wrangle Data
####################################################
#Format 2007 and 2008-2016 so they can be combined, and clean-up
#Remove semana column from 08-16
rutinaria_08_16<-rutinaria_08_16 %>% dplyr::select(-semana_)
#Rename Datos as conteo for 07
rutinaria_2007<-rutinaria_2007 %>% dplyr::rename(conteo=Datos)
#Rbind the files
rutinaria_07_16<-rbind(rutinaria_2007,rutinaria_08_16)
#Rename columns
colnames(rutinaria_07_16)<-c("Event_Code", "Event_Name", "Dept_Code", "Dept", 
                             "Muni_Code", "Muni", "Week", "Year", "Cases")
#Make week columns numeric, and double check no NA
rutinaria_07_16<-rutinaria_07_16 %>% 
  dplyr::mutate(Week=as.numeric(Week)) %>% 
  na.omit() #Double check (there were no NA)

#Format 2017 so same format as 2007-2016, and add in code names
#Join 2017 with code names (rename first column as COD_EVE to match)
colnames(rutinaria_2017)[1]<-"COD_EVE"
colnames(rutinaria_2017_codes)[2]<-"Nombre"
rutinaria_2017_codes<-rutinaria_2017_codes %>% distinct() #Only distinct codes
#Join with code names, and then format so matches 07-16
rutinaria_17<-left_join(rutinaria_2017, rutinaria_2017_codes, by="COD_EVE") %>% #Join with codenames
  dplyr::select(COD_EVE, Nombre, everything()) %>% #Select code name column before all else
  gather(Semana, Cases, Semana_01:Semana_52) %>% #Wide to long format
  separate(Semana, c("Semana", "Week")) %>% #Make separate week column
  dplyr::mutate(Week=as.numeric(Week),
                COD_MUN_O=as.character(COD_MUN_O),
                Year=2017) %>% #Add year column, and make code columns numeric and muni character
  dplyr::select(-"Grand Total", -Semana) %>% #Remove grand total and old semana column
  select(-Cases,everything()) %>% #Move cases to end
  na.omit() #Got rid of rows with NA- were NA values for Muni and Cases
colnames(rutinaria_17)<-c("Event_Code", "Event_Name", "Dept_Code", "Dept", 
                          "Muni_Code", "Muni", "Week", "Year", "Cases")
#String pad Muni_Code with zeros on left so 5 digits, and combine with Dept_Code for 5 digit muni
str(rutinaria_17)
rutinaria_17<- rutinaria_17 %>% 
  dplyr::mutate(Dept_Code=str_pad(Dept_Code, 2, side = c("left"), pad = 0),
                Muni_Code=str_pad(Muni_Code, 3, side = c("left"), pad = 0),
                Muni_Code=paste0(Dept_Code,Muni_Code))

#Format 2018 so same format as 2007-2016, and add in code names
#Rename Evento as Nombre, Cod_departamento as COD_DPTO, and cod_municipio as Cod_municipio 
#remove mun and Titulo_semana 
#add ANO 
#change encoding of municipio from UTF-8 to unknown (?)
rutinaria_18 <- rutinaria_2018 %>% 
  dplyr::rename(Nombre = Evento,
                COD_DPTO = Cod_departamento,
                Cod_municipio = cod_municipio) %>% 
  dplyr::select(-c(mun, Titulo_semana)) %>% 
  dplyr::mutate(ANO = 2018) 


#Rename columns
colnames(rutinaria_18)<-c("Event_Code", "Event_Name", "Week", "Dept", 
                          "Muni", "Cases", "Dept_Code", "Muni_Code", "Year")
#Make week columns numeric, and double check no NA
rutinaria_18<-rutinaria_18 %>% 
  dplyr::mutate(Week=as.numeric(Week)) %>% 
  na.omit() %>% #Double check (there were no NA)
  dplyr::select(names(rutinaria_07_16)) #Rearrange names to match rutinaria_07_16

#Format 2019
#Rename COD_DPTO_O as COD_DPTO, NDEP_PROCE as Departamento, COD_MUN_O as Cod_municipio, NMUN_PROCE as Municipio, and NOM_EVE as Nombre
#Format so matches with 07-16
rutinaria_19 <- rutinaria_2019 %>% 
  rename(COD_DPTO = COD_DPTO_O,
         Departamento = NDEP_PROCE,
         Cod_municipio = COD_MUN_O,
         Municipio = NMUN_PROCE,
         Nombre = NOM_EVE) %>% 
  dplyr::select(COD_EVE, Nombre, everything()) %>% #Select code name column before all else
  gather(Semana, Cases, SEMANA_01:SEMANA_52) %>% #Wide to long format
  separate(Semana, c("Semana", "Week")) %>% #Make separate week column
  dplyr::mutate(Week=as.numeric(Week),
                Cod_municipio=as.character(Cod_municipio),
                Year=2019) %>% #Add year column, and make code columns numeric and muni character
  dplyr::select(-X59, -Semana) %>% #Remove grand total and old semana column
  select(-Cases,everything()) %>% #Move cases to end
  na.omit() #Got rid of rows with NA- were NA values for Muni and Cases
colnames(rutinaria_19)<-c("Event_Code", "Event_Name", "Dept_Code", "Dept", 
                          "Muni_Code", "Muni", "Week", "Year", "Cases")
#String pad Muni_Code with zeros on left so 5 digits, and combine with Dept_Code for 5 digit muni
str(rutinaria_19)
rutinaria_19<- rutinaria_19 %>% 
  dplyr::mutate(Dept_Code=str_pad(Dept_Code, 2, side = c("left"), pad = 0),
                Muni_Code=str_pad(Muni_Code, 3, side = c("left"), pad = 0),
                Muni_Code=paste0(Dept_Code,Muni_Code))

#Format 2020
#Rename COD_DPTO_O as COD_DPTO, NDEP_PROCE as Departamento, COD_MUN_O as Cod_municipio, NMUN_PROCE as Municipio, and NOM_EVE as Nombre
#Format so matches with 07-16
rutinaria_20 <- rutinaria_2020 %>% 
  rename(COD_DPTO = COD_DPTO_O,
         Departamento = NDEP_PROCE,
         Cod_municipio = COD_MUN_O,
         Municipio = NMUN_PROCE,
         Nombre = NOM_EVE) %>% 
  dplyr::select(COD_EVE, Nombre, everything()) %>% #Select code name column before all else
  gather(Semana, Cases, SEMANA_01:SEMANA_53) %>% #Wide to long format
  separate(Semana, c("Semana", "Week")) %>% #Make separate week column
  dplyr::mutate(Week=as.numeric(Week),
                Cod_municipio=as.character(Cod_municipio),
                Year=2020) %>% #Add year column, and make code columns numeric and muni character
  dplyr::select(-'Grand Total', -Semana) %>% #Remove grand total and old semana column
  select(-Cases,everything()) %>% #Move cases to end
  na.omit() #Got rid of rows with NA- were NA values for Muni and Cases
colnames(rutinaria_20)<-c("Event_Code", "Event_Name", "Dept_Code", "Dept", 
                          "Muni_Code", "Muni", "Week", "Year", "Cases")
#String pad Muni_Code with zeros on left so 5 digits, and combine with Dept_Code for 5 digit muni
str(rutinaria_20)
rutinaria_20<- rutinaria_20 %>% 
  dplyr::mutate(Dept_Code=str_pad(Dept_Code, 2, side = c("left"), pad = 0),
                Muni_Code=str_pad(Muni_Code, 3, side = c("left"), pad = 0),
                Muni_Code=paste0(Dept_Code,Muni_Code))

#Format 2021 (similar to 2017)
rutinaria_21 <- rutinaria_2021 %>% 
  rename(COD_DPTO = COD_DPTO_O,
         Departamento = Departamento_Ocurrencia,
         Cod_municipio = COD_MUN_O,
         Municipio = Municipio_Ocurrencia,
         Nombre = evento) %>% 
  dplyr::select(COD_EVE, Nombre, everything()) %>% #Select code name column before all else
  gather(Semana, Cases, Semana_1:Semana_53) %>% #Wide to long format
  separate(Semana, c("Semana", "Week")) %>% #Make separate week column
  dplyr::mutate(Week=as.numeric(Week),
                Cod_municipio=as.character(Cod_municipio),
                Year=2021) %>% #Add year column, and make code columns numeric and muni character
  dplyr::select(-'Grand Total', -Semana) %>% #Remove grand total and old semana column
  select(-Cases,everything()) %>% #Move cases to end
  na.omit() #Got rid of rows with NA- were NA values for Muni and Cases
colnames(rutinaria_21)<-c("Event_Code", "Event_Name", "Dept_Code", "Dept", 
                          "Muni_Code", "Muni", "Week", "Year", "Cases")

#String pad Muni_Code with zeros on left so 5 digits, and combine with Dept_Code for 5 digit muni
str(rutinaria_21)
rutinaria_21<- rutinaria_21 %>% 
  dplyr::mutate(Dept_Code=str_pad(Dept_Code, 2, side = c("left"), pad = 0),
                Muni_Code=str_pad(Muni_Code, 3, side = c("left"), pad = 0),
                Muni_Code=paste0(Dept_Code,Muni_Code))

#Combine all years data
rutinaria<-rbind(rutinaria_07_16, rutinaria_17, rutinaria_18, rutinaria_19, rutinaria_20, rutinaria_21) %>% arrange(Event_Code, Muni_Code, Week, Year) %>% 
  mutate_if(is.character, ~gsub('[^ -~]', '', .)) %>% #Fix invalid encoding issues
  dplyr::mutate(Muni_Code=str_pad(Muni_Code, 5, side = c("left"), pad = 0)) %>%#Ensure all Muni Codes 5 digits
  dplyr::filter(Dept_Code!="01"&Dept_Code!="00"&Dept_Code!="0") %>% #Remove 01- which are exterior areas, 0/00 which are unknown provence
  dplyr::filter(!grepl("MUNICIPIO DESCONOCIDO",Muni)) #Remove unknown municipios

#Separate data into different vector-borne diseases (one per dataset)
chikungunya<-rutinaria %>% filter(Event_Name=="CHIKUNGUNYA")
dengue<- rutinaria %>% filter(Event_Name=="DENGUE") 
dengue_grave<- rutinaria %>% filter(Event_Name== "DENGUE GRAVE") 
cutaneous_leishmaniasis<-rutinaria %>% filter(Event_Name=="LEISHMANIASIS CUTANEA")
mucosal_leishmaniasis<-rutinaria %>% filter(Event_Name=="LEISHMANIASIS MUCOSA")
visceral_leishmaniasis<-rutinaria %>% filter(Event_Name=="LEISHMANIASIS VISCERAL")
malaria_asociada<-rutinaria %>% filter(Event_Name=="MALARIA ASOCIADA (FORMAS MIXTAS)")                                   
malaria<-rutinaria %>% filter(Event_Name=="MALARIA")                                                      
malaria_pfal<-rutinaria %>% filter(Event_Name=="MALARIA FALCIPARUM")                                                
malaria_malarie<-rutinaria %>% filter(Event_Name=="MALARIA MALARIE")                                                     
malaria_vivax<-rutinaria %>% filter(Event_Name=="MALARIA VIVAX")                                                       
malaria_complicada<-rutinaria %>% filter(Event_Name=="MALARIA COMPLICADA") 
malaria_mortalidad<-rutinaria %>% filter(Event_Name=="MORTALIDAD POR MALARIA") 
zika<-rutinaria %>% filter(Event_Name=="ZIKA")
yellowfever<-rutinaria %>% filter(Event_Name=="FIEBRE AMARILLA")  


#Create function to balance each disease
clean_disease_data<-function(data, name){
  
  #Create balanced panel of weeks, years, and muni codes to add cases to
  #53 weeks, # years based on available
  #Muni based on colombia muni shapefile
  uniq_Muni_Code<-unique(colombia$Muni_Code)
  # uniq_Year<-sort(unique(data$Year)) #Change due to no years
  uniq_Year <- min(unique(data$Year)):max(unique(data$Year))
  df_bal<-data.frame()
  for (i in 1:length(uniq_Muni_Code)){
    list_week <- 1:53 #53 weeks
    list_year<-rep(uniq_Year[1]:uniq_Year[length(uniq_Year)],length(list_week)) %>% sort() #Repeat first year to last year for each week and then sort numerically
    list_muni <- rep(uniq_Muni_Code[i],length(list_week)*length(uniq_Year)) #Number years times 53 weeks
    muni_df <- data.frame(Muni_Code = list_muni, Year = list_year, Week=list_week,stringsAsFactors = FALSE) #combine list in dataframe
    df_bal<-rbind(df_bal, muni_df)#Bind each district
  }
  
  #Join Distinct ID Information to Balanced Dataframe and add Week-Yr Column
  data_all<-data %>% 
    dplyr::select(Muni_Code, Muni) %>% 
    dplyr::distinct()  %>% 
    full_join(df_bal, by=c("Muni_Code"))  %>% #Join first with dataset to get all ID values
    full_join(data, by=c("Muni_Code", "Muni", "Year", "Week"))%>%  #Join with cases to get case numbers and diagnosis type
    dplyr::select(-c(Event_Code, Event_Name, Dept_Code, Dept)) %>% #Not needed
    dplyr::mutate(Week_Yr=lubridate::make_date(Year)+weeks(Week))
  #Balance dataframe by filling NA Cases with 0 (for added Years/Weeks)
  data_all$Cases[is.na(data_all$Cases)] <- 0
  #Join data with Colombia shapefile district list and keep matches
  data_all<-data_all %>% inner_join(colombia, by="Muni_Code") #Remove countries without defined municipios
  #Write Csv
  write_csv(data_all, paste0("~/AmazonMalaria/Data/1_DataProcessing/Disease/Colombia/Colombia_Disease_v4/", name, ".csv"))
  
}#End Function

#Apply function by disease
####################################################
clean_disease_data(data=chikungunya, name="chikungunya")
clean_disease_data(data=dengue, name="dengue")
clean_disease_data(data=dengue_grave, name="dengue_grave")
clean_disease_data(data=cutaneous_leishmaniasis, name="cutaneous_leishmaniasis")
clean_disease_data(data=mucosal_leishmaniasis, name="mucosal_leishmaniasis")
clean_disease_data(data=visceral_leishmaniasis, name="visceral_leishmaniasis")
clean_disease_data(data=malaria_asociada, name="malaria_asociada")
clean_disease_data(data=malaria, name="malaria")
clean_disease_data(data=malaria_pfal, name="malaria_pfal")
clean_disease_data(data=malaria_malarie, name="malaria_malarie")
clean_disease_data(data=malaria_vivax, name="malaria_vivax")
clean_disease_data(data=malaria_complicada, name="malaria_complicada")
clean_disease_data(data=malaria_mortalidad, name="malaria_mortalidad")
clean_disease_data(data=zika, name="zika")
clean_disease_data(data=yellowfever, name="yellowfever")

