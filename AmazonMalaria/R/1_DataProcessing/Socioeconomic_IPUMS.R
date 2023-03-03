#Clean IPUMS Census Data and Summarize

#Census data from IPUMS
#GIS: https://international.ipums.org/international/gis_yrspecific_2nd.shtml
#Census: https://international.ipums.org/international-action/variables/group

#2005 Colombia Census, 2007 Peru Census, and 2010 Brazil Census

#Packages
####################################################
library(tidyverse) #Data wrangling
library(sf) #shapefiles
library(purrr)#reduce function
options(scipen = 999) #no scientific notation

#Read in Shapefiles to have district/municipios names
####################################################
peru<-sf::read_sf("Data/Raw/Municipios/Municipios_Projected/Peru_Municipios_projected.shp")%>% 
  dplyr::select(IDDIST, NOMBDIST) %>% dplyr::rename(Code=IDDIST, Name=NOMBDIST) 

colombia<-sf::read_sf("Data/Raw/Municipios/Municipios_Projected/Colombia_Municipios_projected.shp") %>% 
  dplyr::select(MPIOS, NOMBRE_MPI) %>% dplyr::rename(Code=MPIOS, Name=NOMBRE_MPI) 

brazil<-sf::read_sf("Data/Raw/Municipios/Municipios_Projected/Brazil_Municipios_projected.shp")%>% 
  dplyr::select(codigo_ibg, nome) %>% dplyr::rename(Code=codigo_ibg, Name=nome)


#Read in GIS files for IPUMS data
####################################################
peru_province<-read_sf('Data/1_DataProcessing/Socioeconomic/IPUMS/GIS/geo2_pe2007/geo2_pe2007.shp') %>% 
  st_transform(crs=3857)#EPSG 3857 used for all 3 countries
colombia_muni<-read_sf('Data/1_DataProcessing/Socioeconomic/IPUMS/GIS/geo2_co2005/geo2_co2005.shp') %>% 
  st_transform(crs=3857)
brazil_muni<-read_sf('Data/1_DataProcessing/Socioeconomic/IPUMS/GIS/geo2_br2010/geo2_br2010.shp') %>% 
  st_transform(crs=3857)


#Read in Census Data for IPUMS
################################################
brazil_census<-read_csv("Data/1_DataProcessing/Socioeconomic/IPUMS/Census/Brazil/ipumsi_00009.csv")
colombia_census<-read_csv("Data/1_DataProcessing/Socioeconomic/IPUMS/Census/Colombia/ipumsi_00010.csv")
peru_census<-read_csv("Data/1_DataProcessing/Socioeconomic/IPUMS/Census/Peru/ipumsi_00011.csv")


#Data wrangle
################################################
#Clean-up census so same columns for all 3 countries
  brazil_census_df<-brazil_census %>%  
    dplyr::select(GEO2_BR2010, SERIAL, HHWT, URBAN, ELECTRIC, WATSUP,PERWT, LIT, EDATTAIN) %>% 
    dplyr::rename(IPUMCode=GEO2_BR2010) %>% dplyr::mutate(Country="Brazil")
  colombia_census_df<-colombia_census %>%  
    dplyr::select(GEO2_CO2005, SERIAL, HHWT, URBAN, ELECTRIC, WATSUP,PERWT, LIT, EDATTAIN) %>% 
    dplyr::rename(IPUMCode=GEO2_CO2005) %>% dplyr::mutate(Country="Colombia")
  peru_census_df<-peru_census %>%  
    dplyr::select(GEO2_PE2007, SERIAL, HHWT, URBAN, ELECTRIC, WATSUP,PERWT, LIT, EDATTAIN) %>% 
    dplyr::rename(IPUMCode=GEO2_PE2007) %>% dplyr::mutate(Country="Peru")
  census_df<-rbind(brazil_census_df, colombia_census_df, peru_census_df)

#Separate into household and person datasets
  #Household
  household<-census_df %>% 
    dplyr::select(Country, IPUMCode, SERIAL, HHWT, URBAN, ELECTRIC, WATSUP) %>% 
    distinct() #Each unique household
  #Person
  person<-census_df %>% 
    dplyr::select(Country, IPUMCode, SERIAL, PERWT, LIT, EDATTAIN) #Keep each person (not distinct)
  
#Calculate each of the household and person datasets in %
  #Urban:Household
  urbanrural<-household %>% 
    dplyr::select(Country, IPUMCode, SERIAL, HHWT, URBAN) %>% 
    group_by(Country, IPUMCode, URBAN) %>% 
    summarize(HHWT=sum(HHWT)) %>% #Add household weights for each Muni ranking
    ungroup() %>% 
    dplyr::mutate(URBAN=as.character(URBAN),
                  URBAN=ifelse(URBAN=="1", "Rural",
                               ifelse(URBAN=="2", "Urban", NA))) %>% 
    spread(URBAN, HHWT) %>% #Spread values by Rural and Urban status
    dplyr::mutate(Rural = replace_na(Rural, 0),
                  Urban = replace_na(Urban, 0),#Replace NA with 0 (No amount rural/urban if NA)
                  PerUrban=Urban/(Urban+Rural),
                  PerRural=Rural/(Urban+Rural))%>% #Calculate Percent Urban and Rural 
    dplyr::select(Country,IPUMCode, PerRural) #keep percent rural
  
  #Electric:Household
  electric<-household %>% 
    dplyr::select(Country, IPUMCode, SERIAL, HHWT, ELECTRIC) %>% 
    group_by(Country, IPUMCode, ELECTRIC) %>% 
    summarize(HHWT=sum(HHWT)) %>% #Add household weights for each Muni ranking
    ungroup()%>% 
    dplyr::mutate(ELECTRIC=as.character(ELECTRIC),
                  ELECTRIC=ifelse(ELECTRIC=="0", "NIU", #Not in Universe
                             ifelse(ELECTRIC=="1", "YesElectric",
                                    ifelse(ELECTRIC=="2", "NoElectric", NA)))) %>% 
    spread(ELECTRIC, HHWT)  %>% #Spread values by Electricity Status
    dplyr::mutate(NIU = replace_na(NIU, 0),
                  YesElectric = replace_na(YesElectric, 0),
                  NoElectric = replace_na(NoElectric, 0),#Replace NA with 0 
                  PerYesElectric=YesElectric/(YesElectric+NoElectric+NIU),
                  PerNoElectric=NoElectric/(YesElectric+NoElectric+NIU)) %>% #Calculate Percent Electricity
    dplyr::select(Country, IPUMCode, PerNoElectric) #keep percent no electricity
  
  #Water Supply:Household
  watersupply<-household %>% 
    dplyr::select(Country, IPUMCode, SERIAL, HHWT, WATSUP) %>% 
    group_by(Country, IPUMCode, WATSUP) %>% 
    summarize(HHWT=sum(HHWT)) %>% #Add household weights for each Muni ranking
    ungroup() %>% 
    dplyr::mutate(WATSUP=as.character(WATSUP),
                  WATSUP=ifelse(WATSUP=="0", "NIU", #Not in Universe
                            ifelse(WATSUP=="10", "YesPiped",
                                ifelse(WATSUP=="11", "PipedInside",
                                    ifelse(WATSUP=="15", "PipedOutsideDwelling",
                                      ifelse(WATSUP=="16", "PipedBuilding", 
                                        ifelse(WATSUP=="18", "PublicPiped",
                                          ifelse(WATSUP=="20", "NoPiped", NA)))))))) %>%
    spread(WATSUP, HHWT)  %>% #Spread values by Water Supply Status
    dplyr::mutate(NIU = replace_na(NIU, 0),
                  YesPiped = replace_na(YesPiped, 0),
                  PipedInside = replace_na(PipedInside, 0),
                  PipedOutsideDwelling = replace_na(PipedOutsideDwelling, 0),
                  PipedBuilding = replace_na(PipedBuilding, 0),
                  PublicPiped = replace_na(PublicPiped, 0),
                  NoPiped = replace_na(NoPiped, 0),#Replace NA with 0
                  PerNoPipedWater=NoPiped/(NoPiped+PipedInside+PipedBuilding+NIU+
                                             YesPiped+PipedOutsideDwelling+PublicPiped)) %>% #Calculate Percent no piped water
    dplyr::select(Country, IPUMCode, PerNoPipedWater)#Keep percent no piped water
     
  #Literacy: Person
  literacy<-person %>% 
    dplyr::select(Country, IPUMCode, SERIAL, PERWT, LIT) %>% 
    group_by(Country, IPUMCode, LIT) %>% 
    summarize(PERWT=sum(PERWT)) %>% #Sum person weights for each MUNI
    ungroup() %>% 
    dplyr::mutate(LIT=as.character(LIT),
                  LIT=ifelse(LIT=="0", "NIU", #Not in Universe
                          ifelse(LIT=="1", "Illiterate",
                               ifelse(LIT=="2", "Literate", 
                                      ifelse(LIT=="9","UnknownLit",NA))))) %>% 
    spread(LIT, PERWT) %>% 
    dplyr::mutate(NIU = replace_na(NIU, 0),
                  Illiterate = replace_na(Illiterate, 0),
                  Literate = replace_na(Literate, 0),
                  UnknownLit = replace_na(UnknownLit, 0),
                  PerIlliterate=Illiterate/(Literate+Illiterate+NIU+UnknownLit)) %>% #Calculate literacy percentages, leaving out NIU 
    dplyr::select(Country, IPUMCode, PerIlliterate)#Keep percent illiterate
  
  #Education Attained: Person
  education<-person %>% 
    dplyr::select(Country, IPUMCode, SERIAL, PERWT, EDATTAIN) %>% 
    group_by(Country, IPUMCode, EDATTAIN) %>% 
    summarize(PERWT=sum(PERWT)) %>% #Sum person weights for each MUNI
    ungroup() %>% 
    dplyr::mutate(EDATTAIN=as.character(EDATTAIN),
                  EDATTAIN=ifelse(EDATTAIN=="0", "NIU", #Not in Universe
                              ifelse(EDATTAIN=="1", "LessPrimary", #Less than primary
                                ifelse(EDATTAIN=="2", "Primary", #2-4 schooling completed
                                    ifelse(EDATTAIN=="3", "Secondary", 
                                           ifelse(EDATTAIN=="4", "University",
                                               ifelse(EDATTAIN=="9", "Unknown",NA))))))) %>%
    spread(EDATTAIN, PERWT) %>% 
    dplyr::mutate(NIU = replace_na(NIU, 0),
                  LessPrimary = replace_na(LessPrimary, 0),
                  Primary = replace_na(Primary, 0),
                  Secondary = replace_na(Secondary, 0),
                  University = replace_na(University, 0),
                  Unknown = replace_na(Unknown, 0),
                  PerLessPrimary=LessPrimary/(LessPrimary+Primary+Secondary+University+NIU+Unknown))%>% #Calculate less than primary/no schooling percent
    dplyr::select(Country, IPUMCode, PerLessPrimary) #keep percent less than primary schooling

  
#Create full socioeconomic dataset for all Muni/Districts
########################################################
#Combine all variable datasets
  census_variables<-list(urbanrural, electric, watersupply, 
                       literacy, education) %>% 
      purrr::reduce(full_join, by=c("Country", "IPUMCode"))%>% 
      dplyr::mutate(IPUMCode=str_pad(as.character(IPUMCode), 6, "left", "0"))

#Separate dataset by country
  brazil_census_variables<-census_variables %>% dplyr::filter(Country=="Brazil")
  colombia_census_variables<-census_variables %>% dplyr::filter(Country=="Colombia")
  peru_census_variables<-census_variables %>% dplyr::filter(Country=="Peru")

#Join country census df to IPUMS shapefiles 
  brazil_census_sf<-full_join(brazil_muni, brazil_census_variables, by=c("IPUM2010"="IPUMCode"))
  colombia_census_sf<-full_join(colombia_muni, colombia_census_variables, by=c("IPUM2005"="IPUMCode"))
  peru_census_sf<-full_join(peru_province, peru_census_variables, by=c("IPUM2007"="IPUMCode"))
    #IPUMS GIS has Lake Titicaca (888888), for which there is no census info or area in the full shapefile below
      #Will fill in with Puno Province data (join to final clean dataset in place of NA values)
      peru_puno_amantini<-peru_census_sf %>% dplyr::filter(ADMIN_NAME=="Puno") %>% 
        st_set_geometry(NULL) %>% #Give Lake Titicaca Puno Province Values
        dplyr::select(PerRural, PerNoElectric, PerNoPipedWater, PerIlliterate, PerLessPrimary)%>% 
        dplyr::mutate(Country="Peru",Code="210103",Name="AMANTANI") %>% #Values from full dataset
        dplyr::select(Country, Code, Name, everything())
  
#Join country census data with complete Muni/District shapefiles, keeping join with most overlap
  brazil_census_full<-sf::st_join(brazil, brazil_census_sf, largest = T)
  colombia_census_full<-sf::st_join(colombia, colombia_census_sf, largest = T)
  peru_census_full<-sf::st_join(peru, peru_census_sf, largest = T)

#Clean up country datasets and remove geometry so they can be recombined
  #Can add in ADMIN_NAME if want to see which Muni/Districts were combined
  brazil_census_clean<-brazil_census_full %>% st_set_geometry(NULL) %>% 
    dplyr::select(Country, Code, Name, PerRural, PerNoElectric, 
                  PerNoPipedWater, PerIlliterate, PerLessPrimary) %>% 
    dplyr::mutate(Code=as.character(Code))#All Codes 7 digits-so good
  colombia_census_clean<-colombia_census_full %>% st_set_geometry(NULL) %>% 
    dplyr::select(Country, Code, Name, PerRural, PerNoElectric, 
                  PerNoPipedWater, PerIlliterate, PerLessPrimary) %>% 
    dplyr::filter(!is.na(Country)) %>% distinct() %>% 
    dplyr::mutate(Code=str_pad(as.character(Code), 5, "left", "0"))#Needs to be 5 digits padded with 0 left
        #19809 and 25839 repeated; 19809 NA values only & 25839 identical values 
          #so removed both with is.na and distinct
  peru_census_clean<-peru_census_full %>% st_set_geometry(NULL) %>% 
    dplyr::select(Country, Code, Name, PerRural, PerNoElectric, 
                  PerNoPipedWater, PerIlliterate, PerLessPrimary) %>% 
    dplyr::filter(!is.na(Country))#Remove Amatini
  #Add Puno province values to Amantini (Lake Titicaca) by rbinding to dataframe, and fix Code
  peru_census_clean<-rbind(peru_census_clean,peru_puno_amantini) %>% 
    dplyr::mutate(Code=str_pad(as.character(Code), 6, "left", "0"))#Needs to be 6 digits padded with 0 left


#Combine all datasets together and write csv
final_census<-rbind(brazil_census_clean, colombia_census_clean, peru_census_clean)
write_csv(final_census, "Data/1_DataProcessing/Socioeconomic/IPUMS/Census/socioeconomic_ipums_census.csv")
