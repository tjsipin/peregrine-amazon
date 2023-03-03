#Clean-up Landscape Metrics CSVS computed on R Studio Server 

#LandscapMetrics.Rmd file performed landscape metrics on PanAmazonian landscape files by country municipio, 
  #including total municipio area, percent land cover, mean path size, edge length, patch isolation, and adjacencies.
  #Performed on R Server because computationally heavy & need landscapemetrics package.
  #LandscapeMetrics.Rmd available under R/1_DataProcessing along with this R script.
#The final output of Landscape.Rmd was 1)csv of class landscape metrics &
  #2)csv of adjacencies between classes (both combined across years and countries)
#Reading them in here and cleaning them up into final desired variables for analysis


#Packages
####################################################
library(tidyverse) #Data wrangling
options(scipen = 999) #no scientific notation


#Read in landscapemetrics csvs, sourced from R server calculations of landscape metrics of Amazonian land cover
####################################################
#Read in classmetrics
classmetrics<-readr::read_csv("Data/1_DataProcessing/LandscapeMetrics/LandscapeMetrics_v3/PanAmaMuni_allclasses/metric_csv/classmetrics.csv",
                              guess_max = min(100000, Inf)) #Guess more to overcome parsing failures
#Read in adjacencymatrix
adjacencymatrix<-readr::read_csv("Data/1_DataProcessing/LandscapeMetrics/LandscapeMetrics_v3/PanAmaMuni_allclasses/metric_csv/adjacencymatrix.csv",
                                 guess_max = min(100000, Inf)) #Guess more to overcome parsing failures


#Clean-Up Dataframes
####################################################

#Columns to keep for landscapemetrics:
  # Muni_TotalArea, pland_forest, pland_floodedforest, pland_wetland, pland_farming, pland_nonveg, pland_riverlakeocean, 
  # area_mn_forest, area_mn_wetland, area_mn_farming, area_mn_nonveg, te_forest, te_wetland, te_farming, te_nonveg, 
  # te_riverlakeocean, enn_mn_forest, enn_mn_wetland, enn_mn_farming, enn_mn_nonveg, enn_mn_riverlakeocean

#Columns to keep for adjacency:
  # Forest_Savanna, Forest_Floodforest, Forest_Wetland, Forest_Farming, Forest_Nonveg, Forest_Riverlakeocean, 
  # Savanna_Floodforest, Savanna_Wetland, Savanna_Farming, Savanna_Nonveg, Savanna_Riverlakeocean, Floodforest_Wetland, 
  # Floodforest_Farming, Floodforest_Nonveg, Floodforest_Riverlakeocean, Wetland_Farming, Wetland_Nonveg, Farming_Nonveg, 
  # Farming_Riverlakeocean, Nonveg_Riverlakeocean

#Going to clean datasets up to only have those columns
classmetrics_df<-classmetrics %>% 
  dplyr::select(Muni_ID, Country, Year,
                Muni_TotalArea, pland_forest, pland_floodedforest, pland_wetland, pland_farming, 
                pland_nonveg, pland_riverlakeocean, area_mn_forest, area_mn_wetland, area_mn_farming, 
                area_mn_nonveg, te_forest, te_wetland, te_farming, te_nonveg, te_riverlakeocean, 
                enn_mn_forest, enn_mn_wetland, enn_mn_farming, enn_mn_nonveg, enn_mn_riverlakeocean) %>% 
  dplyr::rename(Code=Muni_ID) %>% 
  dplyr::mutate(Code=ifelse(Country=="Peru", substr(Code,3,8), Code)) #Have Peru Code without PE at the front

adjacency_df<-adjacencymatrix %>% 
  dplyr::select(Muni_ID, Country, Year,
                Forest_Savanna, Forest_Floodforest, Forest_Wetland, Forest_Farming, Forest_Nonveg, 
                Forest_Riverlakeocean, Savanna_Floodforest, Savanna_Wetland, Savanna_Farming, 
                Savanna_Nonveg, Savanna_Riverlakeocean, Floodforest_Wetland, Floodforest_Farming, 
                Floodforest_Nonveg, Floodforest_Riverlakeocean, Wetland_Farming, Wetland_Nonveg, 
                Farming_Nonveg, Farming_Riverlakeocean, Nonveg_Riverlakeocean) %>% 
  dplyr::rename(Code=Muni_ID) %>% 
  dplyr::mutate(Code=ifelse(Country=="Peru", substr(Code,3,8), Code)) #Have Peru Code without PE at the front


#Will not be "full" datasets aka when joined with other datasets will only be joined with Code/Country/Year with full_join
  #Meanining codes and years with no landscapemetrics (those outside the Amazon Basin) will have NA values


#Write csvs
write_csv(classmetrics_df, "Data/1_DataProcessing/LandscapeMetrics/classmetrics_df_v3.csv")
write_csv(adjacency_df, "Data/1_DataProcessing/LandscapeMetrics/adjacency_df_v3.csv")
