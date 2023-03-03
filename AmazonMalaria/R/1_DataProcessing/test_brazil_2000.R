
# Comment out by parts to check progress

# Part 1

rasterOptions(tmpdir = "Data/RasterTemp", tmptime=1) #temporary raster storage

lc_2000<-raster("Data/Raw/PanAmazonia/PanAmazonia_Projected_v3/PanAmazonia_2000_LULC_v3_Pro.tif")

brazil_muni<-read_sf("Data/1_DataProcessing/LandscapeMetrics/Municipios/Amazonian_Municipios_Projected/Brazil_Amazonian_Municipios_Projected.shp")

#Brazil: Municipios to Loop Through
uniq_brazil_muni<-unique(as.numeric(brazil_muni$codigo_ibg))
#Crop and mask raster to municipios
ptm <- proc.time()#Time process
for(i in 1:length(uniq_brazil_muni)){
  muni<-uniq_brazil_muni[i] #get municipio
  muni_sf<-subset(brazil_muni, brazil_muni$codigo_ibg==muni) #subset municipios dataset to municipio
  municipio <- sf:::as_Spatial(muni_sf)#sf back to sp
  lc_crop<-raster::crop(lc_2000, extent(municipio))#crop land use raster to municipio
  lcmuni<-raster::mask(lc_crop, municipio)#mask land use raster to municipio

  if(length(unique(lcmuni))>1){ #If multiple values, even with background, save raster
    writeRaster(lcmuni, paste0("Data/1_DataProcessing/LandscapeMetrics/PanAmazonia_Municipios/Brazil/lcmuni_2000_test/", muni, ".tif"), overwrite=T)
  }else if(length(unique(lcmuni))==1){ #If one value, save if value is not 0 (background)
    if(unique(lcmuni)>0){
      writeRaster(lcmuni, paste0("Data/1_DataProcessing/LandscapeMetrics/PanAmazonia_Municipios/Brazil/lcmuni_2000_test/", muni, ".tif"), overwrite=T)
    }else if(unique(lcmuni)==0){
      print(paste0(muni, " is only background"))
    }else
      NULL
  }else
    NULL

}#End the for loop

proc.time() - ptm #Stop time (about 2.5 hrs)

#Read in names of rasters in folder and subset shapefile and save-for final muni shapefile
brazil_final_munis<-substring(list.files("Data/1_DataProcessing/LandscapeMetrics/PanAmazonia_Municipios/Brazil/lcmuni_2000_test"), 1, 7) %>%
  as.data.frame(stringsAsFactors=FALSE)
colnames(brazil_final_munis)<-c("codigo_ibg")
brazil_muni_final <- right_join(brazil_muni, brazil_final_munis, by="codigo_ibg")
#Write shapefile
write_sf(brazil_muni_final, "Data/1_DataProcessing/LandscapeMetrics/Municipios/Final_Amazonian_Municipios_Projected_test/Final_Brazil_Amazonian_Municipios_Projected.shp", delete_layer = TRUE)

# Part 2
brazil_municipios<-read_sf("Data/1_DataProcessing/LandscapeMetrics/Municipios/Final_Amazonian_Municipios_Projected_test/Final_Brazil_Amazonian_Municipios_Projected.shp")
lc_2000<-raster("Data/1_DataProcessing/LandscapeMetrics/PanAmazonia/PanAmazonia_Projected/PanAmazonia_2000_LULC_projected.tif")


#Brazil Function
CropMaskBrazil<-function(year, ras){
  ptm = proc.time()
  year<-year
  lc<-ras
  munis<-unique(as.numeric(brazil_municipios$codigo_ibg))#Municipios to loop through

  #Temporary raster directory
  dir.create (file.path("Data/1_DataProcessing/LandscapeMetrics_v3/RasterTmp"), showWarnings = T)  #creates unique filepath for temp directory
  rasterOptions(tmpdir=file.path("Data/1_DataProcessing/LandscapeMetrics_v3/RasterTmp")) #sets temp directory

  registerDoParallel(cores=15)#initiate multiple cores for processing (set to under computer/server limit)
  result<-foreach(c=1:length(munis))%dopar%{

    #Subset land use raster by municipio
    muni<-munis[c] #get municipio
    print(muni)
    muni_sf<-subset(brazil_municipios, brazil_municipios$codigo_ibg==muni) #subset municipios dataset to municipio
    municipio <- sf:::as_Spatial(muni_sf)#sf back to sp
    lc_crop<-raster::crop(lc, extent(municipio))#crop land use raster to municipio
    lcmuni<-raster::mask(lc_crop, municipio)#mask land use raster to municipio

    #Write Raster (none should be empty or all background from earlier analysis)
    writeRaster(lcmuni, paste0("Data/1_DataProcessing/LandscapeMetrics_v3/PanAmazonia_Municipios_test/Brazil/lcmuni_", year, "/", muni, ".tif"),
                overwrite=TRUE, progress = 'text')
  }#End Brazil Function
  unlink(file.path("Data/1_DataProcessing/LandscapeMetrics_v3/RasterTmp"), recursive = TRUE) #removes entire temp directory
}

CropMaskBrazil(2000, lc_2000)

##########TEST############

#Brazil Function
CropMaskBrazil<-function(year, ras){
  ptm = proc.time()
  year<-2000
  lc<-lc_2000
  munis<-unique(as.numeric(brazil_municipios$codigo_ibg))#Municipios to loop through
  
  #Temporary raster directory
  dir.create (file.path("~/AmazonMalaria/Data/1_DataProcessing/LandscapeMetrics_v3/RasterTmp"), showWarnings = T)  #creates unique filepath for temp directory
  rasterOptions(tmpdir=file.path("Data/1_DataProcessing/LandscapeMetrics_v3/RasterTmp")) #sets temp directory
  
  registerDoParallel(cores=8)#initiate multiple cores for processing (set to under computer/server limit)
  result<-foreach(c=1:length(munis))%dopar%{
    
    #Subset land use raster by municipio
    muni<-munis[c] #get municipio
    print(muni)
    muni_sf<-subset(brazil_municipios, brazil_municipios$codigo_ibg==muni) #subset municipios dataset to municipio
    municipio <- sf:::as_Spatial(muni_sf)#sf back to sp
    lc_crop<-raster::crop(lc, extent(municipio))#crop land use raster to municipio
    lcmuni<-raster::mask(lc_crop, municipio)#mask land use raster to municipio
    
    #Write Raster (none should be empty or all background from earlier analysis)
    writeRaster(lcmuni, paste0("Data/1_DataProcessing/LandscapeMetrics_v3/PanAmazonia_Municipios_test/Brazil/lcmuni_", year, "/", muni, ".tif"),
                overwrite=TRUE, progress = 'text')
    
    unlink(file.path("Data/1_DataProcessing/LandscapeMetrics_v3/RasterTmp"), recursive = TRUE) #removes entire temp directory
  }#End Brazil Function
}

CropMaskBrazil(2000, lc_2000)

#########END TEST#########


# Part 3

#Landscape Metrics function calculates landscape metrics for each municipio (all classes and reclasses),
#and saves classmetrics and adjacency matrix dataframes for each municipio
#Performs for each country and year input (loops through crop/mask rasters)

#Landscape Metrics Function
CalculateLandscapeMetrics<-function(country, year){
  ptm <- proc.time()
  country<-country
  year<-year
  lc_munis<-list.files(paste0("Data/1_DataProcessing/LandscapeMetrics_v3/PanAmazonia_Municipios_test/",
                              country, "/lcmuni_", year), full.names=TRUE)
  #Temporary raster directory
  dir.create (file.path("Data/1_DataProcessing/LandscapeMetrics_v3/RasterTmp"), showWarnings = FALSE)#unique filepath for temps
  rasterOptions(tmpdir=file.path("Data/1_DataProcessing/LandscapeMetrics_v3/RasterTmp")) #sets temp directory

  library(raster)
  library(landscapemetrics)

  registerDoParallel(cores=18)#initiate multiple cores for processing (set to under computer/server limit)
  result<-foreach(c=1:length(lc_munis))%dopar%{

    print(paste0(c, '/', length(lc_munis)))

    #Subset and Read in municipio land use raster
    muni<-tools::file_path_sans_ext(basename(lc_munis[c]))
    lcmuni<-raster::raster(lc_munis[c])


    #Fragstat class and landscape metrics
    #Directions/neighborhood=4 is rook's case
    ta<-landscapemetrics::lsm_l_ta(lcmuni, directions=4)#total area of landscape in hectares
    pland<-landscapemetrics::lsm_c_pland(lcmuni, directions = 4)#percentage of landscape
    mna<-landscapemetrics::lsm_c_area_mn(lcmuni, directions = 4)#mean patch area/size in hectares
    el<-landscapemetrics::lsm_c_te(lcmuni, count_boundary = FALSE, directions = 4)#edge length in meters
    enn<-landscapemetrics::lsm_c_enn_mn(lcmuni, directions = 4, verbose = TRUE) #Mean of euclidean nearest-neighbor distance in meters
    #ENN_MN is an ’Aggregation metric’& Simple way to describe patch isolation
    #It summarises each class as the mean of each patch belonging to class i.
    #Distance to the nearest neighbouring patch of the same class i in meters

    adj<-landscapemetrics::get_adjacencies(lcmuni, neighbourhood = 4, what="triangle")#Calculate adjacencies
    #Full matrix, triangle so adjacencies only counted once
    #Adjacency matrix describes configuration of landscape
    #A cellwise count of edges between classes
    #Count the number of times that a cell with a certain value occurs to a cell with a certain value

    #Combine class metrics into dataframe
    classmetrics<-as.data.frame(rbind(ta, pland, mna, el, enn))

    #Adjacency matrix into dataframe
    adjdf<-as.data.frame(adj) %>% #Adjacency matrix to dataframe
      tibble::rownames_to_column(var="Land_Cover_Row") %>% #Rownames to column
      gather(key="Land_Cover_Column", value="EdgeCounts", -Land_Cover_Row) %>% #Other edge types to column
      dplyr::mutate(Land_Cover_Column=str_sub(Land_Cover_Column, start=2))%>%
      #Remove X from Column name, generated by as.data.frame
      na.omit() #Remove rows with NA (NA from using triangle so not doublecounting adjacencies)

    #Save both dataframes for each municipality
    write_csv(classmetrics, paste0("Data/1_DataProcessing/LandscapeMetrics/LandscapeMetrics_v3/PanAmaMuni_allclasses_test/",
                                   country, "/lcmuni_", year, "/classmetrics/", muni, ".csv"))
    write_csv(adjdf, paste0("Data/1_DataProcessing/LandscapeMetrics/LandscapeMetrics_v3/PanAmaMuni_allclasses_test/",
                            country, "/lcmuni_", year, "/adjacencymatrix/", muni, ".csv"))
    #These dataframes will later be joined across all municipios, years, and then countries,
    #and then the metrics will be cleaned-up and brought together

  }#End parallel processing
  unlink(file.path("Data/1_DataProcessing/LandscapeMetrics_v3/RasterTmp"), recursive = TRUE) #removes entire temp directory

  print(proc.time() - ptm)
}#End Landscape Metrics Function

CalculateLandscapeMetrics("Brazil", 2000)

# Part 4

#Combine municipality csvs for classmetrics and adjacencymatrix
#Saving as two csv under year folders with municipality ID column added
library(tidyverse)
library(stringr)
library(purrr)

#Loop through country
countries<-c("Colombia", "Peru", "Brazil")
countries <-c('Brazil') # TRIAL
for (c in countries){
  country<-c
  #Loop through year
  years<-2000:2017
  years <- 2000 # TRIAL
  for (y in years){
    year<-y
    #Loop through classmetrics and adjacency matrix
    metrics<-c("classmetrics", "adjacencymatrix")
    for (m in metrics){
      metric<-m
      #List of municipio files
      list_of_files <- list.files(path = paste0("Data/1_DataProcessing/LandscapeMetrics/LandscapeMetrics_v3/PanAmaMuni_allclasses_test/", country, "/lcmuni_", year, "/", metric),
                                  pattern = ".csv",
                                  full.names = TRUE)
      #Read in files and combine, adding muni_ID column
      df <- list_of_files %>%
        purrr::set_names() %>%
        purrr::map_dfr(~ read_csv(.x, col_types = cols(), col_names = TRUE),.id = "Muni_ID")

      #Ifelse by country for shortening file name
      if(country=="Colombia"){
        df<-df %>% dplyr::mutate(Muni_ID=stringr::str_sub(Muni_ID,-9,-5))
      }else if (country=="Brazil"){
        df<-df %>% dplyr::mutate(Muni_ID=stringr::str_sub(Muni_ID,-11,-5))
      }else if (country=="Peru"){
        df<-df %>% dplyr::mutate(Muni_ID=stringr::str_sub(Muni_ID,-12,-5))
      }else
        NULL

      #Add year and country column
      df<-df %>% dplyr::mutate(Year=year, Country=country) %>% dplyr::select(Muni_ID, Country, Year, everything())

      #Save csv
      write_csv(df, paste0("Data/1_DataProcessing/LandscapeMetrics/LandscapeMetrics_v3/PanAmaMuni_allclasses_test/metric_csv/",metric, "/", country, "_", year, ".csv"))

    }#End metrics for loop
  }#End year for loop
}#End country for loop



#Combine all classmetric and adjacency cvs across years and countries
#Clean up both csvs and save (not combine because a lot of information)

#Class metrics
filelist_cm <- list.files(path = "Data/1_DataProcessing/LandscapeMetrics/LandscapeMetrics_v3/PanAmaMuni_allclasses_test/metric_csv/classmetrics",
                          pattern = ".csv", full.names = TRUE)
cm <- Reduce(rbind, lapply(filelist_cm, read_csv))#Combine all csvs
#Join with classmetrics csv for column headers to gather by
cm_names<-read_csv("Data/1_DataProcessing/LandscapeMetrics/LandscapeMetrics_v3/PanAmaMuni_allclasses/metric_csv/classmetrics_column_names_v3.csv")
  #filter(class %in% (cm$class %>% unique()))
classmetrics<-full_join(cm, cm_names, by=c("metric", "class")) %>%
  dplyr::select(-c(layer, level, class, id, metric)) %>% #Remove unnecessary columns
  tidyr::pivot_wider(names_from = Metric_Name, values_from = value) #long to wide format
  
#Remove rows with NA for Muni_ID and then make all NA values 0
#(land cover type is 0 for that Muni_ID if NA)
classmetrics[is.na(classmetrics)] <- 0 #All NA to 0
classmetrics<-classmetrics %>% filter(Muni_ID!=0) %>% #remove row with Muni=0 (was NA)
  dplyr::select(Muni_ID, Country, Year, Muni_TotalArea, everything()) %>%  #Move ID and total area columns to beginning
  arrange(Country, Muni_ID, Year)
#Write classmetrics csv
write_csv(classmetrics, "Data/1_DataProcessing/LandscapeMetrics/LandscapeMetrics_v3/PanAmaMuni_allclasses_test/metric_csv/classmetrics.csv")

#Adjacency Matrix
filelist_adj <- list.files(path = "Data/1_DataProcessing/LandscapeMetrics/LandscapeMetrics_v3/PanAmaMuni_allclasses_test/metric_csv/adjacencymatrix",
                           pattern = ".csv", full.names = TRUE)
adj <- Reduce(rbind, lapply(filelist_adj, read_csv))#Combine all csvs
adj_names<-read_csv("Data/1_DataProcessing/LandscapeMetrics/LandscapeMetrics_v3/PanAmaMuni_allclasses_test/metric_csv/adjacencymatrix_column_names_v3.csv")
adjacencymatrix<-full_join(adj, adj_names, by=c("Land_Cover_Row", "Land_Cover_Column"))%>%
  dplyr::select(-c(Land_Cover_Row, Land_Cover_Column)) %>% #Remove unnecessary columns
  tidyr::spread(Adjacency_Name, EdgeCounts) %>% #long to wide format
  filter(!is.na(Country))
#Remove rows with NA for Muni_ID and then make all NA values 0
#(land cover type is 0 for that Muni_ID if NA)
adjacencymatrix[is.na(adjacencymatrix)] <- 0 #All NA to 0
adjacencymatrix<-adjacencymatrix %>% filter(Muni_ID!=0) %>% #remove row with Muni=0 (was NA)
  arrange(Country, Muni_ID, Year)
#Write adjacencymatrix csv
write_csv(adjacencymatrix, "Data/1_DataProcessing/LandscapeMetrics/LandscapeMetrics_v3/PanAmaMuni_allclasses_test/metric_csv/adjacencymatrix.csv")


brazil_cutleish_0106 <- brazil_cutleish_0106 %>% 
  mutate(Muni_Code = as.integer(Muni_Code))
(brazil_cutleish_0106 %>% 
    filter(codigo_ibg %in% brazil_muni$codigo_ibg))$codigo_ibg %>% unique() %>% length()

brazil_muni 

opttemp_df_monthly <- read.csv("~/AmazonMalaria/Data/1_DataProcessing/Environmental/Temp/MOD11A2Temp/opttemp_df_monthly.csv")

(opttemp_df_monthly %>% 
    filter(Code %in% brazil_muni$codigo_ibg))$Code %>% unique() %>% length()
