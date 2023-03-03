##################################################################
##                       Author: TJ Sipin                       ##
##                   Start date: Feb 24, 2023                   ##
##################################################################
##################################################################
##################################################################
##                           Packages                           ##
##################################################################

library(tidyverse)
library(dplyr)
library(sf)

##################################################################
##                             Data                             ##
##################################################################

files <- list.files("~/AmazonMalaria/Data/Raw/Disease_Data/Brazil", )

encodingFunc <- function(file){
  res <- read_csv(
    file = paste0(
      "~/AmazonMalaria/Data/Raw/Disease_Data/Brazil/", file), 
    col_names = F, 
    locale = locale(encoding = "latin1")
    ) %>% 
    janitor::row_to_names(1)
  
  # return(write.csv(res, paste0("~/AmazonMalaria/Data/Raw/Disease_Data/Brazil/", str_sub(file, end = -5), "_latin1.csv")))
  
  res
}

b2000 <- encodingFunc(files[1])


