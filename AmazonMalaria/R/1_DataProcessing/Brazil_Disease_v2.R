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

files <- list.files("~/AmazonMalaria/Data/Raw/Disease_Data/Brazil")

b2007 <- read_csv("~/AmazonMalaria/Data/Raw/Disease_Data/Brazil/autoctones_especie_mun_full_data_data_2007.csv", locale=locale(encoding="latin1")) %>% 
  mutate_if(is.character, ~gsub(">", "", gsub("<U\\+", "\\\\u", .)) %>% stringi::stri_trans_general("latin-ascii"))

b2007_ <- read.csv("~/AmazonMalaria/Data/Raw/Disease_Data/Brazil/autoctones_especie_mun_full_data_data_2007.csv")

b2007_v2 <- b2007 %>% 
  mutate_if(is.character, ~gsub('[^ -~]', '', .)) #Fix invalid encoding issues
f <- function(x) {
  
  x <- gsub(">", "", gsub("<U\\+", "\\\\u", x))
  stringi::stri_unescape_unicode(x)
}

b2007_v2_2 <- b2007_v3 %>% 
  mutate_if(is.character, ~stringi::stri_trans_general(., "ASCII")) 

b2007_v2_3 <- b2007_ %>% 
  mutate_if(is.character, ~iconv(., "UTF-8"))

write.csv(b2007_v2, "~/AmazonMalaria/Data/Raw/Disease_Data/Brazil/test2.csv", fileEncoding = "UTF-8")

read.csv("~/AmazonMalaria/Data/Raw/Disease_Data/Brazil/test2.csv", fileEncoding = "ISO-8859-1")

stringi::stri_enc_detect(sapply(b2007[1,1],as.character)) 
  
b2007_v3 <- read.csv("~/AmazonMalaria/Data/Raw/Disease_Data/Brazil/autoctones_especie_mun_full_data_data_2007.csv", fileEncoding = "latin1")

file = "~/AmazonMalaria/Data/Raw/Disease_Data/Brazil/autoctones_especie_mun_full_data_data_2007.csv"


b2007_v32 <- b2007_v2_3 %>% 
  mutate_if(is.character, ~as.character(.)[1] %>% utf8ToInt())
b2007_v4 <- read.csv("~/AmazonMalaria/Data/Raw/Disease_Data/Brazil/autoctones_especie_mun_full_data_data_2007_test_v2.csv", encoding = "UTF-8")

b2007_v5 <- read.csv("~/AmazonMalaria/Data/Raw/Disease_Data/Brazil/autoctones_especie_mun_full_data_data_2007_test_v4.csv", fileEncoding = "latin1")



b2007__ <- read.csv("~/AmazonMalaria/Data/Raw/Disease_Data/Brazil/autoctones_especie_mun_full_data_data_2007.csv", fileEncoding = 'latin1') %>% 
  mutate_if(is.character, ~stringi::stri_trans_general("latin-ascii"))

library(data.table)
library(stringi)

df <- b2007

fwrite(df, "temp.csv")

b2007_v6 <- fread("temp.csv", encoding="Latin-1") %>% 
  mutate_if(is.character, ~iconv(., "ISO 8859-1"))

b2007_v7 <- read_csv("~/AmazonMalaria/Data/Raw/Disease_Data/Brazil/b2007.csv", col_names = F, locale = locale(encoding = "latin1")) # this worked...

b2007_v7_2 <- b2007_v7 %>% 
  janitor::row_to_names(1)

b2007_v7_2[, 5] %>% unique()



b2020_v1 <- read_csv("~/AmazonMalaria/Data/Raw/Disease_Data/Brazil/Autoctones_especie_Mun_Full_Data_data_2020.csv") 
