library(rsample)
library(sf)
library(spatialsample)
library(dplyr)

# Data

old_mun <- read_sf("~/peregrine_amazon/data/colombia/shapefiles/Colombia_Amazonian_Municipios_Projected.shp")

data <- read.csv("~/peregrine_amazon/data/full_colombia_monthly_df.csv") %>%
  filter(Code %in% old_mun$MPIOS)

# PanAmazonia municipality coordinates
## QUESTION: Do by country to account for political differences across countries? ####


mun <- read_sf("~/peregrine_amazon/data/colombia/shapefiles/MGN_MPIO_POLITICO_2018_EPSG3857.shp") %>%
  dplyr::rename(Code = MPIO_CCNCT) %>%
  dplyr::filter(Code %in% old_mun$MPIOS)


### spatial_clustering_cv

# 5-fold split procedure
test <- spatial_clustering_cv(mun, v = 3)

splits_df <- c()
for(i in 1:3){

  new_df <- assessment(test$splits[[i]])
  new_df$fold <- i
  new_df <- new_df[,c("Code", "fold")]

  splits_df <- rbind(splits_df, new_df)

}

## drop geometry
splits_df <- st_drop_geometry(splits_df)


data <- merge(data, splits_df, by = "Code")

write.csv(data, "~/peregrine_amazon/data/colombia/full_data_splits.csv")

data %>% dim()

### leave-location out

# 5-fold split procedure
test <- spatial_leave_location_out_cv(mun, v = 3, group = 'geometry')

autoplot(test)

test_v2 <- spatial_block_cv(mun, v = 5) # arbitrary buffer

autoplot(test_v2)

splits_df_v2 <- c()
for(i in 1:5){

  new_df <- assessment(test_v2$splits[[i]])
  new_df$fold <- i
  new_df <- new_df[,c("Code", "fold")]

  splits_df_v2 <- rbind(splits_df_v2, new_df)

}

## drop geometry
splits_df_v2 <- st_drop_geometry(splits_df_v2) %>%
  mutate(Code = as.integer(Code))

## read in full_data_v6
full_data_v6 <- read.csv("~/peregrine_amazon/data/colombia/full_data_colombia_v6.csv") %>%
  select(-fold)

full_data_v7 <- full_data_v6 %>%
  full_join(splits_df_v2, by='Code')

write.csv(full_data_v7, "~/peregrine_amazon/data/colombia/full_data_colombia_v7.csv")
