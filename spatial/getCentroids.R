library(raster)
library(rgeos)
library(sf)

# Data

data <- read.csv("~/peregrine_amazon/data/colombia/colombia_monthly_filtered.csv")

old_mun <- read_sf("~/peregrine_amazon/data/colombia/shapefiles/Colombia_Amazonian_Municipios_Projected.shp")

mun <- read_sf("~/peregrine_amazon/data/colombia/shapefiles/MGN_MPIO_POLITICO_2018_EPSG3857.shp") %>%
  dplyr::rename(MuniCode = MPIO_CCNCT) %>%
  dplyr::filter(MuniCode %in% old_mun$MPIOS)

# get centroid
mun$centroid <- st_transform(mun, 3857) %>%
  st_centroid() %>%
  # this is the crs from d, which has no EPSG code:
  # st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  # since you want the centroids in a second geometry col:
  st_geometry()


# check with
plot(st_geometry(mun))
plot(mun$centroid, add = T, col = 'red', pch = 19) # good


mun$centroid_coords <- mun$centroid %>% sf::st_coordinates()

mun_df <- mun %>% as.data.frame() %>%
  dplyr::select(c(MuniCode, MPIO_NAREA,
                  centroid_coords, centroid_coords))

write.csv(mun_df, "~/peregrine_amazon/data/colombia/municipio_centroid_data.csv")

