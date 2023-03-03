library(dplyr)
library(bannerCommenter)
library(sf)

###########################################################################
###########################################################################
###                                                                     ###
###                              COUNTRIES                              ###
###                                                                     ###
###########################################################################
###########################################################################

old_mun_colombia <- read_sf("~/peregrine_amazon/data/colombia/shapefiles/Colombia_Amazonian_Municipios_Projected.shp")

old_mun_peru <- read_sf("~/peregrine_amazon/data/peru/shapefiles/Final_Peru_Amazonian_Municipios_Projected.shp")

old_mun_brazil <- read_sf("~/peregrine_amazon/data/brazil/shapefiles/Final_Brazil_Amazonian_Municipios_Projected.shp")

old_muns <- c(old_mun_colombia$MPIOS, old_mun_peru$IDDIST, old_mun_brazil$codigo_ibg)

old_muns_colombia <- data.frame(MPIOS = old_mun_colombia$MPIOS)
old_muns_peru <- data.frame(IDDIST = old_mun_peru$IDDIST)
old_muns_brazil <- data.frame(codigo_ibg = old_mun_brazil$codigo_ibg)

write.csv(old_muns_colombia, "~/peregrine_amazon/Restructured021623/data/shapefiles/old_muns_colombia.csv")
write.csv(old_muns_peru, "~/peregrine_amazon/Restructured021623/data/shapefiles/old_muns_peru.csv")
write.csv(old_muns_brazil, "~/peregrine_amazon/Restructured021623/data/shapefiles/old_muns_brazil.csv")
