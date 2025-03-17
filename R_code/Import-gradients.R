library(geodata)
library(sf)
library(tidyverse)
library(raster)
library(dismo)
library(DBI)
library(RSQLite)

library(rgee)

reticulate::use_virtualenv("rgee")
ee_Initialize()


source("R_code/Functions_to_extract_variables_from_GEE.R")


con <-dbConnect(SQLite(), 'Data/Data_all_combined.db')

Pop_info<-dbReadTable(con,
                      'pop_info_2022_and_2023') %>% 
  dplyr::select(Pop,Latitude,Longitude) %>% rename(all_of(c(Lat='Latitude',Long='Longitude')))


# data source: 
# https://git.wur.nl/isric/soilgrids/soilgrids.notebooks/-/blob/master/markdown/access_on_gee.md
data_new<-Gradient_extract(Data = Pop_info,GEE_path="projects/soilgrids-isric/",subpath='bdod_mean',Grad_var="Soil")

# data source:
# https://developers.google.com/earth-engine/datasets/catalog/ESA_WorldCover_v200
data_new<-Gradient_extract(Data = data_new,GEE_path = 'ESA/WorldCover/v200',Grad_var ="Landcover")

# data source:
# https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD13A2
#data_new<-Gradient_extract(Data = data_new,GEE_path = 'MODIS/006/MOD13A2',Grad_var = 'NDVI',subpath = "NDVI")

# data source:
# https://csidotinfo.wordpress.com/2019/01/24/global-aridity-index-and-potential-evapotranspiration-climate-database-v3/
# This raster was downloaded and then multiplied by 0.0001 to set the original scale.
# Then this raster was hosted on a personal GEE account.
data_new<-Gradient_extract(Data = data_new,GEE_path='projects/ee-jakeberger92/assets/Aridity',Grad_var="Aridity")

# data source:
# https://developers.google.com/earth-engine/datasets/catalog/WORLDCLIM_V1_BIO
data_new<-Gradient_extract(Data = data_new,GEE_path='WORLDCLIM/V1/BIO',Grad_var="Climate")

# data source:
# https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_MONTHLY_AGGR#bands
data_new<-Gradient_extract(Data = data_new,GEE_path='ECMWF/ERA5_LAND/MONTHLY_AGGR',Grad_var="LeafIndex")


New_soil<-data_new %>% 
  as.data.frame() %>% 
  dplyr::select(Pop,soil_temp_surf)


Old_soil<-dbReadTable(con,"Soil") 

comb_soil<-left_join(Old_soil[,grep("surf",colnames(Old_soil),invert = T)],New_soil)

dbWriteTable(con,"Soil",comb_soil,overwrite=T)


New_clim<-data_new %>% 
  as.data.frame() %>% 
  dplyr::select(Pop,Aridity)


Old_clim<-dbReadTable(con,"Bioclims_1970_ave") %>% 
  dplyr::select(-any_of("Aridity"))

comb_clim<-left_join(Old_clim,New_clim)

dbWriteTable(con,"Bioclims_1970_ave",comb_clim,overwrite=T)


veg<-data_new %>% 
  as.data.frame() %>% 
  dplyr::select(low_veg_mean_LI_year:high_veg_mean_LI_growing)

dbWriteTable(con,"Vegetation",veg,overwrite=T)

