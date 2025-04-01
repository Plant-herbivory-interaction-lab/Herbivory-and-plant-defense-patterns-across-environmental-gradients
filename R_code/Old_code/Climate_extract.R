library(rgee)
library(DBI)
library(RSQLite)
library(tidyverse)
library(sf)

#These commented commands below need be ran when running this on a different computer.
#See this github page for documentation https://github.com/r-spatial/rgee

# They recommend to use other commands, 
# but I find that the reticulate package works well with python environments.

#reticulate::install_python() # make sure python is installed
#reticulate::virtualenv_create("rgee") #create a virtual python environment for rgee to use
#reticulate::use_virtualenv("rgee")
#reticulate::py_install("numpy","rgee","earthengine-api") # install some python modules

#ee_install_upgrade() 
#ee_Authenticate()

reticulate::use_virtualenv("rgee")
ee_Initialize() 

con <-dbConnect(SQLite(), 'Data/Data.db')

Pop_info<-dbReadTable(con,
                      'pop_info_2022_and_2023') %>% 
  select(Pop,Latitude,Longitude)

pp3<-sf::st_buffer(
  sf::st_as_sf(Pop_info,
               coords=c("Longitude","Latitude"),
               crs="+proj=longlat +datum=WGS84"),100)

Datebegin<-as.character(as.Date("9/01/2022",tryFormats = "%m/%d/%Y"))
Dateend<-as.character(as.Date("8/31/2023",tryFormats = "%m/%d/%Y"))


# I abtained the data from this data set on the google engine website:
# https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_MONTHLY_AGGR#bands

for (i in 1:length(Pop_info)) {
  terraclimate <- ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$select('temperature_2m_max') %>%
    ee$ImageCollection$filterDate(Datebegin[1], Dateend[1])
  
  tmax <- ee_extract(
    x = terraclimate,
    y = pp3,
    scale = 1000,
    sf = FALSE
  )[,-1] - 273.15
  
  tmax<-as.matrix(tmax)

}

for (i in 1:length(Pop_info)) {
  terraclimate <- ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$select('temperature_2m_min') %>%
    ee$ImageCollection$filterDate(Datebegin[1], Dateend[1])
  
  tmin <- ee_extract(
    x = terraclimate,
    y = pp3,
    scale = 1000,
    sf = FALSE
  )[,-1] - 273.15
  
  tmin<-as.matrix(tmin)
  
}

for (i in 1:length(Pop_info)) {
  terraclimate <- ee$ImageCollection("ECMWF/ERA5_LAND/MONTHLY_AGGR")$select('total_precipitation_sum') %>%
    ee$ImageCollection$filterDate(Datebegin[1], Dateend[1])
  
  prec <- as.matrix((ee_extract(
    x = terraclimate,
    y = pp3,
    scale = 1000,
    sf = FALSE
  )[,-1]*1000))
  
  prec<-as.matrix(prec)
  
}

Bioclims_2023<-as.data.frame(dismo::biovars(prec,tmin,tmax)) %>% 
  cbind(Pop=Pop_info[,"Pop"])




