## Code by: Jacob Herschberger
## Date: September 2025
## Email: j.herschberger@ufl.edu
## Project: Culprits of plant defense variation across a latitudinal gradient.

# Import packages ----
# Run install.packages([Package name]) if package is not installed
library(tidyverse)
library(DBI)
library(RSQLite)
library(geodata)
library(sf)

# Climate data ----
con <-dbConnect(SQLite(), 'Data/Data.db') # Connect to the database

Coords<-dbReadTable(con,"Field_2023_pop_info") %>% 
  select(Pop,Latitude,Longitude) %>% 
  rbind(.,dbReadTable(con,"Horsenettle_Jun22_data_field_cords") %>% 
          select(Latitude:Pop)) %>% drop_na() %>% 
  distinct(Pop, .keep_all = TRUE)

## Download bioclim data ----
bioclim<-worldclim_global("bio",10,path=tempdir(),version="2.1")

## Project coordinates and extract bioclim variables ----
pp1<-st_as_sf(Coords,coords=c('Longitude','Latitude'),crs="+proj=longlat +datum=WGS84")

Clim_ave<-st_drop_geometry(cbind(pp1,extract(bioclim,pp1)[,-1])) %>% 
  right_join(Coords)

