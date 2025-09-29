## Code by: Jacob Herschberger
## Date: September 2025
## Email: j.herschberger@ufl.edu
## Project: Culprits of plant defense variation across a latitudinal gradient.

# Import packages ----
# Run install.packages([Package name]) if package is not installed
library(conflicted)
library(tidyverse)
library(DBI)
library(RSQLite)
library(geodata)
library(sf)
library(terra)
library(dismo)

conflict_prefer("select", "dplyr")
conflict_prefer("extract", "raster")

# Climate data ----
con <-dbConnect(SQLite(), 'Data/Data.db') # Connect to the database

Coords<-dbReadTable(con,"Field_2023_pop_info") %>% 
  select(Pop,Latitude,Longitude) %>% 
  rbind(.,dbReadTable(con,"Horsenettle_Jun22_data_field_cords") %>% 
          select(Latitude:Pop)) %>% drop_na() %>% 
  distinct(Pop, .keep_all = TRUE)

## Download bioclim data ----
vars <- c("prec", "tmax", "tmin")
lats <- c(40, 20)

results <- list()

for (v in vars) {
  for (i in seq_along(lats)) {
    nm <- paste0(v, i)  # e.g. prec1, prec2, tmax1 …
    results[[nm]] <- stack(
      worldclim_tile(v, lon = -80, lat = lats[i], path = tempdir())
    )
  }
}


## Project coordinates and extract bioclim variables ----
pp1<-st_as_sf(Coords,coords=c('Longitude','Latitude'),crs="+proj=longlat +datum=WGS84")

biovari<-data.frame()

for (i in 1:nrow(pp1)){
  if(all(is.na(extract(results$tmax1,pp1[i,])[,-1]))){
    reg<-2
  }else{reg<-1}
  
  tmax<-as.vector(as.matrix(extract(results[[paste0('tmax',reg)]],pp1[i,])[,-1]))
  tmin<-as.vector(as.matrix(extract(results[[paste0('tmin',reg)]],pp1[i,])[,-1]))
  prec<-as.vector(as.matrix(extract(results[[paste0('prec',reg)]],pp1[i,])[,-1]))

  vars<-data.frame(Pop=Coords[i,"Pop"],Latitude=Coords[i,"Latitude"],biovars(prec,tmin,tmax))

  biovari<-rbind(biovari,vars)

}

Clim_ave<-biovari %>% 
  select(Pop,Latitude,bio1,bio4,bio12,bio18) %>% 
  mutate(
    bio18=sqrt(bio18),
    Clim_ave_PC1= prcomp(across(bio1:bio18), scale. = TRUE)$x[, c("PC1")],
    Clim_ave_PC2= prcomp(across(bio1:bio18), scale. = TRUE)$x[, c("PC2")],
    'Climate PC1' = -Clim_ave_PC1
  ) %>% 
  rename(
    MAT = bio1,
    Tsd = bio4,
    AP = bio12,
    PWQ = bio18
  ) 

dbWriteTable(con,"Bioclims_1970_ave")

# Glycoalkaloid concentration calculations ----

