# Code by: Jacob Herschberger
# Date: July 2024

# Required packages -----
# Run install.package([package name]) if needed
library(conflicted)
library(tidyverse)
library(geodata)
library(raster)
library(DBI)
library(RSQLite)

conflicts_prefer(raster::extract()) # This ensures the proper function are used.
conflicts_prefer(dplyr::select())

# Import data ----
con <-dbConnect(SQLite(), 'Data/Data.db') # Connect to the database

Pop_info<-dbReadTable(con, # Reads specific data sheet from the database
                      'pop_info_2022_and_2023') %>% 
  select(Pop,Latitude,Longitude) %>% 
  rename('Latt'=Latitude,'Long'=Longitude)

# Download bioclim data ----
bioclim<-worldclim_global("bio",0.5,path=tempdir(),version="2.1")


# Project coordinates and extract bioclim variables ----
pp1<-sf::st_as_sf(Pop_info,coords=c('Long','Latt'),crs="+proj=longlat +datum=WGS84")

Data<-sf::st_drop_geometry(cbind(pp1,extract(bioclim,pp1)[,-1])) %>% 
  right_join(Pop_info)

# Extract soil variables ----
pp2<-sf::st_transform(pp1,'+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs')# transform to the soil projection (in meters)
  
  
 # Soil variables of interest 
Soil_var<-c("ocs","bdod", "cfvo", "clay", "nitrogen", "ocd",
            
            "phh2o", "sand", "silt", "soc")
# Function for the initial soil variable extraction.
fun1<-function(var){
  if(var1=="ocs"){depth<-30}else{depth<-5}
  extracted<-raster::extract(soil_world_vsi(var, depth=depth,stat = "mean"),pp2)
  Data[,var]<-extracted[,2]
  return(Data)}
# This function creates a buffer around the original point and then extracts an averages the values from that raster buffer.
fun2<-function(var){
  for (circ1 in seq(50,10000,50)) {
    circ<-circ1
    if(length(Data[is.na(Data[,var]),'Latt'])>0){
      Lat<-Data[is.na(Data[,var]),'Latt']
      Lon<-Data[is.na(Data[,var]),'Long']
      
      new.points2<-as.data.frame(na.omit(as.matrix(cbind(Lon,Lat))))
      new.points1<-sf::st_as_sf(new.points2,coords=c("Lon","Lat"),crs="+proj=longlat +datum=WGS84")
      new.points3<-sf::st_transform(new.points1,'+proj=igh')# transform to the soil projection (in meters)
      new.points<-sf::st_buffer(new.points3,circ)
      
      if(var1=="ocs"){depth<-30}else{depth<-5}
      
      ras<-soil_world_vsi(var, depth=depth,stat = "mean")
      extracted1<-raster::extract(ras,new.points,fun=mean,na.rm=T)
      
      Data[is.na(Data[,var]),var]<-extracted1[,2]
      
      
    }else{print(paste(var,"raster buffering completed",'at',circ,sep=" "))
      break
      
    } 
    
  }
  return(Data)}

# Wrapper function for the functions checks whether the column already exists and then extracts the soil variables if not.
for (var1 in Soil_var) {
  if(length(Data[,grep(var1,colnames(Data))])==0){
    Data<-fun1(var1)
    Data<-fun2(var1)
    
  }else{
    print(paste(var1,"column exists",sep=" "))
    Data<-fun2(var1)}
  
 Soil<-Data
}

# Obtain the aridity data from figshare ----
# Define the download URL
url <- "https://figshare.com/ndownloader/files/14118800"

# Define the output file path
output_file <- tempfile(fileext = ".zip")

# Download the file
download.file(url, output_file, mode = "wb")

# Define extraction directory
extract_dir <- tempfile()
dir.create(extract_dir)

# Unzip the downloaded file
unzip(output_file, exdir = extract_dir)

raster_files <- list.files(list.files(extract_dir,full.names = T), pattern = "\\.tif$", full.names = TRUE)

r <- raster(raster_files[1])

Data[,'Aridity']<-extract(r,pp1)/1000

# Write the data ----
dbWriteTable(con,"Soil",Data %>% select('Pop',"ocs":"soc"),overwrite=T)
dbWriteTable(con,"Bioclims_1970_ave",Data %>% select('Pop':"wc2.1_30s_bio_19",'Aridity'),overwrite=T)
