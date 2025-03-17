## Code by: Jacob Herschberger
## Date: November 2024
## Email: j.herschberger@ufl.edu
## Project: Dissertation chapter 1

#### Import packages ----
# Run install.packages([Package name]) if package is not installed
library(DBI)
library(RSQLite)
library(tidyverse)
library(geodata)
library(raster)
library(terra)
library(tidyterra)
library(patchwork)

source("R_code/Google_Engine_init.R")
#### Database connection ----
con <-dbConnect(SQLite(), 'Data/Data_all_combined.db')

#### Import the coordinates ----
Pop_info<-dbReadTable(con,
                      'pop_info_2022_and_2023') %>% 
  select(Pop,Latitude,Longitude)

Field_2022<-dbReadTable(con,"Field_2022") %>% 
  dplyr::select(Pop,Herbivory)

Field_2023<-dbReadTable(con,"Field_2023") %>% 
  dplyr::select(Pop,herb_p) %>% 
  rename(.,Herbivory=herb_p)

Pop_info<-rbind(Field_2022,Field_2023) %>% 
  right_join(Pop_info) %>% 
  mutate(lat_new=round(Latitude,2)) %>% 
  group_by(lat_new) %>% 
  dplyr::summarise(Latitude=mean(Latitude),
                   Longitude=mean(Longitude),
                   Herbviory=length(levels(factor(Herbivory))))

# Map ----
## Collection sites
# load map, clip for plotting
cn <- gadm(country = "USA", level = 1,path=tempdir())
#soilN<-terra::rast("Data/soil_world/nitrogen_0-5cm_mean_30s.tif")
#bdod<-rast("Data/soil_world/bdod_0-5cm_mean_30s.tif")
#mat <- raster(worldclim_global("tavg",res=10,path=tempdir()))
#This did not bring in the proper temperature values
ocd <- raster(soil_world("nitrogen",5,path=tempdir()))



clipxy <- c(-100,-65,25,50)

#soilN<-terra::crop(soilN,clipxy)
#bdod<-terra::crop(bdod,clipxy)
mat<-ee_as_rast(image=ee$Image("WORLDCLIM/V1/BIO")$select('bio01'),
                region=ee$Geometry$
                  Rectangle(list(c(clipxy[1], clipxy[3]), c(clipxy[2], clipxy[4]))),
                scale=1000,
                via = "getDownloadURL")
mat[mat<(-40)]<-NA

ocd<-rast(crop(ocd,clipxy))

OR <- crop(cn,clipxy)

br <- colorRampPalette(c("darkblue","blue","cyan","green","yellow","orange","red"))

map_clim<-ggplot() + 
  geom_spatraster(data=mat*0.1)+
  geom_spatvector(data=OR,fill="NA",color="black") +
  geom_point(data=Pop_info,aes(x=Longitude,y=Latitude),shape=1,size=4)+
  scale_fill_gradientn(colors=br(10),
                        name=bquote("Mean annual", "temperature (\u00B0C)"),
                        na.value="transparent")+
  theme_void(base_size = 24)+
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.key.width = unit(2,"cm"),
    legend.key.height = unit(1,"cm"),
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5),
    legend.position = "top",
    plot.margin = unit(c(0, 0.5, 0, 0),"cm"))

map_soil<-ggplot() + 
  geom_spatraster(data=ocd)+
  geom_spatvector(data=OR,fill="NA",color="black") +
  geom_point(data=Pop_info,aes(x=Longitude,y=Latitude),shape=1,size=4)+
  scale_fill_continuous(low="yellow",high="red", 
                        breaks = c(5,10,15,20,25),
                        guide="colorbar",na.value="transparent",
                        name=bquote("Nitrogen g/kg"))+
  theme_void(base_size = 24)+
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.key.width = unit(2,"cm"),
    legend.key.height = unit(1,"cm"),
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5),
    legend.position = "top",
    plot.margin = unit(c(0, 0, 0, 0.5),"cm"))

map<-(map_clim | map_soil)+
  plot_annotation(tag_levels = "A") &
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        plot.tag = element_text(vjust = -10))

ggsave("map_Gorden_2025.png",
       device = "png",plot = map,
       bg = "transparent",
       path = "Figures",dpi = 300,width = 14, 
       height = 6,limitsize = F)
