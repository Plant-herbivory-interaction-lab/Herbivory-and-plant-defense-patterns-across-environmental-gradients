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

#### Database connection ----
con <-dbConnect(SQLite(), 'Data/Data.db')

#### Import the coordinates ----
Pop_info<-dbReadTable(con,
                      'pop_info_2022_and_2023') %>% 
  select(Pop,Latitude,Longitude)

# Map ----
cn <- gadm(country = "USA", level = 1,path=tempdir())

mat <- raster(worldclim_global("bio",res=2.5,path=tempdir()))*10

ocd <- raster(soil_world("nitrogen",5,path=tempdir()))

clipxy <- c(-100,-65,25,50)

ocd<-rast(crop(ocd,clipxy))
mat<-rast(crop(mat,clipxy))

OR <- crop(cn,clipxy)

br <- colorRampPalette(c("darkblue","blue","cyan","green","yellow","orange","red"))

map_clim<-ggplot() + 
  geom_spatraster(data=mat*0.1)+
  geom_spatvector(data=OR,fill="NA",color="black") +
  geom_point(data=Pop_info,aes(x=Longitude,y=Latitude),shape=1,size=4)+
  scale_fill_gradientn(colors=br(10),
                        name=bquote(atop("Mean annual","Temperature (\u00B0C)")),
                        na.value="transparent")+
  theme(panel.background = element_blank(),
        legend.position = "top")+
  theme_void(base_size = 18);map_clim

map_soil<-ggplot() + 
  geom_spatraster(data=ocd)+
  geom_spatvector(data=OR,fill="NA",color="black") +
  geom_point(data=Pop_info,aes(x=Longitude,y=Latitude),shape=1,size=4)+
  scale_fill_continuous(low="yellow", high="red", 
                        guide="colorbar",na.value="transparent",
                        name=bquote("Nitrogen g/kg"))+
  theme(panel.background = element_blank(),
        legend.position = "top")+
  theme_void(base_size = 18);map_soil

map<-(map_clim | map_soil)+plot_annotation(tag_levels = "A")

ggsave("map_Chap_1_2024_fig.pdf",
       device = "pdf",plot = map,
       path = "Figures",dpi = 200,width = 12, 
       height = 6,limitsize = F)
