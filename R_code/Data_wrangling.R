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
library(car)

conflict_prefer("select", "dplyr")
conflict_prefer("extract", "raster")
conflict_prefer("filter", "dplyr")

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

# Glycoalkaloid concentration calculations Field 2022----

## standard curve ----

Stand<-dbReadTable(con,"Glycoalk_Spring_2023_standard_curve") %>% 
  drop_na(Conc..mg.mL)



lm1 <- Stand %>% 
  mutate(Conc..mg.mL = as.numeric(Conc..mg.mL))  %>% 
  lm(Abs~Conc..mg.mL, data=.) # Absorbence per mg/mL 

Anova(lm1, type=2)
summary(lm1) 

hist(lm1$residuals)
plot(lm1$residuals~lm1$fitted.values)
abline(h=0) 

qqPlot(lm1$residuals)

Stand %>% 
  ggplot(.,aes(x=Conc..mg.mL,y=Abs))+
  geom_point()+
  geom_smooth(method = "lm")

## Total glycoalkaloid ----
Glyco<-dbReadTable(con,"Glycoalk_Spring_2023_abs")%>% 
  dplyr::select(!c(Notes,rep))%>% 
  group_by(Sample_Label) %>% 
  summarise(Abs=mean(Abs,na.rm=T))

weight<-dbReadTable(con,"Glycoalk_Spring_2023_weight")%>% 
  dplyr::select(!Notes)

field_total_glyco_weight<-full_join(Glyco,weight) %>% 
  dplyr::select(Sample_Label,Abs,Weight,Collect_Label) %>% 
  mutate(Conc=(Abs-lm1$coefficients[1])/lm1$coefficients[2]*0.5/0.3*1.5/Weight) %>% 
  drop_na() %>% mutate(
    Plant_ID=Collect_Label,
    Year=2022,
    Time="2022",
    loc="Field"
  ) %>% select(Plant_ID,Time,Year,Conc)
# (y-b)/slope gives the glycoalkaloid conc in samples
# 1.5 mL is the total volume of the sample before transferring
# I transferred 0.3 mL
# 5 mL is the volume used before final step
#group_by(Latitude,pop1) %>% 
#summarise(Conc=mean(Conc,na.rm = T)) %>% 

# 2023 glycoalkaloids ----
## standard curve ----

Plate<-dbReadTable(con,"Glycoalk_fall_2023_blank_plates") %>% 
  select(Well,Plate,Abs) %>% rename(.,Blank_ABS=Abs)

Stand<-dbReadTable(con,"Glycoalk_fall_2023_Standard_curve") %>% 
  left_join(Plate) %>% mutate(Abs=(Abs-Blank_ABS)) 





lm1 <- Stand %>% 
  lm(Abs~Conc..mg.mL., data=.)

Anova(lm1, type=2)
summary(lm1) 

hist(lm1$residuals)
plot(lm1$residuals~lm1$fitted.values)
abline(h=0) 

qqPlot(lm1$residuals)

Stand %>% 
  ggplot(.,aes(x=Conc..mg.mL.,y=Abs))+
  geom_point()+
  geom_smooth(method = "lm")

## Total glycoalkaloid ----
Samples<-dbReadTable(con,"Glycoalk_fall_2023_Samples") %>% 
  left_join(Plate) %>% 
  filter(Sample!="Blank1",Abs!="NA") %>% 
  mutate(Abs=(as.numeric(Abs)-Blank_ABS)) %>% 
  left_join(
    dbReadTable(con,"Glycoalk_fall_2023_Weight_data")
    ) %>% 
  mutate(Sample=as.numeric(Sample))

Total_glyc<-Samples %>% 
  group_by(.,Plant_ID,Leaf_location) %>% 
  reframe(Abs=mean(Abs),
          Leaf_weight=unique(Leaf_weight),
          Sample=unique(Sample),
          Experiement=unique(Experiement)) %>% 
  mutate(Conc=as.numeric((Abs-lm1$coefficients[1])/lm1$coefficients[2]*0.5/0.3*1.5/Leaf_weight*1000)) %>% 
  ungroup()
# (y-b)/slope gives the glycoalkaloid conc in samples
# 1.5 mL is the total volume of the sample before transferring
# I transferred 0.3 mL
# 5 mL is the volume used before final step
