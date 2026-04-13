## Code by: Jacob Herschberger
## Date: September 2025
## Email: j.herschberger@ufl.edu
## Project: Culprits of plant defense variation across a latitudinal gradient.

# Import packages ----
# Run install.packages([Package name]) if package is not installed
library(conflicted)
library(tidyverse)
library(geodata)
library(sf)
library(terra)
library(dismo)
library(car)
library(fuzzyjoin)

conflict_prefer("select", "dplyr")
conflict_prefer("extract", "raster")
conflict_prefer("filter", "dplyr")


source("R_code/Functions.R")
# Glycoalkaloid concentration calculations Field 2022----

## standard curve ----

Stand<-read.csv("Data/Glycoalk_Spring_2023_standard_curve.csv") %>% 
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
Glyco<-read.csv("Data/Glycoalk_Spring_2023_abs.csv")%>% 
  dplyr::select(!c(Notes,rep))%>% 
  group_by(Sample_Label) %>% 
  summarise(Abs=mean(Abs,na.rm=T))

weight<-read.csv("Data/Glycoalk_Spring_2023_weight.csv")%>% 
  dplyr::select(!Notes)

glyc_2022<-full_join(Glyco,weight) %>% 
  dplyr::select(Sample_Label,Abs,Weight,Collect_Label) %>% 
  mutate(Conc=(Abs-lm1$coefficients[1])/lm1$coefficients[2]*0.5/0.3*1.5/Weight) %>% 
  drop_na() %>% mutate(
    Plant_ID=Collect_Label,
    Year=2022,
    Time="2022",
    Loc="Field"
  ) %>% select(Plant_ID,Time,Year,Conc)
# (y-b)/slope gives the glycoalkaloid conc in samples
# 1.5 mL is the total volume of the sample before transferring
# I transferred 0.3 mL
# 5 mL is the volume used before final step
#group_by(Latitude,pop1) %>% 
#summarise(Conc=mean(Conc,na.rm = T)) %>% 

# 2023 glycoalkaloids ----
## standard curve ----

Plate<-read.csv("Data/Glycoalk_fall_2023_blank_plates.csv") %>% 
  select(Well,Plate,Abs) %>% rename(.,Blank_ABS=Abs)

Stand<-read.csv("Data/Glycoalk_fall_2023_Standard_curve.csv") %>% 
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
Samples<-read.csv("Data/Glycoalk_fall_2023_Samples.csv") %>% 
  left_join(Plate) %>% 
  filter(Sample!="Blank1",Abs!="NA",!is.na(Abs)) %>% 
  mutate(Abs=(as.numeric(Abs)-Blank_ABS)) %>% 
  mutate(Sample=as.numeric(Sample)) %>% 
  left_join(
    read.csv("Data/Glycoalk_fall_2023_Weight_data.csv")
    ) %>% 
  mutate(Sample=as.numeric(Sample))

glyc_2023<-Samples %>% 
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

# Import field -----
## Field 2022 ----
Field_2022<-read.csv("Data/Field_2022_traits.csv")%>% 
  select(Date,Height,Herbivory,L_area,L_weight,Trichomes,
         Latitude,Longitude,Collect_Label,Fl_herm,Fl_male,Height) %>% 
  mutate(
    fl_m=Fl_male,
    fl_h=Fl_herm,
    Loc='Field',
    Leaves=NA,
    herb_p=Herbivory/100,
    Plant_ID=Collect_Label,
    Date= gsub("-(?!2023$)\\d{2,4}$", "-2022", Date,perl = T),
    Date=as.Date(Date, format = "%b-%d-%Y")
  ) %>% full_join(glyc_2022,by=join_by(Collect_Label==Plant_ID)) %>% 
  difference_full_join(
    read.csv("Data/Field_2022_cords.csv") %>% 
      drop_na(Pop),
    by = c("Latitude","Longitude"),
    max_dist = 0.01, # tolerance in degrees (~11 m)
    distance_col = NULL
  ) %>% 
  select(Pop,Date,Plant_ID,Loc,Time,Year,L_area,L_weight,Trichomes,
         Conc,herb_p,fl_m,fl_h,Leaves,Height) %>% 
  # Filtered data from the field collected late in the year.
  # This data does not change the outcome of the results, but the data isn't consistent with other herbivory data.
  # This data was collected when we collected the roots to be grown in the common garden.
  filter(Date<as.Date("2022-09-01")) %>% drop_na(Trichomes)

## Field 2023 ----
Field_2023<-read.csv("Data/Field_2023_traits.csv") %>% 
  mutate(
    fl_m=Fl_male,
    fl_h=Fl_herm,
    Loc="Field",
    Time="2023",
    Year="2023",
    herb_p=(Epitrix_herb+Chew_herb)/100,
    L_area=Leaf_Area,
    L_weight=Leaf_Weight,
    Date=as.Date(Date, format = "%m/%d/%y")
  ) %>% left_join(glyc_2023) %>% 
  select(Pop,Date,Plant_ID,Loc,Time,Year,L_area,L_weight,Trichomes,
         Conc,herb_p,fl_m,fl_h,Leaves,Height)

# Garden data ----
Garden_2023<-glyc_2023 %>% 
  filter(Experiement=="Dudu") %>% pivot_wider(id_cols = Plant_ID,
                                              names_from = Leaf_location,
                                              values_from = Conc,
                                              values_fn = mean
  ) %>% rename(Conc_T="T",Conc_B="B") %>% 
  full_join(read.csv("Data/Garden_leaf_traits.csv" ) %>% 
              mutate(
                Plant_ID=paste0(Pop,"-",Plant_ID,Rep)
              )) %>%
  mutate(Trichomes = rowMeans(across(starts_with("Trichomes"),
                                     as.numeric), na.rm = TRUE),
         Spines= rowMeans(across(starts_with("Spines"),
                                 as.numeric), na.rm = TRUE),
         Conc=rowMeans(across(starts_with("Conc"),as.numeric), 
                       na.rm = TRUE),
         L_area=rowMeans(across(starts_with("L_area"),
                                as.numeric), na.rm = TRUE),
         L_weight=rowMeans(across(starts_with("L_weight"),
                                  as.numeric), na.rm = TRUE))%>% 
  select(Plant_ID,L_area,L_weight,Trichomes,Conc) %>% drop_na() %>% 
  right_join(read.csv("Data/Garden_plant_info.csv") %>% 
               right_join(read.csv("Data/Garden_survey.csv")) %>% 
               filter(Treatment=="Cont"),by=join_by(Plant_ID==Plant_ID)) %>%
  mutate(
    Leaves=leaves,
    herb_p=as.numeric(Chewing)/100,
    Date=as.Date(Date, format = "%m/%d/%y"),
    Year="2023",
    Loc="Garden",
   Time=cut(Date, 
            breaks = seq(min(Date, na.rm = T),
                         max(Date, na.rm = T),
                         length.out=4), 
            labels = c("Early", "Mid", "Late"), 
            include.lowest = TRUE, 
            right = FALSE)
  ) %>% 
  drop_na(Date) %>% 
  select(Pop,Date,Time,Plant_ID,Loc,Year,L_area,L_weight,Trichomes,
         Conc,herb_p,fl_m,fl_h,Leaves,Height)

# Combine data ----
Combined_data<-rbind(Field_2022,Field_2023,Garden_2023) %>% 
  drop_na(Trichomes,Conc,herb_p) %>% 
  mutate(SLA=L_area/L_weight) %>% 
  filter(Conc>0)

write.csv(Combined_data,"Data/Combined_herbivory_and_trait_data.csv",row.names = F)


# Climate data ----
coords<-rbind(read.csv("Data/Field_2023_pop_info.csv") %>% 
                select(Pop,Latitude,Longitude),
              read.csv("Data/Field_2022_cords.csv") %>% 
                select(Pop,Latitude,Longitude)) %>% 
  filter(Pop %in% Combined_data$Pop) %>% 
  distinct(.,Pop,.keep_all = TRUE)

## NPP ----
PP_yearly<-Extract_var_with_const_date(
  sf::st_as_sf(coords,coords=c('Longitude','Latitude'),
               crs="+proj=longlat +datum=WGS84"),
  "MODIS/061/MOD17A3HGF",c("Npp","Npp_QC"),unit="Year",begin = 2013,
  end=2026,
  select = T,
  buffer = 4000,scale = 500)

PP_8day<-Extract_var_with_const_date(
  sf::st_as_sf(coords,coords=c('Longitude','Latitude'),
               crs="+proj=longlat +datum=WGS84"),
  "MODIS/061/MOD17A2HGF",c("PsnNet","Psn_QC"),select = T,
  unit1 = "Year", begin1 = 2013, end1 = 2024,
  buffer = 4000,scale = 500)


PP_yearly_sum<-PP_yearly %>%
  filter(Npp_QC <= 30) %>%
  group_by(Pop) %>%
  summarise(NPP_y=sqrt(mean(Npp)/1e4))



PP_8day_sum_pix<-PP_8day %>%
  mutate(Year=year(image_date)) %>%
  filter(
    bitwAnd(Psn_QC, 1) == 0 &                    # good quality
      bitwAnd(bitwShiftR(Psn_QC, 2), 1) == 0 &     # detectors OK
      bitwAnd(bitwShiftR(Psn_QC, 3), 3) == 0 &     # clear sky
      bitwAnd(bitwShiftR(Psn_QC, 5), 7) == 0       # best confidence
  ) %>%
  summarise(.by=c(Pop,Year,image_date), NPP_g=mean(PsnNet)/1e4)

PP_g<-PP_8day_sum_pix %>% 
  summarise(.by=c(Pop,Year), NPP_g=sum(NPP_g))

PP_8day_sum_10y<- PP_g %>% 
  summarise(.by=c(Pop), NPP_g_10y=mean(NPP_g))


PP_8day_sum_season<- PP_g %>% 
  filter(Year=="2023"|Year=="2022")


## Download bioclim data ----
clim_vars <- c("bio01","bio04","bio12","bio18")


Bioclims<-Extract_var_with_const_date(
  sf::st_as_sf(coords,coords=c('Longitude','Latitude'),
               crs="+proj=longlat +datum=WGS84"),
  "WORLDCLIM/V1/BIO",clim_vars,unit="",select = F,
  buffer = 500,scale = 500) %>% 
  select(clim_vars,"Pop") %>% 
  summarise(
    .by = "Pop",
    across(
      where(is.numeric),
      ~ mean(., na.rm = TRUE)
    )
  ) %>% 
  left_join(PP_8day_sum_10y,join_by(Pop)) %>% 
  left_join(PP_8day_sum_season,join_by(Pop)) %>% 
  left_join(PP_yearly_sum,join_by(Pop)) %>% 
  mutate(NPP_y=log(NPP_y),
         bio18=sqrt(bio18),
         Year = as.character(Year),
         across(where(is.numeric), 
                ~as.numeric(scale(.,center = T)))
  ) %>% right_join(coords)


PCs <- Bioclims %>%  
  {
    df <- .
    
    # ONLY climate variables go into PCA
    pca <- prcomp(
      df %>% select(all_of(clim_vars)),
    )
    
    weights <- summary(pca)$importance[2, c("PC1","PC2")]
    
    # origin population
    origin_pc <- pca$x[df$Pop == "DUDU", c("PC1","PC2")][1, ]
    
    # compute weighted distance
    WeightedCD <- weighted_cd(
      data = as.data.frame(pca$x),
      origin_pc = origin_pc,
      pc_cols = c("PC1","PC2"),
      weights = weights
    )
    
    mutate(df,
           Clim_PC1 = pca$x[, "PC1"],
           Clim_PC2 = pca$x[, "PC2"],
           WeightedCD = WeightedCD
           
           
    ) %>% 
      rename(
        `MAT` = bio01,
        `Tsd` = bio04,
        `AP`  = bio12,
        `PWQ` = bio18,
        NPPg=NPP_g,
        NPP = NPP_y
        
      )}



write.csv(PCs, "Data/clim_data.csv",row.names = F)

