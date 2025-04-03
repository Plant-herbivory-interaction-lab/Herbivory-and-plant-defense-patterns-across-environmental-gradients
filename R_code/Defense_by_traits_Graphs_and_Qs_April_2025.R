## Code by: Jacob Herschberger
## Date: March 2025
## Email: j.herschberger@ufl.edu
## Project: Culprits of plant defense variation across a latitudinal gradient.

# Import packages ----
# Run install.packages([Package name]) if package is not installed
library(DBI)
library(RSQLite)
library(glmmTMB)
library(piecewiseSEM)
library(performance)
library(car)
library(lme4)


# Database connection ----
con <-dbConnect(SQLite(), 'Data/Data.db')

# Import and prep data ----
source("R_code/Prep_data_for_all_analysis.R")
source("R_code/Psem_graphing.R")


Data_prep<-function(loc="Field",PopLevel=F,long=F,ClimateLong=F,Treatment=F){
Field_2022<-Field_2022 %>% 
  select(Trichomes:herb) %>% 
  mutate(Loc="Field",
         Loc1="Field 2022",
         Year="2022")

Field_2023<-Field_2023 %>% 
  select(herb_p:herb) %>% 
  mutate(Loc="Field",
         Loc1="Field 2023",
         Year="2023",
         Conc=Conc*2.5) # This is do to using different amounts of leaf material.

Garden<-Garden %>% 
  select(Pop:herb) %>% 
  mutate(Loc="Garden",Loc1="Garden",Year="2023")

Combined_data1<-rbind(Field_2022,Field_2023,Garden)%>% filter(SLA>3.9) %>% 
  left_join(PCs %>% 
              select(Latitude,Pop,Clim_ave_PC1,Soil_PC2,Clim_ave_PC2,Aridity)) %>% 
  drop_na(Latitude)%>% 
  filter(SLA>40) %>% 
  mutate(
    Conc_t=log(Conc),
    SLA_t=log(SLA),
    Trichomes_t=log(Trichomes),
    herb_p_t=logit(herb_p),
    Clim_ave_PC1=-Clim_ave_PC1,
    Clim_ave_PC1_sq=Clim_ave_PC1^2,
    Clim_ave_PC1_sc=scale(Clim_ave_PC1)[,1],
    Clim_PC1_sq_sc=scale(Clim_ave_PC1_sq)[,1],
    Conc_t_sc=scale(Conc_t)[,1],
    SLA_t_sc=scale(SLA_t)[,1],
    Trichomes_t_sc=scale(Trichomes_t)[,1],
    herb_p_t_sc=scale(herb_p_t)[,1],
    Plant=c(1:length(Conc))
  )  

if(loc=="Field"|loc=="Garden"){Combined_data1<-Combined_data1 %>% 
  #select(!c(Spines)) %>% 
  #drop_na() %>% 
  dplyr::filter(Loc==loc)}


if(PopLevel==T&Treatment==T){Combined_data1<-Combined_data1 %>% 
  group_by(Pop,Loc,Treatment,Year)}

if(PopLevel==T&Treatment==F){Combined_data1<-Combined_data1 %>% 
  group_by(Pop,Loc,Loc1,Year)}


if(PopLevel==T){Combined_data1<-Combined_data1 %>% 
  summarise(across(where(is.numeric), \ (x) mean(x, na.rm = TRUE))) %>% 
  ungroup()}


if(long==T&ClimateLong==T){Combined_data1<-Combined_data1 %>%
  left_join(Pop_info %>% select(Pop,Latitude))%>% 
  pivot_longer(cols = c(Trichomes_t_sc,SLA_t_sc,Conc_t_sc,Clim_PC1_sq_sc,Clim_ave_PC1_sc)) 
}

if(long==T&ClimateLong==F){Combined_data1<-Combined_data1 %>%
  left_join(Pop_info %>% select(Pop,Latitude))%>% 
  pivot_longer(cols = c(Trichomes_t_sc,SLA_t_sc,Conc_t_sc))
}
  
if(long==T){Combined_data1<-Combined_data1 %>%
  mutate(name=case_when(
    name=="Conc_t_sc"~"Glycoalkaloids (mg/g)",
    name=='Clim_ave_PC1_sc'~'Climate',
    name=='Clim__PC1_sq_sc'~'Climate (sq)',
    name=="Trichomes_t_sc"~"Trichomes",
    name=='SLA_t_sc'~'SLA',
    .default = name))}


Combined_data1
}



# SEMs ----
# SEM of the plant traits, climate and herbivore relations at the plant individual level.

SEM_results <- function(Loc = "Field", model = c("lm1", "lm2", "lm3", "lm4"), random = "Pop:Year") {
  DF_short_I_field <- Data_prep(loc=Loc) %>%
    drop_na()
  
  lm1 <- glmmTMB(as.formula(paste0('herb_p_t_sc ~ (Clim_ave_PC1_sc + Clim_PC1_sq_sc) * (Trichomes_t_sc + Conc_t_sc + SLA_t_sc) + (1|', random, ')')), DF_short_I_field)
  lm1.1 <- glmmTMB(as.formula(paste0('herb_p_t_sc ~ (Clim_ave_PC1_sc + Clim_PC1_sq_sc) + (Trichomes_t_sc + Conc_t_sc + SLA_t_sc) + (1|', random, ')')), DF_short_I_field)
  lm1.2 <- update(lm1.1, . ~ . - Clim_ave_PC1_sc)
  lm1.3 <- update(lm1.1, . ~ . - Clim_PC1_sq_sc)
  
  lm2 <- glmmTMB(as.formula(paste0('Conc_t_sc ~ Clim_ave_PC1_sc + Clim_PC1_sq_sc + (1|', random, ')')), DF_short_I_field)
  lm2.1 <- update(lm2, . ~ . - Clim_ave_PC1_sc)
  lm2.2 <- update(lm2, . ~ . - Clim_PC1_sq_sc)
  
  lm3 <- glmmTMB(as.formula(paste0('SLA_t_sc ~ Clim_ave_PC1_sc + Clim_PC1_sq_sc + (1|', random, ')')), DF_short_I_field)
  lm3.1 <- update(lm3, . ~ . - Clim_ave_PC1_sc)
  lm3.2 <- update(lm3, . ~ . - Clim_PC1_sq_sc)
  
  lm4 <- glmmTMB(as.formula(paste0('Trichomes_t_sc ~ Clim_ave_PC1_sc + Clim_PC1_sq_sc + (1|', random, ')')), DF_short_I_field)
  lm4.1 <- update(lm4, . ~ . - Clim_ave_PC1_sc)
  lm4.2 <- update(lm4, . ~ . - Clim_PC1_sq_sc)
  
  # Create a named list of models
  model_list <- list(lm1 = lm1, lm2 = lm2, lm3 = lm3, lm4 = lm4,
                     lm1.1 = lm1.1,lm1.2 = lm1.2,lm1.3 = lm1.3,
                     lm2.1 = lm2.1,lm2.2 = lm2.2,
                     lm3.1 = lm3.1,lm3.2 = lm3.2,
                     lm4.1 = lm4.1,lm4.2 = lm4.2)
 
  
  fit <- psem(
    model_list[[model[[1]]]],
    model_list[[model[[2]]]],
    model_list[[model[[3]]]],
    model_list[[model[[4]]]],
    SLA_t_sc %~~% Trichomes_t_sc,
    SLA_t_sc %~~% Conc_t_sc,
    data = DF_short_I_field
  )
  return(fit)
}
# Field results ----
# This model tests the indirect effects of climate on herbivory via defense traits.
interaction_field<-SEM_results()
summary(interaction_field)
AIC(interaction_field)

# This model only includes the direct effects of climate and defense traits on herbivory.
Non_interaction_field<-SEM_results(model = c("lm1.1", "lm2", "lm3", "lm4"))
summary(Non_interaction_field)
AIC(Non_interaction_field)

# This model includes climate only
climate_field<-SEM_results(model = c("lm1.2", "lm2.1", "lm3.1", "lm4.1"))
summary(climate_field)
AIC(climate_field)

# This model includes the climate squared term only
climate_sq_field<-SEM_results(model = c("lm1.3", "lm2.2", "lm3.2", "lm4.2"))
summary(climate_sq_field)
AIC(climate_sq_field)


# This model tests the indirect effects of climate on herbivory via defense traits.
interaction_Garden<-SEM_results(Loc="Garden",model = c("lm1", "lm2", "lm3", "lm4"))
summary(interaction_Garden)
AIC(interaction_Garden)

# This model only includes the direct effects of climate and defense traits on herbivory.
Non_interaction_Garden<-SEM_results(Loc="Garden",model = c("lm1.1", "lm2", "lm3", "lm4"))
summary(Non_interaction_Garden)
AIC(Non_interaction_Garden)

Climate_Garden<-SEM_results(Loc="Garden",model = c("lm1.2", "lm2.1", "lm3.1", "lm4.1"))
summary(Climate_Garden)
AIC(Climate_Garden)

Climate_sq_Garden<-SEM_results(Loc="Garden",model = c("lm1.3", "lm2.2", "lm3.2", "lm4.2"))
summary(Climate_sq_Garden)
AIC(Climate_sq_Garden)

