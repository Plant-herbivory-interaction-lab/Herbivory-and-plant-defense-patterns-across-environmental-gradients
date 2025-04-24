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
library(tidyverse)
library(patchwork)
library(ggeffects)


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
         #Conc=Conc*2.5
         ) # This is do to using different amounts of leaf material.

Garden<-Garden %>% 
  select(Pop,Spines,Trichomes,SLA,Conc,herb_p,herb) %>% 
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

# Graph theme setup----
C_theme<-theme_bw(base_size = 18)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

# Appendix figures ----
Pop_level<-Garden1 %>% group_by(Pop,Date)%>% 
  summarise(count=n(),
            flowers = sum(fl_m>0, fl_h>0, na.rm = TRUE),
    across(c(ht:leaves,Herby,Trichomes:Spines), \ (x) mean(x, na.rm = TRUE))) %>%
  mutate(Flowering_p=flowers/count) %>% 
  ungroup()

Leaves_time<-ggplot(Pop_level,aes(Date,leaves,,col=Pop)) +
  geom_point(show.legend=F)+
  geom_smooth(method="glm",show.legend=F,formula = y~poly(x,2),se=F)+
  labs(y="Leaves")+
  C_theme;Leaves_time

Flowers_time<-ggplot(Pop_level,aes(Date,Flowering_p,col=Pop)) +
  geom_point(show.legend=F)+
  geom_smooth(show.legend=F,se=F)+
  labs(y='Proportion flowering')+
  C_theme;Flowers_time

Herbivory_time<-ggplot(Pop_level,aes(Date,Herby,col=Pop)) +
  geom_point()+
  geom_smooth(method="glm",se=F,formula = y~poly(x,2))+
  labs(y="Herbivory (%)")+
  C_theme;Herbivory_time

appendix<-((Leaves_time | Flowers_time)/(Herbivory_time | plot_spacer()))+plot_annotation(tag_levels = "A");appendix

ggsave("Appendix.jpg",
       device = "jpg",plot = appendix,
       path = "Figures",dpi = 400,width = 12, 
       height = 12,limitsize = F)

# SEMs ----
# SEM of the plant traits, climate and herbivore relations at the plant individual level.

SEM_results <- function(Loc = "Field", mod_fun='glmmTMB',model = c("lm1", "lm2", "lm3", "lm4"), random = "+ (1|Pop:Year)",corError = list(
  quote(SLA_t_sc %~~% Trichomes_t_sc),
  quote(SLA_t_sc %~~% Conc_t_sc)
)) {
  DF_short_I_field <- Data_prep(loc=Loc) %>%
    drop_na()
  
  method<-get(mod_fun)
  
  lm1 <- method(as.formula(paste0('herb_p_t_sc ~ Clim_PC1_sq_sc + Clim_ave_PC1_sc *(Trichomes_t_sc + Conc_t_sc + SLA_t_sc)', random)), DF_short_I_field)
  lm1.1 <- method(as.formula(paste0('herb_p_t_sc ~ Clim_ave_PC1_sc + Clim_PC1_sq_sc + Trichomes_t_sc + Conc_t_sc + SLA_t_sc', random)), DF_short_I_field)
  lm1.2 <- update(lm1.1, . ~ . - Clim_PC1_sq_sc)
  lm1.3 <- update(lm1, . ~ . - Clim_PC1_sq_sc)
  
  lm2 <- method(as.formula(paste0('Conc_t_sc ~ Clim_ave_PC1_sc + Clim_PC1_sq_sc', random)), DF_short_I_field)
  lm2.1 <- update(lm2, . ~ . - Clim_PC1_sq_sc)
  
  lm3 <- method(as.formula(paste0('SLA_t_sc ~ Clim_ave_PC1_sc + Clim_PC1_sq_sc', random)), DF_short_I_field)
  lm3.1 <- update(lm3, . ~ . - Clim_PC1_sq_sc)
  
  lm4 <- method(as.formula(paste0('Trichomes_t_sc ~ Clim_ave_PC1_sc + Clim_PC1_sq_sc', random)), DF_short_I_field)
  lm4.1 <- update(lm4, . ~ . - Clim_PC1_sq_sc)
  
  # Create a named list of models
  model_list <- list(lm1 = lm1, lm1.1 = lm1.1,lm1.2 = lm1.2, lm1.3 = lm1.3,
                     lm2 = lm2, lm2.1 = lm2.1,
                     lm3 = lm3, lm3.1 = lm3.1,
                     lm4 = lm4, lm4.1 = lm4.1)
 
  AICs<-sapply(model_list,FUN=AIC)
  
  names(AICs)<-c("interaction_sq","non_interaction_sq","non_interaction_lin","interaction_lin",
                 "Conc_sq", "Conc_lin",
                 "SLA_sq", "SLA_lin",
                 "Trich_sq","Trich_lin")
  
  print(AICs)
  
  for (m in model) {
    model_list[[m]]$call[[1]] <- as.name(mod_fun)
  }
  
  fit <- do.call(psem, c(
    list(
      model_list[[model[1]]],
      model_list[[model[2]]],
      model_list[[model[3]]],
      model_list[[model[4]]]
    ),
    corError,
    list(data = DF_short_I_field)
  ))
  
  return(fit)
}
# Field SEM results ----
# This model tests the indirect effects of climate on herbivory via defense traits (linear only).
interaction_field<-SEM_results(model = c("lm1.3", "lm2.1", "lm3.1", "lm4.1"))
summary(interaction_field)
AIC(interaction_field)

# This model tests the indirect effects of climate on herbivory via defense traits (with quadratic term).
interaction_field_clim2<-SEM_results()
summary(interaction_field_clim2)
AIC(interaction_field_clim2)

# This model only includes the direct effects of climate and defense traits on herbivory.
Non_interaction_field<-SEM_results(model = c("lm1.1", "lm2", "lm3", "lm4"))
summary(Non_interaction_field)
AIC(Non_interaction_field)

# This model includes climate as a linear term only
climate_field<-SEM_results(model = c("lm1.2", "lm2.1", "lm3.1", "lm4.1"))
summary(climate_field)
AIC(climate_field)

# Garden SEM results ----
# This model tests the indirect effects of climate on herbivory via defense traits (linear).
interaction_Garden<-SEM_results(Loc="Garden",model = c("lm1.3", "lm2.1", "lm3.1", "lm4.1"),random = '',mod_fun='lm',
                                corError = list(quote(Conc_t_sc %~~% Trichomes_t_sc)))
summary(interaction_Garden)
AIC(interaction_Garden)

# This model tests the indirect effects of climate on herbivory via defense traits (Quadratic).
interaction_Garden_clim2<-SEM_results(Loc="Garden",random = "",mod_fun='lm',
                                      corError = list(quote(Conc_t_sc %~~% Trichomes_t_sc)))
summary(interaction_Garden_clim2)
AIC(interaction_Garden_clim2)

# This model only includes the direct effects of climate and defense traits on herbivory.
Non_interaction_Garden<-SEM_results(Loc="Garden",model = c("lm1.1", "lm2", "lm3", "lm4"),random = "",mod_fun='lm',
                                    corError = list(quote(Conc_t_sc %~~% Trichomes_t_sc)))
summary(Non_interaction_Garden)
AIC(Non_interaction_Garden)

# This model includes climate as a linear term only
Climate_Garden<-SEM_results(Loc="Garden",model = c("lm1.2", "lm2.1", "lm3.1", "lm4.1"),random = "",mod_fun='lm',
                            corError = list(quote(Conc_t_sc %~~% Trichomes_t_sc)))
summary(Climate_Garden)
AIC(Climate_Garden)



# Graph of Climate PCA and PCA across latitude ----
# We used 
source('R_code/Biplot_function.R')
ClimBiplot<-PCbiplot(Clim_ave,rot_x=-1)+ C_theme;ClimBiplot

Clim_x_latitude<-ggplot(PCs,aes(x=Latitude,y=-Clim_ave_PC1))+
  geom_point()+
  C_theme+
  labs(y="Climate PC1 (Productivity)");Clim_x_latitude

Clim_PC<-(ClimBiplot| Clim_x_latitude)+plot_annotation(tag_levels = "A");Clim_PC

ggsave("Clim_PC.jpg",
       device = "jpg",plot = Clim_PC,
       path = "Figures",dpi = 400,width = 12, 
       height = 5,limitsize = F)

# SEM figures ----
Field_sem<-semGraph(Non_interaction_field);Field_sem

Garden_sem<-semGraph(Non_interaction_Garden);Garden_sem

SEM_fig<-(Field_sem | Garden_sem)+plot_annotation(tag_levels = "A");SEM_fig


ggsave("SEM.jpg",
       device = "jpg",plot = SEM_fig,
       path = "Figures",dpi = 400,width = 10, 
       height = 5,limitsize = F)


# Significant trends in the SEM ---- 
## Field ----
# Field climate versus trichomes
ClimXtrich<-ggplot(Data_prep(loc = "Field"),aes(x=Clim_ave_PC1,y=Trichomes))+
  geom_smooth(method = "glm",formula = y~poly(x,2))+
  geom_point() + 
  labs(x="Climate productivity") +
  C_theme;ClimXtrich

ClimXglyc<-ggplot(Data_prep(loc = "Field"),aes(x=Clim_ave_PC1,y=Conc))+
  geom_smooth(method = "glm")+
  geom_point() + 
  labs(x="Climate productivity",y="Glycoalkaloids (mg/mg)") +
  C_theme;ClimXglyc

# Field climate versus Herbivores
ClimsqXherb<-ggplot(Data_prep(loc = "Field"),aes(x=Clim_ave_PC1,y=herb_p))+
  geom_point() + 
  geom_smooth(method = "glm",formula = y~poly(x,2),method.args=list(family=beta_family()))+
  labs(x="Climate productivity",y="Herbivory (%)") +
  C_theme;ClimsqXherb

# Field glycoalkaloids versus Herbivory
glycXherb<-ggplot(Data_prep(loc = "Field"),aes(x=Conc,y=herb_p))+
  geom_point() + 
  geom_smooth(method = "glm",formula = y~x,method.args=list(family=beta_family()))+
  labs(x="Glycoalkaloids (mg/mg)",y='Herbivory (%)') +
  C_theme;glycXherb

Field_pan<-((ClimXtrich|ClimXglyc)/(ClimsqXherb|glycXherb))+plot_annotation(tag_levels = "A");Field_pan

ggsave("Field_pan.jpg",
       device = "jpg",plot = Field_pan,
       path = "Figures",dpi = 400,width = 12, 
       height = 12,limitsize = F)

## Garden ----

# Garden climate versus glycoalkaloids
climXglyc<-ggplot(Data_prep(loc = "Garden"),aes(x=Clim_ave_PC1,y=Conc))+
  geom_smooth(method = "glm",formula = y~poly(x,2),method.args = list(family = gaussian(link = "log")))+
  geom_point() + 
  labs(x="Climate productivity",y="Glycoalkaloids (mg/mg)") +
  C_theme;climXglyc

# Garden climate versus trichomes
climsqXtri_gard<-ggplot(Data_prep(loc = "Garden"),aes(x=Clim_ave_PC1,y=Trichomes))+
  geom_smooth(method = "glm",formula = y~poly(x,2),method.args=list(family=poisson()) )+
  geom_point() + 
  labs(x="Climate productivity") +
  C_theme;climsqXtri_gard

herbXSLA_gard<-ggplot(Data_prep(loc = "Garden"),aes(x=SLA,y=herb_p))+
  geom_smooth(method = "glm",formula = y~x,method.args=list(family=beta_family()) )+
  geom_point() + 
  labs(x="SLA",y="Herbivory (%)") +
  C_theme;herbXSLA_gard


gard_pan<-((climXglyc|climsqXtri_gard)/(herbXSLA_gard|plot_spacer()))+plot_annotation(tag_levels = "A");gard_pan


ggsave("gard_pan.jpg",
       device = "jpg",plot = gard_pan,
       path = "Figures",dpi = 400,width = 12, 
       height = 12,limitsize = F)


