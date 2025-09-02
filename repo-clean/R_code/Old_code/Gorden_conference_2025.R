## Code by: Jacob Herschberger
## Date: January 2025
## Email: j.herschberger@ufl.edu
## Project: Gorden Conference 2025

# Import packages ----
# Run install.packages([Package name]) if package is not installed
library(DBI)
library(RSQLite)
library(glmmTMB)
library(car)
library(DHARMa)
library(patchwork)
library(piecewiseSEM)
library(flextable)
library(emmeans)
library(lme4)


# Database connection ----
con <-dbConnect(SQLite(), 'Data/Data_all_combined.db')

# Import and prep data ----
source("R_code/Prep_data_for_all_analysis.R")

Field_2022<-Field_2022 %>% 
  select(Trichomes:herb_p) %>% 
  mutate(Loc="Field",
         Treatment=NA)

Field_2023<-Field_2023 %>% 
  select(Pop,Trichomes:SLA,Conc) %>% 
  mutate(Loc="Field",
         Treatment=NA)

Garden<-Garden %>% 
  #filter(Treatment=="Cont"|Treatment=="us") %>% 
  select(Pop:Conc,herb_p,Treatment) %>% 
  mutate(Loc="Garden")

Combined_data<-rbind(Field_2022,Field_2023,Garden)%>% 
  filter(Treatment=="Cont"|Treatment=="us"|is.na(Treatment)==T) %>% 
  left_join(PCs %>% 
              select(Latitude,Pop,Clim_ave_PC1,Soil_PC2)) %>% 
  drop_na(Latitude)%>% 
  #group_by(Pop,Loc) %>% 
  #summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE), .names = "{.col}"), .groups = "drop") %>% 
  mutate(
    Clim_ave_PC1=-Clim_ave_PC1,
    Soil_PC2=-Soil_PC2,
    Clim_ave_PC1.1=Clim_ave_PC1,
    Clim_ave_PC1=poly(Clim_ave_PC1,2)[,1]*100,
    Clim_PC1_sq=poly(Clim_ave_PC1,2)[,2]*100
  ) 


Combined_data1<-Combined_data%>% 
  mutate(
    Conc=log(Conc),
    Spines=log(Spines+1),
    herb_p_t=logit(herb_p)) %>% 
  mutate_at(c('Trichomes','Conc','SLA','Spines','herb_p_t','Soil_PC2','Clim_ave_PC1'),scale)%>% 
  mutate_at(c('Trichomes','Conc','SLA','Spines'),as.numeric)

Combined_data_long<-Combined_data1 %>%
  left_join(Pop_info %>% select(Pop,Latitude))%>% 
  pivot_longer(cols = c(Trichomes:Conc)) %>% 
  mutate(name=if_else(
    name=="Conc","Glycoalkaloids (mg/g)",name))

# Graphs -----
theme_mod<-theme_bw(base_size = 20)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position.inside = c(0.82,0.82))


Graph1<-ggplot(Combined_data,aes(x=Clim_ave_PC1,y=Conc))+
  geom_point()+
  labs(x="Climate productivity",y="Glycoalkaloids (mg/g)")+
  geom_smooth(method = "glm")+
  theme_mod



Graph2<-ggplot(Combined_data,aes(y=herb_p,x=Conc))+
  geom_point()+
  geom_smooth(method = "glm")+
  labs(x="Glycoalkaloids (mg/g)",y="Herbivory")+
  theme_mod

Graph3<-ggplot(Combined_data,aes(x=Soil_PC2,y=herb_p))+
  geom_point()+
  geom_smooth(method = "glm")+
  labs(x="Soil productivity",y="Herbivory")+
  theme_mod


# SEM; test all hypothesis together----
psem_data<-Combined_data1 %>% 
  select(!c(Treatment,Spines)) %>% 
  drop_na() %>% 
  dplyr::filter(Loc!="Garden")

lm1<-lmer(herb_p_t~(Clim_ave_PC1+Soil_PC2+Trichomes+Conc+SLA)+(1|Pop),psem_data)
lm2<-lmer(Conc~(Clim_ave_PC1+Soil_PC2)+(1|Pop),psem_data)
lm3<-lmer(SLA~(Clim_ave_PC1+Soil_PC2)+(1|Pop),psem_data)
lm4<-lmer(Trichomes~(Clim_ave_PC1+Soil_PC2)+(1|Pop),psem_data)

fit<-psem(
  lm1,
  lm2,
  lm3,
  lm4,
  
  SLA %~~%Trichomes,
  SLA %~~% Conc,
  
  data = psem_data
)
psem_out<-summary(fit,standardized=T, fit=T,rsquare=T,conserve=T)$coefficients

psem_out

psem_data<-Combined_data1 %>% 
  select(!c(Treatment,Spines)) %>% 
  drop_na() %>% 
  dplyr::filter(Loc=="Garden")

lm1<-lmer(herb_p_t~(Clim_ave_PC1+Soil_PC2+Trichomes+Conc+SLA)+(1|Pop),psem_data)
lm2<-lmer(Conc~(Clim_ave_PC1+Soil_PC2)+(1|Pop),psem_data)
lm3<-lmer(SLA~(Clim_ave_PC1+Soil_PC2)+(1|Pop),psem_data)
lm4<-lmer(Trichomes~(Clim_ave_PC1+Soil_PC2)+(1|Pop),psem_data)

fit<-psem(
  lm1,
  lm2,
  lm3,
  lm4,
  
  SLA %~~%Trichomes,
  SLA %~~% Conc,
  
  data = psem_data
)
psem_out<-summary(fit,standardized=T, fit=T,rsquare=T,conserve=T)$coefficients

psem_out
