## Code by: Jacob Herschberger
## Date: March 2025
## Email: j.herschberger@ufl.edu
## Project: Culprits of plant defense variation across a latitudinal gradient.

# Import packages ----
# Run install.packages([Package name]) if package is not installed
library(conflicted)
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
library(geodata)
library(raster)
library(tidyterra)

conflicts_prefer(dplyr::select(),
                 dplyr::filter)


# Database connection ----
con <-dbConnect(SQLite(), 'Data/Data.db')


# Import and prep data ----
source("R_code/Prep_data_for_all_analysis.R")
source("R_code/Psem_graphing.R")


Data_prep<-function(loc="Field",PopLevel=F,long=F,ClimateLong=F,Treatment=F,Time.var='mid',start_date="2023-06-15",end_date="2023-07-29",byDate=F,...){
Field_2022<-Field_2022 %>% 
  select(Plant_ID,Date,Trichomes:herb) %>% 
  mutate(Loc="Field",
         Time="2022")

Field_2023<-Field_2023 %>% 
  select(Plant_ID,Date,herb_p:herb) %>% 
  mutate(Loc="Field",
         Time="2023",
         #Conc=Conc*2.5
         ) # This is do to using different amounts of leaf material.

Garden<-Garden %>% 
  select(Plant_ID,Date,Pop,Spines,Trichomes,SLA,Conc,herb_p,herb,Time) %>% 
  mutate(Loc="Garden")

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


if(loc=="Garden"){
  
  
  if(byDate==T){
    Combined_data1<-Combined_data1 %>%
    filter(Date >= as.Date(start_date) & Date <= as.Date(end_date))}
    else{
      Combined_data1<-Combined_data1 %>%
        filter(Time==Time.var)
  }
  
  Combined_data1<-Combined_data1 %>%
  group_by(Plant_ID) %>% 
  summarise(Pop=unique(Pop),
            across(where(is.numeric), ~ mean(., na.rm = TRUE))) %>% 
  ungroup() %>% drop_na(SLA)
  }


if(PopLevel==T&Treatment==T){Combined_data1<-Combined_data1 %>% 
  group_by(Pop,Loc,Treatment,Time)}

if(PopLevel==T&Treatment==F){Combined_data1<-Combined_data1 %>% 
  group_by(Pop,Loc,Time)}


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

# SEMs ----
# SEM of the plant traits, climate and herbivore relations at the plant individual level.

SEM_results <- function(Loc = "Field", mod_fun='glmmTMB',model = c("lm1", "lm2", "lm3", "lm4"), random = "+ (1|Time:Pop)",Time="mid",
                        byDate=F,start_date="2023-06-15",end_date="2023-07-29",corError = list(
                          quote(SLA_t_sc %~~% Trichomes_t_sc),
                          quote(SLA_t_sc %~~% Conc_t_sc)
                        )) {
  DF_short_I_field <- Data_prep(loc=Loc,Time.var=Time,byDate = byDate,start_date = start_date, end_date = end_date) %>%
    drop_na()
  
  #print(DF_short_I_field)
  
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
# SEM results ----
# Hypothesis 1: Herbivory is associated with climate productivity indirectly via plant defense traits and there is no quadratic 
# relationship with herbivory and plant defense traits.
# Field
interaction_field<-SEM_results(model = c("lm1.3", "lm2.1", "lm3.1", "lm4.1"))
summary(interaction_field)
AIC(interaction_field)

#Garden
interaction_Garden<-SEM_results(Loc="Garden",model = c("lm1.3", "lm2.1", "lm3.1", "lm4.1"),random = '',mod_fun = "lm",
                                corError = list(quote(Conc_t_sc %~~% Trichomes_t_sc)),
                                byDate = T)
summary(interaction_Garden)
AIC(interaction_Garden,aicc = T)

# Hypothesis 2: Herbivory is associated with climate productivity indirectly via plant defense traits and there is an 
# additional direct quadratic relationship with herbivory and plant defense traits.
# Field
interaction_field_clim2<-SEM_results()
summary(interaction_field_clim2)
AIC(interaction_field_clim2)

#Garden
interaction_Garden_clim2<-SEM_results(Loc="Garden",random = "",mod_fun='lm',
                                      corError = list(quote(Conc_t_sc %~~% Trichomes_t_sc)),
                                      byDate = T)
summary(interaction_Garden_clim2)
AIC(interaction_Garden_clim2,aicc = T)

# Hypothesis 3: Herbivory and plant traits are only directly and linearly associated with climate productivity.
# Field
climate_field<-SEM_results(model = c("lm1.2", "lm2.1", "lm3.1", "lm4.1"))
summary(climate_field)
AIC(climate_field)

#Garden
Climate_Garden<-SEM_results(Loc="Garden",model = c("lm1.2", "lm2.1", "lm3.1", "lm4.1"),random = "",mod_fun='lm',
                            corError = list(quote(Conc_t_sc %~~% Trichomes_t_sc)),
                            byDate = T)
summary(Climate_Garden)
AIC(Climate_Garden,aicc = T)

# Hypothesis 4: Herbivory and plant traits are only directly and quadratically associated with climate productivity.
# Field
Non_interaction_field<-SEM_results(model = c("lm1.1", "lm2", "lm3", "lm4"))
summary(Non_interaction_field)
AIC(Non_interaction_field)

# Garden
Non_interaction_Garden<-SEM_results(Loc="Garden",model = c("lm1.1", "lm2", "lm3", "lm4"),random = "",mod_fun='lm',
                                    corError = list(quote(Conc_t_sc %~~% Trichomes_t_sc)),
                                    byDate = T)
summary(Non_interaction_Garden)
AIC(Non_interaction_Garden,aicc = T)

# Graph of Climate PCA and temperature across latitude ----


## Map ----
Pop_info<-dbReadTable(con,
                      'pop_info_2022_and_2023') %>% 
  select(Pop,Latitude,Longitude)

cn <- gadm(country = "USA", level = 1,path=tempdir())

mat <- raster(worldclim_global("bio",res=2.5,path=tempdir()))*10

clipxy <- c(-100,-65,25,50)

mat<-rast(crop(mat,clipxy))

OR <- crop(cn,clipxy)

br <- colorRampPalette(c("darkblue","blue","cyan","green","yellow","orange","red"))

map_clim<-ggplot() + 
  geom_spatraster(data=mat*0.1)+
  geom_spatvector(data=OR,fill="NA",color="black") +
  geom_point(data=Pop_info,aes(x=Longitude,y=Latitude),shape=1,size=3)+
  scale_fill_gradientn(colors=br(10),
                       name=bquote(atop("Mean annual","Temperature (\u00B0C)")),
                       na.value="transparent")+
  theme(panel.background = element_blank(),
        legend.position = "top")+
  theme_void(base_size = 18);map_clim

## Biplot figure ----

source('R_code/Biplot_function.R')

ClimBiplot<-PCbiplot(Clim_ave,rot_x=-1)+ C_theme;ClimBiplot

Clim_PC<-(map_clim|ClimBiplot)+plot_annotation(tag_levels = "A");Clim_PC

ggsave("Clim_PC.jpg",
       device = "jpg",plot = Clim_PC,
       path = "Figures",dpi = 400,width = 12, 
       height = 5,limitsize = F)


cor.test(as.matrix(Data_prep(PopLevel = T)[,c('Latitude')]),as.matrix(Data_prep(PopLevel = T)[,c('Clim_ave_PC1')]))

# SEM figures ----
Field_sem<-semGraph(Non_interaction_field);Field_sem

Garden_sem<-semGraph(Non_interaction_Garden);Garden_sem

SEM_fig<-(Field_sem | Garden_sem)+plot_annotation(tag_levels = "A");SEM_fig


ggsave("SEM.jpg",
       device = "jpg",plot = SEM_fig,
       path = "Figures",dpi = 400,width = 10, 
       height = 5,limitsize = F)





# Significant trends in the SEM ---- 
Custom_ggplot<-function(loc="Field",response='Trichomes',predictor='Clim_ave_PC1',deg=2,random="+(1|Pop:Time)",family="poisson"){
  Data<-Data_prep(loc=loc) %>% 
  drop_na() %>% 
    mutate(Pop=as.factor(Pop))
  
  Data_pop<-Data_prep(loc=loc,PopLevel = T) %>% 
    drop_na()
  
  max_l<-max(Data[,predictor],na.rm=T)
  
  min_l<-min(Data[,predictor],na.rm=T)
  
  values<-seq(from=min_l,to=max_l,length.out=100)
  
  m<-glmmTMB(as.formula(paste0(response,'~poly(',predictor,',',deg,')', random)),Data,family = family)
  
  predicted<-as.data.frame(predict_response(m,
                                     terms=c(paste0(predictor,'[',paste(values, collapse = ", "),']')), margin="empirical",
                                     ))

  
  ggplot(data=predicted,aes(x=x,y=predicted))+
    geom_point(data=Data,aes(x=!!sym(predictor),y=!!sym(response)),alpha=0.3,shape = 16)+
    geom_point(data=Data_pop,aes(x=!!sym(predictor),y=!!sym(response)),col="darkred",size=3)+
    geom_ribbon(aes(x=x,y=predicted,ymin=conf.low,ymax = conf.high), fill = "grey70",alpha=0.5) + 
    geom_line(size=1)
  
}

## Field figure----


# Field climate versus trichomes
ClimXtrich<-Custom_ggplot()+
  labs(x="Climate productivity",y="Trichomes") +
  C_theme;ClimXtrich

ClimXglyc<-Custom_ggplot(predictor = 'Clim_ave_PC1',response = "Conc", family = "gaussian",deg=1)+
  labs(x="Climate productivity",y="Glycoalkaloids (mg/mg)") +
  C_theme;ClimXglyc

# Field climate versus Herbivores
ClimsqXherb<-Custom_ggplot(predictor = 'Clim_ave_PC1',response = "herb_p", family = beta_family(),deg=2)+
  labs(x="Climate productivity",y="Herbivory (%)") +
  C_theme;ClimsqXherb

# Field glycoalkaloids versus Herbivory
glycXherb<-Custom_ggplot(predictor = 'Conc',response = "herb_p", family = beta_family(),deg=1)+
  labs(x="Glycoalkaloids (mg/mg)",y='Herbivory (%)') +
  C_theme;glycXherb

Field_pan<-((ClimXtrich|ClimXglyc)/(ClimsqXherb|glycXherb))+plot_annotation(tag_levels = "A");Field_pan

ggsave("Field_pan.jpg",
       device = "jpg",plot = Field_pan,
       path = "Figures",dpi = 400,width = 12, 
       height = 12,limitsize = F)

## Garden figure ----

# Garden climate versus glycoalkaloids
climXglyc<-Custom_ggplot(loc = "Garden",predictor = 'Clim_ave_PC1',response = "Conc", family = gaussian(link = "log"),deg=2)+
  labs(x="Climate productivity",y="Glycoalkaloids (mg/mg)") +
  C_theme;climXglyc

# Garden climate versus trichomes
climsqXtri_gard<-Custom_ggplot(loc = "Garden",predictor = 'Clim_ave_PC1',response = "Trichomes", family = gaussian(link = "log"),deg=2)+
  labs(x="Climate productivity",y="Trichomes") +
  C_theme;climsqXtri_gard

herbXSLA_gard<-Custom_ggplot(loc = "Garden",predictor = 'SLA',response = "herb_p", family = beta_family(),deg=1)+
  labs(x="SLA",y="Herbivory (%)") +
  C_theme;herbXSLA_gard


gard_pan<-((climXglyc|climsqXtri_gard)/(herbXSLA_gard|plot_spacer()))+plot_annotation(tag_levels = "A");gard_pan


ggsave("gard_pan.jpg",
       device = "jpg",plot = gard_pan,
       path = "Figures",dpi = 400,width = 12, 
       height = 12,limitsize = F)
# Appendix AICs -----
# Herbviory data from early in the year
AIC(SEM_results(Loc="Garden",model = c("lm1.1", "lm2", "lm3", "lm4"),random = "",mod_fun='lm',
                corError = list(quote(Conc_t_sc %~~% Trichomes_t_sc)),
                Time='Early'),aicc = T)

# Herbviory data from the middle of the year
AIC(SEM_results(Loc="Garden",model = c("lm1.1", "lm2", "lm3", "lm4"),random = "",mod_fun='lm',
                corError = list(quote(Conc_t_sc %~~% Trichomes_t_sc)),
                Time='Mid'),aicc = T)

# Herbviory data from the end of the year
AIC(SEM_results(Loc="Garden",model = c("lm1.1", "lm2", "lm3", "lm4"),random = "",mod_fun='lm',
                corError = list(quote(Conc_t_sc %~~% Trichomes_t_sc)),
                Time='Late'),aicc = T)

# Herbivory from all time points of the experiment
AIC(SEM_results(Loc="Garden",model = c("lm1.1", "lm2", "lm3", "lm4"),random = "",mod_fun='lm',
                corError = list(quote(Conc_t_sc %~~% Trichomes_t_sc),quote(SLA_t_sc %~~% Trichomes_t_sc)),
                byDate = T, start_date = "2023-05-15",end_date = "2023-10-15"),aicc = T)

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
