## Code by: Jacob Herschberger
## Date: September 2025
## Email: j.herschberger@ufl.edu
## Project: Culprits of plant defense variation across a latitudinal gradient.

# Import packages ----
# Run install.packages([Package name]) if package is not installed
library(conflicted)
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
library(DHARMa)
library(emmeans)
library(GGally)
library(ggplotify)
library(grid)


conflicts_prefer(dplyr::select(),
                 dplyr::filter)

# Import and prep data ----
source("R_code/Functions.R")

Trait_data<-read.csv("Data/Combined_herbivory_and_trait_data.csv") %>% 
  mutate(
    herb_p=(transform_perc(herb_p)),
    Date=as.Date(Date,origin = "1970-01-01"))

PCs<-read.csv(
                 "Data/Bioclims_1970_ave.csv") %>% mutate(Latitude_sc=scale(Latitude),
                                                 Latitude_sc_sq=Latitude_sc^2,
                                                 `Climate PC1`=-Clim_PC1,
                                                 `Productivity PC`=`Climate PC1`,
                                                 'Productivity_sc'= scale(`Climate PC1`,center = T),
                                                 'Productivity_sc_sq' =  Productivity_sc^2) %>% 
  mutate(
    'MAT (°C)' = MAT,
    'Tsd (°C)' = Tsd,
    'AP (cm)' = AP,
    'PWQ (cm)' = PWQ
  ) 


# SEM results ----
Field_sem_sum<-SEM_results(lin="Productivity",sq='Productivity',model = c("lm1.1", "lm2", "lm3", "lm4"))
summary(Field_sem_sum)


# Garden
Garden_sem_sum<-SEM_results(lin="Productivity",sq='Productivity',Loc="Garden",model = c("lm1.1", "lm2", "lm3", "lm4"),
                        random = "+ (1|Pop)",mod_fun='glmmTMB',
                                    corError = list(quote(Conc_t_sc %~~% Trichomes_t_sc)),
                                    byDate = T,group = c("Plant_ID","Pop"))
summary(Garden_sem_sum)


## Figure 1: Maps ----
Pop_info<-read.csv("Data/Field_2022_cords.csv") %>% 
  select(Pop,Latitude,Longitude) %>% 
  rbind(.,read.csv("Data/Field_2023_pop_info.csv") %>% 
          select(Pop,Latitude,Longitude)) %>% 
  filter(Pop!="") %>% distinct(.,Pop,.keep_all = TRUE) %>% 
  right_join(Data_prep(PopLevel = T) %>% 
               select(Pop,Year),
             by=join_by(Pop)) %>% 
  group_by(Pop) %>% summarise(
    Year = if (n_distinct(Year) == 1) as.character(first(Year)) else "Both Years",
    across(where(is.numeric), first),
    .groups   = "drop"
    )

tempdir<-tempdir()

cn <- gadm(country = "USA", level = 1,path=tempdir)

clipxy <- c(-100,-65,25,50)

OR <- crop(cn,clipxy)

obs<-read.csv("Data/Solanum_carolinense_inat.csv") %>% 
  drop_na(latitude) %>% 
  rename(Longitude=longitude,
         Latitude=latitude) %>% 
  filter(
    Longitude >= clipxy[1], Longitude <= clipxy[2],
    Latitude  >= clipxy[3], Latitude  <= clipxy[4]
  )

map_clim<-ggplot() + 
  geom_spatvector(data=OR,fill="NA",color="black") +
  geom_point(data=obs,aes(x=Longitude,y=Latitude),shape=21,size=0.1,fill="grey",alpha=0.25)+
  geom_point(data=Pop_info,aes(x=Longitude,y=Latitude,shape = Year, fill = Year),size=3,stroke=1.5)+
  scale_shape_manual(values = c(21,22,23))+
  scale_fill_manual(values = c("darkred","darkgray","white"))+
  theme_void(base_size = 16) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.2)
    );map_clim

PC_plot<-PCbiplot(PCs %>% select('MAT':'PWQ'),font_size = 4,rot_x = -1) 

vars<-make_plots(PCs, "Latitude", 
                 c("Productivity PC","AP (cm)","MAT (°C)","PWQ (cm)","Tsd (°C)"), 
                 ncol = 2,
                 extra_plots = list("1"=PC_plot))

clim_vars<-(map_clim|vars) + 
  plot_annotation(tag_levels = "A")

ggsave("fig_1.jpg",
       device = "jpg",plot = clim_vars,
       path = "Figures",dpi = 400,width = 12, 
       height = 6)

# Figure 2: SEMs ----
Field_sem<-semGraph(Field_sem_sum);Field_sem

Garden_sem<-semGraph(Field_sem_sum);Garden_sem

SEM_fig<-(Field_sem | Garden_sem)+plot_annotation(tag_levels = "A");SEM_fig

ggsave("SEM.jpg",
       device = "jpg",plot = SEM_fig,
       path = "Figures",dpi = 400,width = 10, 
       height = 5,limitsize = F)

# Figures 3&4: Significant trends in the SEM ---- 

## Field figure----


# Field climate versus trichomes
ClimXtrich<-Custom_ggplot(predictor = "Productivity_sc")+
  labs(y="Trichomes",x="Productivity PC") +
  C_theme();ClimXtrich

ClimXglyc<-Custom_ggplot(predictor = "Productivity_sc",response = "Conc", family = "gaussian",deg=1)+
  labs(x="Productivity PC",y="Glycoalkaloids (mg/mg)") +
  C_theme();ClimXglyc

# Field climate versus Herbivores
ClimsqXherb<-Custom_ggplot(predictor = "Productivity_sc",response = "herb_p", family = beta_family(),deg=2)+
  labs(x="Productivity PC",y="Herbivory (%)") +
  scale_y_continuous(labels = function(x) paste0(x * 100))+
  C_theme();ClimsqXherb

# Field glycoalkaloids versus Herbivory
glycXherb<-Custom_ggplot(predictor = 'Conc',response = "herb_p", family = beta_family(),deg=1)+
  labs(x="Glycoalkaloids (mg/mg)",y='Herbivory (%)') +
  scale_y_continuous(labels = function(x) paste0(x * 100))+
  C_theme();glycXherb

Field_pan<-((ClimXtrich|ClimXglyc)/(ClimsqXherb|glycXherb))+plot_annotation(tag_levels = "A");Field_pan

ggsave("Field_pan.jpg",
       device = "jpg",plot = Field_pan,
       path = "Figures",dpi = 400,width = 12, 
       height = 11,limitsize = F)

## Garden figure ----

# Garden climate versus glycoalkaloids
climXglyc<-Custom_ggplot(loc = "Garden",predictor = "Productivity_sc",response = "Conc", family = gaussian(link = "log"),deg=1,random = "+(1|Pop)")+
  labs(x="Productivity PC",y="Glycoalkaloids (mg/mg)") +
  C_theme();climXglyc

# Garden climate versus trichomes
climsqXtri_gard<-Custom_ggplot(loc = "Garden",predictor = "Productivity_sc",response = "Trichomes", family = gaussian(link = "log"),deg=2,random = "+(1|Pop)")+
  labs(x="Productivity PC",y="Trichomes") +
  C_theme();climsqXtri_gard

climsqXherb_gard<-Custom_ggplot(loc = "Garden",predictor = "Productivity_sc",response = "herb_p", 
                                family = gaussian(link = "log"),deg=2,random = "+(1|Pop)",
                                Trend = F)+
  labs(x="Productivity PC",y="Herbivory (%)") +
  scale_y_continuous(labels = function(x) paste0(x * 100))+
  C_theme();climsqXherb_gard


gard_pan<-wrap_plots(list(climsqXtri_gard,climXglyc,climsqXherb_gard),ncol = 1)+
  plot_annotation(tag_levels = "A");gard_pan


ggsave("gard_pan.jpg",
       device = "jpg",plot = gard_pan,
       path = "Figures",dpi = 400,width = 7, 
       height = 15,limitsize = F)

# Population level correlation of defense traits ----

Pop_cor<-full_join(
Data_prep(loc="Field",PopLevel = T) %>% 
  select(Pop,Trichomes,SLA,Conc,herb_p) %>% 
  pivot_longer(.,cols=c(Trichomes,SLA,Conc,herb_p),
               values_to = "Field"), 
Data_prep(loc="Garden",PopLevel = T,byDate = F,Time.var = "Mid|Early|Late")%>% 
  select(Pop,Trichomes,SLA,Conc,herb_p) %>% 
  pivot_longer(.,cols=c(Trichomes,SLA,Conc,herb_p),
               values_to = "Garden"),
by = join_by(Pop==Pop,name==name)
) %>% rename(.,"Trait"=name) 

Pop_cor%>% filter(Trait == "Trichomes") %>% 
  select(Field,Garden) %>% drop_na() %>% 
  summarise(cor = list(cor.test(Field, Garden, method = "spearman"))) %>%
  pull(cor)

Pop_cor%>% filter(Trait == "Conc") %>% 
  select(Field,Garden) %>% drop_na() %>% 
  summarise(cor = list(cor.test(Field, Garden, method = "spearman"))) %>%
  pull(cor)


gard_v_field_herb<-glmmTMB(herb_p~Loc,data=Data_prep(loc = "Garden|Field"),
                      family = beta_family())

Anova(gard_v_field_herb)

emmeans(gard_v_field_herb,"Loc",
        type="response", infer = T)

# Height versus defense traits ----
Data_prep(loc="Graden|Field") %>% 
  select(Loc,Height,Trichomes_t_sc,SLA_t_sc,Conc_t_sc) %>% 
  mutate(Height=scale(Height)) %>% 
  pivot_longer(.,cols=c(Trichomes_t_sc,SLA_t_sc,Conc_t_sc)) %>% 
  lm(value~Height*name*Loc,.,) %>% 
  emtrends(.,~name*Loc,var="Height",infer=T)

  
  
# Figure S2: Herbivory and phenology through time in the garden----
Pop_level<-Data_prep(loc = "Garden",byDate = T,
                     start_date ="2023-04-15" , end_date = "2023-08-29",
                     group = c('Pop','Date')) %>% 
  mutate(Flowering_p=flowers/count) 

Leaves_time<-ggplot(Pop_level,aes(Date,Leaves,,col=Pop)) +
  geom_point(show.legend=F)+
  geom_smooth(method="glm",show.legend=F,formula = y~poly(x,2),se=F)+
  labs(y="Leaves")+
  C_theme(14);Leaves_time

Flowers_time<-ggplot(Pop_level,aes(Date,Flowering_p,col=Pop)) +
  geom_point(show.legend=F)+
  geom_smooth(show.legend=F,se=F)+
  labs(y='Proportion flowering')+
  C_theme(14);Flowers_time

Herbivory_time<-ggplot(Pop_level,aes(Date,max_herb,col=Pop)) +
  geom_point()+
  geom_smooth(method="glm",se=F,formula = y~poly(x,2))+
  labs(y="Herbivory (%)")+
  scale_y_continuous(labels = function(x) paste0(x * 100))+
  C_theme(14);Herbivory_time

appendix<-((Leaves_time | Flowers_time)/(Herbivory_time | plot_spacer()))+plot_annotation(tag_levels = "A");appendix

ggsave("Appendix_S2.jpg",
       device = "jpg",plot = appendix,
       path = "Figures",dpi = 400,width = 12, 
       height = 11,limitsize = F)

# Herbivory in response to plant traits and climete at different times of the year ----
# Herbivory graphing function
Herb_by_time<-function(herb='herb_p_t_sc',family.var='poisson',time="Early",ad_form=""){
  Data<-Data_prep(loc = "Garden",byDate = F,Time.var = time,group = c('Plant_ID','Time'))
  Data1<-Data_prep(loc = "Garden",byDate = T,
                   start_date = "2023-04-15",end_date = "2023-09-15",group = c('Plant_ID')) %>% 
    mutate(Time="All",
           Loc='Garden')
  
  Data<- rbind(Data,Data1)
  
  model<-glmmTMB(as.formula(paste0(herb, '~ Latitude_sc + Latitude_sc_sq + (Trichomes_t_sc + Conc_t_sc + SLA_t_sc)',ad_form)),
                 family=family.var,data=Data)
  list(Data,model)
}

# Models of herbivory using the 75th quantile, mean, and max herbivory.
all_herb_mod_quant<-Herb_by_time(herb='quant_herb_0.75',family=beta_family(),time='Early|Mid|Late',ad_form='*Time')
AIC(all_herb_mod_quant[[2]])
emtrends(all_herb_mod_quant[[2]],~Time,var="SLA_t_sc",infer=T)
emtrends(all_herb_mod_quant[[2]],~Time,var="Conc_t_sc",infer=T)
emtrends(all_herb_mod_quant[[2]],~Time,var="Trichomes_t_sc",infer=T)
emtrends(all_herb_mod_quant[[2]],~Time,var="Latitude_sc",infer=T)
emtrends(all_herb_mod_quant[[2]],~Time,var="Latitude_sc_sq",infer=T)

all_herb_mod_mean<-Herb_by_time(herb='herb_p',family=beta_family(),time='Early|Mid|Late',ad_form='*Time')
AIC(all_herb_mod_mean[[2]])
emtrends(all_herb_mod_mean[[2]],~Time,var="SLA_t_sc",infer=T)
emtrends(all_herb_mod_mean[[2]],~Time,var="Conc_t_sc",infer=T)
emtrends(all_herb_mod_mean[[2]],~Time,var="Trichomes_t_sc",infer=T)
emtrends(all_herb_mod_mean[[2]],~Time,var="Latitude_sc",infer=T)
emtrends(all_herb_mod_mean[[2]],~Time,var="Latitude_sc_sq",infer=T)

all_herb_mod_max<-Herb_by_time(herb='max_herb',family=beta_family(),time='Early|Mid|Late',ad_form='*Time')
AIC(all_herb_mod_max[[2]])
emtrends(all_herb_mod_max[[2]],~Time,var="SLA_t_sc",infer=T)
emtrends(all_herb_mod_max[[2]],~Time,var="Conc_t_sc",infer=T)
emtrends(all_herb_mod_max[[2]],~Time,var="Trichomes_t_sc",infer=T)

# Appendix: AICs at different herbivory observations -----
# Herbviory data from early in the year
early_sem<-SEM_results(Loc="Garden",model = c("lm1.1", "lm2", "lm3", "lm4"),random = "",mod_fun='lm',
                       corError = list(quote(Conc_t_sc %~~% Trichomes_t_sc)),
                       Time='Early',group = "Plant_ID",ICC=F)
AIC(early_sem,aicc=T)
summary(early_sem)

# Herbviory data from the middle of the year
mid_sem<-SEM_results(Loc="Garden",model = c("lm1.1", "lm2", "lm3", "lm4"),random = "",mod_fun='lm',
                     corError = list(quote(Conc_t_sc %~~% Trichomes_t_sc)),
                     Time='Mid',group = "Plant_ID",ICC=F)

AIC(mid_sem,aicc = T)
summary(mid_sem)

# Herbviory data from the end of the year
end_sem<-SEM_results(Loc="Garden",model = c("lm1.1", "lm2", "lm3", "lm4"),random = "",mod_fun='lm',
                     corError = list(quote(Conc_t_sc %~~% Trichomes_t_sc)),
                     Time='Late',group = "Plant_ID",ICC=F)

AIC(end_sem,aicc = T)
summary(end_sem)

# Herbivory from all time points of the experiment
all_sem<-SEM_results(Loc="Garden",model = c("lm1.1", "lm2", "lm3", "lm4"),random = "",mod_fun='lm',
                     corError = list(quote(Conc_t_sc %~~% Trichomes_t_sc),quote(SLA_t_sc %~~% Trichomes_t_sc)),
                     byDate = T, start_date = "2023-05-15",end_date = "2023-10-15",group = "Plant_ID",ICC=F)

AIC(all_sem,aicc = T)
summary(all_sem)

# Figure S3: Herbivory by traits in different times of the year ----
trait_x_herb_plot<-function(response,y_lab){
  Data<-all_herb_mod_quant[[1]] %>% 
    pivot_longer(cols = c(Trichomes,SLA,Conc)) %>% 
    mutate(Time=as.factor(Time))
  ggplot(data = Data,
       aes(x=value,y=!!sym(response),col = Time)) +
  geom_point()+
  geom_smooth(method="glm",formula=y~x,
              method.args=list(family=beta_family()),
              se=F)+
  facet_wrap(~name,scale="free_x",nrow=2)+
  C_theme(14) + 
    theme(legend.position = c(0.75,0.2))+
    labs(x="Trait value",y=y_lab)}


trait_x_herb_ave<-trait_x_herb_plot("herb_p","Average herbviory (%)")+
  scale_y_continuous(labels = function(x) paste0(x * 100))

trait_x_herb_plot("max_herb","Max herbviory")

trait_x_herb_plot("quant_herb_0.75","Herbviory (75th quantile)")

ggsave("Appendix_S3.jpg",
       device = "jpg",plot = trait_x_herb_ave,
       path = "Figures",dpi = 400,width = 12, 
       height = 11,limitsize = F)

