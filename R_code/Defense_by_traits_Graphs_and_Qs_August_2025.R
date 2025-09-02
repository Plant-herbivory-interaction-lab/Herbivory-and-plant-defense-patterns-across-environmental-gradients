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
library(DHARMa)
library(emmeans)

conflicts_prefer(dplyr::select(),
                 dplyr::filter)

# Database connection ----
con <-dbConnect(SQLite(), 'Data/Data.db')


# Import and prep data ----
source("R_code/Prep_data_for_all_analysis_July.R")
source("R_code/Psem_graphing.R")


Data_prep<-function(loc="Field",PopLevel=F,long=F,ClimateLong=F,
                    Treatment=F,Time.var='Mid',start_date="2023-06-15",
                    end_date="2023-07-29",byDate=F,grouptime=F,...){


  
Combined_data1<-Combined_data1%>% 
  left_join(PCs %>% 
              select(Latitude,Pop,Clim_ave_PC1,Soil_PC1,Soil_PC2,Clim_ave_PC2,Aridity,Latitude_quad)
            %>% mutate(Latitude_sc=scale(Latitude),
                       Latitude_sc_sq=Latitude_sc^2,)) %>% 
  drop_na()%>% left_join(dbReadTable(con,"Combined_herbs_pop") %>% 
                                   select(Pop,Time,loc,N_Herbivores_mean,N_Herbivores_sum,Feeding_guild) %>%
                                   filter(Feeding_guild=="Chewing"),
                                 by = join_by(Pop, Year==Time,Loc==loc)) 

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
        filter(str_detect(Time,Time.var))
  }
 
  
  Combined_data1<-Combined_data1 %>%
  {if(grouptime==F){group_by(.,Plant_ID)}else{group_by(.,Plant_ID,Time,Loc)}} %>% 
  summarise(Pop=unique(Pop),
            Loc = if (PopLevel) unique(Loc) else first(Loc),
            Time = if (PopLevel) unique(Time) else first(Time),
            Date=sample(c(min(Date,na.rm = T),max(Date,na.rm = T)),size=1),
            quant_herb_0.75=as.vector(quantile(herb_p,na.rm = T)[3]),
            max_herb=max(herb_p,na.rm = T),
            across(where(is.numeric), ~ mean(., na.rm = TRUE))) %>% 
  ungroup() #%>% 
    #filter(SLA>40&SLA<270)
}

 

if(PopLevel==T&Treatment==T){Combined_data1<-Combined_data1 %>% 
  group_by(Pop,Loc,Treatment,Time)}

if(PopLevel==T&Treatment==F){Combined_data1<-Combined_data1 %>% 
  group_by(Pop,Loc,Time)}


if(PopLevel==T){Combined_data1<-Combined_data1 %>% 
  summarise(across(where(is.numeric), \ (x) mean(x, na.rm = TRUE)),Plant_N=length(Pop)) %>% 
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


Combined_data1<-Combined_data1 %>% ungroup() %>% 
  mutate(
    Conc_t=log(Conc),
    SLA_t=log(SLA),
    Trichomes_t=log(Trichomes),
    herb_p_t=logit(herb_p),
    Conc_t_sc=scale(Conc_t)[,1],
    SLA_t_sc=scale(SLA_t)[,1],
    Trichomes_t_sc=scale(Trichomes_t)[,1],
    herb_p_t_sc=scale(herb_p_t)[,1],
    Plant=c(1:length(Conc))
  )  %>% filter(SLA_t_sc>-3&SLA_t_sc<3)

Combined_data1
}

# Graph theme setup----
C_theme<-theme_bw(base_size = 18)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

# SEMs ----
# SEM of the plant traits, climate and herbivore relations at the plant individual level.

SEM_results <- function(Loc = "Field", mod_fun='glmmTMB',model = c("lm1.1", "lm2", "lm3", "lm4"), random = "+ (1|Pop:Time)",Time="mid",
                        byDate=F,start_date="2023-06-15",end_date="2023-07-29",corError = list(
                          quote(SLA_t_sc %~~% Trichomes_t_sc),
                          quote(SLA_t_sc %~~% Conc_t_sc))) {

 
  method<-get(mod_fun)
  
  formula_strings <- c(
    lm1.1 = paste0('herb_p_t_sc ~ Latitude_sc + Latitude_sc_sq + Trichomes_t_sc + Conc_t_sc + SLA_t_sc', random), 
    lm1.2 = paste0('herb_p_t_sc ~ Latitude_sc + Trichomes_t_sc + Conc_t_sc + SLA_t_sc', random),
    lm2 = paste0('Conc_t_sc ~ Latitude_sc + Latitude_sc_sq', random),           
    lm2.1 = paste0('Conc_t_sc ~ Latitude_sc',random),           
    lm3 = paste0('SLA_t_sc ~ Latitude_sc + Latitude_sc_sq', random),
    lm3.1 = paste0('SLA_t_sc ~ Latitude_sc', random),           
    lm4 = paste0('Trichomes_t_sc ~ Latitude_sc + Latitude_sc_sq', random),  
    lm4.1 = paste0('Trichomes_t_sc ~ Latitude_sc', random)                  
  )
  
  DF_short_I_field <- Data_prep(loc=Loc,Time.var=Time,byDate = byDate,
                                start_date = start_date, 
                                end_date = end_date) %>% 
    select(c(Date,unique(unlist(lapply(formula_strings, function(fstr) {
           all.vars(as.formula(fstr))
      }))))) %>% drop_na()
  
  
  print(min(DF_short_I_field$Date))
  print(max(DF_short_I_field$Date))
  
  model_list <- lapply(formula_strings, function(fstr) method(as.formula(fstr), DF_short_I_field))
 
  AICs<-sapply(model_list,FUN=AIC)
  
  names(AICs)<-c("Quadratic","Linear",
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

# Hypothesis 1: Herbivory and plant traits are linearly associated with latitude.
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

# Hypothesis 2: Herbivory and plant traits are quadratically associated with latitude.
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

## Figure 1: Maps ----
Pop_info<-dbReadTable(con,
                      'pop_info_2022_and_2023') %>% 
  select(Pop,Latitude,Longitude) %>% 
  right_join(Data_prep(PopLevel = T) %>% select(Pop,Time)) %>% 
  group_by(Pop) %>% summarise(
    Latitude  = first(Latitude),
    Longitude = first(Longitude),
    Year = if (n_distinct(Time) == 1) as.character(first(Time)) else "Both Years",
    .groups   = "drop"
    )

tempdir<-tempdir()

cn <- gadm(country = "USA", level = 1,path=tempdir)

obs<-dbReadTable(con,"Plant_range")

clipxy <- c(-100,-65,25,50)

OR <- crop(cn,clipxy)

map_clim<-ggplot() + 
  geom_spatvector(data=OR,fill="NA",color="black") +
  geom_point(data=obs,aes(x=longitude,y=latitude),shape=21,size=0.1,fill="grey",alpha=0.25)+
  geom_point(data=Pop_info,aes(x=Longitude,y=Latitude,shape = Year, fill = Year),size=3)+
  scale_shape_manual(values = c(21,22,23))+
  scale_fill_manual(values = c("darkred","darkgray","white"))+
  theme(panel.background = element_blank(),
        legend.position = "top")+
  theme_void(base_size = 10);map_clim



ggsave("map_clim_fig_1.jpg",
       device = "pdf",plot = map_clim,
       path = "Figures",dpi = 400,width = 6, 
       height = 4)

# Figure 2: SEMs ----
Field_sem<-semGraph(Non_interaction_field);Field_sem

Garden_sem<-semGraph(Non_interaction_Garden);Garden_sem

SEM_fig<-(Field_sem | Garden_sem)+plot_annotation(tag_levels = "A");SEM_fig


ggsave("SEM.jpg",
       device = "jpg",plot = SEM_fig,
       path = "Figures",dpi = 400,width = 10, 
       height = 5,limitsize = F)

# Figures 3&4: Significant trends in the SEM ---- 
Custom_ggplot<-function(loc="Field",response='Trichomes',predictor='Clim_ave_PC1',deg=2,random="+(1|Pop:Time)",family="poisson"){
  Data<-Data_prep(loc=loc) %>% 
    mutate(Pop=as.factor(Pop))
  
  Data_pop<-Data_prep(loc=loc,PopLevel = T,grouptime = T) 
  
  max_l<-max(Data[,predictor],na.rm=T)
  
  min_l<-min(Data[,predictor],na.rm=T)
  
  values<-seq(from=min_l,to=max_l,length.out=100)
  
  m<-glmmTMB(as.formula(paste0(response,'~poly(',predictor,',',deg,')', random)),Data,family = family)
  
  predicted<-as.data.frame(predict_response(m,
                                     terms=c(paste0(predictor,'[',paste(values, collapse = ", "),']')), margin="empirical",
                                     ))

  
  ggplot(data=predicted,aes(x=x,y=predicted))+
    geom_ribbon(aes(x=x,y=predicted,ymin=conf.low,ymax = conf.high), fill = "grey70",alpha=0.5) + 
    geom_line(linewidth=1)+
    geom_point(data=Data,aes(x=!!sym(predictor),y=!!sym(response)),alpha=0.3,shape = 16)+
    geom_point(data=Data_pop,aes(x=!!sym(predictor),y=!!sym(response)),col="darkred",size=3)
  
}

## Field figure----


# Field climate versus trichomes
ClimXtrich<-Custom_ggplot(predictor = "Latitude")+
  labs(y="Trichomes",x="Latitude") +
  C_theme;ClimXtrich

ClimXglyc<-Custom_ggplot(predictor = "Latitude",response = "Conc", family = "gaussian",deg=1)+
  labs(x="Latitude",y="Glycoalkaloids (mg/mg)") +
  C_theme;ClimXglyc

# Field climate versus Herbivores
ClimsqXherb<-Custom_ggplot(predictor = "Latitude",response = "herb_p", family = beta_family(),deg=2)+
  labs(x="Latitude",y="Herbivory (%)") +
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
climXglyc<-Custom_ggplot(loc = "Garden",predictor = "Latitude",response = "Conc", family = gaussian(link = "log"),deg=1,random = "+(1|Pop)")+
  labs(x="Latitude",y="Glycoalkaloids (mg/mg)") +
  C_theme;climXglyc

# Garden climate versus trichomes
climsqXtri_gard<-Custom_ggplot(loc = "Garden",predictor = "Latitude",response = "Trichomes", family = gaussian(link = "log"),deg=2,random = "+(1|Pop)")+
  labs(x="Latitude",y="Trichomes") +
  C_theme;climsqXtri_gard

herbXSLA_gard<-Custom_ggplot(loc = "Garden",predictor = 'SLA',response = "herb_p", family = beta_family(),deg=1,random = "+(1|Pop)")+
  labs(x="SLA",y="Herbivory (%)") +
  C_theme;herbXSLA_gard


gard_pan<-(climsqXtri_gard|climXglyc)+plot_annotation(tag_levels = "A");gard_pan


ggsave("gard_pan.jpg",
       device = "jpg",plot = gard_pan,
       path = "Figures",dpi = 400,width = 12, 
       height = 6,limitsize = F)

# Figure S1: PCs and latitude by PCs graphs ----

source('R_code/Biplot_function.R')

ClimBiplot<-PCbiplot(Clim_ave,rot_x = -1)+ C_theme;ClimBiplot

SoilBiplot<-PCbiplot(Soil[,-1])+ C_theme;SoilBiplot

SC_data<-Data_prep(PopLevel = T)[,c('Latitude','Clim_ave_PC1','Clim_ave_PC2','Soil_PC2','Soil_PC1')] %>% 
  pivot_longer(cols=c('Clim_ave_PC1','Clim_ave_PC2','Soil_PC2','Soil_PC1'),
               values_to = "PC value",
               names_to = c('Variable',"Axis"),
               names_pattern = "^(.*?)_(?:ave_)?(PC\\d+)$" ) %>% 
  mutate(Variable=case_when(Variable=="Clim"~"Climate",
                            .default = Variable),
         `PC value`=case_when(Variable=="Climate"&Axis=="PC1"~-`PC value`,
                              .default = `PC value`))

PC1_graph<-SC_data %>% filter(Axis=="PC1") %>% 
  ggplot(aes(x=Latitude,y=`PC value`,col=Variable)) +
  geom_point() + geom_smooth(method = "glm") + labs(y = "PC1 value")+
  C_theme + theme(legend.position = c(0.85,0.88));PC1_graph

PC2_graph<-SC_data %>% filter(Axis=="PC2") %>% 
  ggplot(aes(x=Latitude,y=`PC value`,col=Variable)) +
  geom_point(show.legend=F) + 
  geom_smooth(method = "glm",show.legend=F,formula = y ~ poly(x,2)) +
  labs(y = "PC2 value")+
  C_theme;PC2_graph

PC_appendix<-((ClimBiplot|SoilBiplot)/(PC1_graph|PC2_graph))+plot_annotation(tag_levels = "A");PC_appendix

ggsave("Appendix_S1.jpg",
       device = "jpg",plot = PC_appendix,
       path = "Figures",dpi = 400,width = 12, 
       height = 12,limitsize = F)

cor_data<-as.data.frame(Data_prep(PopLevel = T))[
  ,c('Latitude','Clim_ave_PC1','Clim_ave_PC2','Soil_PC2','Soil_PC1')] %>% 
  unique()

cor.test(cor_data[,c('Latitude')],cor_data[,c('Clim_ave_PC1')])
cor.test(scale(cor_data[,c('Latitude')])^2,cor_data[,c('Clim_ave_PC2')])

cor.test(cor_data[,c('Latitude')],cor_data[,c('Soil_PC1')])
cor.test(scale(cor_data[,c('Latitude')])^2,cor_data[,c('Soil_PC2')])


# Figure S2: Herbivory and phenology through time ----
Pop_level<-Garden %>% group_by(Pop,Date)%>% 
  summarise(count=n(),
            flowers = sum(fl_m>0, fl_h>0, na.rm = TRUE),
            quant_herb_0.75=as.vector(quantile(herb_p)[3]),
            maxHerb=max(Herby),
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

Herbivory_time<-ggplot(Pop_level,aes(Date,maxHerb,col=Pop)) +
  geom_point()+
  geom_smooth(method="glm",se=F,formula = y~poly(x,2))+
  labs(y="Herbivory (%)")+
  C_theme;Herbivory_time

appendix<-((Leaves_time | Flowers_time)/(Herbivory_time | plot_spacer()))+plot_annotation(tag_levels = "A");appendix

ggsave("Appendix_S2.jpg",
       device = "jpg",plot = appendix,
       path = "Figures",dpi = 400,width = 12, 
       height = 12,limitsize = F)

# Herbivory in response to plant traits and climete at different times of the year ----
# Herbivory graphing function
Herb_by_time<-function(herb='herb_p_t_sc',family.var='poisson',time="Early",ad_form=""){
  Data<-Data_prep(loc = "Garden",byDate = F,Time.var = time,grouptime = T)
  Data1<-Data_prep(loc = "Garden",byDate = T,grouptime = F,start_date = "2023-04-15",end_date = "2023-09-15") %>% 
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
                       Time='Early')
AIC(early_sem,aicc=T)
summary(early_sem)

# Herbviory data from the middle of the year
mid_sem<-SEM_results(Loc="Garden",model = c("lm1.1", "lm2", "lm3", "lm4"),random = "",mod_fun='lm',
                     corError = list(quote(Conc_t_sc %~~% Trichomes_t_sc)),
                     Time='Mid')

AIC(mid_sem,aicc = T)
summary(mid_sem)

# Herbviory data from the end of the year
end_sem<-SEM_results(Loc="Garden",model = c("lm1.1", "lm2", "lm3", "lm4"),random = "",mod_fun='lm',
                     corError = list(quote(Conc_t_sc %~~% Trichomes_t_sc)),
                     Time='Late')

AIC(end_sem,aicc = T)
summary(end_sem)

# Herbivory from all time points of the experiment
all_sem<-SEM_results(Loc="Garden",model = c("lm1.1", "lm2", "lm3", "lm4"),random = "",mod_fun='lm',
                     corError = list(quote(Conc_t_sc %~~% Trichomes_t_sc),quote(SLA_t_sc %~~% Trichomes_t_sc)),
                     byDate = T, start_date = "2023-05-15",end_date = "2023-10-15")

AIC(all_sem,aicc = T)
summary(all_sem)

# Figure S3: Herbivory by traits in different times of the year ----
trait_x_herb_plot<-function(response,y_lab){ggplot(data = all_herb_mod_max[[1]] %>% pivot_longer(cols = c(Trichomes,SLA,Conc)),
       aes(x=value,y=!!sym(response),col=Time)) +
  geom_point()+
  geom_smooth(method="glm",formula=y~x,
              method.args=list(family=beta_family()),
              se=F)+
  facet_wrap(~name,scale="free_x",nrow=2)+
  C_theme + theme(legend.position = c(0.75,0.2))+
    labs(x="Trait value",y=y_lab)}


trait_x_herb_ave<-trait_x_herb_plot("herb_p","Average herbviory")

trait_x_herb_plot("max_herb","Max herbviory")

trait_x_herb_plot("quant_herb_0.75","Herbviory (75th quantile)")

ggsave("Appendix_S3.jpg",
       device = "jpg",plot = trait_x_herb_ave,
       path = "Figures",dpi = 400,width = 12, 
       height = 12,limitsize = F)

# Figure S4: Herbivory versus glycoalkaloids in different populations ----
ggplot(Data_prep(),aes(x=Conc,y=herb_p)) +
  geom_point()+
  geom_smooth(method="glm",
              method.args=list(family=beta_family())) +
  facet_wrap(~round(Latitude,2),scales="free",nrow=7) +
  C_theme + theme(text = element_text(size=12))

ggplot(Data_prep(),aes(x=Trichomes,y=herb_p)) +
  geom_point()+
  geom_smooth(method="glm",
              method.args=list(family=beta_family())) +
  facet_wrap(~round(Latitude,2),scales="free",nrow=7) +
  C_theme + theme(text = element_text(size=12))

