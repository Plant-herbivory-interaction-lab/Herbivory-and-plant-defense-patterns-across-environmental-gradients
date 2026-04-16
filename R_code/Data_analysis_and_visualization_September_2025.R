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
library(tidyterra)
library(DHARMa)
library(emmeans)
library(GGally)
library(ggplotify)
library(grid)


conflicts_prefer(dplyr::select,
                 dplyr::filter)

# Import and prep data ----
source("R_code/Functions.R")

Trait_data<-read.csv("Data/Combined_herbivory_and_trait_data.csv") %>% 
  mutate(
    herb_p=(transform_perc(herb_p)),
    Date=as.Date(Date,origin = "1970-01-01"))
  

PCs<-read.csv("Data/clim_data.csv")

PCbiplot(data1 = PCs %>% select(.,"MAT" ,"Tsd" ,"AP" ,"PWQ"))

# SEM results ----
### Field ----
### Model with nested pop within year ----
summary(glmmTMB(Trichomes_t_sc ~ Climate_PC1_sc + Climate_PC1_sc_sq + 
                  (1|Year/Pop), data = Data_prep()))

Field_sem<-SEM_results(Prod = "",
                       lat_traits = "Climate_PC1_sc + Climate_PC1_sc_sq", 
                       lat_main = "Climate_PC1_sc + Climate_PC1_sc_sq",
                           model = c("lm1", "lm2", "lm3", "lm4"),
                           main_ran = "(1|Year:Pop)",
                           random = "(1|Year:Pop)",year_ran="(1|Year)")
summary(Field_sem)

AIC_psem(Field_sem,AIC.type = "dsep")

### Garden ----
Garden_sem<-SEM_results(Prod = "", Loc = "Garden", main_ran = "(1|Pop)", clim_t = T,
                       lat_traits = "Climate_PC1_sc + Climate_PC1_sc_sq", 
                       lat_main = "Climate_PC1_sc + Climate_PC1_sc_sq",
                       model = c("lm1", "lm2", "lm3", "lm4"),
                       random = "(1|Pop)",year_ran="", corError = list())
summary(Garden_sem)

AIC_psem(Garden_sem,AIC.type = "dsep")




## Figure 1: Maps ----
Pop_info<-Data_prep(loc = "Field|Garden",group = c("Pop","Loc","Year"), PopLevel = T) %>% 
               select(Pop,Year,Loc,NPP, herb_p,
                      Climate_PC1,Longitude, Latitude,
                      Conc,SLA,Trichomes) %>% summarise(
    .by = c(Pop),

    Year = case_when(
      n_distinct(Year[Loc == "Field"]) == 2 ~ "Both Years",
      .default = as.character(first(Year))
    ),
    Loc = case_when(
      n_distinct(Loc) == 2 ~ "Field & Garden",
      .default = first(Loc)
    ),
    across(where(is.numeric), first),
    )

#### Plants per population summary ----
Data_prep(loc = "Field|Garden",group = c("Pop","Loc","Year","Plant_ID"),
                PopLevel = T) %>%
  summarise(.by = c(Pop,Loc),
            Plants = n()) %>% 
  summarise(.by = Loc,
                        Plants_mean=mean(Plants),
                        Plants_sd=sd(Plants))


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
  geom_point(data=obs,aes(x=Longitude,y=Latitude),shape=21,size=0.025,fill="lightgrey",alpha=0.1)+
  geom_point(data=Pop_info,aes(x=Longitude,y=Latitude,shape = factor(Year), color = Loc),size=3,stroke=1.5)+
  scale_shape_manual(name="Year",values = c(24,21,22))+
  scale_color_manual(name="Location",values = c("#440154FF","#21908CFF","#FDE725FF"))+
  theme_void(base_size = 13) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.83, 0.87),
    legend.box =  "horizontal",
    legend.margin = margin(5, 5, 5, 5),
    legend.box.background = element_rect(fill = scales::alpha("white", 0.7), color = "black")
    );map_clim

pca<-PCbiplot(data1 = PCs %>% select(., all_of(c('MAT',
                                               'AP','PWQ',"Tsd"))),
              font_size = 4,rot_x = -1,ext = 2) + C_theme(size=13);pca

npp<-ggplot(data=Pop_info %>% filter(grepl("Field",Loc)), aes(x = Latitude, y = NPP)) +
  geom_point() + geom_smooth(method = "glm") + 
  ylab(expression("Ten year NPP"[g]*" (kg*C/m"^"2"*")")) +
  C_theme(size=13);npp

pc1_lat<-ggplot(data=Pop_info %>% filter(grepl("Field",Loc)), aes(x = Latitude, y = Climate_PC1)) +
  geom_point() + geom_smooth(method = "glm") + 
  ylab(expression("Climate PC1")) +
  C_theme(size=13);pc1_lat

clim_vars<-(map_clim|(pca/pc1_lat)) + 
  plot_annotation(tag_levels = "A") +
  plot_layout(widths = c(1.25,0.65))

ggsave("fig_1.jpg",
       device = "jpg",plot = clim_vars,
       path = "Figures",dpi = 400,width = 11, 
       height = 6)

# Hypotheses Figure ----
### Model 1 -- Lat indirect ----

sem_lat_indirect<-semGraph(Field_sem, H=T,marg_y = 5,
                           node_locs = list("Climate PC1"=c(-0.5,-1),
                                            "Climate PC1 (sq)"=c(0.5,-1)),
                           curve_locs = list(c("Climate PC1","Herbivory",5.7),
                                             c("Climate PC1 (sq)","Herbivory",-5.7)));sem_lat_indirect




# Figure 2: SEMs ----
Field_semgraph<-semGraph(Field_sem, marg_y = 5,
                         node_locs = list("Climate PC1"=c(-0.5,-1),
                                          "Climate PC1 (sq)"=c(0.5,-1)),
                         edge_locs = list(c("Climate PC1","Herbivory",0.65),
                                          c("Climate PC1 (sq)","Herbivory",0.65),
                                          c("Climate PC1 (sq)","Trichomes",0.2),
                                          c("Climate PC1","Glycoalkaloids",0.2),
                                          c("Climate PC1","Trichomes",0.8),
                                          c("Climate PC1 (sq)","Glycoalkaloids",0.8)),
                         curve_locs = list(c("Climate PC1","Herbivory",5.7),
                                      c("Climate PC1 (sq)","Herbivory",-5.7)));Field_semgraph

Garden_semgraph<-semGraph(Garden_sem, marg_y = 5,
                          node_locs = list("Climate PC1"=c(-0.5,-1),
                                           "Climate PC1 (sq)"=c(0.5,-1)),
                          edge_locs = list(c("Climate PC1","Herbivory",0.65),
                                           c("Climate PC1 (sq)","Herbivory",0.65),
                                           c("Climate PC1 (sq)","Trichomes",0.2),
                                           c("Climate PC1","Glycoalkaloids",0.2),
                                           c("Climate PC1","Trichomes",0.8),
                                           c("Climate PC1 (sq)","Glycoalkaloids",0.8)),
                          curve_locs = list(c("Climate PC1","Herbivory",5.7),
                                            c("Climate PC1 (sq)","Herbivory",-5.7)));Garden_semgraph

SEM_fig<-(Field_semgraph | Garden_semgraph)+plot_annotation(tag_levels = "A");SEM_fig

ggsave("SEM_R.jpg",
       device = "jpg",plot = SEM_fig,
       path = "Figures",dpi = 400,width = 10, 
       height = 5,limitsize = F)

# Figures 3&4: Significant trends in the SEM ---- 

## Field figure----


# Field climate versus trichomes
ClimXtrich<-Custom_ggplot(predictor = "Climate_PC1",Trend = T)+
  labs(y=expression("Trichomes (n/cm"^"2"*")"),x="Climate PC1") +
  C_theme();ClimXtrich

ClimXglyc<-Custom_ggplot(predictor = "Climate_PC1",response = "Conc", family = "gaussian",deg=1)+
  labs(x="Climate PC1",y="Glycoalkaloids (mg/mg)") +
  C_theme();ClimXglyc

# Field climate versus Herbivores
ClimsqXherb<-Custom_ggplot(predictor = "Climate_PC1",response = "herb_p", family = beta_family(),deg=2)+
  labs(x="Climate PC1",y="Herbivory (%)") +
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
climXglyc<-Custom_ggplot(loc = "Garden",predictor = "Climate_PC1",response = "Conc", family = gaussian(link = "log"),deg=1,random = "+(1|Pop)")+
  labs(x="Climate PC1",y="Glycoalkaloids (mg/mg)") +
  C_theme();climXglyc

# Garden climate versus trichomes
climsqXtri_gard<-Custom_ggplot(loc = "Garden",predictor = "Climate_PC1",response = "Trichomes", family = gaussian(link = "log"),deg=2,random = "+(1|Pop)")+
  labs(x="Climate PC1",y=expression("Trichomes (n/cm"^"2"*")")) +
  C_theme();climsqXtri_gard

climsqXherb_gard<-Custom_ggplot(loc = "Garden",predictor = "Climate_PC1",response = "herb_p", 
                                family = gaussian(link = "log"),deg=2,random = "+(1|Pop)",
                                Trend = F)+
  labs(x="Climate PC1",y="Herbivory (%)") +
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
Data_prep(loc="Garden|Field") %>% 
  select(Loc,Height,Trichomes_t_sc,SLA_t_sc,Conc_t_sc) %>% 
  mutate(Height=scale(Height)) %>% 
  pivot_longer(.,cols=c(Trichomes_t_sc,SLA_t_sc,Conc_t_sc)) %>% 
  lm(value~Height*name*Loc,.,) %>% 
  emtrends(.,~name*Loc,var="Height",infer=T)


# Figure S1: Correlation of NPP and the climate variables that we used ----
ggplot(Pop_info, aes(x=Climate_PC1, y=NPP)) + 
  geom_point() +
  geom_smooth(method = "glm")+
  labs(x="Climate PC1",y=expression("Ten year NPP"[g]*" (kg*C/m"^"2"*")"))+
  C_theme()
  
  
# Figure S2: Herbivory and phenology through time in the garden----
Pop_level<-Data_prep(loc = "Garden",byDate = T,
                     start_date ="2023-04-15" , end_date = "2023-08-29",
                     group = c('Pop','Date')) %>% 
  mutate(Flowering_p=flowers/count) 

Leaves_time<-ggplot(Pop_level %>% drop_na(Leaves),aes(Date,Leaves,group = Date)) +
  geom_boxplot(outliers = F)+
  geom_point(show.legend=F, position = position_jitter(width=1))+
  labs(y="Leaves")+
  C_theme(14);Leaves_time

Flowers_time<-ggplot(Pop_level %>% drop_na(Flowering_p),
                     aes(Date,Flowering_p,group = Date)) +
  geom_boxplot(outliers = F)+
  geom_point(show.legend=F, position = position_jitter(width=1))+
  labs(y='Proportion flowering')+
  C_theme(14);Flowers_time

Herbivory_time<-ggplot(Pop_level,aes(Date,max_herb,group=Date)) +
  geom_boxplot(outliers = F)+
  geom_point(show.legend=F, position = position_jitter(width=1))+
  labs(y="Herbivory (%)")+
  scale_y_continuous(labels = function(x) paste0(x * 100))+
  C_theme(14);Herbivory_time

appendix<-((Leaves_time | Flowers_time)/(Herbivory_time | plot_spacer()))+plot_annotation(tag_levels = "A");appendix

ggsave("Appendix_S2.jpg",
       device = "jpg",plot = appendix,
       path = "Figures",dpi = 400,width = 12, 
       height = 11,limitsize = F)

# Herbivory in response to plant traits and climate at different times of the year ----
# Herbivory graphing function
Herb_by_time<-function(herb='herb_p_t_sc',family.var='poisson',time="Early",ad_form=""){
  Data<-Data_prep(loc = "Garden",byDate = F,Time.var = time,group = c('Plant_ID','Time'))
  Data1<-Data_prep(loc = "Garden",byDate = T,
                   start_date = "2023-04-15",end_date = "2023-09-15",group = c('Plant_ID')) %>% 
    mutate(Time="All",
           Loc='Garden')
  
  Data<- rbind(Data,Data1)
  
  model<-glmmTMB(as.formula(paste0(herb, '~ Climate_PC1_sc + Climate_PC1_sc_sq + (Trichomes_t_sc + Conc_t_sc + SLA_t_sc)',ad_form, " + (1|Pop)")),
                 family=family.var,data=Data)
  list(Data,model)
}

# Models of herbivory using the 75th quantile, mean, and max herbivory.
all_herb_mod_quant<-Herb_by_time(herb='quant_herb_0.75',family=beta_family(),time='Early|Mid|Late',ad_form='*Time')
AIC(all_herb_mod_quant[[2]])
emtrends(all_herb_mod_quant[[2]],~Time,var="SLA_t_sc",infer=T)
emtrends(all_herb_mod_quant[[2]],~Time,var="Conc_t_sc",infer=T)
emtrends(all_herb_mod_quant[[2]],~Time,var="Trichomes_t_sc",infer=T)
emtrends(all_herb_mod_quant[[2]],~Time,var="Climate_PC1_sc",infer=T)
emtrends(all_herb_mod_quant[[2]],~Time,var="Climate_PC1_sc_sq",infer=T)

all_herb_mod_mean<-Herb_by_time(herb='herb_p',family=beta_family(),time='Early|Mid|Late',ad_form='*Time')
AIC(all_herb_mod_mean[[2]])
emtrends(all_herb_mod_mean[[2]],~Time,var="SLA_t_sc",infer=T)
emtrends(all_herb_mod_mean[[2]],~Time,var="Conc_t_sc",infer=T)
emtrends(all_herb_mod_mean[[2]],~Time,var="Trichomes_t_sc",infer=T)
emtrends(all_herb_mod_mean[[2]],~Time,var="Climate_PC1_sc",infer=T)
emtrends(all_herb_mod_mean[[2]],~Time,var="Climate_PC1_sc_sq",infer=T)

all_herb_mod_max<-Herb_by_time(herb='max_herb',family=beta_family(),time='Early|Mid|Late',ad_form='*Time')
AIC(all_herb_mod_max[[2]])
emtrends(all_herb_mod_max[[2]],~Time,var="SLA_t_sc",infer=T)
emtrends(all_herb_mod_max[[2]],~Time,var="Conc_t_sc",infer=T)
emtrends(all_herb_mod_max[[2]],~Time,var="Trichomes_t_sc",infer=T)

# Appendix: AICs at different herbivory observations -----
# Herbviory data from early in the year
early_sem<-SEM_results(Loc="Garden",model = c("lm1", "lm2", "lm3", "lm4"),
                       main_ran = "(1|Pop)",
                       lat_traits = "Climate_PC1_sc + Climate_PC1_sc_sq", 
                       lat_main = "Climate_PC1_sc + Climate_PC1_sc_sq",
                       corError = list(), byDate = F,
                       Time='Early',group = "Plant_ID",ICC=F)
AIC(early_sem,aicc=T)
summary(early_sem)

# Herbviory data from the middle of the year
mid_sem<-SEM_results(Loc="Garden",model = c("lm1", "lm2", "lm3", "lm4"),
                     main_ran = "(1|Pop)",
                     lat_traits = "Climate_PC1_sc + Climate_PC1_sc_sq", 
                     lat_main = "Climate_PC1_sc + Climate_PC1_sc_sq",
                     corError = list(), byDate = F,
                     Time='Mid',group = "Plant_ID",ICC=F)

AIC(mid_sem,aicc = T)
summary(mid_sem)

# Herbviory data from the end of the year
end_sem<-SEM_results(Loc="Garden",model = c("lm1", "lm2", "lm3", "lm4"),
                     main_ran = "(1|Pop)",
                     lat_traits = "Climate_PC1_sc + Climate_PC1_sc_sq", 
                     lat_main = "Climate_PC1_sc + Climate_PC1_sc_sq",
                     corError = list(), byDate = F,
                     Time='Late',group = "Plant_ID",ICC=F)

AIC(end_sem,aicc = T)
summary(end_sem)

# Herbivory from all time points of the experiment
all_sem<-SEM_results(Loc="Garden",model = c("lm1", "lm2", "lm3", "lm4"),
                     main_ran = "(1|Pop)",
                     lat_traits = "Climate_PC1_sc + Climate_PC1_sc_sq", 
                     lat_main = "Climate_PC1_sc + Climate_PC1_sc_sq",
                     corError = list(), byDate = T,
                     Time='Early',group = "Plant_ID",ICC=F,
                    start_date = "2023-05-15",end_date = "2023-10-15")
  


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


trait_x_herb_ave<-trait_x_herb_plot("herb_p","Average herbivory")+
  scale_y_continuous(labels = function(x) paste0(x * 100))

trait_x_herb_plot("max_herb","Max herbivory")

trait_x_herb_plot("quant_herb_0.75","Herbivory (75th quantile)")

ggsave("Appendix_S3.jpg",
       device = "jpg",plot = trait_x_herb_ave,
       path = "Figures",dpi = 400,width = 12, 
       height = 11,limitsize = F)

