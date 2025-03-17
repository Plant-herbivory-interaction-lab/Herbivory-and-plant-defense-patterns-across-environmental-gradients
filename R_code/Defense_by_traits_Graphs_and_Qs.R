## Code by: Jacob Herschberger
## Date: January 2025
## Email: j.herschberger@ufl.edu
## Project: Culprits of plant defense variation across a latitudinal gradient.

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
library(performance)
library(lme4)
library(GGally)

# Database connection ----
con <-dbConnect(SQLite(), 'Data/Data_all_combined.db')

# Import and prep data ----
source("R_code/Prep_data_for_all_analysis.R")
source("R_code/Psem_graphing.R")

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

Combined_data1<-rbind(Field_2022,Field_2023,Garden)%>% 
  filter(Treatment=="Cont"|Treatment=="us"|is.na(Treatment)==T) %>% filter(SLA>3.9) %>% 
  left_join(PCs %>% 
              select(Latitude,Pop,Clim_ave_PC1,Soil_PC2,Clim_ave_PC2)) %>% 
  drop_na(Latitude)%>% 
  mutate(
    Conc=log(Conc)+4,
    #Spines=log(Spines+1),
    herb_p_t=logit(herb_p),
    Clim_ave_PC1=-Clim_ave_PC1,
    Soil_PC2=-Soil_PC2,
    Clim_ave_PC1.1=Clim_ave_PC1,
    Clim_ave_PC1=poly(Clim_ave_PC1,2)[,1]*100,
    Clim_PC1_sq=poly(Clim_ave_PC1,2)[,2]*100,
    Plant=c(1:length(Conc))
  ) %>% 
  mutate_at(c('Trichomes','Conc','SLA'),~as.numeric(scale(.x,center=F)))

Combined_data_long<-Combined_data1 %>%
  left_join(Pop_info %>% select(Pop,Latitude))%>% 
  pivot_longer(cols = c(Trichomes,SLA,Conc)) %>% 
  mutate(name=if_else(
    name=="Conc","Glycoalkaloids (mg/g)",name))

# Question 1: Is herbivore pressure predicted by productivity associated climate variables (i.e. temperature and precipitation, etc)? ----
MD.2<-glmmTMB(herb_p~(Clim_ave_PC1+Clim_PC1_sq)*Loc,data=Combined_data1,family = beta_family())
coef.MOD.2<-as.data.frame(summary(MD.2)$coefficients$cond) %>% 
  rownames_to_column(.,var="term") %>% 
  filter(term!="(Intercept)") %>% 
  mutate(Predictor=case_when(grepl("Soil",term)~"Soil",
                             grepl("Clim_ave_PC1",term)~"Climate",
                             grepl("sq",term)~"Climate (Sq)",
                             .default = "NA"),
         Contrast=if_else(grepl("Garden",term),"Garden Effect","None")) %>% 
  rename(c("Standard Error" = "Std. Error", 
           "Z value" = "z value",
           "P value" = "Pr(>|z|)")) %>% 
  select(Contrast,Predictor,Estimate,"Standard Error","Z value","P value") %>% 
  arrange(.,Contrast,Predictor) %>% 
  mutate(Contrast=if_else(!duplicated(Contrast),Contrast,NA))

emtrends(MD.2,pairwise~Loc,var =  "Clim_ave_PC1",infer=T,type="response")
emtrends(MD.2,pairwise~Loc,var =  "Clim_PC1_sq",infer=T,type="response")
emtrends(MD.2,pairwise~Loc,var =  "Soil_PC2",infer=T,type="response")

source("R_code/Table.R")
Table(data1 = coef.MOD.2,col = "P value")
  
Anova(MD.2)
plot(simulateResiduals(MD.2))

clim_herb_fig<-ggplot(Combined_data1,aes(y=herb_p,x=Clim_ave_PC1.1,
                              colour = Loc,fill = Loc))+
  geom_point()+
  geom_smooth(method = 'glm',formula = y ~ poly(x,2),
              method.args=list(family=beta_family()))+
  theme_bw(base_size = 20)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = c(0.82,0.82))+
  scale_color_manual(name="Location",values = c("grey","darkred"))+
  scale_fill_manual(name="Location",values = c("grey","darkred"))+
  labs(y="Herbivory",x="Climate productivity");clim_herb_fig

Soil_herb_fig<-ggplot(Combined_data1,aes(y=herb_p,x=Soil_PC2,
                              colour = Loc,fill = Loc))+
  geom_point()+
  geom_smooth(method = 'glm',formula = y ~ poly(x,2),
              method.args=list(family=beta_family()))+
  theme_bw(base_size = 20)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = c(0.82,0.82))+
  scale_color_manual(name="Location",values = c("grey","darkred"))+
  scale_fill_manual(name="Location",values = c("grey","darkred"))+
  labs(y="Herbivory",x="Soil productivity");Soil_herb_fig

herb_presh<-(clim_herb_fig| Soil_herb_fig)+plot_annotation(tag_levels = "A");herb_presh

ggsave("herb_presh.pdf",
       device = "pdf",plot = herb_presh,
       path = "Figures",dpi = 400,width = 12, 
       height = 5,limitsize = F)

# Question 2: Do defense traits correlate with herbivory across a latitudinal gradient? ----
MD.1<-glmmTMB(value~name*herb_p*Loc+(1|Pop),data=Combined_data_long,family = gaussian())
coef.MD.1<-as.data.frame(summary(MD.1)$coefficients$cond)
Anova(MD.1)
plot(simulateResiduals(MD.1))

emtrends(MD.1,pairwise~name|Loc,var =  "herb_p",infer=T)

traits_herb<-ggplot(Combined_data_long,aes(x=herb_p,y=value,
                              colour = Loc,fill=Loc))+
  geom_point()+
  geom_smooth(method = 'glm',formula=y~logit(x))+
  facet_wrap(~name,scales = "free_y")+
  theme_bw(base_size = 20)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = c(0.92,0.66))+
  scale_color_manual(name="Location",values = c("grey","darkred"))+
  scale_fill_manual(name="Location",values = c("grey","darkred"))+
  labs(x="Herbivory",y="Trait value (SD)");traits_herb

ggsave("traits_herb.pdf",
       device = "pdf",plot = traits_herb,
       path = "Figures",dpi = 400,width = 10, 
       height = 7,limitsize = F)

# Question 3: Do soil and climate variables predict plant defense traits across a latitudinal gradient? ----
MD.3<-glmmTMB(value~name*(Clim_ave_PC1+Clim_PC1_sq)*Loc,data=Combined_data_long,family = gaussian())
coef.MD.3<-as.data.frame(summary(MD.3)$coefficients$cond)
Anova(MD.3)
plot(simulateResiduals(MD.3))

emtrends(MD.3,pairwise~name|Loc,var =  "Clim_ave_PC1",infer=T)
emtrends(MD.3,pairwise~name|Loc,var =  "Clim_PC1_sq",infer=T)

traits_clim<-ggplot(Combined_data_long,aes(x=Clim_ave_PC1,y=value,
                                           colour = Loc,fill=Loc))+
  geom_point()+
  geom_smooth(method = 'glm',formula = y~poly(x,2))+
  facet_wrap(~name,scales = "free_y")+
  theme_bw(base_size = 20)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = c(0.9,0.7))+
  scale_color_manual(name="Location",values = c("grey","darkred"))+
  scale_fill_manual(name="Location",values = c("grey","darkred"))+
  labs(x="Climate productivity",y="Trait value (SD)");traits_clim

ggsave("traits_clim.pdf",
       device = "pdf",plot = traits_clim,
       path = "Figures",dpi = 400,width = 10, 
       height = 7,limitsize = F)

MD.4<-glmmTMB(value~name*Soil_PC2*Loc,data=Combined_data_long,family = gaussian())
coef.MD.4<-as.data.frame(summary(MD.4)$coefficients$cond)
Anova(MD.4)
plot(simulateResiduals(MD.4))

emtrends(MD.4,pairwise~name|Loc,var =  "Soil_PC2",infer=T)

traits_soil<-ggplot(Combined_data_long,aes(x=Soil_PC2,y=value,
                                           colour = Loc,fill=Loc))+
  geom_point()+
  geom_smooth(method = 'glm',formula = y~poly(x,2))+
  facet_wrap(~name,scales = "free_y")+
  theme_bw(base_size = 20)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = c(0.9,0.7))+
  scale_color_manual(name="Location",values = c("grey","darkred"))+
  scale_fill_manual(name="Location",values = c("grey","darkred"))+
  labs(x="Soil productivity",y="Trait value (SD)");traits_soil

ggsave("traits_soil.pdf",
       device = "pdf",plot = traits_soil,
       path = "Figures",dpi = 400,width = 10, 
       height = 7,limitsize = F)
# SEM; test all hypothesis together----
# Individual data points
psem_data<-Combined_data1 %>% 
  select(!c(Treatment,Spines)) %>% 
  drop_na() %>% 
  dplyr::filter(Loc=="Field") 


#ggpairs(psem_data %>% select(herb_p_t,Clim_ave_PC1,Clim_PC1_sq,Trichomes:Conc))
# Removed soil PC because it was highly correlated with climate squared
lm1<-lmer(herb_p_t~(Clim_ave_PC1+Clim_PC1_sq+Trichomes+Conc+SLA)+(1|Pop),psem_data)
lm2<-lmer(Conc~(Clim_ave_PC1+Clim_PC1_sq)+(1|Pop),psem_data)
lm3<-lmer(SLA~(Clim_ave_PC1+Clim_PC1_sq)+(1|Pop),psem_data)
lm4<-lmer(Trichomes~(Clim_ave_PC1+Clim_PC1_sq)+(1|Pop),psem_data)

fit<-psem(
  lm1,
  lm2,
  lm3,
  lm4,
  
  SLA %~~%Trichomes,
  SLA %~~% Conc,
  
  data = psem_data
)
semGraph("Figures/Field_individual_SEM")

psem_data<-Combined_data1 %>% 
  select(!c(Treatment,Spines)) %>% 
  drop_na() %>% 
  dplyr::filter(Loc=="Garden") 


#ggpairs(psem_data %>% select(herb_p_t,Clim_ave_PC1,Clim_PC1_sq,Trichomes:Conc))

lm1<-lmer(herb_p_t~(Clim_ave_PC1+Clim_PC1_sq+Trichomes+Conc+SLA)+(1|Pop),psem_data)
lm2<-lmer(Conc~(Clim_ave_PC1+Clim_PC1_sq)+(1|Pop),psem_data)
lm3<-lmer(SLA~(Clim_ave_PC1+Clim_PC1_sq)+(1|Pop),psem_data)
lm4<-lmer(Trichomes~(Clim_ave_PC1+Clim_PC1_sq)+(1|Pop),psem_data)

fit<-psem(
  lm1,
  lm2,
  lm3,
  lm4,
  
  SLA %~~%Trichomes,
  SLA %~~% Conc,
  
  data = psem_data
)

semGraph("Figures/Garden_individual_SEM")

# across populations
Pop_level<-Combined_data1 %>% 
group_by(Pop,Loc,Treatment) %>% 
  summarise(Conc=mean(Conc,na.rm=T),
    Trichomes=mean(Trichomes,na.rm=T),
    SLA=mean(SLA,na.rm=T),
    herb_p_t=mean(herb_p_t,na.rm=T),
    Spines=mean(Spines,na.rm=T)) %>% 
ungroup()%>% 
  left_join(PCs %>% 
              select(Latitude,Pop,Clim_ave_PC1,Soil_PC2,Clim_ave_PC2)) %>% 
  mutate(Clim_ave_PC1=-Clim_ave_PC1,
         Soil_PC2=-Soil_PC2,
         Clim_ave_PC1.1=Clim_ave_PC1,
         Clim_ave_PC1=poly(Clim_ave_PC1,2)[,1]*100,
         Clim_PC1_sq=poly(Clim_ave_PC1,2)[,2]*100,)

#ggpairs(psem_data %>% select(herb_p_t,Clim_ave_PC1,Clim_PC1_sq,Trichomes:Conc))

psem_data<-Pop_level %>% 
  select(!c(Treatment,Spines)) %>% 
  drop_na() %>% 
  dplyr::filter(Loc=="Field") 

#ggpairs(psem_data %>% select(herb_p_t,Clim_ave_PC1,Clim_PC1_sq,Trichomes:Conc))
# Removed soil PC because it was highly correlated with climate squared
lm1<-lm(herb_p_t~(Clim_ave_PC1+Clim_PC1_sq+Trichomes+Conc+SLA),psem_data)
lm2<-lm(Conc~(Clim_ave_PC1+Clim_PC1_sq),psem_data)
lm3<-lm(SLA~(Clim_ave_PC1+Clim_PC1_sq),psem_data)
lm4<-lm(Trichomes~(Clim_ave_PC1+Clim_PC1_sq),psem_data)

fit<-psem(
  lm1,
  lm2,
  lm3,
  lm4,
  
  SLA %~~%Trichomes,
  SLA %~~% Conc,
  
  data = psem_data
)
semGraph("Figures/Field_Pop_SEM")

psem_data<-Combined_data1 %>% 
  select(!c(Treatment,Spines)) %>% 
  drop_na() %>% 
  dplyr::filter(Loc=="Garden")

ggpairs(psem_data %>% select(herb_p_t,Clim_ave_PC1,Clim_PC1_sq,Trichomes:Conc))

lm1<-lm(herb_p_t~(Clim_ave_PC1+Clim_PC1_sq+Trichomes+Conc+SLA),psem_data)
lm2<-lm(Conc~(Clim_ave_PC1+Clim_PC1_sq),psem_data)
lm3<-lm(SLA~(Clim_ave_PC1+Clim_PC1_sq),psem_data)
lm4<-lm(Trichomes~(Clim_ave_PC1+Clim_PC1_sq),psem_data)

fit<-psem(
  lm1,
  lm2,
  lm3,
  lm4,
  
  SLA %~~%Trichomes,
  SLA %~~% Conc,
  
  data = psem_data
)

semGraph("Figures/Garden_Pop_SEM")

# population level trend of herbivory versus plant defense traits ----

ggplot(Combined_data1,aes(SLA,herb_p_t,col=Loc))+
  geom_point()+
  geom_smooth(method = "glm")+
  facet_wrap(~Pop)

ggplot(Combined_data1,aes(Trichomes,herb_p_t,col=Loc))+
  geom_point()+
  geom_smooth(method = "glm")+
  facet_wrap(~Pop,scales="free")

ggplot(Combined_data1,aes(Conc,herb_p_t,col=Loc))+
  geom_point()+
  geom_smooth(method = "glm")+
  facet_wrap(~Pop)+
  facet_wrap(~Pop,scales="free")

