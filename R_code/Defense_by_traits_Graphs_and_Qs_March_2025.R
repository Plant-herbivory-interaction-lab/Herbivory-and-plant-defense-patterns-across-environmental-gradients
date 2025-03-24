## Code by: Jacob Herschberger
## Date: March 2025
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
library(brms)

# Database connection ----
con <-dbConnect(SQLite(), 'Data/Data.db')

# Import and prep data ----
source("R_code/Prep_data_for_all_analysis.R")
source("R_code/Psem_graphing.R")


Data_prep<-function(loc="Field",PopLevel=F,long=F,ClimateLong=F,Treatment=F){
Field_2022<-Field_2022 %>% 
  select(Trichomes:herb_p) %>% 
  mutate(Loc="Field",
         Loc1="Field 2022",
         Year="2022")

Field_2023<-Field_2023 %>% 
  select(Pop,Trichomes:SLA,Conc,herb_p) %>% 
  mutate(Loc="Field",
         Loc1="Field 2023",
         Year="2023",
         Conc=Conc*2.5) # This is do to use using different amounts of leaf material.

Garden<-Garden %>% 
  select(Pop:Conc,herb_p) %>% 
  mutate(Loc="Garden",Loc1="Garden",Year="2023")

Combined_data1<-rbind(Field_2022,Field_2023,Garden)%>% filter(SLA>3.9) %>% 
  left_join(PCs %>% 
              select(Latitude,Pop,Clim_ave_PC1,Soil_PC2,Clim_ave_PC2)) %>% 
  drop_na(Latitude)%>% 
  filter(SLA>40) %>% 
  mutate(
    Conc_t=log(Conc),
    SLA_t=log(SLA),
    Trichomes_t=log(Trichomes),
    herb_p_t=logit(herb_p),
    Clim_ave_PC1=-Clim_ave_PC1,
    #Soil_PC2=-Soil_PC2,
    Clim_ave_PC1_sq=Clim_ave_PC1^2,
    Clim_ave_PC1_sc=scale(Clim_ave_PC1),
    Clim_PC1_sq_sc=scale(Clim_ave_PC1_sq),
    Conc_t_sc=scale(Conc_t),
    SLA_t_sc=scale(SLA_t),
    Trichomes_t_sc=scale(Trichomes_t),
    herb_p_t_sc=scale(herb_p_t),
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
  pivot_longer(cols = c(Trichomes_t_sc,SLA_t_sc,Conc_t_sc,Clim_PC1_sq_sc,Clim_ave_PC1_sc)) %>% 
  mutate(name=case_when(
    name=="Conc_t_sc"~"Glycoalkaloids (mg/g)",
    name=='Clim_ave_PC1_sc'~'Climate',
    name=='Clim__PC1_sq_sc'~'Climate (sq)',
    name=="Trichomes_t_sc"~"Trichomes",
    name=='SLA_t_sc'~'SLA',
    .default = name))}

if(long==T&ClimateLong==F){Combined_data1<-Combined_data1 %>%
  left_join(Pop_info %>% select(Pop,Latitude))%>% 
  pivot_longer(cols = c(Trichomes_t_sc,SLA_t_sc,Conc_t_sc)) %>% 
  mutate(name=case_when(
    name=="Conc_t_sc"~"Glycoalkaloids (mg/g)",
    name=='Clim_ave_PC1_sc'~'Climate',
    name=='Clim__PC1_sq_sc'~'Climate (sq)',
    name=="Trichomes_t_sc"~"Trichomes",
    name=='SLA_t_sc'~'SLA',
    .default = name))}


Combined_data1
}

# Question 1: Is herbivore pressure predicted by productivity associated climate variables (i.e. temperature and precipitation, etc) and plant defense traits? ----
Data=Data_prep(loc = "Field")
MD.1<-glmmTMB(herb_p_t~Clim_ave_PC1_sc+Clim_PC1_sq_sc+SLA+Conc+Trichomes+(1|Pop/Year),data=Data)
summary(MD.1)
Anova(MD.1)
plot(simulateResiduals(MD.1))
check_collinearity(MD.1)

Data=Data_prep(loc = "Garden")
MD.1.1<-glmmTMB(herb_p_t~((Clim_ave_PC1+Clim_PC1_sq_sc)+SLA+Conc+Trichomes)+(1|Pop),data=Data)
summary(MD.1.1)

Anova(MD.1.1)
plot(simulateResiduals(MD.1.1))


Herb_response_individual<-ggplot(Data_prep(loc="NA",long=T,ClimateLong = T),aes(y=herb_p,x=value,
                              colour = Loc1,fill = Loc1))+
  geom_point()+
  geom_smooth(method = 'glm',
              method.args=list(family=beta_family()))+
  theme_bw(base_size = 20)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = c(0.85,0.25))+
  scale_color_manual(name="Location",values = c("black","grey","darkred"))+
  scale_fill_manual(name="Location",values = c("black","grey","darkred"))+
  labs(y="Herbivory",x="Productivity or trait value")+
  facet_wrap(~name,scales="free");Herb_response_individual


Data=Data_prep(loc = "Field",PopLevel = T)
MD.2<-glmmTMB(herb_p_t~Clim_ave_PC1+Clim_ave_PC1_sq+SLA+Conc+Trichomes+(1|Year),data=Data)
summary(MD.2)
Anova(MD.2)
plot(simulateResiduals(MD.2))

Data=Data_prep(loc = "Garden",PopLevel = T)
MD.2.1<-glmmTMB(herb_p_t~Clim_ave_PC1+Clim_PC1_sq_sc+SLA+Conc+Trichomes,data=Data)
summary(MD.2.1)
Anova(MD.2.1)
plot(simulateResiduals(MD.2.1))

Herb_response_pop<-ggplot(Data_prep(loc="NA",long=T,PopLevel = T,ClimateLong = T),aes(y=herb_p,x=value,
                                                                colour = Loc1,fill = Loc1))+
  geom_point()+
  geom_smooth(method = 'glm',
              method.args=list(family=beta_family()))+
  theme_bw(base_size = 20)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = c(0.85,0.25))+
  scale_color_manual(name="Location",values = c("black","grey","darkred"))+
  scale_fill_manual(name="Location",values = c("black","grey","darkred"))+
  labs(y="Herbivory",x="Productivity or trait value")+
  facet_wrap(~name,scales="free");Herb_response_pop

# Question 2: Does climate productivity predict plant traits? ----
Data=Data_prep(loc = "Field",long = T)

MD.3<-glmmTMB(value~name*(Clim_ave_PC1_sc+Clim_PC1_sq_sc)+(1|Pop/Year),data=Data)
summary(MD.3)
plot(simulateResiduals(MD.3))

emtrends(MD.3,pairwise~name,var = "Clim_ave_PC1_sc",infer=T,type="response")
emtrends(MD.3,pairwise~name,var =  "Clim_PC1_sq_sc",infer=T,type="response")

Data=Data_prep(loc = "Garden",long = T)
MD.4<-glmmTMB(value~name*(Clim_ave_PC1+Clim_PC1_sq_sc),data=Data)
summary(MD.4)
plot(simulateResiduals(MD.4))

emtrends(MD.4,pairwise~name,var = "Clim_ave_PC1",infer=T,type="response")
emtrends(MD.4,pairwise~name,var =  "Clim_PC1_sq_sc",infer=T,type="response")

Trait_response_individual_clim<-ggplot(Data_prep(loc="NA",long=T),aes(y=value,x=Clim_ave_PC1,
                                                                                colour = Loc1,fill = Loc1))+
  geom_point()+
  geom_smooth(method = 'glm')+
  theme_bw(base_size = 20)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = c(0.85,0.25))+
  scale_color_manual(name="Location",values = c('black',"grey","darkred"))+
  scale_fill_manual(name="Location",values = c('black',"grey","darkred"))+
  labs(y="Trait (SD)",x="Climate")+
  facet_wrap(~name,scales="free");Trait_response_individual_clim

Trait_response_individual_clim_sq<-ggplot(Data_prep(loc="NA",long=T),aes(y=value,x=Clim_ave_PC1,
                                                                      colour = Loc1,fill = Loc1))+
  geom_point()+
  geom_smooth(method = 'glm',formula = y ~ poly(x,2))+
  theme_bw(base_size = 20)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = c(0.85,0.25))+
  scale_color_manual(name="Location",values = c('black',"grey","darkred"))+
  scale_fill_manual(name="Location",values = c('black',"grey","darkred"))+
  labs(y="Trait (SD)",x="Climate (sq)")+
  facet_wrap(~name,scales="free");Trait_response_individual_clim_sq

Data=Data_prep(loc = "Field",long = T,PopLevel = T)

MD.3<-glmmTMB(value~name*(Clim_ave_PC1+Clim_ave_PC1_sq),data=Data)
summary(MD.3)
plot(simulateResiduals(MD.3))

emtrends(MD.3,pairwise~name,var = "Clim_ave_PC1",infer=T,type="response")
emtrends(MD.3,pairwise~name,var =  "Clim_ave_PC1_sq",infer=T,type="response")

Data=Data_prep(loc = "Garden",long = T,PopLevel = T)
MD.4<-glmmTMB(value~name*(Clim_ave_PC1+Clim_ave_PC1_sq),data=Data)
summary(MD.4)
plot(simulateResiduals(MD.4))

emtrends(MD.4,pairwise~name,var = "Clim_ave_PC1",infer=T,type="response")
emtrends(MD.4,pairwise~name,var =  "Clim_ave_PC1_sq",infer=T,type="response")

Trait_response_pop_clim<-ggplot(Data_prep(loc="NA",long=T,PopLevel = T),aes(y=value,x=Clim_ave_PC1,
                                                                      colour = Loc1,fill = Loc1))+
  geom_point()+
  geom_smooth(method = 'glm')+
  theme_bw(base_size = 20)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = c(0.85,0.25))+
  scale_color_manual(name="Location",values = c("black","grey","darkred"))+
  scale_fill_manual(name="Location",values = c('black',"grey","darkred"))+
  labs(y="Trait (SD)",x="Climate")+
  facet_wrap(~name,scales="free");Trait_response_pop_clim

Trait_response_pop_clim_sq<-ggplot(Data_prep(loc="NA",long=T,PopLevel = T),aes(y=value,x=Clim_ave_PC1,
                                                                         colour = Loc1,fill = Loc1))+
  geom_point()+
  geom_smooth(method = 'glm',formula = y ~ poly(x,2))+
  theme_bw(base_size = 20)+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = c(0.85,0.25))+
  scale_color_manual(name="Location",values = c("black","grey","darkred"))+
  scale_fill_manual(name="Location",values = c("black","grey","darkred"))+
  labs(y="Trait (SD)",x="Climate (sq)")+
  facet_wrap(~name,scales="free");Trait_response_pop_clim_sq


# SEM; test all hypothesis together----
# Individual data points
psem_data<-Data_prep(loc="Field") %>% 
  drop_na()

lm1<-lmer(herb_p_t~((Clim_ave_PC1_sc+Clim_PC1_sq_sc)+Trichomes_t+Conc_t+SLA_t)+(1|Pop/Year),psem_data)
lm2<-lmer(Conc_t~(Clim_ave_PC1_sc+Clim_PC1_sq_sc)+(1|Pop),psem_data)
lm3<-lmer(SLA_t~(Clim_ave_PC1_sc+Clim_PC1_sq_sc)+(1|Pop/Year),psem_data)
lm4<-lmer(Trichomes_t~(Clim_ave_PC1_sc+Clim_PC1_sq_sc)+(1|Pop/Year),psem_data)

fit<-psem(
  lm1,
  lm2,
  lm3,
  lm4,
  
  SLA_t %~~%Trichomes_t,
  SLA_t %~~% Conc_t,
  
  data = psem_data
)

semGraph("Figures/Field_individual_SEM")

#ggpairs(psem_data %>% select(herb_p_t,Clim_ave_PC1,Clim_PC1_sq,Trichomes:Conc))
# Removed soil PC because it was highly correlated with climate squared

brm_fit <- brm(
  bf(herb_p ~ (Clim_ave_PC1 + Clim_PC1_sq) + Trichomes + Conc + SLA + (1|Year/Pop)) +
    bf(Conc ~ (Clim_ave_PC1 + Clim_PC1_sq) + (1|Year/Pop)) +
    bf(SLA ~ (Clim_ave_PC1 + Clim_PC1_sq) + (1|Year/Pop)) +
    bf(Trichomes ~ (Clim_ave_PC1 + Clim_PC1_sq) + (1|Year/Pop)), #+
    #set_rescor(TRUE),  # Specify residual correlation
  data = psem_data,
  family = list('beta','lognormal','lognormal','lognormal'),  # Adjust if necessary (e.g., binomial, etc.)
  chains = 4, cores = 6, 
  iter = 4000, warmup = 1000,
  control = list(adapt_delta = 0.95),
  init_r = 0.1
)


summary(brm_fit)  # Model coefficients
pp_check(brm_fit)  # Posterior predictive check
bayes_R2(brm_fit)  # Bayesian R-squared for model fit
plot(brm_fit)
fixef(brm_fit)

psem_data<-Data_prep(loc = "Garden")


#ggpairs(psem_data %>% select(herb_p_t,Clim_ave_PC1,Clim_PC1_sq,Trichomes:Conc))

lm1<-lmer(herb_p_t~((Clim_ave_PC1_sc+Clim_PC1_sq_sc)+Trichomes_t+Conc_t+SLA_t)+(1|Pop),psem_data)
lm2<-lmer(Conc_t~(Clim_ave_PC1_sc+Clim_PC1_sq_sc)+(1|Pop),psem_data)
lm3<-lmer(SLA_t~(Clim_ave_PC1_sc+Clim_PC1_sq_sc)+(1|Pop),psem_data)
lm4<-lmer(Trichomes_t~(Clim_ave_PC1_sc+Clim_PC1_sq_sc)+(1|Pop),psem_data)

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
psem_data<-Data_prep(loc="Field",PopLevel = T) %>% 
  mutate(Year=ifelse(Year=="2022",0,1)) %>% 
  drop_na()

#ggpairs(psem_data %>% select(herb_p_t,Clim_ave_PC1,Clim_PC1_sq,Trichomes:Conc))

# Removed soil PC because it was highly correlated with climate squared
lm1<-lmer(herb_p_t~((Clim_ave_PC1_sc+Clim_PC1_sq_sc)+Trichomes_t+Conc_t+SLA_t)+(1|Pop),psem_data)
lm2<-lm(Conc_t~(Clim_ave_PC1_sc+Clim_PC1_sq_sc),psem_data)
lm3<-lm(SLA_t~(Clim_ave_PC1_sc+Clim_PC1_sq_sc),psem_data)
lm4<-lm(Trichomes_t~(Clim_ave_PC1_sc+Clim_PC1_sq_sc),psem_data)

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

psem_data<-Data_prep(loc = "Garden",PopLevel = T)

#ggpairs(psem_data %>% select(herb_p_t,Clim_ave_PC1,Clim_PC1_sq,Trichomes:Conc))

lm1<-lm(herb_p_t~((Clim_ave_PC1+Clim_PC1_sq)+Trichomes+Conc+SLA),psem_data)
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

