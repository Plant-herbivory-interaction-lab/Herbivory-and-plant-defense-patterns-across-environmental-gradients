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

# Database connection ----
con <-dbConnect(SQLite(), 'Data/Data_all_combined.db')

# Import and prep data ----
source("R_code/Prep_data_for_all_analysis.R")

Field_2022<-Field_2022 %>% 
  select(Trichomes:herb_p) %>% 
  mutate(Expr="Field",
         Treatment=NA)

Field_2023<-Field_2023 %>% 
  select(Pop,Trichomes:SLA,Conc) %>% 
  mutate(Expr="Field",
         Treatment=NA)

Garden<-Garden %>% 
  #filter(Treatment=="Cont"|Treatment=="us") %>% 
  select(Pop:Conc,herb_p,Treatment) %>% 
  mutate(Expr="Garden")

Combined_data<-rbind(Field_2022,Field_2023,Garden)

Combined_data_long<-Combined_data  %>% 
  mutate(
         Conc=scale(log(Conc)),
         Spines=log(Spines+1),
         herb_p_t=logit(herb_p)
         )  %>% 
  filter(Treatment=="Cont"|Treatment=="us"|is.na(Treatment)==T) %>% 
  mutate_at(c('Trichomes','Conc','SLA','Spines'),scale) %>%
  left_join(Pop_info %>% select(Pop,Latitude))%>% 
  pivot_longer(cols = c(Trichomes:Conc)) %>% 
  drop_na(Latitude)

# Question 1: Do defense traits correlate with herbivory across a latitudinal gradient? ----
MD.1<-glmmTMB(value~name*herb_p_t*Expr,data=Combined_data_long,family = gaussian())
test<-as.data.frame(summary(MD.1)$coefficients$cond)
Anova(MD.1)
plot(simulateResiduals(MD.1))

ggplot(Combined_data_long,aes(x=herb_p,y=value,colour = Expr))+
  geom_point()+
  geom_smooth(method = 'glm')+
  facet_wrap(~name,scales = "free_y")

MD.2<-glmmTMB(herb_p~Latitude*Expr,data=Combined_data_long,family = beta_family())
test<-as.data.frame(summary(MD.2)$coefficients$cond)
Anova(MD.2)
plot(simulateResiduals(MD.2))

ggplot(Combined_data_long,aes(y=herb_p,x=Latitude,colour = Expr))+
  geom_point()+
  geom_smooth(method = 'glm')+
  facet_wrap(~name,scales = "free_y")

