test_data<-Data_prep(loc = "Garden",byDate = T,start_date = "01-05-2023") %>% 
  select(Pop,Year,Plant_ID,Date,NPP_g_10y,NPP_g,grep("sc",colnames(.),value=T),herb_p)

m1<-glmmTMB(herb_p_t_sc ~ NPP_g_10y + Trichomes_t_sc + Conc_t_sc + SLA_t_sc + (1|Pop),data = test_data)


# Test model without the dummy random effect ----
test_mod<-psem(
  glmmTMB(herb_p_t_sc ~ NPP_g_10y + Trichomes_t_sc + Conc_t_sc + SLA_t_sc + (1|Pop),data = test_data),
  glmmTMB(Conc_t_sc ~ NPP_g_10y + (1|Pop),data = test_data),
  glmmTMB(SLA_t_sc ~ NPP_g_10y + (1|Pop),data = test_data),
  glmmTMB(Trichomes_t_sc ~ NPP_g_10y + (1|Pop), data = test_data),
  glmmTMB(NPP_g_10y ~ Latitude_sc, data = test_data),
  SLA_t_sc %~~% Trichomes_t_sc,
  SLA_t_sc %~~% Conc_t_sc,
  data = test_data
)


summary(test_mod)

test_data$dummy<-1

plot(simulateResiduals(glmmTMB(NPP_g_10y ~ Latitude_sc, data = test_data)))
plot(simulateResiduals(glmmTMB(NPP_g ~ Latitude_sc, data = test_data)))
plot(simulateResiduals(glmmTMB(NPP_g ~ Latitude_sc + (1|dummy), data = test_data)))


# Model with the dummy random effects but no latitude except in the NPP_y model ----
test_mod<-psem(
  glmmTMB(herb_p_t_sc ~ NPP_g_10y + Trichomes_t_sc + Conc_t_sc + SLA_t_sc + (1|Pop),data = test_data),
  glmmTMB(Conc_t_sc ~ NPP_g_10y + (1|Pop),data = test_data),
  glmmTMB(SLA_t_sc ~ NPP_g_10y + (1|Pop),data = test_data),
  glmmTMB(Trichomes_t_sc ~ NPP_g_10y + (1|Pop), data = test_data),
  glmmTMB(NPP_g_10y ~ Latitude_sc + (1|dummy), data = test_data),
  SLA_t_sc %~~% Trichomes_t_sc,
  SLA_t_sc %~~% Conc_t_sc,
  data = test_data
)


summary(test_mod)

# Latittude squared term included ----
test_mod<-psem(
  glmmTMB(herb_p_t_sc ~ NPP_g_10y + Latitude_sc + Latitude_sc_sq + Trichomes_t_sc + Conc_t_sc + SLA_t_sc + (1|Pop),data = test_data),
  glmmTMB(Conc_t_sc ~ NPP_g_10y + Latitude_sc + Latitude_sc_sq + (1|Pop),data = test_data),
  glmmTMB(SLA_t_sc ~ NPP_g_10y + Latitude_sc + Latitude_sc_sq + (1|Pop),data = test_data),
  glmmTMB(Trichomes_t_sc ~ NPP_g_10y + Latitude_sc + Latitude_sc_sq + (1|Pop), data = test_data),
  glmmTMB(NPP_g_10y ~ Latitude_sc + (1|dummy), data = test_data),
  SLA_t_sc %~~% Trichomes_t_sc,
  SLA_t_sc %~~% Conc_t_sc,
  data = test_data
)


summary(test_mod)

m2<-glmmTMB(herb_p_t_sc ~ NPP_g_10y + Latitude_sc + Latitude_sc_sq + Trichomes_t_sc + Conc_t_sc + SLA_t_sc + as.factor(Date) + (1|Pop),data = test_data)

Anova(m2)

# Date as categorical----
test_mod<-psem(
  glmmTMB(herb_p_t_sc ~ NPP_g_10y_sc + Latitude_sc + Latitude_sc_sq + Trichomes_t_sc + Conc_t_sc + SLA_t_sc + Date_fc + (1|Pop) + (1|Plant_ID),data = test_data),
  glmmTMB(Conc_t_sc ~ NPP_g_10y_sc + Latitude_sc + Latitude_sc_sq + (1|Pop),data = test_data),
  glmmTMB(SLA_t_sc ~ NPP_g_10y_sc + Latitude_sc + Latitude_sc_sq + (1|Pop),data = test_data),
  glmmTMB(Trichomes_t_sc ~ NPP_g_10y_sc + Latitude_sc + Latitude_sc_sq + (1|Pop), data = test_data),
  glmmTMB(NPP_g_10y_sc ~ Latitude_sc + (1|dummy), data = test_data),
  SLA_t_sc %~~% Trichomes_t_sc,
  SLA_t_sc %~~% Conc_t_sc,
  data = test_data
)


summary(test_mod)



test_mod<-psem(
  glmmTMB(herb_p ~ Latitude_sc + Latitude_sc_sq + Trichomes_t_sc + Conc_t_sc + SLA_t_sc + (1|Year/Pop),data = test_data,family = beta_family()),
  glmmTMB(Conc_t_sc ~ Latitude_sc + Latitude_sc_sq + (1|Year/Pop),data = test_data),
  glmmTMB(SLA_t_sc ~ Latitude_sc + Latitude_sc_sq + (1|Year/Pop),data = test_data),
  glmmTMB(Trichomes_t_sc ~ Latitude_sc + Latitude_sc_sq + (1|Pop:Year), data = test_data),
  SLA_t_sc %~~% Trichomes_t_sc,
  SLA_t_sc %~~% Conc_t_sc,
  data = test_data
)

summary(test_mod)
