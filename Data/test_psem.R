test_data<-Data_prep() %>% 
  select(Pop,Year,NPP_g_10y,NPP_g,grep("sc",colnames(.),value=T),herb_p)


# Test model without the dummy random effect ----
test_mod<-psem(
  glmmTMB(herb_p_t_sc ~ NPP_y + Trichomes_t_sc + Conc_t_sc + SLA_t_sc + (1|Year/Pop),data = test_data),
  glmmTMB(Conc_t_sc ~ NPP_y + (1|Year/Pop),data = test_data),
  glmmTMB(SLA_t_sc ~ NPP_y + (1|Year/Pop),data = test_data),
  glmmTMB(Trichomes_t_sc ~ NPP_y + (1|Pop:Year), data = test_data),
  glmmTMB(NPP_y ~ Latitude_sc, data = test_data),
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
  glmmTMB(herb_p_t_sc ~ NPP_g + Trichomes_t_sc + Conc_t_sc + SLA_t_sc + (1|Year/Pop),data = test_data),
  glmmTMB(Conc_t_sc ~ NPP_g + (1|Year/Pop),data = test_data),
  glmmTMB(SLA_t_sc ~ NPP_g + (1|Year/Pop),data = test_data),
  glmmTMB(Trichomes_t_sc ~ NPP_g + (1|Pop:Year), data = test_data),
  glmmTMB(NPP_g ~ Latitude_sc + (1|dummy), data = test_data),
  SLA_t_sc %~~% Trichomes_t_sc,
  SLA_t_sc %~~% Conc_t_sc,
  data = test_data
)


summary(test_mod)

# Latittude squared term included ----
test_mod<-psem(
  glmmTMB(herb_p_t_sc ~ NPP_g + Latitude_sc + Latitude_sc_sq + Trichomes_t_sc + Conc_t_sc + SLA_t_sc + (1|Year/Pop),data = test_data),
  glmmTMB(Conc_t_sc ~ NPP_g + Latitude_sc + Latitude_sc_sq + (1|Year/Pop),data = test_data),
  glmmTMB(SLA_t_sc ~ NPP_g + Latitude_sc + Latitude_sc_sq + (1|Year/Pop),data = test_data),
  glmmTMB(Trichomes_t_sc ~ NPP_g + Latitude_sc + Latitude_sc_sq + (1|Pop:Year), data = test_data),
  glmmTMB(NPP_g ~ Latitude_sc + (1|dummy), data = test_data),
  SLA_t_sc %~~% Trichomes_t_sc,
  SLA_t_sc %~~% Conc_t_sc,
  data = test_data
)


summary(test_mod)


test_mod<-psem(
  glmmTMB(herb_p_t_sc ~ NPP_g + NPP_y + Latitude_sc + Latitude_sc_sq + Trichomes_t_sc + Conc_t_sc + SLA_t_sc + (1|Year/Pop),data = test_data),
  glmmTMB(Conc_t_sc ~ NPP_g + NPP_y + Latitude_sc + Latitude_sc_sq + (1|Year/Pop),data = test_data),
  glmmTMB(SLA_t_sc ~ NPP_g + NPP_y + Latitude_sc + Latitude_sc_sq + (1|Year/Pop),data = test_data),
  glmmTMB(Trichomes_t_sc ~ NPP_g + NPP_y + Latitude_sc + Latitude_sc_sq + (1|Pop:Year), data = test_data),
  glmmTMB(NPP_y ~ Latitude_sc + (1|dummy), data = test_data),
  glmmTMB(NPP_g ~ Latitude_sc + (1|dummy), data = test_data),
  SLA_t_sc %~~% Trichomes_t_sc,
  SLA_t_sc %~~% Conc_t_sc,
  NPP_y %~~% NPP_g,
  NPP_y %~~% Latitude_sc_sq,
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
