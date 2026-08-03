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
library(gt)
library(lubridate)

# Load in Modified functions ----
source("R_code/Functions.R")


conflicts_prefer(dplyr::select,
                 dplyr::filter,
                 patchwork::area)
# Read in data ####



## climate data
#write.csv(PCs, 'Data/climate_data.csv')
clim_data <- read_csv('Data/clim_data.csv') %>% 
  distinct(Pop, .keep_all = TRUE)
head(clim_data)

hist(log10(clim_data$NPP))


ggpairs(clim_data %>% select(MAT, Tsd, AP, PWQ, NPP_g_10y, NPPg, NPP, Latitude, Clim_PC1, Clim_PC2, WeightedCD))

clim_pca1 <- princomp(clim_data %>%  select(MAT, Tsd, AP, PWQ), cor = TRUE)
summary(clim_pca1)
clim_pca2 <- princomp(clim_data %>% select(MAT, Tsd, AP, PWQ, NPP), cor = TRUE)
summary(clim_pca2)

biplot(clim_pca1)
biplot(clim_pca2)

plot(clim_pca1$scores[,1], clim_pca2$scores[,1], xlab = "PC1", ylab = "PC2")
plot(clim_pca1$scores[,1], clim_data$NPP, xlab = "PC1", ylab = "PC2")
plot(clim_pca1$scores[,1], clim_data$Clim_PC1)

clim_data$ClimPC1 <- clim_pca1$scores[,1]
clim_data$ClimProd1 <- clim_pca2$scores[,1]

ggpairs(clim_data %>% select(Latitude, NPP, ClimPC1, ClimProd1))


## Herbivore data ####
herbdat <- read_csv('Data/Combined_herbivores_data.csv') %>% 
  full_join(clim_data, by="Pop") %>% 
  mutate(Latitude=round(Latitude,3),
    Species=gsub("Meallybug", "Mealybug", Species),
                                 Species=gsub("White Fly", "White fly", Species),
                                 Species=gsub("Scales", "Scale", Species)) %>% 
  dplyr::filter(#Species == "Epitrix fuscula" | Species == "Leptinotarsa juncta" #| 
  #          #Species == "Mealybug" | Species == "Scale" |Species == "White fly" |
        Loc == "Field"  ) 
head(herbdat)
unique(herbdat$Species)
herb_summary <- herbdat %>% group_by(Pop,Species,Latitude,Year.x) %>% 
  rename(Year=Year.x) %>% 
  summarise(Amount=sum(Amount, na.rm=T)) %>% 
  pivot_wider(names_from = Species, values_from = Amount, values_fill = 0)

# write.csv(herb_summary, "Figures/HerbivoreTable.csv")

## plot Amount by climate facet_wrap by species 
ggplot(herb_summary, aes(y=Amount, x=ClimPC1))+
  geom_point() + 
  geom_smooth(method="glm.nb", formula= y~poly(x,2)) + 
  facet_wrap(~Species, scales = "free")+
  theme_bw(base_size = 16)

epi_mod <- glmmTMB(Amount ~ poly(ClimPC1,1) , data=herb_summary %>% filter(Species=='Epitrix fuscula'))
summary(epi_mod)

## herbivory and trait data from field and garden
#write.csv(Combined_data, 'Data/Combined_data.csv')
d1 <- read_csv('Data/Combined_data.csv')
head(d1)

# field and garden ####
field_garden <- d1 %>% 
  mutate(Ppop_ID = gsub("([A-Za-z0-9]+-[0-9]+)[A-Z]$", "\\1", Plant_ID)) %>% 
  full_join(.,clim_data, by=c("Pop")) %>% drop_na(Date)
head(field_garden)

library(dplyr)
library(stringr)

clean_ppop_id <- function(df) {
  df %>%
    mutate(Ppop_ID = case_when(
      
      # ARB: "Plant-12-Arb" -> "ARB-12"
      str_detect(Ppop_ID, "Plant-\\d+-Arb") ~
        paste0("ARB-", str_extract(Ppop_ID, "\\d+")),
      
      # CAH: "Plant-11-CAH" -> "CAH-11"
      str_detect(Ppop_ID, "Plant-\\d+-CAH") ~
        paste0("CAH-", str_extract(Ppop_ID, "\\d+")),
      
      # DUDU: "Plant1_Pop Dudu" -> "DUDU-1"
      str_detect(Ppop_ID, "Plant\\d+_Pop Dudu") ~
        paste0("DUDU-", str_extract(Ppop_ID, "\\d+")),
      
      # FLAT: "Flat1" -> "FLAT-1"
      str_detect(Ppop_ID, "^Flat\\d+$") ~
        paste0("FLAT-", str_extract(Ppop_ID, "\\d+")),
      
      # ICH: "Ich1" -> "ICH-1"
      str_detect(Ppop_ID, "^Ich\\d+$") ~
        paste0("ICH-", str_extract(Ppop_ID, "\\d+")),
      
      # I1: "I1 - Plant 1" -> "I1-1"
      str_detect(Ppop_ID, "^I1 - Plant \\d+$") ~
        paste0("I1-", str_extract(Ppop_ID, "\\d+$")),
      
      # I2: "I2 - Plant 1" -> "I2-1"
      str_detect(Ppop_ID, "^I2 - Plant \\d+$") ~
        paste0("I2-", str_extract(Ppop_ID, "\\d+$")),
      
      # I3: "I3 - Plant 1" -> "I3-1"
      str_detect(Ppop_ID, "^I3 - Plant \\d+$") ~
        paste0("I3-", str_extract(Ppop_ID, "\\d+$")),
      
      # I4: "I4 - Plant 1" -> "I4-1"
      str_detect(Ppop_ID, "^I4 - Plant \\d+$") ~
        paste0("I4-", str_extract(Ppop_ID, "\\d+$")),
      
      # N1: "N1 - Plant 1" -> "N1-1"
      str_detect(Ppop_ID, "^N1 - Plant \\d+$") ~
        paste0("N1-", str_extract(Ppop_ID, "\\d+$")),
      
      # N2: "N2 - Plant 1" -> "N2-1"
      str_detect(Ppop_ID, "^N2 - Plant \\d+$") ~
        paste0("N2-", str_extract(Ppop_ID, "\\d+$")),
      
      # N3: "N3 - Plant 1" -> "N3-1"
      str_detect(Ppop_ID, "^N3 - Plant \\d+$") ~
        paste0("N3-", str_extract(Ppop_ID, "\\d+$")),
      
      # N4: "N4 - Plant 1" -> "N4-1"
      str_detect(Ppop_ID, "^N4 - Plant \\d+$") ~
        paste0("N4-", str_extract(Ppop_ID, "\\d+$")),
      
      # N5: "N5 - Plant 1" -> "N5-1"
      str_detect(Ppop_ID, "^N5 - Plant \\d+$") ~
        paste0("N5-", str_extract(Ppop_ID, "\\d+$")),
      
      # N6: "N6 - Plant 1" -> "N6-1"
      str_detect(Ppop_ID, "^N6 - Plant \\d+$") ~
        paste0("N6-", str_extract(Ppop_ID, "\\d+$")),
      
      # N7: "N7A - Plant 1" -> "N7-1"
      str_detect(Ppop_ID, "^N7A - Plant \\d+$") ~
        paste0("N7-", str_extract(Ppop_ID, "\\d+$")),
      
      # N8: "N8 - Plant 1" -> "N8-1"
      str_detect(Ppop_ID, "^N8 - Plant \\d+$") ~
        paste0("N8-", str_extract(Ppop_ID, "\\d+$")),
      
      # BR: "BlackRiver1" -> "BR-1"
      str_detect(Ppop_ID, "^BlackRiver\\d+$") ~
        paste0("BR-", str_extract(Ppop_ID, "\\d+")),
      
      # ECO: "Eco1" -> "ECO-1"
      str_detect(Ppop_ID, "^Eco\\d+$") ~
        paste0("ECO-", str_extract(Ppop_ID, "\\d+")),
      
      # N5 Culvan: "Culvan-1" -> "N5-1" (these are N5 plants)
      str_detect(Ppop_ID, "^Culvan-\\d+$") ~
        paste0("N5-", str_extract(Ppop_ID, "\\d+")),
      
      # N7 Deer Run: "Deer-Run-1" -> "N7-1" (these are N7 plants)
      str_detect(Ppop_ID, "^Deer-Run-\\d+$") ~
        paste0("N7-", str_extract(Ppop_ID, "\\d+")),
      
      # N9 UWM: "UWM-1" -> "N9-1"
      str_detect(Ppop_ID, "^UWM-\\d+$") ~
        paste0("N9-", str_extract(Ppop_ID, "\\d+")),
      
      # N10 Kettle: "Kettle-1" -> "N10-1"
      str_detect(Ppop_ID, "^Kettle-\\d+$") ~
        paste0("N10-", str_extract(Ppop_ID, "\\d+")),
      
      # PS field: "PS1" -> "PS-1"
      str_detect(Ppop_ID, "^PS\\d+$") ~
        paste0("PS-", str_extract(Ppop_ID, "\\d+")),
      
      # All other IDs remain unchanged
      TRUE ~ Ppop_ID
    ))
}

# Apply to your dataframe
field_garden <- field_garden %>% clean_ppop_id()

# Verify results
fgcheck <- field_garden %>%
  select(Pop, Ppop_ID) %>%
  distinct() %>%
  arrange(Pop, Ppop_ID) 

# Summarize population averages and pivot wider
field_garden_wide <- field_garden %>%
  group_by(Pop, Loc) %>%
  summarise(
    Trichomes = mean(Trichomes, na.rm = TRUE),
    SLA = mean(SLA, na.rm = TRUE),
    Conc = mean(Conc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Loc,
    values_from = c(Trichomes, SLA, Conc)
  ) %>%
  drop_na()  # retain only the 15 overlapping populations

#write.csv(field_garden_wide, "field_garden_wide.csv")

# Run correlations
cor_trichomes <- cor.test(field_garden_wide$Trichomes_Field, 
                          field_garden_wide$Trichomes_Garden)

cor_sla <- cor.test(field_garden_wide$SLA_Field, 
                    field_garden_wide$SLA_Garden)

cor_conc <- cor.test(field_garden_wide$Conc_Field, 
                     field_garden_wide$Conc_Garden)

# Print results
cor_trichomes
cor_sla
cor_conc


t1 <- glmmTMB(Trichomes_Field ~ Trichomes_Garden + (1 | Pop), data = field_garden_wide)
summary(t1)
r2(t1)
t1 <- glmmTMB(SLA_Field ~ SLA_Garden + (1 | Pop), data = field_garden_wide)
summary(t1)
t1 <- glmmTMB(Conc_Field ~ Conc_Garden + (1 | Pop), data = field_garden_wide)
summary(t1)

## figures of field ~~ garden correlations ####
# A) Trichomes
pA_cor <- ggplot(field_garden_wide, aes(x = Trichomes_Field, y = Trichomes_Garden)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "black") +
  annotate("text", x = Inf, y = Inf,
           label = paste0("r = ", round(cor_trichomes$estimate, 2),
                          "\np = ", round(cor_trichomes$p.value, 3)),
           hjust = 1.1, vjust = 1.5, size = 3.5) +
  labs(x = "Field Trichomes", y = "Garden Trichomes", tag = "A") +
  theme_bw()

# B) SLA
pB_cor <- ggplot(field_garden_wide, aes(x = SLA_Field, y = SLA_Garden)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "black") +
  annotate("text", x = Inf, y = Inf,
           label = paste0("r = ", round(cor_sla$estimate, 2),
                          "\np = ", round(cor_sla$p.value, 3)),
           hjust = 1.1, vjust = 1.5, size = 3.5) +
  labs(x = "Field SLA", y = "Garden SLA", tag = "B") +
  theme_bw()

# C) Glycoalkaloids
pC_cor <- ggplot(field_garden_wide, aes(x = Conc_Field, y = Conc_Garden)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "black") +
  annotate("text", x = Inf, y = Inf,
           label = paste0("r = ", round(cor_conc$estimate, 2),
                          "\np = ", round(cor_conc$p.value, 3)),
           hjust = 1.1, vjust = 1.5, size = 3.5) +
  labs(x = "Field Glycoalkaloids", y = "Garden Glycoalkaloids", tag = "C") +
  theme_bw()

# Combine panels
fg_cors <- pA_cor | pB_cor | pC_cor

ggsave("Figures/field_garden_correlations.png", fg_cors, width = 12, height = 4, dpi = 300)


# Field data ####
field_data <- d1  %>% filter(Loc == "Field") %>% 
  full_join(clim_data, by=c("Pop")) %>% drop_na(Date) %>% 
  mutate(Year=Year.x) %>% 
  filter(Pop != "BR")
head(field_data)

colnames(field_data)
ggpairs(field_data %>% select(herb_p, Trichomes, SLA, Conc, Clim_PC1, ClimProd1, WeightedCD))

fcount <- field_data %>% count(Pop,Year)
fsum <- field_data %>% mutate(obs=1) %>%
  summarise(nplant = sum(obs),
            .by = c(Pop,Year)) %>% 
  summarize(mean = mean(nplant), sd = sd(nplant), min = min(nplant), max = max(nplant))



# Log-transform and scale the relevant variables
field_datas <- field_data %>%
  mutate(
    herb_p_s    = (logit(herb_p)),      # logit if preferred
    Trichomes_s = scale(log(Trichomes))[,1],
    SLA_s       = scale(log(SLA))[,1],
    Conc_s      = scale(log(Conc))[,1],
    Clim_PC1_s  = scale(Clim_PC1*-1)[,1],
    Clim_PC1_s2 = Clim_PC1_s^2,              # quadratic term
    ClimPC1_s  = scale(ClimPC1)[,1],
    ClimPC1_s2 = ClimPC1_s^2,              # quadratic term
    ClimProd1_s  = scale(ClimProd1)[,1],
    ClimProd1_s2 = ClimProd1_s^2,              # quadratic term
    NPP_s  = scale(NPP)[,1],
    NPP_s2  = NPP_s^2,
    NPPg_s       = scale(NPPg)[,1]
  ) #%>% filter(SLA>50)

ggpairs(field_datas %>% select(herb_p_s, herb_p_s, Trichomes_s, SLA_s, Conc_s, NPP_s, Clim_PC1_s, ClimPC1_s, ClimProd1_s))
cor.test(field_datas$ClimPC1, field_datas$NPP)

## linear Sub-models ####
# Herbivory model (population nested within year)
mod_herb1 <- glmmTMB(herb_p ~ ClimPC1_s +Trichomes_s + SLA_s + Conc_s +
                      (1 | Year/Pop), family=ordbeta,
                    data = field_datas)
summary(mod_herb1)
simulateResiduals(mod_herb1, plot=T)

pred_herb <- ggpredict(mod_herb1, terms = c("ClimPC1_s"))
plot(pred_herb)
pred_herb1 <- ggpredict(mod_herb1, terms = c("Conc_s"))
plot(pred_herb1)

# Trichomes model (year:population random effect due to singularity)
mod_trich1 <- glmmTMB(Trichomes_s ~ ClimPC1_s + 
                       (1 | Year:Pop),
                     data = field_datas)
summary(mod_trich1)
simulateResiduals(mod_trich1, plot=T)

pred_trich <- ggpredict(mod_trich1, terms = c("ClimPC1_s [all]"))
plot(pred_trich)

# SLA model (population nested within year)
mod_SLA1 <- glmmTMB(SLA_s ~ ClimPC1_s + 
                     (1 | Year/Pop),
                   data = field_datas)
summary(mod_SLA1)
simulateResiduals(mod_SLA1, plot=T)

# Glycoalkaloids model (population nested within year)
mod_Conc1 <- glmmTMB(Conc_s ~ ClimPC1_s + 
                      (1 | Year/Pop),
                    data = field_datas)
summary(mod_Conc1)
simulateResiduals(mod_Conc1, plot=T)

pred_Conc <- ggpredict(mod_Conc1, terms = c("ClimPC1_s"))
plot(pred_Conc)

# Assemble pSEM
field_psem1 <- psem(
  mod_herb1,
  mod_trich1,
  mod_SLA1,
  mod_Conc1,
  SLA_s %~~% Trichomes_s,
  #Conc_s %~~% SLA_s,
  data = field_datas
)

## Summary of field psem ####
summary(field_psem1, .progressBar = FALSE)
coefs(field_psem1)


## Quadratic Sub-models ####
# Herbivory model (population nested within year)
mod_herb <- glmmTMB(herb_p ~ ClimPC1_s + ClimPC1_s2 + Trichomes_s + SLA_s + Conc_s +
                   (1 | Year/Pop), family=ordbeta,
                 data = field_datas)
summary(mod_herb)
simulateResiduals(mod_herb, plot=T)

pred_herb <- ggpredict(mod_herb, terms = c("ClimPC1_s"))
plot(pred_herb)
pred_herb1 <- ggpredict(mod_herb, terms = c("Conc_s"))
plot(pred_herb1)

# Trichomes model (year:population random effect due to singularity)
mod_trich <- glmmTMB(Trichomes_s ~ ClimPC1_s + ClimPC1_s2  +
                    (1 | Year:Pop),
                  data = field_datas)
summary(mod_trich)
simulateResiduals(mod_trich, plot=T)

pred_trich <- ggpredict(mod_trich, terms = c("ClimPC1_s [all]"))
plot(pred_trich)

# SLA model (population nested within year)
mod_SLA <- glmmTMB(SLA_s ~ ClimPC1_s + ClimPC1_s2  +
                  (1 | Year/Pop),
                data = field_datas)
summary(mod_SLA)
simulateResiduals(mod_SLA, plot=T)

# Glycoalkaloids model (population nested within year)
mod_Conc <- glmmTMB(Conc_s ~ ClimPC1_s + ClimPC1_s2  +
                   (1 | Year/Pop),
                 data = field_datas)
summary(mod_Conc)
simulateResiduals(mod_Conc, plot=T)

pred_Conc <- ggpredict(mod_Conc, terms = c("ClimPC1_s"))
plot(pred_Conc)

# Assemble pSEM
field_psem <- psem(
  mod_herb,
  mod_trich,
  mod_SLA,
  mod_Conc,
  SLA_s %~~% Trichomes_s,
  #Conc_s %~~% SLA_s,
  data = field_datas
)

## Summary of field psem ####
summary(field_psem, .progressBar = FALSE)
coefs(field_psem)

summary(field_psem1, .progressBar = FALSE)$AIC #linear 
summary(field_psem, .progressBar = FALSE)$AIC  #quadratic


plot(partialResid(herb_p ~ Conc_s, field_psem)$xresid,
     partialResid(herb_p ~ Conc_s, field_psem)$yresid)
abline(partialResid(herb_p ~ Conc_s, field_psem)$xresid,
       partialResid(herb_p ~ Conc_s, field_psem)$yresid)

## 2022 data ----
# field_datas_2022 <- field_datas %>% 
#   filter(Year.x == "2022") %>% 
#   filter(Pop %in% pull(d1 %>% filter(Loc=="Garden"), Pop))
# 
# mod_herb <- glmmTMB(herb_p ~ ClimPC1_s + ClimPC1_s2 + Trichomes_s + SLA_s + Conc_s +
#                       (1 | Pop), family=ordbeta,
#                     data = field_datas_2022)
# summary(mod_herb)
# simulateResiduals(mod_herb, plot=T)
# 
# pred_herb <- ggpredict(mod_herb, terms = c("ClimPC1_s"))
# plot(pred_herb)
# pred_herb1 <- ggpredict(mod_herb, terms = c("Conc_s"))
# plot(pred_herb1)
# 
# # Trichomes model (year:population random effect due to singularity)
# mod_trich <- glmmTMB(Trichomes_s ~ ClimPC1_s + ClimPC1_s2  +
#                        (1 | Pop),
#                      data = field_datas_2022)
# summary(mod_trich)
# simulateResiduals(mod_trich, plot=T)
# 
# pred_trich <- ggpredict(mod_trich, terms = c("ClimPC1_s [all]"))
# plot(pred_trich)
# 
# # SLA model (population nested within year)
# mod_SLA <- glmmTMB(SLA_s ~ ClimPC1_s + ClimPC1_s2  +
#                      (1 | Pop),
#                    data = field_datas_2022)
# summary(mod_SLA)
# simulateResiduals(mod_SLA, plot=T)
# 
# # Glycoalkaloids model (population nested within year)
# mod_Conc <- glmmTMB(Conc_s ~ ClimPC1_s + ClimPC1_s2  +
#                       (1 | Pop),
#                     data = field_datas_2022)
# summary(mod_Conc)
# simulateResiduals(mod_Conc, plot=T)
# 
# pred_Conc <- ggpredict(mod_Conc, terms = c("ClimPC1_s"))
# plot(pred_Conc)
# 
# # Assemble pSEM
# field_psem <- psem(
#   mod_herb,
#   mod_trich,
#   mod_SLA,
#   mod_Conc,
#   SLA_s %~~% Trichomes_s,
#   #Conc_s %~~% SLA_s,
#   data = field_datas_2022
# )
# 
# ## Summary of field psem ####
# summary(field_psem, .progressBar = FALSE)
# coefs(field_psem)
# 
# 
# 
# 

## FIELD table ####
# Extract coefficients from psem summary
psem_summary <- summary(field_psem, .progressBar = FALSE)

# Build coefficient table
coef_table <- psem_summary$coefficients %>%
  select(Response, Predictor, Estimate, Std.Error, P.Value) %>%
  mutate(
    Estimate = round(as.numeric(Estimate), 3),
    Std.Error = ifelse(Std.Error == "-", "-", round(as.numeric(Std.Error), 3)),
    P.Value = round(as.numeric(P.Value), 3)
  )

# Add R2 values
r2_table <- psem_summary$R2 %>%
  select(Response, Marginal, Conditional) %>%
  rename(R2m = Marginal, R2c = Conditional) %>%
  mutate(
    R2m = round(R2m, 2),
    R2c = round(R2c, 2)
  )

# Join R2 to coefficient table
coef_table <- coef_table %>%
  left_join(r2_table, by = "Response")

# Build gt table
coef_table_full <- coef_table %>%
  gt(groupname_col = "Response") %>%
  tab_header(
    title = "Piecewise Structural Equation Model Results",
    subtitle = "Field Data"
  ) %>%
  cols_label(
    Predictor = "Predictor",
    Estimate = "Estimate",
    Std.Error = "SE",
    P.Value = "p-value",
    R2m = "R²m",
    R2c = "R²c"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  fmt_number(
    columns = c(Estimate, Std.Error),
    decimals = 3
  ) %>%
  fmt_number(
    columns = P.Value,
    decimals = 3
  ) %>%
  tab_options(
    row_group.background.color = "#f0f0f0",
    table.font.size = 12
  )

gtsave(coef_table_full, "Figures/psem_table.docx")  

## Field bivariate plots ####
# Calculate population averages
field_pop_avg <- field_datas %>%
  group_by(Pop) %>%
  summarise(
    ClimPC1 = mean(ClimPC1, na.rm = TRUE),
    ClimPC1_s = mean(ClimPC1_s, na.rm = TRUE),
    Trichomes = mean(Trichomes, na.rm = TRUE),
    Conc = mean(Conc, na.rm = TRUE),
    herb_p = mean(herb_p, na.rm = TRUE),
    Height = mean(Height, na.rm = TRUE),
    Leaves = mean(Leaves, na.rm = TRUE),
    NPP = mean(NPP, na.rm = TRUE)
  )

# A) Trichomes by ClimPC1 (quadratic)
pA <- ggplot() +
  geom_smooth(data = field_pop_avg, aes(x = ClimPC1, y = Trichomes),
              method = "lm", formula = y ~ x + I(x^2), color = "black") +
  geom_point(data = field_datas, aes(x = ClimPC1, y = Trichomes), 
             alpha = 0.2, size = 1) +
  geom_point(data = field_pop_avg, aes(x = ClimPC1, y = Trichomes), 
             size = 3) +
  labs(x = "Climate PC1", y = "Trichomes (#/cm^2)", tag = "A") +
  annotate("text", x = 1.5, y = 380,
           label = "x: italic(p) == 0.21", parse=T, hjust=0, size = 5) +
  annotate("text", x = 1.5, y = 350,
           label = "x^2: italic(p) == 0.003", parse=T, hjust=0, size = 5) +
  theme_bw(base_size = 18)

# B) Conc by ClimPC1 (linear)
pB <- ggplot() +
  geom_smooth(data = field_pop_avg, aes(x = ClimPC1, y = Conc),
              method = "lm", formula = y ~ x, color = "black") +
  labs(x = "Climate PC1", y = "Glycoalkaloids (mg/g)", tag = "B") +
  geom_point(data = field_datas, aes(x = ClimPC1, y = Conc), 
             alpha = 0.2, size = 1) +
  geom_point(data = field_pop_avg, aes(x = ClimPC1, y = Conc), 
             size = 3) +
  annotate("text", x = 1.5, y = 1.3,
           label = "x: italic(p) == 0.070", parse=T, hjust=0, size = 5) +
  annotate("text", x = 1.5, y = 1.2,
           label = "x^2: italic(p) == 0.50", parse=T, hjust=0, size = 5) +
  theme_bw(base_size = 18)

# C) herb_p by ClimPC1 (quadratic)
herb1 <- glmmTMB(herb_p ~ poly(ClimPC1, 2) + Conc + (1 | Year/Pop), family=ordbeta, data = field_datas)
pred_herb_clim <- ggpredict(herb1, terms = "ClimPC1 [all]")
pred_herb_conc <- ggpredict(herb1, terms = "Conc [all]")

pC <- ggplot() +
  geom_line(data = pred_herb_clim, aes(x = x, y = predicted), linewidth=1.25, color = "black") +
  geom_ribbon(data = pred_herb_clim, aes(x = x, ymin = conf.low, ymax = conf.high),
              alpha = 0.2) +
  geom_point(data = field_datas, aes(x = ClimPC1, y = herb_p),
             alpha = 0.2, size = 1) +
  geom_point(data = field_pop_avg, aes(x = ClimPC1, y = herb_p),
             size = 3) +
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Climate PC1", y = "Herbivore damage", tag = "C") +
  annotate("text", x = 1.5, y = .48,
           label = "x: italic(p) == 0.026", parse=T, hjust=0, size = 5) +
  annotate("text", x = 1.5, y = .42,
           label = "x^2: italic(p) == 0.003", parse=T, hjust=0, size = 5) +
  theme_bw(base_size = 18)

# D) herb_p by Conc
pD <- ggplot() +
  geom_line(data = pred_herb_conc, aes(x = x, y = predicted), linewidth=1.25, color = "black") +
  geom_ribbon(data = pred_herb_conc, aes(x = x, ymin = conf.low, ymax = conf.high),
              alpha = 0.2) +
  geom_point(data = field_datas, aes(x = Conc, y = herb_p),
             alpha = 0.2, size = 1) +
  geom_point(data = field_pop_avg, aes(x = Conc, y = herb_p),
             size = 3) +
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Glycoalkaloids (mg/mg)", y = "Herbivore damage", tag = "D") +
  annotate("text", x = .9, y = .4,
           label = "italic(p) == 0.004", parse=T, hjust=0, size = 5) +
  theme_bw(base_size = 18)

# Combine panels
fig3 <- (pA | pB) / (pC | pD)

ggsave("Figures/field_bivariate_plots.png", fig3, width = 12, height = 10, dpi = 300)

## plot of plant height ####
hmod1 <- lm(Height ~ poly(ClimPC1_s, 2), data = field_pop_avg)
summary(hmod1)
Anova(hmod1)

lmod1 <- lm(NPP ~ poly(ClimPC1_s, 1), data = field_pop_avg)
summary(lmod1)
Anova(lmod1)

lmod1 <- lm(Leaves ~ poly(ClimPC1_s, 1), data = field_pop_avg)
summary(lmod1)
Anova(lmod1)

height_plot <- ggplot(field_pop_avg, aes(x = ClimPC1_s, y = Height)) +
  geom_point() +
  geom_smooth(method= "lm", formula = y~poly(x,2))+
  theme_bw(base_size = 18)

lvs_plot <- ggplot(field_pop_avg, aes(x = ClimPC1, y = Leaves)) +
  geom_point() +
  geom_smooth(method= "lm", formula = y~x)+
  theme_bw(base_size = 13) +
  annotate("text", x = -.5, y = 15,
           label = paste0("r = 0.76","\np = 0.004"), size = 4.5) +
  xlab("Climate PC1")
figS1 <- height_plot + lvs_plot
ggsave("Figures/height_leaves_climpc1.png", figS1, width = 12, height = 6, dpi = 300)


## Field NPP ~ PC plot ####
proc_PC <- ggplot(field_pop_avg, aes(x = ClimPC1, y = NPP)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(x = "Climate PC1", y = expression("Ten year NPP (kg*C/m"^"2"*")")) +
  annotate("text", x = -.5, y = 1,
           label = paste0("r = 0.69","\np < 0.001"), size = 4.5) +
  theme_bw(base_size = 13)
ggsave("Figures/NPP_climpc1.png", proc_PC, width = 6, height = 5, dpi = 300)

# Garden DATA ####
garden_data <- d1 %>% filter(Loc == "Garden") %>% 
  full_join(clim_data, by=c("Pop")) %>% drop_na(Date)
head(garden_data)

colnames(garden_data)
ggpairs(garden_data %>% select(herb_p, Trichomes, SLA, Conc, Clim_PC1, ClimProd1))


## plot herbivory data ####
ggplot(garden_data, aes(x = Date, y = herb_p, group = Date)) +
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(width=.2, height=0) +
  labs(x = "Date", y = "Herbivory (proportion)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## summarize garden data ####
garden_summary <- garden_data %>%
  mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
  filter(month(Date) %in% c(6, 7)) %>%
  summarise(
    .by = Plant_ID,
    across(-c(Date, herb_p), first),
    mean_herb = mean(herb_p, na.rm = TRUE),
    median_herb = median(herb_p, na.rm = TRUE),
    max_herb = max(herb_p, na.rm = TRUE)
  )

hist(garden_summary$SLA)

gsum <- garden_summary %>% mutate(obs=1) %>%  group_by(Pop) %>% 
  summarise(nplant = sum(obs)) %>% 
  ungroup() %>% 
  summarize(mean = mean(nplant), sd = sd(nplant), min = min(nplant), max = max(nplant))


# Log-transform and scale the relevant variables
garden_summary <- garden_summary %>%
  mutate(
    herb_p_s    = (logit(mean_herb)),      # logit if preferred
    Trichomes_s = scale(log(Trichomes))[,1],
    SLA_s       = scale(log(SLA))[,1],
    Conc_s      = scale(log(Conc))[,1],
    Clim_PC1_s  = scale(Clim_PC1*-1)[,1],
    Clim_PC1_s2 = Clim_PC1_s^2,              # quadratic term
    ClimPC1_s  = scale(ClimPC1)[,1],
    ClimPC1_s2 = ClimPC1_s^2,              # quadratic term
    ClimProd1_s  = scale(ClimProd1)[,1],
    ClimProd1_s2 = ClimProd1_s^2,              # quadratic term
    NPP_s  = scale(NPP)[,1],
    NPP_s2  = NPP_s^2,
    NPPg_s       = scale(NPPg)[,1]
  ) #%>% filter(SLA<250)

ggpairs(garden_summary %>% select(herb_p_s, mean_herb, max_herb, median_herb, Trichomes_s, SLA_s, Conc_s, NPP_s, Clim_PC1_s, ClimPC1_s, ClimProd1_s))


# SEM garden models ####

## linear sub-model ####
# Herbivory model (population random effect)
mod_herb_g1 <- glmmTMB(mean_herb ~ ClimPC1_s + Trichomes_s + SLA_s + Conc_s +
                        (1 | Pop), family = ordbeta,
                      data = garden_summary)
summary(mod_herb_g1)
simulateResiduals(mod_herb_g1, plot = T)

# Trichomes model (population random effect)
mod_trich_g1 <- glmmTMB(Trichomes_s ~ ClimPC1_s + 
                         (1 | Pop),
                       data = garden_summary)
summary(mod_trich_g1)
simulateResiduals(mod_trich_g1, plot = T)

# SLA model (population random effect)
mod_SLA_g1 <- glmmTMB(SLA_s ~ ClimPC1_s +
                       (1 | Pop),
                     data = garden_summary)
summary(mod_SLA_g1)
simulateResiduals(mod_SLA_g1, plot = T)

# Glycoalkaloids model (population random effect)
mod_Conc_g1 <- glmmTMB(Conc_s ~ ClimPC1_s + 
                        (1 | Pop),
                      data = garden_summary)
summary(mod_Conc_g1)
simulateResiduals(mod_Conc_g1, plot = T)

# Assemble pSEM
garden_psem1 <- psem(
  mod_herb_g1,
  mod_trich_g1,
  mod_SLA_g1,
  mod_Conc_g1,
  SLA_s %~~% Trichomes_s,
  #Conc_s %~~% SLA_s,
  data = garden_summary
)

# Summary with AICc
summary(garden_psem1, .progressBar = FALSE)

## Quad Sub-models ####
# Herbivory model (population random effect)
mod_herb_g <- glmmTMB(mean_herb ~ ClimPC1_s + ClimPC1_s2 + Trichomes_s + SLA_s + Conc_s +
                        (1 | Pop), family = ordbeta,
                      data = garden_summary)
summary(mod_herb_g)
simulateResiduals(mod_herb_g, plot = T)

# Trichomes model (population random effect)
mod_trich_g <- glmmTMB(Trichomes_s ~ ClimPC1_s + ClimPC1_s2 +
                         (1 | Pop),
                       data = garden_summary)
summary(mod_trich_g)
simulateResiduals(mod_trich_g, plot = T)

# SLA model (population random effect)
mod_SLA_g <- glmmTMB(SLA_s ~ ClimPC1_s + ClimPC1_s2 +
                       (1 | Pop),
                     data = garden_summary)
summary(mod_SLA_g)
simulateResiduals(mod_SLA_g, plot = T)

# Glycoalkaloids model (population random effect)
mod_Conc_g <- glmmTMB(Conc_s ~ ClimPC1_s + ClimPC1_s2 +
                        (1 | Pop),
                      data = garden_summary)
summary(mod_Conc_g)
simulateResiduals(mod_Conc_g, plot = T)

# Assemble pSEM
garden_psem <- psem(
  mod_herb_g,
  mod_trich_g,
  mod_SLA_g,
  mod_Conc_g,
  SLA_s %~~% Trichomes_s,
  #Conc_s %~~% SLA_s,
  data = garden_summary
)

# Summary with AICc
summary(garden_psem, .progressBar = FALSE)

## Compare linear and quad models with AICc ###
summary(garden_psem1)$AIC #linear
summary(garden_psem)$AIC #quadratic

## GARDEN bivariate plots ####
# Calculate population averages for garden
garden_pop_avg <- garden_summary %>%
  group_by(Pop) %>%
  summarise(
    ClimPC1 = mean(ClimPC1, na.rm = TRUE),
    ClimPC1_s = mean(ClimPC1_s, na.rm = TRUE),
    Trichomes = mean(Trichomes, na.rm = TRUE),
    Conc = mean(Conc, na.rm = TRUE),
    SLA = mean(SLA, na.rm = TRUE),
    mean_herb = mean(mean_herb, na.rm = TRUE)
  )

# Get predictions from ordbeta garden models
herb2 <- glmmTMB(mean_herb ~ poly(ClimPC1_s, 2) + SLA , 
                 family=ordbeta, data = garden_summary # %>% filter(SLA < 250)
                 )
summary(herb2)  

pred_herb_clim_g <- ggpredict(herb2, terms = "ClimPC1_s [all]")
pred_herb_SLA_g <- ggpredict(herb2, terms = "SLA [all]")

# A) Trichomes by ClimPC1 (quadratic)
pA_g <- ggplot() +
  geom_smooth(data = garden_summary, aes(x = ClimPC1, y = Trichomes),
              method = "lm", formula = y ~ x + I(x^2), color = "black") +
  geom_point(data = garden_summary, aes(x = ClimPC1, y = Trichomes),
             alpha = 0.2, size = 1.25) +
  geom_point(data = garden_pop_avg, aes(x = ClimPC1, y = Trichomes),
             size = 3) +
  labs(x = "Climate PC1", y = "Trichomes (#/cm^2)", tag = "A") +
  annotate("text", x = -1.5, y = 150,
           label = "x: italic(p) == 0.98", parse=T, hjust=0, size = 5) +
  annotate("text", x = -1.5, y = 135,
           label = "x^2: italic(p) == 0.055", parse=T, hjust=0, size = 5) +
  theme_bw(base_size = 18)

# B) Conc by ClimPC1 (linear)
pB_g <- ggplot() +
  geom_smooth(data = garden_pop_avg, aes(x = ClimPC1, y = Conc),
              method = "lm", formula = y ~ x, color = "black") +
  geom_point(data = garden_summary, aes(x = ClimPC1, y = Conc),
             alpha = 0.2, size = 1.25) +
  geom_point(data = garden_pop_avg, aes(x = ClimPC1, y = Conc),
             size = 3) +
  labs(x = "Climate PC1", y = "Glycoalkaloids (mg/g)", tag = "B") +
  annotate("text", x = 1.8, y = 1.2,
           label = "x: italic(p) == 0.003", parse=T, hjust=0, size = 5) +
  annotate("text", x = 1.8, y = 1.1,
           label = "x^2: italic(p) == 0.11", parse=T, hjust=0, size = 5) +
  theme_bw(base_size = 18)

# C) mean_herb by ClimPC1 (quadratic) with ordbeta predictions
pC_g <- ggplot() +
  #geom_line(data = pred_herb_clim_g, aes(x = x, y = predicted), linewidth = 1.25, color = "black") +
  #geom_ribbon(data = pred_herb_clim_g, aes(x = x, ymin = conf.low, ymax = conf.high),
            #  alpha = 0.2) +
  geom_point(data = garden_summary, aes(x = ClimPC1, y = mean_herb),
             alpha = 0.2, size = 1.25) +
  geom_point(data = garden_pop_avg, aes(x = ClimPC1, y = mean_herb),
             size = 3) +
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Climate PC1", y = "Herbivore damage", tag = "C") +
  annotate("text", x = 1.8, y = .4,
           label = "x: italic(p) == 0.33", parse=T, hjust=0, size = 5) +
  annotate("text", x = 1.8, y = .35,
           label = "x^2: italic(p) == 0.27", parse=T, hjust=0, size = 5) +
  theme_bw(base_size = 18)

# D) mean_herb by SLA with ordbeta predictions
pD_g <- ggplot() +
  geom_line(data = pred_herb_SLA_g, aes(x = x, y = predicted), linewidth = 1.25, color = "black") +
  geom_ribbon(data = pred_herb_SLA_g, aes(x = x, ymin = conf.low, ymax = conf.high),
              alpha = 0.2) +
  geom_point(data = garden_summary, aes(x = SLA, y = mean_herb),
             alpha = 0.2, size = 1.25) +
  geom_point(data = garden_pop_avg, aes(x = SLA, y = mean_herb),
             size = 3) +
  scale_y_continuous(labels = scales::percent)+
  labs(x = "SLA", y = "Herbivore damage", tag = "D") +
  annotate("text", x = 240, y = .45,
           label = "x: italic(p) == 0.001", parse=T, hjust=0, size = 5) +
  theme_bw(base_size = 18)

# Combine panels
fig4 <- (pA_g | pB_g) / (pC_g | pD_g)
ggsave("Figures/garden_bivariate_plots.png", fig4, width = 12, height = 10, dpi = 300)

# Full table ####
# Extract field coefficients
field_coef <- psem_summary$coefficients %>%
  select(Response, Predictor, Estimate, Std.Error, P.Value) %>%
  mutate(
    Response = ifelse(Response == "herb_p", "herb", Response),
    Predictor = ifelse(Predictor == "herb_p", "herb", Predictor),
    Estimate = round(as.numeric(Estimate), 3),
    Std.Error = ifelse(Std.Error == "-", "-", round(as.numeric(Std.Error), 3)),
    P.Value = round(as.numeric(P.Value), 3),
    Dataset = "Field"
  )

# Extract garden coefficients
garden_summary_stats <- summary(garden_psem, .progressBar = FALSE)

garden_coef <- garden_summary_stats$coefficients %>%
  select(Response, Predictor, Estimate, Std.Error, P.Value) %>%
  mutate(
    Response = ifelse(Response == "mean_herb", "herb", Response),
    Predictor = ifelse(Predictor == "mean_herb", "herb", Predictor),
    Estimate = round(as.numeric(Estimate), 3),
    Std.Error = ifelse(Std.Error == "-", "-", round(as.numeric(Std.Error), 3)),
    P.Value = round(as.numeric(P.Value), 3),
    Dataset = "Garden"
  )

# Add missing Conc_s ~~ SLA_s row to garden with NAs
garden_coef <- garden_coef %>%
  bind_rows(data.frame(
    Response = "~~Conc_s",
    Predictor = "~~SLA_s",
    Estimate = NA,
    Std.Error = NA,
    P.Value = NA,
    Dataset = "Garden"
  ))

# Extract R2 values
field_r2 <- psem_summary$R2 %>%
  select(Response, Marginal, Conditional) %>%
  rename(R2m = Marginal, R2c = Conditional) %>%
  mutate(
    Response = ifelse(Response == "herb_p", "herb", Response),
    R2m = round(R2m, 2),
    R2c = round(R2c, 2),
    Dataset = "Field"
  )

garden_r2 <- garden_summary_stats$R2 %>%
  select(Response, Marginal, Conditional) %>%
  rename(R2m = Marginal, R2c = Conditional) %>%
  mutate(
    Response = ifelse(Response == "mean_herb", "herb", Response),
    R2m = round(R2m, 2),
    R2c = round(R2c, 2),
    Dataset = "Garden"
  )

# Combine field and garden coefficients wide format
combined_wide <- field_coef %>%
  left_join(field_r2, by = c("Response", "Dataset")) %>%
  select(-Dataset) %>%
  rename(
    Field_Estimate = Estimate,
    Field_SE = Std.Error,
    Field_P = P.Value,
    Field_R2m = R2m,
    Field_R2c = R2c
  ) %>%
  full_join(
    garden_coef %>%
      left_join(garden_r2, by = c("Response", "Dataset")) %>%
      select(-Dataset) %>%
      rename(
        Garden_Estimate = Estimate,
        Garden_SE = Std.Error,
        Garden_P = P.Value,
        Garden_R2m = R2m,
        Garden_R2c = R2c
      ),
    by = c("Response", "Predictor")
  )

# Build gt table with spanner columns
psem_table <- combined_wide %>%
  gt(groupname_col = "Response") %>%
  tab_header(
    title = "Piecewise Structural Equation Model Results",
    subtitle = "Field and Common Garden"
  ) %>%
  tab_spanner(
    label = "Field",
    columns = c(Field_Estimate, Field_SE, Field_P, Field_R2m, Field_R2c)
  ) %>%
  tab_spanner(
    label = "Common Garden",
    columns = c(Garden_Estimate, Garden_SE, Garden_P, Garden_R2m, Garden_R2c)
  ) %>%
  cols_label(
    Predictor = "Predictor",
    Field_Estimate = "Estimate",
    Field_SE = "SE",
    Field_P = "p-value",
    Field_R2m = "R²m",
    Field_R2c = "R²c",
    Garden_Estimate = "Estimate",
    Garden_SE = "SE",
    Garden_P = "p-value",
    Garden_R2m = "R²m",
    Garden_R2c = "R²c"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners()
  ) %>%
  fmt_number(
    columns = c(Field_Estimate, Field_SE, Garden_Estimate, Garden_SE),
    decimals = 3
  ) %>%
  fmt_number(
    columns = c(Field_P, Garden_P),
    decimals = 3
  ) %>%
  sub_missing(
    columns = everything(),
    missing_text = "-"
  ) %>%
  tab_footnote(
    footnote = "R²m = marginal R-squared (fixed effects); R²c = conditional R-squared (full model).",
    locations = cells_column_labels(columns = Field_R2m)
  ) %>%
  #tab_footnote(
  #  footnote = "Std. errors not available for correlated errors (~~).",
  #  locations = cells_column_labels(columns = c(Field_SE, Garden_SE))
  #) %>%
  tab_options(
    row_group.background.color = "#f0f0f0",
    table.font.size = 12
  )

## save table ####
gtsave(psem_table, "psem_results.docx")

# Table of Direct and Indirect effects ####
## ---- Field path coefficients ----
# Direct effects
field_direct_clim1 <- 0.305      # ClimPC1_s -> Herbivory
field_direct_clim2 <- -0.446      # ClimPC1_s2 -> Herbivory

# Trait paths
field_clim1_conc   <- 0.155       # ClimPC1_s -> Conc (p = 0.085)
field_clim1_trich  <- -0.119      # ClimPC1_s -> Trichomes (p = 0.005)
field_clim2_trich  <- -0.322      # ClimPC1_s2 -> Trichomes (p = 0.005)
field_conc_herb    <- -0.224      # Conc -> Herbivory (p = 0.005)
field_trich_herb   <- -0.068      # Trichomes -> Herbivory (p = 0.141) - marginal, excluded
field_sla_herb     <- -0.120      # SLA -> Herbivory (p = 0.101) - marginal, excluded

# Indirect effects
field_indirect_clim1_conc  <- field_clim1_conc  * field_conc_herb   # ClimPC1_s -> Conc -> Herbivory
field_indirect_clim1_trich <- field_clim1_trich * field_trich_herb  # ClimPC1_s2 -> Trichomes -> Herbivory
field_indirect_clim2_trich <- field_clim2_trich * field_trich_herb  # ClimPC1_s2 -> Trichomes -> Herbivory

# Total effects
field_total_clim1 <- field_direct_clim1 + field_indirect_clim1_conc + field_indirect_clim1_trich
field_total_clim2 <- field_direct_clim2 + field_indirect_clim2_trich

## ---- Garden path coefficients ----
# Direct effects
garden_direct_clim1 <- -0.112     # ClimPC1_s -> Herbivory (p = 0.334) - not significant
garden_direct_clim2 <-  0.096     # ClimPC1_s2 -> Herbivory (p = 0.275) - not significant

# Trait paths
garden_clim1_conc   <- -0.471     # ClimPC1_s -> Conc (p = 0.003)
garden_clim2_trich  <- -0.296     # ClimPC1_s2 -> Trichomes (p = 0.055)
garden_conc_herb    <-  0.011     # Conc -> Herbivory (p = 0.935) - not significant
garden_trich_herb   <-  0.063     # Trichomes -> Herbivory (p = 0.539) - not significant
garden_sla_herb     <-  0.388     # SLA -> Herbivory (p = 0.0002)

# No significant indirect effects through traits to herbivory in garden
# ClimPC1_s -> Conc -> Herbivory: Conc -> Herbivory not significant
# ClimPC1_s2 -> Trichomes -> Herbivory: Trichomes -> Herbivory not significant

## ---- Direct/indirect summary table ----
effects_table <- data.frame(
  Dataset = c(rep("Field", 6), rep("Garden", 6)),
  Predictor = c("ClimPC1_s", "ClimPC1_s", "ClimPC1_s",
                "ClimPC1_s2", "ClimPC1_s2", "ClimPC1_s2",
                "ClimPC1_s", "ClimPC1_s", "ClimPC1_s",
                "ClimPC1_s2", "ClimPC1_s2", "ClimPC1_s2"),
  Effect_Type = c("Direct", "Indirect (via Conc)", "Total",
                  "Direct", "Indirect (via Trichomes)", "Total",
                  "Direct (ns)", "Indirect (via Conc, ns)", "Total",
                  "Direct (ns)", "Indirect (via Trichomes, ns)", "Total"),
  Estimate = round(c(
    field_direct_clim1,
    field_indirect_clim1_conc,
    field_total_clim1,
    field_direct_clim2,
    field_indirect_clim2_trich,
    field_total_clim2,
    garden_direct_clim1,
    garden_clim1_conc * garden_conc_herb,
    garden_direct_clim1 + (garden_clim1_conc * garden_conc_herb),
    garden_direct_clim2,
    garden_clim2_trich * garden_trich_herb,
    garden_direct_clim2 + (garden_clim2_trich * garden_trich_herb)
  ), 3)
)

## ---- Print table ----
effect_table <- effects_table %>%
  gt(groupname_col = "Dataset") %>%
  tab_header(
    title = "Direct and Indirect Effects on Herbivory",
    subtitle = "Field and Common Garden"
  ) %>%
  cols_label(
    Predictor = "Predictor",
    Effect_Type = "Effect Type",
    Estimate = "Estimate"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_options(
    row_group.background.color = "#f0f0f0",
    table.font.size = 12
  )
gtsave(effect_table, "psem_effect_table.docx")

# Map Figure ----
Pop_info<-field_garden %>% 
  mutate(Year=Year.x) %>% 
  select(Pop,Year,Loc,Latitude,Longitude) %>% summarise(
           .by = c(Pop),
           
           Year = case_when(
             n_distinct(Year[Loc == "Field"]) == 2 ~ "Both Years",
             .default = as.character(first(Year))
           ),
           Loc = case_when(
             n_distinct(Loc) == 2 ~ "Field & Garden",
             n_distinct(Loc) == 1 & first(Loc) == "Garden" ~ "Garden only",
             n_distinct(Loc) == 1 & first(Loc) == "Field" ~ "Field only",
             .default = first(Loc)
           ),
           across(where(is.numeric), first),
         )

tempdir<-tempdir()

cn <- gadm(country = "USA", level = 1,path=tempdir)

clipxy <- c(-100,-65,25,50)

OR <- crop(cn,clipxy)

obs<-read_csv("Data/Solanum_carolinense_inat.csv") %>% 
  drop_na(latitude) %>% 
  rename(Longitude=longitude,
         Latitude=latitude) %>% 
  dplyr::filter(
    Longitude >= clipxy[1], Longitude <= clipxy[2],
    Latitude  >= clipxy[3], Latitude  <= clipxy[4]
  )

map_clim<-ggplot() + 
  geom_spatvector(data=OR,fill="NA",color="black") +
  geom_point(data=obs,aes(x=Longitude,y=Latitude),shape=21,size=0.025,fill="lightgrey",alpha=0.1)+
  geom_point(data=Pop_info,aes(x=Longitude,y=Latitude,shape = factor(Year), color = Loc),size=3,stroke=1.5)+
  scale_shape_manual(name="Year of field\n observation",values = c(24,21,22))+
  scale_color_manual(name="Location observed",values = c("#440154FF","#21908CFF","#FDE725FF"))+
  theme_void(base_size = 13) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.83, 0.87),
    legend.box =  "horizontal",
    legend.margin = margin(5, 5, 5, 5),
    legend.box.background = element_rect(fill = scales::alpha("white", 0.7), color = "black")
  );map_clim



pca<-PCbiplot(data1 = clim_data %>% select(., all_of(c('MAT',
                                                 'AP','PWQ',"Tsd"))),
              font_size = 4,rot_x = -1,ext = 2) + theme_bw(base_size=13);pca


clim_vars<-map_clim+
  plot_spacer()+
  inset_element(pca, -0.6, -0.1, 0.9, 0.47,align_to = "panel")+
  (lvs_plot/proc_PC) + 
  plot_annotation(tag_levels = "A") +
  plot_layout(widths = c(1.05,0.45,0.50)) 


ggsave("fig_1.png",
       device = "png",plot = clim_vars,
       path = "Figures",dpi = 400,width = 11, 
       height = 6)

# Full SEM plot ####
Field_semgraph<-semGraph(field_psem, marg_y = 5,
                         node_locs = list("Climate PC1"=c(-0.5,-1),
                                          "Climate PC1 (sq)"=c(0.5,-1)),
                         edge_locs = list(c("Climate PC1","Herbivory",0.65),
                                          c("Climate PC1 (sq)","Herbivory",0.65),
                                          c("Climate PC1 (sq)","Trichomes",0.2),
                                          c("Climate PC1","Glycoalkaloids",0.2),
                                          c("Climate PC1","Trichomes",0.8),
                                          c("Climate PC1 (sq)","Glycoalkaloids",0.8)),
                         curve_locs = list(c("Climate PC1","Herbivory",5.7),
                                           c("Climate PC1 (sq)","Herbivory",-5.7)))
Field_semgraph

Garden_semgraph<-semGraph(garden_psem, marg_y = 5,
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
