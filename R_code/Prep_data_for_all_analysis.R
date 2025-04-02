## Code by: Jacob Herschberger
## Date: January 2025
## Email: j.herschberger@ufl.edu
## Project: Culprits of plant defense variation across a latitudinal gradient.

# Import packages ----
# Run install.packages([Package name]) if package is not installed
library(tidyverse)
library(DBI)
library(RSQLite)

# Functions ----
transform_perc <- function(percentage_vec) {
  # See Cribari-Neto & Zeileis (2010)
  (percentage_vec * (length(percentage_vec) - 1) + 0.5)/length(percentage_vec)
}

# Import data ----

Pop_info<-dbReadTable(con,
                      'pop_info_2022_and_2023') %>% 
  select(Pop,Latitude)

### 2023 field plant traits
Field_2023<-dbReadTable(con,
                        'Field_2023') %>% 
  mutate(
         herb=herb_p,
         herb_p=(transform_perc(herb_p)),
         Date=as.Date(Date,origin = "1970-01-01"))

### Data from the field in 2022
Field_2022<-dbReadTable(con,"Field_2022") %>% 
  mutate(herb_p=Herbivory/100,
         herb=herb_p,
         herb_p=(transform_perc(herb_p)),
         Date=as.Date(Date,origin = "1970-01-01"))

### Data from the common garden 2023
Garden <- dbReadTable(con, "Garden_2023") %>% 
  mutate(
    herb=herb_p,
         herb_p=(transform_perc(herb_p))
         )

#### Import the climate and siol data ----
Clim_select<-c("wc2.1_30s_bio_1","wc2.1_30s_bio_2","wc2.1_30s_bio_3","wc2.1_30s_bio_4",
               "wc2.1_30s_bio_7",
               "wc2.1_30s_bio_12",
               "wc2.1_30s_bio_18"
)



Soil<-dbReadTable(con,"Soil") 

log_trans<-function(x){log(x-min(x)+1)}
sqrt_trans<-function(x){(x-min(x))}

Clim_ave<-dbReadTable(con,"Bioclims_1970_ave") %>%
  select(all_of(Clim_select)) %>% 
  mutate(wc2.1_30s_bio_18=log_trans(wc2.1_30s_bio_18)
  ) %>%  as.data.frame()

tables <- list(Soil=Soil,Clim_ave=Clim_ave)

combined_data <- list()

for (table_name in names(tables)) {
  df <- tables[[table_name]]
  
  numeric_df <- df %>% select(where(is.numeric)) 
  pca_result <- prcomp(numeric_df, scale. = TRUE)
  pca_scores <- as.data.frame(pca_result$x[, c("PC1", "PC2")])
  renamed_pca <- pca_scores %>%
    rename(
      !!paste0(table_name, "_PC1") := PC1,
      !!paste0(table_name, "_PC2") := PC2
    )
  combined_data[[table_name]] <- renamed_pca
} 

PCs <- bind_cols(combined_data) %>% cbind(Pop_info) %>% left_join(dbReadTable(con,"Bioclims_1970_ave") %>% select(Pop,Aridity))


