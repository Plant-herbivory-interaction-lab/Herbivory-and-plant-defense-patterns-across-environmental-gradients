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
con <-dbConnect(SQLite(), 'Data/Data.db')

Pop_info<-dbReadTable(con,
                      'pop_info_2022_and_2023') %>% 
  select(Pop,Latitude)

### Import field and garden herbivore-plant traits
Combined_data1<-dbReadTable(con,
                        "Combined_herbivory_and_trait_data") %>% 
  mutate(
         herb_p=(transform_perc(herb)),
         Date=as.Date(Date,origin = "1970-01-01")) 


### Data from the common garden 2023
Garden<-dbReadTable(con, "Garden_trait_data") %>% 
  mutate(
    herb_p=(transform_perc(herb)),
    Date=as.Date(Date,origin = "1970-01-01")) 

#### Import the climate and soil data ----

Soil<-dbReadTable(con,"Soil") 

log_transform <- function(x) {
  min_val <- min(x, na.rm = TRUE)
  shift <- if (min_val <= 0) abs(min_val) + 1 else 0
  log(x + shift)
}

sqrt_transform <- function(x) {
  min_val <- min(x, na.rm = TRUE)
  shift <- if (min_val <= 0) abs(min_val) + 1 else 0
  sqrt(x + shift)
}

Clim_ave<-dbReadTable(con,"Bioclims_1970_ave") %>% 
  select(!c(Pop,Aridity))  %>% 
  rename_with(.cols = ends_with("bio_1"):ends_with("bio_19"), 
              .fn = ~ str_replace_all(., "wc2.1_30s_bio_(\\d+)$", "BIO\\1")) %>% 
  mutate(across(BIO13:BIO19),sqrt_transform(.)) %>% 
  scale() %>% 
  as.data.frame()

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

PCs <- bind_cols(combined_data) %>% cbind(Pop_info) %>% 
  left_join(dbReadTable(con,"Bioclims_1970_ave") %>% 
              select(Pop,Aridity)) %>% 
  mutate(Latitude_quad=poly(Latitude,2)[,2])

