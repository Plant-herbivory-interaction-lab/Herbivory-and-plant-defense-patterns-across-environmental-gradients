## Code by: Jacob Herschberger
## Date: September 2025
## Email: j.herschberger@ufl.edu
## Project: Culprits of plant defense variation across a latitudinal gradient.

# Import packages ----
# Run install.packages([Package name]) if package is not installed
library(tidyverse)
library(DBI)
library(RSQLite)

# Climate data ----
con <-dbConnect(SQLite(), 'Data/Data.db') # Connect to the database

Coords<-dbReadTable(con,"Field_2023_pop_info") %>% 
  select(Pop,Latitude,Longitude) %>% 
  rbind(.,dbReadTable(con,"Horsenettle_Jun22_data_field_cords") %>% 
          select(Latitude:Pop))
