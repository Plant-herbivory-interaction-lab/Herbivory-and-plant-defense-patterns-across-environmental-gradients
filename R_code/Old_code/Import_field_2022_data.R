packages<-c(
  "DBI",# Used to connect to sqlite databases
  "tidyverse", # Great suite of functions for data wrangling and visualization
  
  "RSQLite" # Driver to help connect with database
  
)

for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages("package_name")
    library(package, character.only = TRUE)
  } else {
    library(package, character.only = TRUE)
  }}


con1 <-dbConnect(SQLite(), 'R_code/')



temp<-readxl::read_excel("~/Downloads/Horsenettle_Jun22_data_field.xlsx",sheet = "Data") %>%
  select(Latitude,Date,Plant_ID) %>% 
  mutate(
    Date= gsub("-(?!2023$)\\d{2,4}$", "-2022", Date,perl = T),
    Date=as.Date(Date, format = "%b-%d-%Y")
    ) %>% 
  right_join(temp1<-read_csv("~/Downloads/Field_2022_data.csv")) %>% 
  mutate(Pop=Pop_abr) %>% 
  select(!Pop_abr)

dbWriteTable(conn = con,'Field_2022',temp,overwrite=T)
 