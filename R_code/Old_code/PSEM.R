library(DBI)



con <-DBI::dbConnect(RSQLite::SQLite(), '../../Data/Data_all_combined.db')

print(dbGetQuery(con,"PRAGMA database_list;"))


### @knitr field data
#Field_plant_traits<-DBI::dbReadTable(con,
 #                      'Horsenettle_field_plant_trait_data_2023') 
