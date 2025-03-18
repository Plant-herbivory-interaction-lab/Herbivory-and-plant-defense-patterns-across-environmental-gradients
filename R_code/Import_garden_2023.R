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


con<-dbConnect(SQLite(), 'Data/Data.db')



leaf_traits_garden <- dbReadTable(con, "Plant_traits_Dudu_2023") %>%
  mutate(Trichomes = rowMeans(across(starts_with("Trichomes"),
                                     as.numeric), na.rm = TRUE),
         Spines= rowMeans(across(starts_with("Spines"),
                                 as.numeric), na.rm = TRUE),
         Conc=rowMeans(across(starts_with("Glyc"),as.numeric), 
                       na.rm = TRUE),
         SLA=rowMeans(across(starts_with("L_area"),
                             as.numeric), na.rm = TRUE)/rowMeans(across(starts_with("L_weight"),as.numeric), na.rm = TRUE),
         Plant_ID=paste(Pop.ab,"-",Plant_ID,Rep,sep="")
  ) %>% 
  select(Plant_ID,Spines,Trichomes,SLA,Conc)

herb<-dbReadTable(con,"Whole_plant_traits_garden_2023_jake") %>% 
  filter(Survey == "8/4/2023" | Survey == "8/10/2023") %>% 
  select(Field.Label,Herby) %>% group_by(Field.Label) %>% 
  summarise(herb_p=mean(Herby,na.rm=T)/100) %>% rbind(
    dbReadTable(con1,"Summer_herbivory_dudu_2023" ) %>% 
      filter(Date == "8/7/2023" | Date == "8/15/2023") %>% 
      select(Field.Label,Chewing,Sucking) %>% 
      group_by(Field.Label) %>% 
      summarise(herb_p=mean(as.numeric(Chewing),na.rm=T),
                Sucking=mean(as.numeric(Sucking),na.rm=T)) %>% 
      drop_na(herb_p) %>% #mutate(
        #herb_p=rowSums(
          #across(c(Sucking, Chewing)), na.rm = TRUE)/100) %>% 
      select(Field.Label,herb_p)) %>% rename(Plant_ID="Field.Label")

Sprout_info<- dbReadTable(con, "sprout_data_garden_2024" ) %>% 
  select(Field.Label,Treatment) %>% 
  rename(Plant_ID="Field.Label") %>% 
  mutate(Pop=sub("-.*", "", Plant_ID)) %>% 
  drop_na(Treatment)

Garden<-full_join(Sprout_info,leaf_traits_garden) %>% 
  full_join(herb)  

dbWriteTable(con,"Garden_2023",Garden,overwrite=T)
