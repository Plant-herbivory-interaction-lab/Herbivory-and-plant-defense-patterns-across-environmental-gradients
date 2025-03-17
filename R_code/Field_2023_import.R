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


con1 <-dbConnect(SQLite(), '../Data/Data_all_combined.db')
con <-dbConnect(SQLite(), 'R_code/Data_all_combined.db')

Plant_traits_field_2023<-dbReadTable(con1,
                                     'Horsenettle_field_plant_trait_data_2023')%>% 
  mutate(f_ratio=Fl_male/(Fl_herm+Fl_male),
         herb=(Epitrix_herb+Chew_herb),
         herb_p=(Epitrix_herb+Chew_herb)/100,
         Stem_dens=Stem_dens/4,
         Herbivores=Epitrix+Spider_Mite+Leptinotarsa+Torti_Beetle+Black_weevils+Spodoptera+Leafhopper+Mealybug,
         SLA=Leaf_Area/Leaf_Weight,
         Date=as.Date(Date, format = "%m/%d/%Y"))%>% 
  select(c(Date,Pop,Plant_ID,Trichomes,Spines,herb_p,SLA))


Floral_traits<-dbReadTable(con1,
                           "Horsenettle_field_floral_trait_data_2023") %>% 
  mutate(F_col_num=case_when(F_Color=="w" ~ 1,
                             F_Color=="lp" ~ 2,
                             F_Color=="p" ~ 3)) %>% 
  group_by(Pop,Plant_ID)  %>% 
  summarise(Stamen=mean(Stamen,na.rm=T),
            Corolla_D=mean(Corolla_D),
            Col=mean(F_col_num)) %>% 
  select(!Stamen)

POl_data<-dbReadTable(con1,
                      "Horsenettle_field_pollinator_data_2023")%>% 
  group_by(Pop,Plant_ID) %>% summarise(Avoid=sum(Avoid,na.rm=T),Vis=sum(Vis,na.rm=T)) %>% 
  mutate(Vis_p=(Vis)/(Vis + Avoid)) %>% # replace infinite values with NA %>% 
  select(Pop,Plant_ID,Vis_p,Vis,Avoid)

glycs<-dbReadTable(con1,
                   "Field_2023_glycs") %>% 
  separate(col=Plant_ID,c("Pop","Plant_ID")) %>% 
  mutate(Plant_ID=as.integer(Plant_ID))


Field_2023<-Plant_traits_field_2023 %>% 
  left_join(Floral_traits) %>% 
  left_join(POl_data) %>% 
  left_join(glycs)

dbWriteTable(conn = con,'Field_2023',Field_2023,overwrite=T)

