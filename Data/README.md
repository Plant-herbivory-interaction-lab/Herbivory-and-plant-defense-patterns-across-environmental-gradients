# Table names and descriptions.

| Table name | description |
| :--- | :--- |
| Bioclims_1970_ave.csv | Climate variable used in the manuscript. |
| Combined_herbivory_and_trait_data.csv |  The final cleaned data used in the analysis.  |
| Field_2022_cords.csv |  Coordinates of the field sites surveyed in 2022. |
| Field_2022_traits.csv |  Field data collected in 2022. |
| Field_2023_pop_info.csv |  Information of the populations surveyed in 2023.   |
| Field_2023_traits.csv |  Field data collected in 2023. |
| Garden_leaf_traits.csv |  Leaf trait data colected from the common garden in 2023.   |
| Garden_survey.csv |  Plant phenology and herbivory from plants in the common garden in 2023.  |
| Glycoalk_Spring_2023_abs.csv |  Absorbance of the glycoalkaloid quantification procedure in spring 2023.  |
| Glycoalk_Spring_2023_standard_curve.csv |  The standard curve data used to quantify gkycoalkaloids in spring 2023.  |
| Glycoalk_Spring_2023_weight.csv |  Weight of the leaf material used during glycoalkaloid quantification  in spring 2023. |                          |
| Glycoalk_fall_2023_Samples.csv |  Absorbance of the glycoalkaloid quantification procedure in fall 2023.                         |
| Glycoalk_fall_2023_Standard_curve.csv |  The standard curve data used to quantify gkycoalkaloids in fall 2023.  |
| Glycoalk_fall_2023_Weight_data.csv |   Weight of the leaf material used during glycoalkaloid quantification  in fall 2023.           |
| Glycoalk_fall_2023_blank_plates.csv |  Blank plates used to standardize the absorbance in samples.  |
| Solanum_carolinense_inat.csv | Coordinates of **Solanum carolinense** inaturalist observations. |




## Column names, units, table locations, and descriptions.

| column name | units | table location(s) | description |
| :--- | :---: | :--- | :--- |
| Abs | absorbance | Glycoalk_Spring_2023_abs.csv, Glycoalk_Spring_2023_standard_curve.csv, Glycoalk_fall_2023_Samples.csv, Glycoalk_fall_2023_Standard_curve.csv, Glycoalk_fall_2023_blank_plates.csv | Absorbance reading from the microplate reader |
|AP | cm | Bioclims_1970_ave.csv | Annual precipitation. |
|Buds | numeric |Field_2022_traits.csv, Field_2023_traits.csv, Garden_survey.csv | The number of flower buds on a plant. |
|Chew_herb | Percent | Field_2023_traits.csv | Chewing herbivory percentage on the whole plant. |
|Chewing | Percent | Garden_survey.csv | Chewing herbivory percentage on the whole plant. |
|	Clim_PC1 | NA | Bioclims_1970_ave.csv | The first axis of the principal component analysis. |
|	Clim_PC2 | NA | Bioclims_1970_ave.csv | The second axis of the principal component analysis. |
| Collect_Label | character | Field_2022_traits.csv, Glycoalk_Spring_2023_weight.csv | Root and leaf collection label. |
| Conc |mg/mg | Combined_herbivory_and_trait_data.csv | Glycoalkaloid concentration per miligram of dried leaf matter. |
|Date | Date | Combined_herbivory_and_trait_data.csv, Field_2022_traits.csv, Field_2023_traits.csv, Garden_leaf_traits.csv, Garden_survey.csv | The number that indicates when the data was collected.|
| Conc (mg/mL) | Numeric | Glycoalk_Spring_2023_standard_curve.csv, Glycoalk_fall_2023_Standard_curve.csv | Concentration of the standard |
|Email | Character | Field_2023_pop_info.csv | Email  of the contacted person for data collection permission.|
|Epitrix_herb | percent | Field_2023_traits.csv | Percent of herbivory on the plants.|
| Experiment | Character | Garden_plant_info.csv, Glycoalk_fall_2023_Weight_data.csv | Indicates which experiment the plant was allocated to. |
|fl_h| numeric |Combined_herbivory_and_trait_data.csv, Garden_survey.csv | The number of hermaphroditic flowers on a plant. |
|Fl_herm| numeric |Field_2022_traits.csv, Field_2023_traits.csv | The number of hermaphroditic flowers on a plant. |
|fl_m| numeric |Combined_herbivory_and_trait_data.csv, Garden_survey.csv | The number of male flowers on a plant. |
|Fl_male| numeric |Field_2022_traits.csv, Field_2023_traits.csv | The number of male flowers on a plant. |
|Fruit(s) | numeric |Field_2023_traits.csv, Garden_survey.csv | The number of fruits on a plant. |
|Height| cm |Combined_herbivory_and_trait_data.csv, Field_2022_traits.csv, Field_2023_traits.csv, Garden_survey.csv | The height of a plant. |
|herb_p | proportion |Combined_herbivory_and_trait_data.csv | The proportion of herbivory estimation of the whole plant. |
| herb.sp | character | Garden_survey.csv | Species of herbivores seen. Numbers at the end indicate the nth species found on the plant during the plant inspection |
| herb.n | numeric | Garden_survey.csv | Abundance of the given herbivore spcecies. Numbers at the end indicate the nth species found on the plant during the plant inspection |
|Herbivory | percent |Field_2022_traits.csv | The percent herbivory estimation of the whole plant. |
|Latitude | Arc degrees | Bioclims_1970_ave.csv, Field_2022_traits.csv, Field_2023_pop_info.csv, Solanum_carolinense_inat.csv | The latitudenal value of the plant population. |
|Leaves | numeric |Combined_herbivory_and_trait_data.csv, Field_2023_traits.csv | The number of leaves on a plant. |
| Longitude | Arc degrees | Field_2022_cords.csv, Field_2022_traits.csv, Field_2023_pop_info.csv, Solanum_carolinense_inat.csv | The longitudenal value of the plant population. | 
| Location_name | Character | Field_2023_pop_info.csv | Population location description. |
| L_area | cm<sup>2</sup> | Combined_herbivory_and_trait_data.csv, Field_2022_traits.csv, Garden_leaf_traits.csv | Leaf area used to calculate SLA. Some columns also contain variations of these names where the ending could be _b1, _b1, t1, or t2. These variation indicate the location of the leaf on the plant (t = top, b = bottom), and the numbers indicate the replication at the top and bottom. |
| Leaf_area | cm<sup>2</sup> | Field_2023_traits.csv | Leaf area used to calculate SLA. |
| Leaf_location | Character | Glycoalk_fall_2023_Weight_data.csv | Leaf location on the plant (B = bottom, T = Top) |
| L_weight| grams | Combined_herbivory_and_trait_data.csv, Field_2022_traits.csv, Garden_leaf_traits.csv | Leaf weight used to calculate SLA.  Some columns also contain variations of these names where the ending could be _b1, _b1, t1, or t2. These variation indicate the location of the leaf on the plant (t = top, b = bottom), and the numbers indicate the replication at the top and bottom.|
| Leaf_weight| grams | Field_2023_traits.csv | Leaf weight used to calculate SLA.  |
| Leaf_weight | grams | Glycoalk_fall_2023_Weight_data.csv | Leaf weight used when quantifying glycoalkaloids |
| Loc | Character | Combined_herbivory_and_trait_data.csv | Location of the data. | 
|MAT | °C | Bioclims_1970_ave.csv | Mean annual temperature. |
|Mining_herb | Percent | Field_2023_traits.csv | Mining damage percentage on the whole plant. |
|Leaves | numeric |Combined_herbivory_and_trait_data.csv | The number of leaves on a plant. |
| phen | Character | Garden_survey.csv | Plant phenological stage (v = vegatative, B = budding, Fl = flowering). | 
| Phenology | Character | Field_2022_traits.csv, Field_2023_traits.csv | Plant phenological stage (v = vegatative, B = budding, Fl = flowering). | 
| Plant_cover | percent | Field_2022_traits.csv | Percent of focal plant coverage. | 
| Pop | Character | Bioclims_1970_ave.csv, Combined_herbivory_and_trait_data.csv, Field_2022_cords.csv, Field_2023_pop_info.csv, Field_2023_traits.csv, Garden_leaf_traits.csv, Garden_plant_info.csv | Unique name for a population. | 
| Pop_area | m<sup>2</sup>| Field_2022_traits.csv, Field_2023_pop_info.csv | The estimated area that the poulation covered. | 
| Pop_ID | Character | Field_2022_cords.csv, Garden_leaf_traits.csv | Unique name for a population. | 
| Population | Character | Field_2022_cords.csv| Unique name for a population. | 
|Plant_ID | Character  | Combined_herbivory_and_trait_data.csv, Field_2022_traits.csv, Field_2023_traits.csv, Garden_plant_info.csv, Garden_survey.csv, Glycoalk_fall_2023_Weight_data.csv | A value that indicates a unique ID of the plants that were sampled. |
| Plate | Character | Glycoalk_fall_2023_Samples.csv, Glycoalk_fall_2023_Standard_curve.csv, Glycoalk_fall_2023_blank_plates.csv | Specific plate that was used during glycoalkaloid quantification |
|PWQ | cm | Bioclims_1970_ave.csv | Precipitation of the wettest quarter. |
|Region | Character | Field_2022_cords.csv, Field_2022_traits.csv | Region of where the poulation was collected. |
| Rep | numeric | Glycoalk_Spring_2023_abs.csv, Glycoalk_fall_2023_Samples.csv | Replication of the glycoalkaloid quantification. |
| Rep | Character | Garden_leaf_traits.csv | Root replication of a specific plant. |
|Roots | Character |  Field_2022_traits.csv | Yes or no whether roots weere collected. |
| Sample | Character | Glycoalk_Spring_2023_standard_curve.csv, Glycoalk_fall_2023_Samples.csv, Glycoalk_fall_2023_Weight_data.csv | Sample name during glycoalkaloid quantification. |
| Sample_Label | Numeric | Glycoalk_Spring_2023_abs.csv, Glycoalk_Spring_2023_weight.csv, Glycoalk_fall_2023_blank_plates.csv | Sample identification used during glycoalkaloid quantification |
|Spines | numeric |  Field_2022_traits.csv, Garden_leaf_traits.csv | Number of leaves on the leaf mid rib. Some columns also contain variations of these names where the ending could be _b1, _b1, t1, or t2. These variation indicate the location of the leaf on the plant (t = top, b = bottom), and the numbers indicate the replication at the top and bottom.|
| Sprout | Date | Garden_plant_info.csv | Date when the plants sprouted |
|SLA | cm<sup>2</sup>/g |  Combined_herbivory_and_trait_data.csv | The specific leaf area of the collected leaf. |
| Survey | numeric | Garden_survey.csv | The survey number. |
|Stem_dens| n per m<sup>2</sup> | Field_2023_traits.csv | Number of stems in 1 square meter surrounding the focal plant. |
| stems | numeric | Garden_survey.csv | Number of stems that the potted plant produced. |
| Sucking | percentage | Garden_survey.csv | Percent sucking herbivory |
| Time | Character | Combined_herbivory_and_trait_data.csv | Time of when the data was collected. The variable can be year or early, middle, and late of the common garden data. | 
| Treatment | Character | Garden_plant_info.csv | The treatment that the plant was placed into (cont = control, Dam = 30% damage, Und = undamaged and sprayed with insecticide).|
| Trichomes | n per 0.24 cm<sup>2</sup>  | Combined_herbivory_and_trait_data.csv, Field_2022_traits.csv, Garden_leaf_traits.csv  | Trichomes counted in the given area. Some columns also contain variations of these names where the ending could be _b1, _b1, t1, or t2. These variation indicate the location of the leaf on the plant (t = top, b = bottom), and the numbers indicate the replication at the top and bottom. | 
|Tsd | °C | Bioclims_1970_ave.csv | Standard deviation of yearly temperature. |
| Weight | grams | Garden_plant_info.csv, Glycoalk_Spring_2023_weight.csv | Weight of the root used to propogate the common garden plants. This column name was also used during glycoalkaloid analysis and indicates the weight of leaf material used for glycoalkaloid quantification| 
| Well | Character | Glycoalk_Spring_2023_standard_curve.csv, Glycoalk_fall_2023_Samples.csv, Glycoalk_fall_2023_Standard_curve.csv, Glycoalk_fall_2023_blank_plates.csv | The well location of the sample on the microplate reader. |
| Year | Character | Combined_herbivory_and_trait_data.csv | Year of when the data was collected. | 
