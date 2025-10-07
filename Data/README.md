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
|AP | cm | Bioclims_1970_ave.csv | Annual precipitation. |
|Buds | numeric |Field_2022_traits.csv, Field_2023_traits.csv | The number of flower buds on a plant. |
|Chew_herb | Percent | Field_2023_traits.csv | Chewing herbivory percentage on the whole plant. |
|	Clim_PC1 | NA | Bioclims_1970_ave.csv | The first axis of the principal component analysis. |
|	Clim_PC2 | NA | Bioclims_1970_ave.csv | The second axis of the principal component analysis. |
| Collect_Label | character | Field_2022_traits.csv | Root and leaf collection label. |
| Conc |mg/mg | Combined_herbivory_and_trait_data.csv | Glycoalkaloid concentration per miligram of dried leaf matter. |
|Date | Date | Combined_herbivory_and_trait_data.csv, Field_2022_traits.csv, Field_2023_traits.csv, Garden_leaf_traits.csv | The number that indicates when the data was collected.|
|Email | Character | Field_2023_pop_info.csv | Email  of the contacted person for data collection permission.|
|Epitrix_herb | percent | Field_2023_traits.csv | Percent of herbivory on the plants.|
|fl_h| numeric |Combined_herbivory_and_trait_data.csv | The number of hermaphroditic flowers on a plant. |
|Fl_herm| numeric |Field_2022_traits.csv, Field_2023_traits.csv | The number of hermaphroditic flowers on a plant. |
|fl_m| numeric |Combined_herbivory_and_trait_data.csv | The number of male flowers on a plant. |
|Fl_male| numeric |Field_2022_traits.csv, Field_2023_traits.csv | The number of male flowers on a plant. |
|Fruits | numeric |Field_2023_traits.csv | The number of fruits on a plant. |
| Group | Character | Garden_leaf_traits.csv | The treatment that the plant was placed into (cont = control, Dam = 30% damage, Und = undamaged and sprayed with insecticide). |
|Height| cm |Combined_herbivory_and_trait_data.csv, Field_2022_traits.csv, Field_2023_traits.csv | The height of a plant. |
|herb_p | proportion |Combined_herbivory_and_trait_data.csv | The proportion of herbivory estimation of the whole plant. |
|Herbivory | percent |Field_2022_traits.csv | The percent herbivory estimation of the whole plant. |
|Latitude | Arc degrees | Bioclims_1970_ave.csv, Field_2022_traits.csv, Field_2023_pop_info.csv | The latitudenal value of the plant population. |
|Leaves | numeric |Combined_herbivory_and_trait_data.csv, Field_2023_traits.csv | The number of leaves on a plant. |
| Longitude | Arc degrees | Field_2022_cords.csv, Field_2022_traits.csv, Field_2023_pop_info.csv | The longitudenal value of the plant population. | 
| Location_name | Character | Field_2023_pop_info.csv | Population location description. |
| L_area | cm<sup>2</sup> | Combined_herbivory_and_trait_data.csv, Field_2022_traits.csv | Leaf area used to calculate SLA. |
| Leaf_area | cm<sup>2</sup> | Field_2023_traits.csv | Leaf area used to calculate SLA. |
| L_weight| grams | Combined_herbivory_and_trait_data.csv, Field_2022_traits.csv | Leaf weight used to calculate SLA.  |
| Leaf_weight| grams | Field_2023_traits.csv | Leaf weight used to calculate SLA.  |
| Loc | Character | Combined_herbivory_and_trait_data.csv | Location of the data. | 
|MAT | °C | Bioclims_1970_ave.csv | Mean annual temperature. |
|Mining_herb | Percent | Field_2023_traits.csv | Mining damage percentage on the whole plant. |
|Leaves | numeric |Combined_herbivory_and_trait_data.csv | The number of leaves on a plant. |
| Phenology | Character | Field_2022_traits.csv, Field_2023_traits.csv | Plant phenological stage (v = vegatative, B = budding, Fl = flowering). | 
| Plant_cover | percent | Field_2022_traits.csv | Percent of focal plant coverage. | 
| Pop | Character | Bioclims_1970_ave.csv, Combined_herbivory_and_trait_data.csv, Field_2022_cords.csv, Field_2023_pop_info.csv, Field_2023_traits.csv, Garden_leaf_traits.csv | Unique name for a population. | 
| Pop_area | m<sup>2</sup>| Field_2022_traits.csv, Field_2023_pop_info.csv | The estimated area that the poulation covered. | 
| Pop_ID | Character | Field_2022_cords.csv, Garden_leaf_traits.csv | Unique name for a population. | 
| Population | Character | Field_2022_cords.csv| Unique name for a population. | 
|Plant_ID | Character  | Combined_herbivory_and_trait_data.csv, Field_2022_traits.csv, Field_2023_traits.csv | A value that indicates a unique ID of the plants that were sampled. |
|PWQ | cm | Bioclims_1970_ave.csv | Precipitation of the wettest quarter. |
|Region | Character | Field_2022_cords.csv, Field_2022_traits.csv | Region of where the poulation was collected. |
| Rep | Character | Garden_leaf_traits.csv | Root replication of a specific plant. |
|Roots | Character |  Field_2022_traits.csv | Yes or no whether roots weere collected. |
|Spines | numeric |  Field_2022_traits.csv  | Number of leaves on the leaf mid rib. |
|SLA | cm<sup>2</sup>/g |  Combined_herbivory_and_trait_data.csv | The specific leaf area of the collected leaf. |
|Stem_dens| n per m<sup>2</sup> | Field_2023_traits.csv | Number of stems in 1 square meter surrounding the focal plant. |
| Time | Character | Combined_herbivory_and_trait_data.csv | Time of when the data was collected. The variable can be year or early, middle, and late of the common garden data. | 
| Trichomes | n per 0.24 cm<sup>2</sup>  | Combined_herbivory_and_trait_data.csv, Field_2022_traits.csv | Trichomes counted in the given area. | 
|Tsd | °C | Bioclims_1970_ave.csv | Standard deviation of yearly temperature. |
| Year | Character | Combined_herbivory_and_trait_data.csv | Year of when the data was collected. | 






|Plant_ID | Numeric  | | Indicates the plant number in a specific population |
|Herbivory | Percent | |  The percent herbivory estimation of the whole plant |
|Trichomes | Numeric |  | The number of trichomes in a 500 mm squared area |       
|SLA | cm<sup>2</sup>/g |  | The specific leaf area of the collected leaf |
|Spines | Numeric |  | Number of spines on the mid-rib of the collected leaf |      
|Conc | mg/mg |  | Glycoalkaloid concentration per miligram of dried leaf matter |
|Pop | Categorical |  | Population acronym of the collected populations |

## Field_2023
This table includes the herbivory and plant defense traits measurements collected during the 2023 field season.
| variables | units | description |
| :--- | :---: | :--- |
|Date | Numeric | The number that indicates when the data was collected. This number indicates a date orgin of 1970-01-01|
|Plant_ID | Character | Indicates the plant ID in a specific population |
|herb_p | proportion | The percent herbivory estimation of the whole plant |
|Trichomes | Numeric | The number of trichomes in a 500 mm squared area |       
|SLA | cm<sup>2</sup>/g | The specific leaf area of the collected leaf |
|Spines | Numeric | Number of spines on the mid-rib of the collected leaf |      
|Conc | mg/mg | Glycoalkaloid concentration per miligram of dried leaf matter |
|Pop | Categorical | Population acronym of the collected populations |

## Garden_2023
This table includes the herbivory and plant defense traits measurements collected of the plants growing in the common garden during 2023.
| variables | units | description |
| :--- | :---: | :--- |
|Plant_ID | Character | Indicates the plant ID in a specific population |
|herb_p | proportion | The percent herbivory estimation of the whole plant |
|Trichomes | Numeric | The number of trichomes in a 500 mm squared area |       
|SLA | cm<sup>2</sup>/g | The specific leaf area of the collected leaf |
|Spines | Numeric | Number of spines on the mid-rib of the collected leaf |      
|Conc | mg/mg | Glycoalkaloid concentration per miligram of dried leaf matter |
|Pop | Categorical | Population acronym of the collected populations |

## Pop_info_2022_and_2023
This table includes the population information of plants collected in the field and planted in the common garden.
| variables | units | description |
| :--- | :---: | :--- |
|Pop | Categorical | Population acronym of the collected populations |
|Latitude | Arc degrees | The latitudenal value of the plant population |
|Longitude | Arc degrees | The longitudinal value of the plant population |
|Pop | Categorical | Population acronym of the collected populations |
|Place | Character | Description of where the plant was collected |
|Notes | Character | Population notes of the plant location |


