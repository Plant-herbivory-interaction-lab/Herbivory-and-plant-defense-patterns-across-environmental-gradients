# Table names and descriptions

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




## Field_2022
This table includes the herbivory and plant defense traits measurements collected during the 2022 field season.

| variables | units | description |
| :--- | :---: | :--- |
|Date | Numeric | The number that indicates when the data was collected. This number indicates a date orgin of 1970-01-01|
|Plant_ID | Numeric | Indicates the plant number in a specific population |
|Herbivory | Percent | The percent herbivory estimation of the whole plant |
|Trichomes | Numeric | The number of trichomes in a 500 mm squared area |       
|SLA | cm<sup>2</sup>/g | The specific leaf area of the collected leaf |
|Spines | Numeric | Number of spines on the mid-rib of the collected leaf |      
|Conc | mg/mg | Glycoalkaloid concentration per miligram of dried leaf matter |
|Pop | Categorical | Population acronym of the collected populations |

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

# Soil
This table includes the soil attributes at the collected poulations using the geodata::soil_world function.
| variables | units | description |
| :--- | :---: | :--- |
|Pop | Categorical | Population acronym of the collected populations |
| bdod      | kg dm<sup>-3</sup> | Bulk density of the fine earth fraction     |
| cec       | cmol(+) kg<sup>-1</sup> | Cation Exchange Capacity of the soil        |
| cfvo      | %                 | Vol. fraction of coarse fragments (> 2 mm)  |
| nitrogen  | g kg<sup>-1</sup>  | Total nitrogen (N)                          |
| phh2o     | -                 | pH (H<sub>2</sub>O)                         |
| sand      | %                 | Sand (> 0.05 mm) in fine earth              |
| silt      | %                 | Silt (0.002-0.05 mm) in fine earth         |
| clay      | %                 | Clay (< 0.002 mm) in fine earth            |
| soc       | g kg<sup>-1</sup>  | Soil organic carbon in fine earth          |
| ocd       | kg m<sup>-3</sup>  | Organic carbon density                     |
| ocs       | kg m<sup>-2</sup>  | Organic carbon stocks                      |



