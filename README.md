# Variation of multiple plant defenses and herbivory reveal different patterns of adaptation across a productivity gradient

Dataset DOI: [10.5061/dryad.pc866t23k](https://doi.org/10.5061/dryad.pc866t23k)

## R code files and descriptions.

| R code file name | Description |
|:---|:---|
| Data_analysis_and_visualization_May_2026.R | Code used to visualize and analyze the data. |
| Data_wrangling.R | Code used to clean the data. |
| Functions.R | Functions used in cleaning, analyzing, or visualizing data. |

## Table names and descriptions.

| Table name | Description |
|:---|:---|
| clim_data.csv | Climate variables used in the manuscript. |
| Combined_data.csv | Combined leaf trait data from the common garden and the field data. |
| Combined_herbivores_data.csv | Herbivores observed in the common garden and the field plants. |
| Combined_herbivory_and_trait_data.csv | The final cleaned data used in the analysis. |
| Field_2022_cords.csv | Coordinates of the 2022 field populations. |
| Field_2022_traits.csv | Field data collected in 2022. |
| Field_2023_pop_info.csv | Information of the populations surveyed in 2023. |
| Field_2023_traits.csv | Field data collected in 2023. |
| Garden_leaf_traits.csv | Leaf trait data collected from the common garden in 2023. |
| Garden_plant_info.csv | Root weight used and sprouting info of the common garden plants. |
| Garden_survey.csv | Plant phenology and herbivory from plants in the common garden in 2023. |
| Glycoalk_fall_2023_blank_plates.csv | Blank plates used to standardize the absorbance in samples. |
| Glycoalk_fall_2023_Samples.csv | Absorbance of the glycoalkaloid quantification procedure in fall 2023. |
| Glycoalk_fall_2023_Standard_curve.csv | The standard curve data used to quantify gkycoalkaloids in fall 2023. |
| Glycoalk_fall_2023_Weight_data.csv | Weight of the leaf material used during glycoalkaloid quantification in fall 2023. |
| Glycoalk_Spring_2023_abs.csv | Absorbance of the glycoalkaloid quantification procedure in spring 2023. |
| Glycoalk_Spring_2023_standard_curve.csv | The standard curve data used to quantify gkycoalkaloids in spring 2023. |
| Glycoalk_Spring_2023_weight.csv | Weight of the leaf material used during glycoalkaloid quantification in spring 2023. |
| Solanum_carolinense_inat.csv | Coordinates of *Solanum carolinense* inaturalist observations. |

## Column names, units, table locations, and descriptions.

| Column name | Units | File(s) | Description |
|----|----|----|----|
| Abs | absorbance | Glycoalk_fall_2023_blank_plates.csv, Glycoalk_fall_2023_Samples.csv, Glycoalk_fall_2023_Standard_curve.csv, Glycoalk_Spring_2023_abs.csv, Glycoalk_Spring_2023_standard_curve.csv | Absorbance reading from the microplate reader. |
| Amount | numeric | Combined_herbivores_data.csv | Number of specific herbivores. |
| AP | cm | clim_data.csv | Annual precipitation. |
| Black_weevils | numeric | Field_2023_traits.csv | Number of black weevils on the plants. |
| Chew_herb | percent | Field_2023_traits.csv | Chewing herbivory percentage on the whole plant. |
| Chewing | percent | Garden_survey.csv | Chewing herbivory percentage on the whole plant. |
| Clim_PC1 | numeric | clim_data.csv | The first axis of the principal component analysis. |
| Clim_PC2 | numeric | clim_data.csv | The second axis of the principal component analysis. |
| Collect_Label | character | Field_2022_traits.csv, Glycoalk_Spring_2023_weight.csv | Root and leaf collection label. |
| Conc | mg/mg | Combined_data.csv, Combined_herbivory_and_trait_data.csv | Glycoalkaloid concentration per milligram of dried leaf matter. |
| Conc..mg.mL | mg/mL | Glycoalk_Spring_2023_standard_curve.csv | Concentration of the standard used in glycoalkaloid analysis. |
| Conc..mg.mL. | mg/mL | Glycoalk_fall_2023_Standard_curve.csv | Concentration of the standard used in glycoalkaloid analysis. |
| Date | date | Combined_data.csv, Combined_herbivores_data.csv, Combined_herbivory_and_trait_data.csv, Field_2022_traits.csv, Field_2023_traits.csv, Garden_leaf_traits.csv, Garden_survey.csv | The date that indicates when the data was collected. |
| Email | character | Field_2023_pop_info.csv | Email of the contacted person for data collection permission. |
| Epitrix | numeric | Field_2023_traits.csv | Number of *Epitrix fuscula* individuals on plants. |
| Epitrix_fuscula | numeric | Field_2022_traits.csv | Number of *Epitrix fuscula* individuals on plants. |
| Epitrix_herb | percent | Field_2023_traits.csv | Percent of *Epitrix fuscula* herbivory on the plants. |
| Experiement | numeric | Glycoalk_fall_2023_Weight_data.csv | Indicates which experiment the plant was allocated to *(Note: typo in raw column header)*. |
| Experiment | character | Garden_plant_info.csv | Indicates which experiment the plant was allocated to. |
| fl_h | numeric | Combined_herbivory_and_trait_data.csv, Garden_survey.csv | The number of hermaphroditic flowers on a plant. |
| fl_m | numeric | Combined_herbivory_and_trait_data.csv, Garden_survey.csv | The number of male flowers on a plant. |
| Height | cm | Combined_data.csv, Combined_herbivory_and_trait_data.csv, Field_2022_traits.csv, Field_2023_traits.csv, Garden_survey.csv | The height of a plant. |
| herb.n1 | numeric | Garden_survey.csv | Abundance of the first herbivore species found on the plant during inspection. |
| herb.n2 | numeric | Garden_survey.csv | Abundance of the second herbivore species found on the plant during inspection. |
| herb.n3 | numeric | Garden_survey.csv | Abundance of the third herbivore species found on the plant during inspection. |
| herb.sp1 | character | Garden_survey.csv | Name of the first herbivore species found on the plant during inspection. |
| herb.sp2 | character | Garden_survey.csv | Name of the second herbivore species found on the plant during inspection. |
| herb.sp3 | character | Garden_survey.csv | Name of the third herbivore species found on the plant during inspection. |
| herb_p | proportion | Combined_data.csv, Combined_herbivory_and_trait_data.csv | The proportion of herbivory estimation of the whole plant. |
| Herbivory | percent | Field_2022_traits.csv | The percent herbivory estimation of the whole plant. |
| L_area | cm² | Combined_data.csv, Combined_herbivory_and_trait_data.csv, Field_2022_traits.csv | Leaf area used to calculate SLA. |
| L_area_b1 | cm² | Garden_leaf_traits.csv | Leaf area used to calculate SLA for bottom leaf, replication 1. |
| L_area_b2 | cm² | Garden_leaf_traits.csv | Leaf area used to calculate SLA for bottom leaf, replication 2. |
| L_area_t1 | cm² | Garden_leaf_traits.csv | Leaf area used to calculate SLA for top leaf, replication 1. |
| L_area_t2 | cm² | Garden_leaf_traits.csv | Leaf area used to calculate SLA for top leaf, replication 2. |
| L_weight | grams | Combined_data.csv, Combined_herbivory_and_trait_data.csv, Field_2022_traits.csv | Leaf weight used to calculate SLA. |
| L_weight_b1 | grams | Garden_leaf_traits.csv | Leaf weight used to calculate SLA for bottom leaf, replication 1. |
| L_weight_b2 | grams | Garden_leaf_traits.csv | Leaf weight used to calculate SLA for bottom leaf, replication 2. |
| L_weight_t1 | grams | Garden_leaf_traits.csv | Leaf weight used to calculate SLA for top leaf, replication 1. |
| L_weight_t2 | grams | Garden_leaf_traits.csv | Leaf weight used to calculate SLA for top leaf, replication 2. |
| latitude | arc degrees | Solanum_carolinense_inat.csv | The latitudinal value of the plant population. |
| Latitude | arc degrees | clim_data.csv, Field_2022_cords.csv, Field_2022_traits.csv, Field_2023_pop_info.csv | The latitudinal value of the plant population. |
| Leaf_Area | cm² | Field_2023_traits.csv | Leaf area used to calculate SLA. |
| Leaf_location | character | Glycoalk_fall_2023_Weight_data.csv | Leaf location on the plant (B = bottom, T = Top). |
| Leaf_rep | numeric | Glycoalk_fall_2023_Weight_data.csv | Leaf replication number. |
| Leaf_weight | grams | Glycoalk_fall_2023_Weight_data.csv | Leaf weight used to calculate SLA / quantify glycoalkaloids. |
| Leaf_Weight | grams | Field_2023_traits.csv | Leaf weight used to calculate SLA. |
| Leafhopper | numeric | Field_2023_traits.csv | Number of leafhoppers on the plants. |
| leaves | numeric | Garden_survey.csv | The number of leaves on a plant. |
| Leaves | numeric | Combined_data.csv, Combined_herbivory_and_trait_data.csv, Field_2023_traits.csv | The number of leaves on a plant. |
| Leptinotarsa | numeric | Field_2023_traits.csv | Number of *Leptinotarsa juncta* on the plants. |
| Leptinotarsa_juncta | numeric | Field_2022_traits.csv | Number of *Leptinotarsa juncta* on the plants. |
| Loc | character | Combined_data.csv, Combined_herbivores_data.csv, Combined_herbivory_and_trait_data.csv | Location of the data. |
| Location.name | character | Field_2023_pop_info.csv | Population location description. |
| longitude | arc degrees | Solanum_carolinense_inat.csv | The longitudinal value of the plant population. |
| Longitude | arc degrees | clim_data.csv, Field_2022_cords.csv, Field_2022_traits.csv, Field_2023_pop_info.csv | The longitudinal value of the plant population. |
| MAT | °C | clim_data.csv | Mean annual temperature. |
| Mealybug | numeric | Field_2023_traits.csv | Number of mealybugs on the plants. |
| Mining_herb | percent | Field_2023_traits.csv | Mining damage percentage on the whole plant. |
| Notes | character | Glycoalk_fall_2023_Weight_data.csv, Glycoalk_Spring_2023_abs.csv, Glycoalk_Spring_2023_weight.csv | Notes taken during data collection. |
| NPP | kg C/m² | clim_data.csv | Average NPP extracted from all the years on GoogleEarth Engine API. |
| NPP_g_10y | kg C/m² | clim_data.csv | Average NPP extracted from years 2013 to 2023 on GoogleEarth Engine API. |
| NPPg | kg C/m² | clim_data.csv | Total NPP from May to August of the year when the field plant data was collected. |
| Place | character | Field_2022_cords.csv | Description of the field location. |
| Plant_abr | character | Field_2023_traits.csv | Population abbreviation of where the plant data was collected. |
| Plant_cover | percent | Field_2022_traits.csv | Percent of focal plant coverage. |
| Plant_ID | character | Combined_data.csv, Combined_herbivores_data.csv, Combined_herbivory_and_trait_data.csv, Field_2022_traits.csv, Field_2023_traits.csv, Garden_leaf_traits.csv, Garden_plant_info.csv, Garden_survey.csv, Glycoalk_fall_2023_Weight_data.csv | A value that indicates a unique ID of the plants that were sampled. |
| Plate | numeric | Glycoalk_fall_2023_blank_plates.csv, Glycoalk_fall_2023_Samples.csv, Glycoalk_fall_2023_Standard_curve.csv | Specific plate that was used during glycoalkaloid quantification. |
| Pop | character | clim_data.csv, Combined_data.csv, Combined_herbivores_data.csv, Combined_herbivory_and_trait_data.csv, Field_2022_cords.csv, Field_2022_traits.csv, Field_2023_pop_info.csv, Field_2023_traits.csv, Garden_leaf_traits.csv, Garden_plant_info.csv | Unique name for a population. |
| Pop_area | m² | Field_2022_traits.csv, Field_2023_pop_info.csv | The estimated area that the population covered. |
| Pop_ID | character | Field_2022_cords.csv | Unique name for a population. |
| Population | character | Field_2022_cords.csv | Unique name for a population. |
| PWQ | cm | clim_data.csv | Precipitation of the wettest quarter. |
| Region | character | Field_2022_cords.csv, Field_2022_traits.csv | Region of where the population was collected. |
| rep | numeric | Glycoalk_Spring_2023_abs.csv | Root replication of a specific plant. |
| Rep | numeric | Garden_leaf_traits.csv, Glycoalk_fall_2023_Samples.csv | Root replication of a specific plant / Replication of the glycoalkaloid quantification. |
| Sample | character | Glycoalk_fall_2023_blank_plates.csv, Glycoalk_fall_2023_Samples.csv, Glycoalk_fall_2023_Weight_data.csv, Glycoalk_Spring_2023_standard_curve.csv | Sample name during glycoalkaloid quantification. |
| Sample_Label | numeric | Glycoalk_Spring_2023_abs.csv, Glycoalk_Spring_2023_weight.csv | Sample identification used during glycoalkaloid quantification. |
| scientific_name | character | Solanum_carolinense_inat.csv | Scientific name of the observation species. |
| SLA | cm²/g | Combined_data.csv, Combined_herbivory_and_trait_data.csv | The specific leaf area of the collected leaf. |
| Species | character | Combined_herbivores_data.csv | Herbivore species observed. |
| Spider_Mite | numeric | Field_2023_traits.csv | Number of spider mites on the plants. |
| Spines | numeric | Field_2022_traits.csv, Field_2023_traits.csv | Number of spines on the leaf midrib. |
| Spines_b1 | numeric | Garden_leaf_traits.csv | Number of spines on the leaf midrib for bottom leaf, replication 1. |
| Spines_b2 | numeric | Garden_leaf_traits.csv | Number of spines on the leaf midrib for bottom leaf, replication 2. |
| Spines_t1 | numeric | Garden_leaf_traits.csv | Number of spines on the leaf midrib for top leaf, replication 1. |
| Spines_t2 | numeric | Garden_leaf_traits.csv | Number of spines on the leaf midrib for top leaf, replication 2. |
| Spodoptera | numeric | Field_2023_traits.csv | Number of *Spodoptera* individuals on the plants. |
| Sprout | date | Garden_plant_info.csv | Date when the plants sprouted in the common garden. |
| Stem_dens | n per m² | Field_2023_traits.csv | Number of stems in 1 square meter surrounding the focal plant. |
| Sucking | percentage | Garden_survey.csv | Percent sucking herbivory. |
| Time | character | Combined_data.csv, Combined_herbivory_and_trait_data.csv | Time of when the data was collected (e.g., year, or early/middle/late). |
| Torti_Beetle | numeric | Field_2023_traits.csv | Number of *Gratiana pallidula* (tortoise beetles) on the plants. |
| Treatment | character | Garden_plant_info.csv | The treatment that the plant was placed into (cont = control, Dam = 30% damage, Und = undamaged and sprayed with insecticide). |
| Trichomes | n per 0.24 cm² | Combined_data.csv, Combined_herbivory_and_trait_data.csv, Field_2022_traits.csv, Field_2023_traits.csv | Trichomes counted in the given area. |
| Trichomes_b1 | numeric | Garden_leaf_traits.csv | Trichomes counted in the given area for bottom leaf, replication 1. |
| Trichomes_b2 | numeric | Garden_leaf_traits.csv | Trichomes counted in the given area for bottom leaf, replication 2. |
| Trichomes_t1 | numeric | Garden_leaf_traits.csv | Trichomes counted in the given area for top leaf, replication 1. |
| Trichomes_t2 | numeric | Garden_leaf_traits.csv | Trichomes counted in the given area for top leaf, replication 2. |
| Tsd | °C | clim_data.csv | Standard deviation of yearly temperature. |
| Weight | grams | Garden_plant_info.csv, Glycoalk_Spring_2023_weight.csv | Weight of the root used to propagate the common garden plants, or weight of leaf material used for glycoalkaloid quantification. |
| WeightedCD | numeric | clim_data.csv | Calculated climate distance based on the climate MAP, AP, PWQ, and Tsd. |
| Well | character | Glycoalk_fall_2023_blank_plates.csv, Glycoalk_fall_2023_Samples.csv, Glycoalk_fall_2023_Standard_curve.csv, Glycoalk_Spring_2023_standard_curve.csv | The well location of the sample on the microplate reader. |
| Year | character | clim_data.csv, Combined_data.csv, Combined_herbivores_data.csv, Combined_herbivory_and_trait_data.csv | Year when the data was collected. |

## Access information

Other publicly accessible locations of the data:

- <https://github.com/Plant-herbivory-interaction-lab/Herbivory-and-plant-defence-patterns-across-environmental-gradients/tree/main/Data>
