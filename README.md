## General data description

This is the organized data used in the accepted Journal of Ecology manuscript. Code associated with organizing all the raw data and extracting the climate data is available in this [Github repository](https://github.com/Plant-herbivory-interaction-lab/Herbivory-and-plant-defence-patterns-across-environmental-gradients.git).

## Table descriptions and column descriptions associated with specific tables.

### clim_data.csv:

This table includes the climate variables used in the manuscript. These climate variables were extracted using the GoogleEarth Engine API. Extraction code for the climate variables can be found in the [Github repository](https://github.com/Plant-herbivory-interaction-lab/Herbivory-and-plant-defence-patterns-across-environmental-gradients.git) associated with this manuscript.

| Column name | Units | Description |
|:---|:---|:---|
| AP | cm | Annual precipitation. |
| Clim_PC1 | numeric | The first axis of the principal component analysis. |
| Clim_PC2 | numeric | The second axis of the principal component analysis. |
| Latitude | arc degrees | The latitudinal value of the plant population. |
| Longitude | arc degrees | The longitudinal value of the plant population. |
| MAT | °C | Mean annual temperature. |
| NPP | kg C/m² | Average NPP extracted from all the years on GoogleEarth Engine API. |
| NPP_g_10y | kg C/m² | Average NPP extracted from years 2013 to 2023 on GoogleEarth Engine API. |
| NPPg | kg C/m² | Total NPP from May to August of the year when the field plant data was collected. |
| Pop | character | Unique name for a population. |
| PWQ | cm | Precipitation of the wettest quarter. |
| WeightedCD | numeric | Calculated climate distance based on the climate MAP, AP, PWQ, and Tsd. |
| Year | character | Year when the data was collected. |

<br />

### Combined_data.csv: 

Combined leaf trait data from the common garden and the field data.

| Column name | Units | Description |
|:---|:---|:---|
| Conc | mg/g | Glycoalkaloid concentration per milligram of dried leaf matter. |
| Date | date | The date that indicates when the data was collected. |
| Height | cm | The height of a plant. |
| herb_p | proportion | The proportion of herbivory estimation of the whole plant. |
| L_area | cm² | Leaf area used to calculate SLA. |
| L_weight | grams | Leaf weight used to calculate SLA. |
| Leaves | count | The number of leaves on a plant. |
| Loc | character | Location of the data. |
| Plant_ID | character | A value that indicates a unique ID of the plants that were sampled. |
| Pop | character | Unique name for a population. |
| SLA | cm²/g | The specific leaf area of the collected leaf. |
| Time | character | Time of when the data was collected (e.g., year, or early/middle/late). |
| Trichomes | n per 0.24 cm² | Trichomes counted in the given area. |
| Year | character | Year when the data was collected. |

<br />

### Combined_herbivores_data.csv:

Herbivores observed in the common garden and the field plants.

| Column name | Units | Description |
|:---|:---|:---|
| Amount | count | Number of specific herbivores. |
| Date | date | The date that indicates when the data was collected. |
| Loc | character | Location of the data. |
| Plant_ID | character | A value that indicates a unique ID of the plants that were sampled. |
| Pop | character | Unique name for a population. |
| Species | character | Herbivore species observed. |
| Time | character | Time of when the data was collected (e.g., year, or early/middle/late). |
| Year | character | Year when the data was collected. |

<br />

### Solanum_carolinense_inat.csv:

Coordinates of *Solanum carolinense* inaturalist observations used to visualize the native distribution *S. carolinense* .

| Column name | Units | Description |
|----|----|----|
| Latitude | arc degrees | The latitudinal value of the plant observations. |
| Longitude | arc degrees | The longitudinal value of the plant observations. |
| scientific_name | character | Scientific name of the observation species. |

<br />

## R code files and descriptions.

### Data_analysis_and_visualization_May_2026.R: 

Code used to visualize and analyze the data.

### Functions.R: 

Functions used in cleaning, analyzing, or visualizing data.

## Access information

Other publicly accessible locations of the data:

- <https://github.com/Plant-herbivory-interaction-lab/Herbivory-and-plant-defence-patterns-across-environmental-gradients/tree/main/Data>
