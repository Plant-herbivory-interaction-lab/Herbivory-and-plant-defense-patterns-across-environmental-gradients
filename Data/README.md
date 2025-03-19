# Repository details: Herbivory and plant defense patterns across a latitudenal gradient.

## *Project overview*
These manuscript analyzes how climate affects the interaction between plant defenses and herbviory across a latitudenal gradient. To test this intereaction we collected hebivory and plant defenses data of 37 different *Solanum carolinense* population across a transect extending from north Florida to south Wisconsin. We also propigated roots collected from these plants in a common garden to test herbivore choice of plants with more or less defensive characteristics. *I will provide more detail here once the manuscript is published*


Data file includes the following tables:

## Bioclims_1970_ave

This table contains the [bioclim](https://developers.google.com/earth-engine/datasets/catalog/WORLDCLIM_V1_BIO#bands) from google earth engine and aridity data from this [publication](https://csidotinfo.wordpress.com/2019/01/24/global-aridity-index-and-potential-evapotranspiration-climate-database-v3/).

| variables | units | min | max | scale | description |
| :--- | :---: | :--- | :--- | :--- | :--- |
| bio01 | °C                      | -29*   | 32*     | 0.1   | Annual mean temperature                                       |
| bio02 | °C                      | 0.9*   | 21.4*   | 0.1   | Mean diurnal range (mean of monthly (max temp - min temp))    |
| bio03 | %                        | 7*     | 96*     |     | Isothermality (bio02/bio07 * 100)                             |
| bio04 | °C                      | 0.62*  | 227.21* | 0.01  | Temperature seasonality (Standard deviation * 100)            |
| bio05 | °C                      | -9.6*  | 49*     | 0.1   | Max temperature of warmest month                              |
| bio06 | °C                      | -57.3* | 25.8*   | 0.1   | Min temperature of coldest month                              |
| bio07 | °C                      | 5.3*   | 72.5*   | 0.1   | Temperature annual range (bio05-bio06)                        |
| bio08 | °C                      | -28.5* | 37.8*   | 0.1   | Mean temperature of wettest quarter                           |
| bio09 | °C                      | -52.1* | 36.6*   | 0.1   | Mean temperature of driest quarter                            |
| bio10 | °C                      | -14.3* | 38.3*   | 0.1   | Mean temperature of warmest quarter                           |
| bio11 | °C                      | -52.1* | 28.9*   | 0.1   | Mean temperature of coldest quarter                           |
| bio12 | mm                       | 0*     | 11401*  |    | Annual precipitation                                          |
| bio13 | mm                       | 0*     | 2949*   |    | Precipitation of wettest month                                |
| bio14 | mm                       | 0*     | 752*    |    | Precipitation of driest month                                 |
| bio15 | Coefficient of Variation | 0*     | 265*    |    | Precipitation seasonality                                     |
| bio16 | mm                       | 0*     | 8019*   |    | Precipitation of wettest quarter                              |
| bio17 | mm                       | 0*     | 2495*   |   | Precipitation of driest quarter                               |
| bio18 | mm                       | 0*     | 6090*   |   | Precipitation of warmest quarter * estimated min or max value |
| bio19 | mm                       | 0*     | 5162*   |   | Precipitation of coldest quarter |
|Pop | categorical | |||Population acronym of the collected populations |
| Aridity | |||| Aridity index |


## Field_2022
This table includes the herbivory and plant defense traits measurements collected during the 2022 field season.

| variables | units | description |
| :--- | :---: | :--- |
|Latitude  | Arc degrees | The latitudenal value of the plant population |
|Date | Numeric | The number that indicates when the data was collected. This number indicates a date orgin of 1970-01-01|
|Plant_ID | Numeric | Indicates the plant number in a specific population |
|Herbivory | Percent | The percent herbivory estimation of the whole plant |
|Trichomes | Numeric | The number of trichomes in a 500 mm squared area |       
|SLA | cm<sup>2</sup>/g | The specific leaf area of the collected leaf |
|Spines | Numeric | Number of spines on the mid-rib of the collected leaf |      
|Conc | 
|Pop
