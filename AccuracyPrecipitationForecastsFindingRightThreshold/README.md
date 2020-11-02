# Code for Accuracy of Precipitation Forecasts: Finding the Right Threshold for What is Considered Rain

For Data Expo at Joint Statistical Meeting 2018, Vancouver. Analysis, graphs and maps were all made entirely using R and RStudio. All scripts are R Markdown (rmd) files.

Authors are Darren Keeley and Eric A. Suess from CSU East Bay.


## How to Use

Code is composed of 3 rmd's. **1 prepare datasets** creates the dataframes necessary to run the analysis form the competition datasets. **2 figures and tables** uses these dataframes to create graphics, except the maps. **3 maps** creates the blue and red maps.

Create a folders **data** and **pics** in the folder where the scripts are located. Place the competition datasets in **data**; these include **histWeather.csv**, **forecast.dat**, and **locations.csv**. 

Uncomment the "save" functions in the rmd **2** and **3**. Run scripts in sequential order.


## Packages

R 4.0.3 was used.

| User Library | Version|
|------------|--------|
| data.table | 1.13.0 |
| dplyr      | 1.0.2  |
| forcats    | 0.5.0  |
| ggplot2    | 3.3.2  |
| gridExtra  | 2.3    |
| gstat      | 2.0-6  |
| maptools   | 1.0-2  |
| png        | 0.1-7  |
| purrr      | 0.3.4  |
| readr      | 1.4.0  |
| sp         | 1.4-4  |
| stringr    | 1.4.0  |
| tibble     | 3.0.3  |
| tidyr      | 1.1.2  |
| tidyverse  | 1.3.0  |

