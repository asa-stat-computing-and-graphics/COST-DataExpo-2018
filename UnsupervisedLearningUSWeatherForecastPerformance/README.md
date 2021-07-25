# Unsupervised-Learning-on-U.S.-Weather-Prediction-Accuray
Using functional data unsupervised learning techniques, FPCA and clustering, to evaluate the performance of U.S weather prediction.

#### This is the reproducible code package for the paper Unsupervised Learning on U.S. Weather Prediction Accuray. The repository includes the real and raw U.S. weather data, R code of real data, and R code for simulation study.

### Programming language of the project and execution system
R 4.0.5 (or above!) and Windows 10. Please run under these environment to reproduce the plot and tables! Under other version or system, the result would be slightly different, and simulation might experience error.

Our simulation data are huge that our device cannot support to save them all and upload to this Git site, so please keep the running environment consistent to reproduce the same result.  

### Datasets Description
1. forecast.zip and forecast.rds

The file contains all the forecast records. To get the original format data (which is used in the real data analysis), please get the forecast.dat file from it first.

We have saved the forecast.dat to forecast.rds with smaller size.

2. location.csv

The file contains all the geoinformation of the weather forecast locations.

3. histWeather.csv

The file contains all the historical records of the real weather features measurements.

4. region-to-state.new.csv

The file contains all the short names of states and the geolocation of the center of the states.

5. Description.txt

This the introduction the contents of the real data files 1. forecast.zip, 2. location.csv, and 3. histWeather.csv.

### real_data_analysis_markdown.Rmd and related output CSV files
#### The output file in HTML format, real-data-analysis-markdown.html is also attached. (But we have not uploaded the workspace Rdata of all the final output, since the file is too large)

This is the Rmd files including all the content related to real data analysis, which would take about 10 mins to run under ASUS laptop with 8th Gen Intel Core i5 and 32G RAM. It contains the following sections:

1. Data importing and dataset merging
2. Data exploration (fig 1,2,3)
3. Smooth FPCA on B-spline Non-parametric Regression (fig 4)
4. Clustering
5. Final Result Visualization (fig 5, 6, 7, table 1)

#### From this R files execultion, 3 more CSV files will be generated to save the output of the real application, *which would be used in the simulation of real data later*.

1. mindiff_day1_nona.csv: The date list with collected temperature difference data.
2. mindiff_state1_nona.csv: The unsmoothed raw data of daily absoluted temperature difference of 50 U.S. states.
3. real_analysis_result.csv: Final clustering result from real data analysis

#### The package "fiftystater" is used for clustering result visualization on U.S. map. It may be unavailable on CRAN, so I attached the achieved fold of the package, the "fiftystater_1.0.1.tar.gz" in the Github repository. Please install it in R first before running the code If you are under Windows system, it would be easy to be installed.

If you don't want to use this package, then please try using the uploaded data file *"fifty_stater.rda"* or using package *usmap* and modify the map plotting code at part 5

#### In addition, we create a function "ipak" at the beginning for checking the existance of the required package. If the packages do not exist in the system, then installation of the packages would start; otherwise, the package would just loaded. You would be able to see whether all the required packages have been loaded in the end

This functions works great in our team, so we keep and use it in all of our code. If you don't like it, please use general library() to load all the required packages in the *packages* list.  

### simulation_on_real_analysis_markdown.Rmd
#### The output file in HTML format, *real-data-analysis-markdown.html* and final workspace after code execution, *simulation_on_real_data_output.rdata* are attached.
This is the R files including all the content related to the analysis from the simulation based on real data, *which would take about 5 hours to run under ASUS laptop with 8th Gen Intel Core i5 and 32G RAM.* **Please run the code real_data_analysis.R first** for real application so as to get the output files from the real data application. The data files that would be used in this code are:

1. mindiff_day1_nona.csv
2. mindiff_state1_nona.csv
3. real_analysis_result.csv
 
The main content of this file includes:
1. Functions for Data simulation and Analysis (figure 9)
2. Clustering Number Selection Validation (table 4)
3. Clustering Validation Study (table 5)

#### The function "ipak" is also applied at the beginning of this code

In addition, FunFEM would try multiple variance and covariance matrix form to get the best result. In some forms, the result would be diverge and you may see some warning or message like

*Error in svd(ginv(t(G) %*% G %*% W) %*% (t(G) %*% Ttilde %*% t(Ttilde) %*% : infinite or missing values in ‘x’" “Error in svd(X) : infinite or missing values in ‘x’*

This is not a real error and the code would keep running.

### simulation_study_analysis_markdown.Rmd
#### The final workspace after code execution, *simulation_study_analysis_output.rdata* are attached. But the HTML output file is too large to be attached
This is the Rmd files including all the content related to simulation study, *which would take about 10 hours to run under ASUS laptop with 8th Gen Intel Core i5 and 32G RAM*. The main content of this file includes:

1. Functions for Data simulation and Result Summary (Fig 8)
2. Smooth FPCA on B-spline Non-parametric Regression
3. Clustering Number Selection Validation (Table 2)
4. Clustering Validation Study (Table 3)

#### The function "ipak" is also applied at the beginning of this code

In addition, FunFEM would try multiple variance and covariance matrix form to get the best result. In some forms, the result would be diverge and you may see some warning or message like

*Error in svd(ginv(t(G) %*% G %*% W) %*% (t(G) %*% Ttilde %*% t(Ttilde) %*% : infinite or missing values in ‘x’" “Error in svd(X) : infinite or missing values in ‘x’*

This is not a real error and the code would keep running.

### null_case_simulation_markdown.Rmd
#### The output file in HTML format, *null_case_simulation_markdown.html* and final workspace after code execution, *null_case_simulation_output.rdata* are attached.
This is the R files including all the content related to the null case simulation study (detecting cluster number K = 1), *which would take about 4 hours to run under ASUS laptop with 8th Gen Intel Core i5 and 32G RAM*.  This code is written to respond the Null Case study of reviwer 2. The main content of this file includes:

1. Functions for Data simulation and Analysis
2. Clustering Number Selection Validation (for final table in supplementary doc)

#### The function "ipak" is also applied at the beginning of this code

In addition, FunFEM would try multiple variance and covariance matrix form to get the best result. In some forms, the result would be diverge and you may see some warning or message like

*Error in svd(ginv(t(G) %*% G %*% W) %*% (t(G) %*% Ttilde %*% t(Ttilde) %*% : infinite or missing values in ‘x’" “Error in svd(X) : infinite or missing values in ‘x’*

This is not a real error and the code would keep running.
