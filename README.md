# ACE 592SAE--landvalue 


---


## Objective
The objective of this paper is to measure the determinants of agricultural land value in the United States. We also aim at understanding how these determinants have changed and how those changes could influence agricultural land value.


## Data

The unit of analysis is the county. We use a wide array of data sources for our analysis. The time frame for our analysis comprises the following years: 1997, 2002, 2007, 2012. 

*   **Land value:** Census of Agriculture (COA).
*   **Income and Population:** USDA Economic Research Services.
*   **Land area:** United Census Bureau (UCB).
*   **Temperature and rainfall:** PRISM climate group (5 years forward moving average). 
* **Pollution (No2 and So2 levels):** from stations located across the country. Data comes from the Environmental Protection Agency.
* **Extreme weather events:** over 50 types of extreme weather events from the Storm Events Database, registered by NOOA's National Weather Services (NWS).

* **Rural/urban decomposition:** rural-urban continuum code (USDA/ERS). Big & Small Enterprises ratio.

## Code 

We provide a brief description of what each code file does. 


* ```02_Summary.R ```: using 'df_land_temp_income.csv', this file generates data visualization (maps and bar charts) on the Agri. Land value, Crop revenue, Cropland, Per-capita income, Population density, Temperature, and Rainfall. In addition, adding pollution and extreme weather data, this file estimates a simple OLS for estimating the determinants of Agri. land value in the US.

* ```ACE592_FINAL_PROJECT.Rmd ``` and ```ACE592_FINAL_PROJECT.html ``` : codes to compute rural/urban decomposition and run panel regressions.

* ```extreme events_Keliang.do ```: code for extreme weather event cleaning.

* ```extreme events_Keliang.ipynb ```: code for generating visualizations on extreme weather events. 

* ```so2_no2_county_byyear.do```: code for cleaning data on air pollutants. 

* ```no2_so2_landval_results.ipynb```: code for visualizations of land value and pollution.

## Output

We put together our output generated in the paper (figures and tables) in the  ```output ``` directory.


