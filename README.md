# Global Lung Cancer Rates
This repository contains files pertaining to the analysis of possible socio-economic and environmental factors associated to global lung cancer incidences. The R script can be found at location Code/Final Project_Lannette_Pierre.R. The final report comprising the results of the analysis can be found at Output/final_report.pdf. 

## Summary
Lung cancer is the most commonly diagnosed and fatal cancer around the world. In 2019 alone it claimed 1.7 million lives globally. Although nations have achieved accelerating economic growth and productivity, increasing industrialization powered by fossil fuels have negatively impacted the health of the people. For instance, eight Indian citiies hold their spots in the list of top ten most polluted cities in the world. Air pollution and smoking tobacco are some of the most prevalent factors linked to the cancer. Therefore, we explored environmental factors along with some socio-economic variables of 152 nations to analyze their effects on lung cancer rates. We used ordinary least square method (OLS) to estimate the parameters of our multiple linear regression models. We developed three OLS models for all nations, developed nations (with GDP per capita > $15,000) and developing nations (with GDP per capita < $15,000). Our predictor vriables included human development index (HDI), smoking prevalence, CO<sub>2</sub> emissions (as a proxy for outdoor pollution), environmental performance index (EPI), coal use, solid fuel use (as a proxy for indoor pollution) and a two-way interaction effect between CO<sub>2</sub> emissions and EPI on lung cancer rates per 100,00 people. The results of our analysis can be found in the final report in the Output folder.

## Investigators

* Lannette Rangel, Masters of Environmental Management, 2020, Nicholas School of the Environment, Duke University

* Pierre Mishra, Masters of Environmental Management, 2021, Nicholas School of the Environment, Duke University

## Keywords
lung cancer, air pollution, carbon dioxide, coal, smoking, gross domestic product, multiple linear regression


## Folder structure and file formats
* Code - Contains .R file for the analysis
* Data - Contains raw and processed data sets in comma separated values format
  + Raw - Contains unedited csv and shapefiles
  + Processed - Contains edited csv file
* Output - Contains .pdf file for presenting results of the analysis
