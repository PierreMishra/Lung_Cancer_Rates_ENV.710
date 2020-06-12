#### Completed: December 2019

# Global Lung Cancer Rates
This repository contains files pertaining to the analysis of possible socio-economic and environmental factors associated to global lung cancer incidences. The R script can be found at location Code/Final Project_Lannette_Pierre.R. The final report comprising the results of the analysis can be found at Output/final_report.pdf. 

## Summary
Lung cancer is the most commonly diagnosed and fatal cancer around the world. In 2019 alone it claimed 1.7 million lives globally. Although nations have achieved accelerating economic growth and productivity, increasing industrialization powered by fossil fuels have negatively impacted the health of the people. For instance, eight Indian citiies hold their spots in the list of top ten most polluted cities in the world. Air pollution and smoking tobacco are some of the most prevalent factors linked to the cancer. Therefore, we explored environmental factors along with some socio-economic variables of 152 nations to analyze their effects on lung cancer rates. We used ordinary least square (OLS) method to estimate the parameters of our multiple linear regression models. We developed three OLS models for all nations, developed nations (with GDP per capita > $15,000) and developing nations (with GDP per capita < $15,000). Our predictor variables included human development index (HDI), smoking prevalence, CO<sub>2</sub> emissions (as a proxy for outdoor pollution), environmental performance index (EPI), coal use, solid fuel use (as a proxy for indoor pollution) and a two-way interaction effect between CO<sub>2</sub> emissions and EPI on lung cancer rates per 100,00 people. The results of our analysis can be found in the final report in the Output folder.

## Investigators

* Lannette Rangel, Masters of Environmental Management, 2020, Nicholas School of the Environment, Duke University

* Pierre Mishra, Masters of Environmental Management, 2021, Nicholas School of the Environment, Duke University

## Keywords
lung cancer, air pollution, carbon dioxide, coal, smoking, gross domestic product, multiple linear regression

## Database Information

We sourced our data from a number of agencies and organizations. All of the datasets were publicly available. 

* Lung cancer incidence rates, smoking prevalence and solid fuel use data was retrieved from The Cancer Atlas, which is produced by the American Cancer Society, the International Agency for Research on Cancer, and the Union for International Cancer Control.
* Human Development Index (HDI) data was sourced from the RAND corporation as a part of its Food-Energy-Water Security Index.
* Environmental Performance Index (EPI) scores were retrieved from Yale University.
* Coal usage data was sourced from International Energy Agency. 

## Folder structure and file formats
* Code - Contains .R file for the analysis
* Data - Contains raw and processed data sets in comma separated values format
  + Raw - Contains unedited csv
  + Processed - Contains edited csv file
* Output - Contains .pdf file for presenting results of the analysis

## Metadata



## Scripts and codes

The following code was used to make the processed data set 'final.csv'.

```R
# Loading data

cancer_data <- read.csv("Data/Raw/Cancer-Atlas-3rd-ed_Map-data-download.csv")
FEWS_Index <- read.csv("Data/Raw/FEWS_Index_Clean.csv")
country_code <- read.csv("Data/Raw/country_code_web.csv")
coal_data <- read.csv("Data/Raw/Coal Use Data2.csv")
yale_epi <- read.csv("Data/Raw/yaleEPI2018.csv")
urban_pop <- read.csv("Data/Raw/urban_pop.csv")

# Cancer data: setting first row as column headers
colnames(cancer_data) <- NULL
names(cancer_data) <- as.character(unlist(cancer_data[1,]))
cancer_data <- cancer_data[-1,] #removing irrelevant row
colnames(cancer_data)[1] <- "country"
cancer_data[,12] <- as.numeric(as.character(cancer_data[,12])) # changing outdoor pollution data to numeric
cancer_data[,13] <- as.numeric(as.character(cancer_data[,13])) # changing indoor pollution data to numeric
cancer_data[,15] <- as.numeric(as.character(cancer_data[,15])) # changing lung cancer data to numeric
cancer_data[,16] <- as.numeric(as.character(cancer_data[,16])) # changing lung cancer data to numeric
cancer_data[,4] <- as.numeric(as.character(cancer_data[,4])) # changing smoking prevalence to numeric
cancer_data[,5] <- as.numeric(as.character(cancer_data[,5])) # changing smoking prevalence to numeric
cancer_data$lung_cancer_total <- rowSums(cancer_data[,c(15,16)]) # adding male and female lung cancer rates
cancer_data$smoking_total <- rowSums(cancer_data[,c(4,5)]) # adding male female smoking prevalence
colnames(cancer_data) <- tolower(colnames(cancer_data))
cancer_data <- cancer_data %>%
  select(1, 2, 12, 13, lung_cancer_total, smoking_total) #retaining country info, smoking and lung cancer data
colnames(cancer_data)[c(3,4)] <- c("outdoor", "indoor") 
rownames(cancer_data) <- NULL # resetting row index

# FEWS_Index & country_code merge
FEWS_w_code <- merge (FEWS_Index, country_code, by.x="Country", by.y="Countries.or.areas.and.their.codes.")
colnames(FEWS_w_code)[24] <- "iso_code"
colnames(FEWS_w_code) <- tolower(colnames(FEWS_w_code))

# Coal data 2015: renaming columns and retaining countries by ISO
coal_data <- select(coal_data, -1)
colnames(coal_data) <- c("iso_code", "Coal") 

# Urban population 2018: renaming columns
urban_pop <- select(urban_pop, -1)
colnames(urban_pop) <- c("iso_code", "urban_pop")

# Merging cancer data with FEWS Index using country code
merge_data <- merge(FEWS_w_code, cancer_data, by.x="iso_code", by.y="iso 3 code")
merge_data <- merge_data %>%
  select(-country.y)
colnames(merge_data)[2] <- "country"
#merge_data_clean <- merge_data[!is.na(merge_data[3]),]
merge_data_clean <- merge_data[-c(1:24),] # Removing rows with no ISO code
rownames(merge_data_clean) <- NULL

# Adding coal data 
final <- merge(merge_data_clean, coal_data, by.x="iso_code", by.y="iso_code")
colnames(final)[2] <- c("country")

# Merging Yale's EPI index data for exploring more variables
yale_epi <- select(yale_epi, -GDP, -country) # to avoid duplicating gdp from FEWS dataset
final <- merge (final, yale_epi, by.x='iso_code', by.y='iso')

# Merging urban population info to the final dataset
final <- merge (final, urban_pop, by.x='iso_code', by.y='iso_code')
colnames(final) <- tolower(colnames(final))

# Export final data in csv
write_csv(final, "Data/Processed/final.csv")
```
