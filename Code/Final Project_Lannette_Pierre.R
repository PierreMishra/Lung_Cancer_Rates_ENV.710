#Name: Pierre and Lannete

library(ggplot2)
library(ggpubr)
library(lmtest)
library(plyr)
library(tidyr)
library(robustbase)
library(tidyverse)
library(sandwich)
library(lmtest)
library(modelr)
library(broom)
library(olsrr)
library(sjPlot)
library(sjmisc)
library(car)
library(GGally)
library(jtools)
library(interactions)
library(ggeffects)
library(stargazer)

# Setting ggplot theme
peaceful.theme <- theme_classic(base_size = 14) +
  theme(axis.text = element_text(color = "black"), 
        legend.position = "right", 
        plot.title = element_text(hjust = 0.5))

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

# Removing duplicated country column
# final <- final%>%
#   subset(., select = which(!duplicated(names(.)))) %>%
#   select(-country.y, country_name)

# To prevent log calculation errors in correlation matrix
# final[final==0] <- 0.0001 # to prevent inf


# Export final data in csv
# write_csv(final, "final.csv")

#------------------------------------------------ Exploratory Data Analysis

# Changing all infinte values to NA
is.na(final)<-sapply(final, is.infinite)

# making histogram and boxplot for our response variable (lung cancer rates)
ggplot (data=final, aes(x=lung_cancer_total)) + 
  geom_histogram(col="black", fill="aquamarine") + 
  labs (title="Distribution of Lung Cancer", x="Lung Cancer Rates", y="Frequency") + 
  peaceful.theme

final$log_lung_cancer<-log(final$lung_cancer_total) #log-tranforming to improved skewed distribution

ggplot (data=final, aes(x=log_lung_cancer)) + 
  geom_histogram(col="black", fill="aquamarine")+ 
  labs (title="Distribution of Log Lung Cancer", x="Log Lung Cancer Rates", y="Frequency") + 
  peaceful.theme

ggplot (data=final, aes(y=lung_cancer_total)) + 
  geom_boxplot(fill="deepskyblue") + 
  labs (title = "Distribution of Lung Cancer", y="Lung Cancer Rates", x="") + 
  peaceful.theme + theme(axis.title.x=element_blank(),
                         axis.text.x=element_blank(),
                         axis.ticks.x=element_blank())

ggplot (data=final, aes(y=log_lung_cancer)) + 
  geom_boxplot(fill="deepskyblue") + 
  labs (title = "Distribution of Log Lung Cancer", y="Log Lung Cancer Rates", x="") + 
  peaceful.theme + theme(axis.title.x=element_blank(),
                         axis.text.x=element_blank(),
                         axis.ticks.x=element_blank())

# log transformation does not help. Lets explore correlation matrix with both logged and unlogged response variable 

# Making scatterplot matrix to avoid multicollinearity
# logged response variable
final.columnselect.log <- c ("hdi","gdp", "infant_mortality", "urban", 
                         "renewable", "indoor", "outdoor", 
                         "smoking_total", "Coal", "log_lung_cancer")  
ggpairs(final, columns=final.columnselect.log, title="", axisLabels = "show")

# unlogged response variable
final.columnselect.unlog<-c("hdi","gdp", "infant_mortality", "urban", 
                       "renewable", "indoor", "outdoor", 
                       "smoking_total", "Coal", "lung_cancer_total")  #hdi, gdp, infant_mortality, urban_renewable, outdoor_pollution, indoor_pollution, smoking_total, coal, lung_cancer 
ggpairs(final, columns=final.columnselect.unlog, title="", axisLabels = "show")

# Transforming some variables and checking their correlation coefficient with lung cancer rates
final$log_hdi <- log(final$hdi)
final$log_gdp <- log(final$gdp)
final$log_infant_mortality <- log(final$infant_mortality)
final$log_urban <- log(final$urban)
final$log_renewable <- log(final$renewable)
final$log_outdoor <- log(final$outdoor)
final$log_indoor <- log(final$indoor)
final$log_smoking <- log(final$smoking_total)
final$log_coal <- log(final$Coal)

# Logged explanatory variables and logged response variable
final.columnselect2.log <- c("log_hdi","log_gdp", "log_infant_mortality", "log_urban", 
                       "log_renewable", "log_indoor", "log_outdoor", 
                       "log_smoking", "log_coal", "log_lung_cancer") 
ggpairs(data=final, column=final.columnselect2.log, title="", axisLabels = "show")

# Logged explanatry varibales and unlogged response variable
final.columnselect2.unlog <- c("log_hdi","log_gdp", "log_infant_mortality", "log_urban", 
                       "log_renewable", "log_indoor", "log_outdoor", 
                       "log_smoking", "log_coal", "lung_cancer_total")
ggpairs(data=final, column=final.columnselect2.unlog, title="", axisLabels = "show")

# Exploring more predictor variables from Yale's EPI index
# Adding column of pm2.5 by concentrating it in urban areas
final$pm25_urban <- (100*final$pm25)/final$urban
# Changing all infinte values to NA
is.na(final)<-sapply(final, is.infinite)

# More scatterplot matrix to check correlation with reponse variable
#is.na(final)<-sapply(final, is.infinite)
final.yaleselect.log <- c("log_hdi","pop","popdensity","landarea",
                        "h2o.current","air.current","epi2018score",
                        "log_lung_cancer") #unlogged response variable
ggpairs(final, column=final.yaleselect.log, title="", axisLabels="show")

final.yaleselect.unlog <- c("log_hdi","pop","popdensity","landarea",
                          "h2o.current","air.current","epi2018score",
                          "lung_cancer_total") #logged response varibale
ggpairs(final, column=final.yaleselect.unlog, title="", axisLabels="show")

# Logging PopDensity and Land Area and other potential variables from yale file
final$log_popdensity <- log(final$popdensity)
final$log_landarea<- log(final$landarea)
final$log_air <- log(final$air.current)
final$log_water <- log(final$h2o.current)
final$log_pop <- log(final$pop)
final$log_urban_pop <- log(final$urban_pop)
final$log_co2 <- log(final$co2)

is.na(final) <- sapply(final, is.infinite)

final.yaleselect2 <- c("log_popdensity","log_landarea",
                     "log_air","log_water",
                     "lung_cancer_total") #unlogged response variable
ggpairs(final, column=final.yaleselect2, title="", axisLabels="show")

final.yaleselect3 <- c("log_co2","urban_pop","log_popdensity",
                     "log_landarea","log_air","log_water",
                     "log_pop","log_lung_cancer") #logged response varibale
ggpairs(final, column=final.yaleselect3, title="", axisLabels="show")

final.almost <- c("log_co2","log_hdi","smoking_total",
                "co2","epi2018score","Coal","indoor",
                "log_lung_cancer") #logged response varibale
ggpairs(final, column=final.almost, title="", axisLabels="show")

#-------------------------------------------------- Model 1  

# final variables for model (log HDI, smoking prevalence, co2*EPI Score, coal, indoor)
final.model.1 <- c("log_hdi","smoking_total","co2",
                   "epi2018score","Coal","indoor",
                   "log_lung_cancer")
ggpairs(data=final, column=final.model1, title="", axisLabels="show")

#removing outlier influence
#fews.df<-fews.df[-c(101), ]

model.1 <- lm(formula = log_lung_cancer ~ log_hdi + smoking_total +
               co2*epi2018score + coal + indoor, 
               data = final, na.action = na.exclude)
summary(model.1)

# plotting interaction effect
p <- ggpredict(model.1, c("co2", "epi2018score [meansd]"))
plot(p) + 
  labs(x = expression(paste(CO[2]," (metric tons/capita)")),
  y = "Log Lung Cancer Rates (per 100,000) \n",
  colour = "EPI Score") +
  ggtitle("Predicted values of log lung cancer rates") +
  peaceful.theme + theme(axis.title.x = element_text(margin = margin(t = 15, 
                                                                     r = 0, 
                                                                     b = 0, 
                                                                     l = 0)))

# making residual vs. fitted plot
model.1.res = resid(model.1, na.action = na.exclude, na.rm=TRUE)
model.1.fit = fitted(model.1, na.action = na.exclude, na.rm=TRUE) 

# including the residual and fitted values into the dataframe
final <- data.frame(final, model.1.res, model.1.fit)

# Plot residual vs fitted graph
ggplot(final, aes(x=model.1.fit, y=model.1.res)) +
  geom_point() +
  geom_hline(yintercept=0, col="red", lwd=2)+
  ggtitle("Model 1 Residual vs Fitted Plot") +
  xlab("Fitted Lung Cancer Rates") + ylab("Residuals")

# Labelling points to know which could be potential outliers
ggplot(final, aes(x=model.1.fit, y=model.1.res)) +
  geom_point() +
  geom_hline(yintercept=0, col="red", lwd=2)+
  geom_text(label=final$country)+
  ggtitle("Model 2: Residual vs Fitted Plot") +
  xlab("Fitted Lung Cancer Rates") + ylab("Residuals")

# Trendline for residual vs fitted  
plot(model.1, which = c(1))
plot(model.1, which = c(2))

# Breusch-Pagan test of heteroskedasticity 
# Ho: Homoskedastic
# Ha: Heteroskedastic
lmtest::bptest(model.1) 
# p-value is low, null must go

# Robust standard errors
all_countries_se <- coeftest(model.1, vcov = vcovHC(model.1))
summary(all_countries_se)

# Cook's distance (test for outlier/leverage/influence)
ols_plot_cooksd_bar(model.1)
#remove outliers and try again! code to remove outliers ---- fews.df<-fews.df[c(-129, )]

# VIF test of multicollinearity
car::vif(model.1)

# Model performance
glance(model.1) %>%
  select(adj.r.squared, sigma, AIC, BIC, p.value)

#-----------------------------------------------------#
#---Dividing countries based off of GDP per capita----#
#-----------------------------------------------------#

developed <- subset(final,gdppc>=15000) # high-income subset (Model 2)
developing <- subset(final,gdppc<15000) # low-income subset (Model 3)

#####################################################################################
#-----------------------------------------------------------------------------------#
#----------------------------------- High-Income SUBSET ----------------------------#
#-----------------------------------------------------------------------------------#
#####################################################################################

final.model.2 <- c("log_hdi","smoking_total","co2",
                   "epi2018score","Coal","indoor",
                   "log_lung_cancer")
ggpairs(data=developed, column=final.model.2, title="", axisLabels="show")

model.2 <- lm(formula = log_lung_cancer ~ log_hdi + smoking_total +
              co2*epi2018score + coal + indoor, 
              data = developed, na.action = na.exclude)
summary(model.2)

# plotting interaction effect
p <- ggpredict(model.2, c("co2", "epi2018score [meansd]"))
plot(p) + 
  labs(x = expression(paste(CO[2]," (metric tons/capita)")),
       y = "Log Lung Cancer Rates (per 100,000) \n",
       colour = "EPI Score") +
  ggtitle("Predicted values of log lung cancer rates") +
  peaceful.theme + theme(axis.title.x = element_text(margin = margin(t = 15, 
                                                                     r = 0, 
                                                                     b = 0, 
                                                                     l = 0)))

# making residual vs. fitted plot
model.2.res = resid(model.2, na.action = na.exclude, na.rm=TRUE)
model.2.fit = fitted(model.2, na.action = na.exclude, na.rm=TRUE) 

# including the residual and fitted values into the dataframe
developed <- data.frame(developed, model.2.res, model.2.fit)

# Plot residual vs fitted graph
ggplot(developed, aes(x=model.2.fit, y=model.2.res)) +
  geom_point() +
  geom_hline(yintercept=0, col="red", lwd=2)+
  ggtitle("Model 2 Residual vs Fitted Plot") +
  xlab("Fitted Lung Cancer Rates") + ylab("Residuals")

# Labelling points to know which could be potential outliers
ggplot(developed, aes(x=model.2.fit, y=model.2.res)) +
  geom_point() +
  geom_hline(yintercept=0, col="red", lwd=2)+
  geom_text(label=developed$country)+
  ggtitle("Model 2: Residual vs Fitted Plot") +
  xlab("Fitted Lung Cancer Rates") + ylab("Residuals")

# Trendline for residual vs fitted  
plot(model.2, which = c(1))
plot(model.2, which = c(2))

# Breusch-Pagan test of heteroskedasticity 
# Ho: Homoskedastic
# Ha: Heteroskedastic
lmtest::bptest(model.2) 
# p-value is low, null must go 

# Robust standard errors
high_income_countries_se <- coeftest(model.2, vcov = vcovHC(model.2))
summary(high_income_countries_se)

# checking if this code allows me to include the models with robust standard errors and their statistics 
#(because usually robust models dont show R2 and other model statistics) 
# stargazer(model1, model.2, type = "text", se = list (all_countries[,"Std. Error"], high_income_countries[,"Std. Error"]), out="robustshit.txt")

# Cook's distance (test for outlier/leverage/influence)
ols_plot_cooksd_bar(model.2)
#remove outliers and try again! code to remove outliers ---- fews.df<-fews.df[c(-129, )]

# VIF test of multicollinearity
car::vif(model.2)

# Model performance
glance(model.2) %>%
  select(adj.r.squared, sigma, AIC, BIC, p.value)

#####################################################################################
#-----------------------------------------------------------------------------------#
#------------------------------- Low-Income SUBSET ---------------------------------#
#-----------------------------------------------------------------------------------#
#####################################################################################

final.model.3 <- c("log_hdi","smoking_total","co2",
                   "epi2018score","Coal","indoor",
                   "log_lung_cancer")
ggpairs(data=developing, column=final.model2, title="", axisLabels="show")

model.3 <- lm(formula = log_lung_cancer ~ log_hdi + smoking_total +
              co2*epi2018score + coal + indoor, 
              data = developing, na.action = na.exclude)
summary(model.3)

#interaction effect is not significant

# making residual vs. fitted plot
model.3.res = resid(model.3, na.action = na.exclude, na.rm=TRUE)
model.3.fit = fitted(model.3, na.action = na.exclude, na.rm=TRUE) 

# including the residual and fitted values into the dataframe
developing <- data.frame(developing, model.3.res, model.3.fit)

# Plot residual vs fitted graph
ggplot(developing, aes(x=model.3.fit, y=model.3.res)) +
  geom_point() +
  geom_hline(yintercept=0, col="red", lwd=2)+
  ggtitle("Model 2 Residual vs Fitted Plot") +
  xlab("Fitted Lung Cancer Rates") + ylab("Residuals")

# Labelling points to know which could be potential outliers
ggplot(developing, aes(x=model.3.fit, y=model.3.res)) +
  geom_point() +
  geom_hline(yintercept=0, col="red", lwd=2)+
  geom_text(label=developing$country)+
  ggtitle("Model 2: Residual vs Fitted Plot") +
  xlab("Fitted Lung Cancer Rates") + ylab("Residuals")

# Trendline for residual vs fitted  
plot(model.3, which = c(1))
plot(model.3, which = c(2))

# Breusch-Pagan test of heteroskedasticity 
# Ho: Homoskedastic
# Ha: Heteroskedastic
lmtest::bptest(model.3) 
# p-value is greater than 0.05 , homoscadestic!!!

# Robust standard errors
low_income_countries_se <- coeftest(model.3, vcov = vcovHC(model.3))
summary(low_income_countries_se)

# Cook's distance (test for outlier/leverage/influence)
ols_plot_cooksd_bar(model.3)
#remove outliers and try again! code to remove outliers ---- fews.df<-fews.df[c(-129, )]

# VIF test of multicollinearity
car::vif(model.3)

# Model performance
glance(model.3) %>%
  select(adj.r.squared, sigma, AIC, BIC, p.value)

#--------------------------------------- Export statistical Results using STARGAZER

stargazer(model.1, model.2, model.3, se = list (all_countries_se[,"Std. Error"],
                                                high_income_countries_se[,"Std. Error"]),
          title="Regression Model Results", 
          column.labels = c("All countries", "High-Income Countries",
                            "Low-Income Countries"), dep.var.labels = "Log Lung Cancer Rates",
          covariate.labels = c("Log HDI", "Smoking Prevalence", "CO2",
                               "EPI Score", "Coal Consumption", "Solid Fuel Use",
                               "CO2:EPI Score"), align=TRUE,
          column.sep.width = "-20pt", out="model_table.htm")


#------------------------------------------------- Summary statistics
summary(final$lung_cancer_total)
sd(final$lung_cancer_total, na.rm=TRUE)

summary(final$hdi)
sd(final$hdi, na.rm=TRUE)

summary(final$smoking_total)
sd(final$smoking_total, na.rm=TRUE)

summary(final$co2)
sd(final$co2, na.rm=TRUE)

summary(final$epi2018score)
sd(final$epi2018score, na.rm=TRUE)

summary(final$coal, na.rm=TRUE)
sd(final$coal, na.rm=TRUE)

summary(final$indoor)
sd(final$indoor, na.rm=TRUE)
