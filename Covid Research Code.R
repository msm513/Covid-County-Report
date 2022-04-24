#----------------------------------------------------------------
# Covid County Analysis
#
# Created:  04/12/2022
#
# Last Modified:  04/17/2022  
#
# Authors:  Miles S. Marimbire, University of Cincinnati
#----------------------------------------------------------------

#----------------------------------------------------------------
# This code is open source feel free to add any modification you wish
#----------------------------------------------------------------
# Set up R 
#----------------------------------------------------------------
# Clear environment, which means totally clear R environment
# of objects and loaded packages
rm(list=ls())
# To clear just the console window, type "Ctrl+L" or use Edit pull down menu

# Specify a display option
#options("scipen"=999, digits=2)

# === Set the working directory.  Note that R uses "/" not "\"
# === So the command is setwd("your directory path") or use the Session pull down menu
setwd("~/R/")
# === NOTE:  If using a MAC computer, you might need to replace the above command with something like
# === setwd("Desktop/R/")
#----------------------------------------------------------------

#----------------------------------------------------------------
# List of packages 
packages <- c("AER","car","ggplot2","gmodels", "haven", "jtools", "pastecs", "psych", "skedastic",
              "stargazer", "summarytools","tidyverse", "corrplot", "olsrr", "RNHANES", "dplyr", 
              "gridExtra", "ggthemes","shiny","shinydashboard", "dlookr", "gtools", "scales", "caret", "rvest","xml2","readr")

# Install the packages
# Run this code to install packages you do not have installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) 
{install.packages(packages[!installed_packages])}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))
search()

# Set the number of digits to display when using the jtools commands, like summ
options("jtools-digits"=4) 
options(scipen = 999)

# You can use these methods if you need a example of how to use a package
browseVignettes("AER")  # Short documents on how to use the packages
?mean # Opens the help page for the mean function
?"+" #  Opens the help page for addition
?"if" # Opens the help page for if, used for branching code
??plotting #  Searches for topics containing words like "plotting"
??"regression model" #  Searches for topics containing phrases like this
#--------------------------------------------------------

#--------------------------------------------------------
# Data
#--------------------------------------------------------
# Load in the datasets

### LOAD DATA

united <- read_csv("United_States_COVID-19_Community_Levels_by_County.csv")
View(united)

# New York counties cases per 100k
ny_kings = united$covid_cases_per_100k[united$county_fips == 36047] # Kings County, NY. 
ny_queens = united$covid_cases_per_100k[united$county_fips == 36081] # Queens County, NY. 
ny_ny = united$covid_cases_per_100k[united$county_fips == 36061] # Suffolk County, NY. 
ny_suffolk = united$covid_cases_per_100k[united$county_fips == 36103]  # Bronx County, NY.

# New York 
par(mfrow = c(2,2))
plot(ny_kings, type="b")
plot(ny_queens, type="b", col=2)
plot(ny_ny, type="b",col=3)
plot(ny_suffolk, type="b",col=4)


# California Counties cases per 100k 
ca_los = united$covid_cases_per_100k[united$county_fips == 6037] # Los Angels County, California. 
ca_san = united$covid_cases_per_100k[united$county_fips == 6073] # San Diego County, California. 
ca_orange = united$covid_cases_per_100k[united$county_fips == 6059] # Orange County, California. 
ca_river = united$covid_cases_per_100k[united$county_fips == 6065] # Riverside, California. 

# Cali 
par(mfrow = c(2,2))
plot(ca_los, type="b")
plot(ca_san, type="b", col=2)
plot(ca_orange, type="b",col=3)
plot(ca_river, type="b",col=4)

# Texas Counties cases per 100k 
tex_har = united$covid_cases_per_100k[united$county_fips == 48201] # Harris County, Texas. 
tex_dall = united$covid_cases_per_100k[united$county_fips == 48113] # Dallas County, Texas. 
tex_tar = united$covid_cases_per_100k[united$county_fips == 48439] # Tarrant County, Texas. 
tex_bex = united$covid_cases_per_100k[united$county_fips == 48029] # Bexar County, Texas. 

mean(tex_har)
mean(ca_los)
mean(ny_kings)

# Texas
par(mfrow = c(2,2))
plot(tex_har, type="b")
plot(tex_dall, type="b", col=2)
plot(tex_tar, type="b",col=3)
plot(tex_bex, type="b",col=4)


par(mfrow = c(2,2))
plot(ca_los, type="b")
plot(ny_kings, type="b", col=2)
plot(tex_har, type="b",col=3)


#--------------------------------------------------------

# New York counties admissions cases per 100k
ny_kings_ad = united$covid_hospital_admissions_per_100k[united$county_fips == 36047] # Kings County, NY. 
ny_queens_ad = united$covid_hospital_admissions_per_100k[united$county_fips == 36081] # Queens County, NY. 
ny_ny_ad = united$covid_hospital_admissions_per_100k[united$county_fips == 36061] # Suffolk County, NY. 
ny_suffolk_ad = united$covid_hospital_admissions_per_100k[united$county_fips == 36103]  # Bronx County, NY.

# New York 
par(mfrow = c(2,2))
plot(ny_kings_ad, type="b")
plot(ny_queens_ad, type="b", col=2)
plot(ny_ny_ad, type="b",col=3)
plot(ny_suffolk_ad, type="b",col=4)


# California Counties cases per 100k 
ca_los_ad = united$covid_hospital_admissions_per_100k[united$county_fips == 6037] # Los Angels County, California. 
ca_san_ad = united$covid_hospital_admissions_per_100k[united$county_fips == 6073] # San Diego County, California. 
ca_orange_ad = united$covid_hospital_admissions_per_100k[united$county_fips == 6059] # Orange County, California. 
ca_river_ad = united$covid_hospital_admissions_per_100k[united$county_fips == 6065] # Riverside, California. 

# Cali 
par(mfrow = c(2,2))
plot(ca_los_ad, type="b")
plot(ca_san_ad, type="b", col=2)
plot(ca_orange_ad, type="b",col=3)
plot(ca_river_ad, type="b",col=4)

# Texas Counties cases per 100k 
tex_har_ad = united$covid_hospital_admissions_per_100k[united$county_fips == 48201] # Harris County, Texas. 
tex_dall_ad = united$covid_hospital_admissions_per_100k[united$county_fips == 48113] # Dallas County, Texas. 
tex_tar_ad = united$covid_hospital_admissions_per_100k[united$county_fips == 48439] # Tarrant County, Texas. 
tex_bex_ad = united$covid_hospital_admissions_per_100k[united$county_fips == 48029] # Bexar County, Texas. 

mean(tex_har_ad)
mean(ca_los_ad)
mean(ny_kings_ad)

# Texas
par(mfrow = c(2,2))
plot(tex_har_ad, type="b")
plot(tex_dall_ad, type="b", col=2)
plot(tex_tar_ad, type="b",col=3)
plot(tex_bex_ad, type="b",col=4)


par(mfrow = c(2,2))
plot(ca_los_ad, type="b")
plot(ny_kings_ad, type="b", col=2)
plot(tex_har_ad, type="b",col=3)



cor(tex_har_ad, tex_har)
cor(ca_los, ca_los_ad)
cor(ny_kings_ad, ny_kings)


my = subset(united$covid_hospital_admissions_per_100k[united$county_fips == 48029]) # Bexar County, Texas. 




Texas <- subset(united, county_fips == 48029,
                  select=c(county_population, covid_cases_per_100k,
                           health_service_area_population,
                           covid_inpatient_bed_utilization,
                           covid_hospital_admissions_per_100k))

Cali <- subset(united, county_fips == 6037,
                select=c(county_population, covid_cases_per_100k,
                         health_service_area_population,
                         covid_inpatient_bed_utilization,
                         covid_hospital_admissions_per_100k))
df <- Cali %>%
  mutate( health_service_area_population=as.numeric(health_service_area_population==13214799))

New_York <- subset(united, county_fips == 36047,
                select=c(county_population, covid_cases_per_100k,
                         health_service_area_population,
                         covid_inpatient_bed_utilization,
                         covid_hospital_admissions_per_100k))



reg_1_cali = lm(covid_cases_per_100k~covid_inpatient_bed_utilization+covid_hospital_admissions_per_100k, Cali)
summ(reg_1_cali)

reg_1_texas = lm(covid_cases_per_100k~covid_inpatient_bed_utilization+covid_hospital_admissions_per_100k, Texas)
summ(reg_1_texas)

reg_1_new = lm(covid_cases_per_100k~covid_inpatient_bed_utilization+covid_hospital_admissions_per_100k, New_York)
summ(reg_1_new)

stargazer(reg_1_cali,reg_1_new,reg_1_texas,type="text",align=TRUE,
          title="Covid Cases per 100k OLS Results",
          dep.var.labels = "Compensation Factors",
          keep.stat=c("n","rsq","f","aic","bic"), no.space=FALSE,df=FALSE,
          report="vcsp",notes="Standard errors in parentheses",
          digit.separator = "",digits=3)






