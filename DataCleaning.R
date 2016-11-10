#### Initial Setup ####

## Libraries ##

library(stargazer)  # for summary statistics and regression tables
library(magrittr)  # for 'piping': more readable code
library(ggplot2)  # the ggplot2 package provides nice function for plotting
library(arm)  # for the sim() function to simulate model estimates
library(interplot)  # for plotting interactions
library(dplyr)  # for data manipulation

## Loading the Data ##

d <- read.csv('ESS1-6e01_1_F1.csv') # ESS Data on Political Engagement
e <- read.csv('Under 25 unemp.csv', stringsAsFactors = F) # Eurostat Data on Unemployment


#### Cleaning Up the Data ####

## Limiting to Youth Data ##

df <- subset(d, d$agea <= 25 & d$agea >= 18) # Limit ESS data to indiivduals 18 - 25
eu <- subset(e, e$AGE == "Less than 25 years") # Limit unemployment data to individuals under 25

# Summary of the tables

summary(df$agea)
summary(eu$AGE)

## Filtering the Eurostat Unemployment Data for Relevant Figures and Years ##

eu <- subset(eu, eu$UNIT == "Percentage of active population") # Limit the to single statistic
eu <- subset(eu, eu$TIME == 2006 |eu$TIME == 2008 |eu$TIME == 2010 |eu$TIME == 2012| eu$TIME == 2014) # Limit to only years in which Eurostat survey occurred

## Checking unique countries in each dataset ##

unique(df$cntry)
unique(eu$GEO)

## Drop EU-related observations from unemployment data ##

dropnames <- c("Euro area (18 countries)", "Euro area (19 countries)", 
               "European Union (28 countries)", "Euro area (EA11-2000, EA12-2006, EA13-2007, EA15-2008, EA16-2010, EA17-2013, EA18-2014, EA19)",
               "European Union (25 countries)", "European Union (27 countries)")

eu <- eu[! eu$GEO %in% dropnames, ]

## Change name of Germany ##

eu$GEO[eu$GEO=="Germany (until 1990 former territory of the FRG)"] <- "Germany"

## Add country code to country names for merging ##

install.packages("countrycode")
library(countrycode)
countrycode("Austria", "country.name", "iso2c")

eu$COUNTRY <- countrycode(eu$GEO, "country.name", "iso2c")




