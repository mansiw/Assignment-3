#### Initial Setup ####

## Libraries ##

library(stargazer)  # for summary statistics and regression tables
library(magrittr)  # for 'piping': more readable code
library(ggplot2)  # the ggplot2 package provides nice function for plotting
library(arm)  # for the sim() function to simulate model estimates
library(interplot)  # for plotting interactions
library(dplyr)  # for data manipulation

## Loading the Data ##

setwd('/Users/toridykes1/GitHub/Assignment-3')

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

library(countrycode)
#countrycode("Austria", "country.name", "iso2c")

eu$cntry <- countrycode(eu$GEO, "country.name", "iso2c")

## Removing Undesired Countries from ESS Dataset ##
temp <- with(df, which(df$cntry == "AT" | df$cntry == "HR" | df$cntry == "IS" | df$cntry == "LT" | df$cntry == "LU" | df$cntry == "RU" | df$cntry == "TR" | df$cntry == "UA", arr.ind=TRUE))
df <- df[-temp, ]

table(df$cntry) # check to see country names remaining

## Removing unwanted columns from ESS data ##

ESSVariables <-c("cntry", "essround", "polintr","trstprl", "trstplt","trstep","vote","contplt","wrkprty","wrkorg","badge","sgnptit","pbldmn","bctprd","clsprty","mmbprty","edulvla","eisced", "eduyrs", "pdwrk", "edctn","uempla", "uempli","dsbld", "pdjobev", "emplrel", "wrkctra", "wkhtot", "uemp12m", "mbtru")

ESSData <- df[ESSVariables]

table(ESSData$cntry)


## Condensing ESS Data ##

table(ESSData$polintr) # want to drop any response greater than 4

ESSData <- subset(ESSData, ESSData$polintr <= 4 & ESSData$trstprl <=10)


#### Merge Datasets ####

YouthData <- merge(df, eu, by = "cntry", all = T)

YouthData$cntry==JP

