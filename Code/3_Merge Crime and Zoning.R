#3: Merge Crime and Zoning
#Adapted from a prior Rmarkdown file

#Setup chunk
library(tidyverse)
library(readxl)
library(haven)
library(labelled)
library(dplyr)
library(tidyr)
library(writexl)

#Import dataset
crimedat = read.csv("Cleaned crime dataset, combined 2020-2022")
zonedat = read.csv("Zoning_Cleaned.csv")

#make a subset of crime data by year
citycrimedat = crimedat|>
  filter(Agency.Type == 'Cities')

#check that cities are in each year (2022 may have been formatted differently)
citycrimedat|>
  group_by(year)|>
  count(year)

#we know that some cities do not have information for all three years.  
#They will be dropped to preserve the experiment create a subset of cities 
#which have all three years in the data set, to serve as a guide to drop observations which do not
citycrimedat2 = citycrimedat|>
  group_by(Agency.Name, State)|>
  count(year)|>
  count(n)|>
  filter(nn == 3)

#left joining the guide data set to the base dataset, then dropping the counting variables
citycrimedat3 = left_join(citycrimedat2, citycrimedat)
citycrimedat3 = citycrimedat3|>
  select(!n)|>
  select(!nn)

#some extra cleaning of the zoning law dataset, Ann Arbor is being dropped because
#they had two zoning laws in the same year, which may throw off our observations
statecolname = colnames(citycrimedat3[,2])
zonedat2 = zonedat|>
  filter(municipality_name != 'Ann Arbor')
zonedat2 = zonedat2|>
  rename(State = state_name)|>
  rename(Agency.Name = municipality_name)|>
  select(!year)
zonedat2$State = toupper(zonedat2$State)

#combining the city crime data and the zoning data
bigdataset = left_join(citycrimedat3, zonedat2)

#Analysis of the merged data
bigdataset|>
  filter(!is.na(description))|>
  count(Agency.Name)