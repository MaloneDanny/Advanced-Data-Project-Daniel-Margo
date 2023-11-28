#4: Transforming Variables
#Adapted from a prior Rmarkdown file

#Libraries Needed
library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(sf)
library(sp)
library(ggplot2)

#Bring in the csv of the dataset.
big_dataset = read_csv("Combined Crime and Zoning Reform", show_col_types = FALSE)

#Transforming variables:
hist(big_dataset$Population1)
#Log transform population
data_1 <- big_dataset |>
  mutate(Ln.Pop = log(Population1))

#Creating a per-capital Total Offenses variable
data_2 <- data_1 |>
  mutate(Total.Offenses.Pc = Total.Offenses/Population1) 
#If we are interested in the rate at which the population has total offenses

#Creating a total offenses rate variable per 1,000 residents - multiply the total offenses per-capita by 1,000
data_2 <- data_2 |>
  mutate(Total.Crime.Rate = Total.Offenses.Pc*1000)

#Conducting the same transformations for the crimes against persons, property, and society
data_3 <- data_2 |>
  mutate(Crimes.Persons.Pc = Crimes.Against.Persons/Population1,
         Crimes.Society.Pc = Crimes.Against.Society/Population1,
         Crimes.Property.Pc = Crimes.Against.Property/Population1)

data_3 <- data_3 |>
  mutate(Crimes.Persons.Rate = Crimes.Persons.Pc*1000,
         Crimes.Society.Rate = Crimes.Society.Pc*1000,
         Crimes.Property.Rate = Crimes.Property.Pc*1000)

class(data_3$Crimes.Persons.Rate) #Checking class
class(data_3$Crimes.Persons.Pc)

#Saving data as a csv for ease of access
write.csv(data_3, "Combined Transformed Vars.csv")
