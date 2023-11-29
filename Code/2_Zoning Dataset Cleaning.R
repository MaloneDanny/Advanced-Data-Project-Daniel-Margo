#2: Zoning Datset Cleaning
#Adapted from a prior Rmarkdown file

#Libraries needed:
library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(sf)
library(sp)
library(ggplot2)

#Bring in the tracked zoning spreadsheet:
zoning_changes = read_csv("Zoning Tracker.csv", show_col_types = FALSE)

#Find a good key (for later, when going from long to wide)!
dups_zoning <- zoning_changes |>
  count(time_status, legislative_number_policy_name) |> #
#Some cities had multiple policies that went into effect on the same day, so this is a good key for later. 
#For merging datasets later on, we will do so by municipality_name
  filter(n>1) 
stopifnot(nrow(dups_zoning) == 0)

#The data looks pretty darn clean, except for some issues that might cause future merging issues (accents)
#and splitting up the reform type into multiple columns/or making it tidy if needed. 
str(zoning_changes)

#Getting rid of the accent
zoning_changes_clean <- zoning_changes |>
  mutate(municipality_name = str_replace_all(municipality_name,"é","e"))

#Turning into tidy format, if needed
zoning_changes_tidy <- zoning_changes_clean |>
  separate_rows(reform_type, sep = ",") |>
  mutate(reform_type = str_squish(reform_type))

#If I need to pivot wider instead of longer for merging purposes later on, the code for that is the following:
zoning_changes_wide <- zoning_changes_clean |>
  separate_longer_delim(reform_type, delim = ",") |>
  group_by(time_status, legislative_number_policy_name) |>
  pivot_wider(names_from = reform_type, values_from = reform_type) |> 
#That did something funky with multiple columns, so fixing that
  unite(col = "TOD Reform", c("TOD Reform", "TOD Reform")) |>
  unite(col = "Plex Reform", c("Plex Reform", "Plex Reform")) |>
  unite(col = "Other Reform", c("Other Reform", "Other Reform"))

#And....there are dummy variables for these already! :/ Keeping above code for posterity. 
#There is quite a bit of summary information about each proposed/enacted legislation, and I want to do just
#a quick read-through to make sure that these policies are not introducing/expanding SFR zoning. 

summary <- zoning_changes |>
  select(description)
#Easiest way to do a visual scan.
write_excel_csv2(summary, file = "Policy Descriptions.csv") 

#Making a quick visual to make sure the lat/long information is sound.
sf_test <- st_as_sf(zoning_changes_clean, coords = c("longitude", "latitude"))
sp::CRS(SRS_string='EPSG:4326')
ggplot(sf_test) +
  geom_sf(aes(color=reform_phase))

#Note that we only have data on crime statistics for 2020-2022, so I want to subset this data to only include
#zoning changes for years 2020 and 2021. 

zoning_cleaned <- zoning_changes_clean |>
  filter(year == "2021") |>
#This city had two distinct zoning laws that passed in 2021; 
#we will be able to merge this on to the FBI crime statistics dataset. As such, we are excluding it
  filter(municipality_name != "Ann Arbor") 

#Saving the data file
write.csv(zoning_cleaned, "Zoning_Cleaned.csv")
