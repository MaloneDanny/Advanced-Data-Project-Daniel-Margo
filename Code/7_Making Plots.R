#7 Making Charts/Maps/Plots - Take Two

#Library needed
library(tidyverse)
library(readxl)
library(haven)
library(labelled)
library(dplyr)
library(tidyr)
library(writexl)
library(ggplot2)
library(RColorBrewer)
library(sf)
library(sp)
library(tmap)
library(shiny)
library(shinyjs)

#Read in the data set that was created in Step 6
dat_6 = read.csv("Combined Unemp Crime Housing V2")
crimedat = read.csv("Cleaned crime dataset, combined 2020-2022")

#Group by state, aggregate up to total mean crime rate stats + calculate rate change
dat_7 <- dat_6 |>
  group_by(State, year) |>
  summarise(meancrimerate = mean(Total.Crime.Rate),
         meanpropertyrate = mean(Crimes.Property.Rate),
         meanpersonrate = mean(Crimes.Persons.Rate),
         meansocietyrate = mean(Crimes.Society.Rate))
dat_8 <- dat_7 |>
  group_by(State) |>
  arrange(desc(year), .by_group = TRUE) |>
  mutate(change_crime = 100 * ((meancrimerate/lag(meancrimerate, 2)) -1),
         change_property = 100 * ((meanpropertyrate/lag(meanpropertyrate, 2)) -1),
         change_person = 100 * ((meanpersonrate/lag(meanpersonrate, 2)) -1),
         change_society = 100 * ((meansocietyrate/lag(meansocietyrate, 2)) -1)) |>
  filter(!is.na(change_crime)) |>
  select(-contains("mean"))

#Read in the USGS state boundaries shapefile
states = st_read("USA_States.shp")
qtm(states)
#Not a fan of that projection - changing to something better
states = st_transform(states, crs = 2163)
qtm(states)
#Making it easier for myself and adding full names
states$NAME = state.name[match(states$STATE_ABBR,state.abb)]
states$NAME = toupper(states$NAME)

#Only the lower 48 - since we dont have data on HI and AK
lower_48 = states |>
  filter(!STATE_ABBR %in% c("AK", "HI"))
qtm(lower_48)

#Easy breezy map
#Add on the crime stats info and then make a map
merged_states = left_join(lower_48, dat_8, by = c("NAME"= "State"))

tm_map <- tm_shape(merged_states) + tm_polygons("change_crime", 
                                      style = "cont",
                                      midpoint = -10,
                                      palette = "plasma",
                                      title = "Rate Change",
                                      legend.is.portrait = FALSE) + 
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.title.size = .8,
            main.title = "Percent Change in Mean Crime Rate, 2020-2022",
            main.title.size = .9)
tm_map
tmap_save(tm_map, "States.png")


