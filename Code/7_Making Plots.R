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
library(colorspace)
library(tibble)
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
  mutate(change_crime = 100 * ((lag(meancrimerate, 2)/meancrimerate) -1),
         change_property = 100 * ((lag(meanpropertyrate, 2)/meanpropertyrate) -1),
         change_person = 100 * ((lag(meanpersonrate, 2)/meanpersonrate) -1),
         change_society = 100 * ((lag(meansocietyrate, 2)/meansocietyrate) -1)) |>
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
                                      midpoint = 0,
                                      breaks = c(-50,0,50,100,150),
                                      palette = "-RdYlBu",
                                      title = "Percent Change",
                                      legend.is.portrait = FALSE) + 
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "bottom",
            legend.outside.size = .15,
            legend.title.size = .8,
            main.title = "Percent Change in Mean Crime Rate, 2020-2022",
            main.title.size = .9)
tm_map
tmap_save(tm_map, "States.png")

#Easy breezy map number two
#Do a quick map of all the cities that enacted zoning reform
#Maybe do a quick population comparison

#First get a dataframe with only the reform cities & convert to point layer
dat_9 <- dat_6 |>
  filter(!is.na(latitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = "wgs84")
dat_9 <- st_transform(dat_9, crs = 2163)
#Checking that worked
colnames(dat_9)
qtm(dat_9)
st_crs(dat_9)
st_crs(lower_48)

#Rename Pop variable
dat_9 <- dat_9 |>
  rename(Population = Population1)

#Map showing population
tm_map1 <-tm_shape(lower_48) + tm_polygons(alpha = .5, 
                                 col = "lightgrey") + 
  tm_shape(dat_9) + tm_dots(col = "green",
                            size = "Population",
                            shape = 21,
                            border.col="blue",
                            alpha =.75) + 
  tm_layout(legend.outside = TRUE,
            legend.outside.size = .15,
            legend.title.size = .8,
            legend.outside.position = "bottom",
            main.title = "Cities with Zoning Reform, 2020-2021",
            main.title.size = .9)
tm_map1
tmap_save(tm_map1, "Cities_pop.png")

#Map with population dots + showing total crime rate change
#First, the code - similar process as above
dat_10 <- dat_9 |>
  group_by(Agency.Name, year) |>
  summarise(meancrimerate = mean(Total.Crime.Rate))

dat_11 <- dat_10 |>
  group_by(Agency.Name) |>
  arrange(desc(year), .by_group = TRUE) |>
  mutate(change_crime = ((lag(meancrimerate, 2)/meancrimerate) - 1) * 100) |>
  filter(!is.na(change_crime)) |>
  select(-contains("mean"))

tm_map2 <- tm_shape(lower_48) + tm_polygons(alpha = .5, 
                                 col = "lightgrey") + 
  tm_shape(dat_11) + tm_dots(col = "change_crime",
                            size = 2,
                            shape = 21,
                            border.col="blue",
                            alpha =.75,
                            legend.is.portrait = FALSE,
                            title = "Percent Change in Mean Crime Rate, 2020-2022") + 
  tm_layout(legend.outside = TRUE,
            legend.outside.size = .15,
            legend.title.size = 1,
            legend.outside.position = "bottom",
            main.title = "Cities with Zoning Reform, 2020-2021",
            main.title.size = .9)
tm_map2
tmap_save(tm_map2, "Cities_ratechange.png")

#Making a heatmap
dat_9 <- dat_8 |>
  select(-contains("year"))
#Pivot longer
dat_10 <- dat_9 |>
  pivot_longer(
    cols = "change_crime":"change_society",
    names_to = "Percent_Change",
    values_to = "value") |>
  mutate(Percent_Change = str_remove_all(Percent_Change, "change_")) |>
  mutate(Percent_Change = str_to_upper(Percent_Change))

dat_10 <- dat_10 |>
  filter(State != "PENNSYLVANIA")
  
ggplot(dat_10, aes(x = Percent_Change, y = State, fill = value)) +
  geom_tile() + 
  scale_fill_continuous_divergingx(palette = "-RdYlBu") +
  ylab("State") + 
  xlab("Percent Change in Rate") + 
  theme(axis.text=element_text(size=5),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

