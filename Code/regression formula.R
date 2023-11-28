library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)
library(readxl)

dat = read.csv("Combined Transformed Vars.csv")

dat2 = dat|>
  group_by(Agency.Name)|>
  mutate(reform = ifelse(is.na(reform_mechanism) == TRUE, 0, 1))

dat2$year = as.factor(dat2$year)

reg = lm(Total.Crime.Rate ~ reform + State - 1 + year - 1, data = dat2)
summary(reg)         

#https://www.ers.usda.gov/data-products/county-level-data-sets/county-level-data-sets-download-data/
unemp = read_xlsx("Unemployment.xlsx", skip = 4, col_names = TRUE)
unemp2 = unemp|>
  separate_wider_position(FIPS_Code, c(statefip = 2, countyfip = 3))|>
  filter(countyfip == "000")|>
  filter(statefip != "00")
unemp2$State = unemp2$Area_Name
unemp2$State = toupper(unemp2$State)
unemp3 = unemp2|>
  select(State, Unemployment_rate_2020, Unemployment_rate_2021, Unemployment_rate_2022)|>
  pivot_longer(cols = Unemployment_rate_2020:Unemployment_rate_2022)|>
  rename(unemp.rate = value)|>
  separate_wider_delim(cols = name, delim = "_", names = c("a", "b", "c"))|>
  rename(year = c)|>
  select(State, year, unemp.rate)
unemp3$year = as.factor(unemp3$year)

dat3 = merge(dat2, unemp3)




reg = lm(Total.Crime.Rate ~ reform + State - 1 + year - 1, data = dat3)
reg2 = lm(Total.Crime.Rate ~ reform + State - 1 + year - 1 + unemp.rate, data = dat3)
reg3 = lm(Crimes.Property.Rate ~ reform + State - 1 + year - 1 + unemp.rate, data = dat3)
reg4 = lm(Crimes.Persons.Rate ~ reform + State - 1 + year - 1 + unemp.rate, data = dat3)
reg5 = lm(Crimes.Society.Rate ~ reform + State - 1 + year - 1 + unemp.rate, data = dat3)
reg6 = lm(Crimes.Persons.Rate ~ reform + State - 1 + year - 1, data = dat3)
summary(reg)
summary(reg2)
summary(reg3)
summary(reg4)
summary(reg5)
summary(reg6)
