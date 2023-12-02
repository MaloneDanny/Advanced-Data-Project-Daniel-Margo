library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)
library(readxl)
library(broom)
library(lmtest)
library(stargazer)
library(memisc)
library(webshot)

dat = read.csv("Combined Transformed Vars.csv")

#create a dummy variable which records if an agency received housing reform
dat2 = dat|>
  group_by(Agency.Name)|>
  mutate(reform = ifelse(is.na(reform_mechanism) == TRUE, 0, 1))

dat2$year = as.factor(dat2$year)

#Imports, cleans, and merges unemployment data with the crime/reform data
#https://www.ers.usda.gov/data-products/county-level-data-sets/county-level-data-sets-download-data/
unemp = read_xlsx("Unemployment.xlsx", skip = 4, col_names = TRUE)
unemp2 = unemp|>
  separate_wider_position(FIPS_Code, c(statefip = 2, countyfip = 3))|>
  filter(countyfip == "000")|>
  filter(statefip != "00")
unemp2$State = unemp2$Area_Name
unemp2$State = toupper(unemp2$State)
unemp3 = unemp2|>
  select(State,Unemployment_rate_2020,Unemployment_rate_2021,Unemployment_rate_2022)
unnemp3 = unemp3|>
  pivot_longer(cols = Unemployment_rate_2020:Unemployment_rate_2022)|>
  rename(unemp.rate = value)|>
  separate_wider_delim(cols = name, delim = "_", names = c("a", "b", "c"))|>
  rename(year = c)|>
  select(State, year, unemp.rate)
unemp3$year = as.factor(unemp3$year)
dat3 = merge(dat2, unemp3)

#creates lagged variables for crime rate in agencies as well as average crime rate
dat3 = dat3|>
  group_by(Agency.Name)|>
  arrange(year)|>
  mutate(laggedcrimerate = lag(Total.Crime.Rate),
         laggedpropertyrate = lag(Crimes.Property.Rate),
         laggedpersonsrate = lag(Crimes.Persons.Rate),
         laggedsocietyrate = lag(Crimes.Society.Rate))

dat3 = dat3|>
  group_by(Agency.Name)|>
  mutate(meancrimerate = mean(Total.Crime.Rate),
         meanpropertyrate = mean(Crimes.Property.Rate),
         meanpersonrate = mean(Crimes.Persons.Rate),
         meansocietyrate = mean(Crimes.Society.Rate))
dat3$year = as.factor(dat3$year)
#runs regressions, records the results, displays the results in the console
reg = lm(Total.Crime.Rate ~ reform + State - 1 + year - 1, data = dat3)
reg14 = lm(Crimes.Property.Rate ~ reform + State - 1 + year - 1, data = dat3)
reg15 = lm(Crimes.Persons.Rate ~ reform + State - 1 + year - 1, data = dat3)
reg16 = lm(Crimes.Society.Rate ~ reform + State - 1 + year - 1, data = dat3)
reg2 = lm(Total.Crime.Rate ~ reform + unemp.rate+ State - 1 + year - 1 , data = dat3)
reg3 = lm(Crimes.Property.Rate ~ reform+ unemp.rate + State - 1 + year - 1 , data = dat3)
reg4 = lm(Crimes.Persons.Rate ~ reform+ unemp.rate + State - 1 + year - 1 , data = dat3)
reg5 = lm(Crimes.Society.Rate ~ reform + unemp.rate+ State - 1 + year - 1 , data = dat3)
reg6 = lm(Total.Crime.Rate ~ reform + unemp.rate + laggedcrimerate+ State - 1 + year - 1 , data = dat3)
reg7 = lm(Crimes.Persons.Rate ~ reform + unemp.rate + laggedpersonsrate+ State - 1 + year - 1 , data = dat3)
reg8 = lm(Crimes.Property.Rate ~ reform + unemp.rate + laggedpropertyrate+ State - 1 + year - 1 , data = dat3)
reg9 = lm(Crimes.Society.Rate ~ reform  + unemp.rate + laggedsocietyrate+ State - 1 + year - 1, data = dat3)
reg10 = lm(Total.Crime.Rate ~ reform + unemp.rate + meancrimerate+ State - 1 + year - 1 , data = dat3)
reg11 = lm(Crimes.Persons.Rate ~ reform + unemp.rate + meanpersonrate+ State - 1 + year - 1 , data = dat3)
reg12 = lm(Crimes.Property.Rate ~ reform + unemp.rate + meanpropertyrate+ State - 1 + year - 1 , data = dat3)
reg13 = lm(Crimes.Society.Rate ~ reform + unemp.rate + meansocietyrate+ State - 1 + year - 1 , data = dat3)

#displays the results of each regression by themselves
summary(reg) #reform on crime
summary(reg14) #reform on property crime
summary(reg15) #reform on persons crime
summary(reg16) #reform on society crime
summary(reg2) #reform on crime w/ unemp.rate
summary(reg3) #reform on property crime w/ unemp.rate
summary(reg4) #reform on person crime w/ unemp.rate
summary(reg5) #reform on society crime w/ unemp.rate
summary(reg6) #reform on crime w/ unemp.rate and lagged crime
summary(reg7) #reform on person crime w/ unemp.rate and lagged person crime
summary(reg8) #reform on property crime w/ unemp.rate and lagged property crime
summary(reg9) #reform on society crime w/ unemp.rate and lagged society crime
summary(reg10) #reform on crime w/ unemp.rate and mean crime
summary(reg11) #reform on person crime w/ unemp.rate and mean person crime
summary(reg12) #reform on property crime w/ unemp.rate and mean property crime
summary(reg13) #reform on society crime w/ unemp.rate and mean society crime

#saves the dataset as a .csv file, then reads it to make sure it worked
write_csv(dat3, file = "Combined Unemp Crime Housing")
dat3 = read_csv(file = "Combined Unemp Crime Housing")

#had a thought;  maybe determining the change in crime rate will give more precise results?
#generate new variables which record the change in crime rates
dat4 = dat3|>
  group_by(Agency.Name)|>
  mutate(changecrimerate = Total.Crime.Rate - laggedcrimerate,
         changepropertyrate = Crimes.Property.Rate - laggedpropertyrate,
         changesocietyrate = Crimes.Society.Rate - laggedsocietyrate,
         changepersonsrate = Crimes.Persons.Rate - laggedpersonsrate)
dat4$year = as.factor(dat4$year)

#runs the regressions using change in crime rate as the dependant variable
creg = lm(changecrimerate ~ reform + State - 1 + year - 1, data = dat4)
creg14 = lm(changepropertyrate ~ reform + State - 1 + year - 1, data = dat4)
creg15 = lm(changepersonsrate ~ reform + State - 1 + year - 1, data = dat4)
creg16 = lm(changesocietyrate ~ reform + State - 1 + year - 1, data = dat4)
creg2 = lm(changecrimerate ~ reform + unemp.rate+ State - 1 + year - 1 , data = dat4)
creg3 = lm(changepropertyrate ~ reform+ unemp.rate + State - 1 + year - 1 , data = dat4)
creg4 = lm(changepersonsrate ~ reform+ unemp.rate + State - 1 + year - 1 , data = dat4)
creg5 = lm(changesocietyrate ~ reform + unemp.rate+ State - 1 + year - 1 , data = dat4)
creg6 = lm(changecrimerate ~ reform + unemp.rate + laggedcrimerate+ State - 1 + year - 1 , data = dat4)
creg7 = lm(changepersonsrate ~ reform + unemp.rate + laggedpersonsrate+ State - 1 + year - 1 , data = dat4)
creg8 = lm(changepropertyrate ~ reform + unemp.rate + laggedpropertyrate+ State - 1 + year - 1 , data = dat4)
creg9 = lm(changesocietyrate ~ reform  + unemp.rate + laggedsocietyrate+ State - 1 + year - 1, data = dat4)
creg10 = lm(changecrimerate ~ reform + unemp.rate + meancrimerate+ State - 1 + year - 1 , data = dat4)
creg11 = lm(changepersonsrate ~ reform + unemp.rate + meanpersonrate+ State - 1 + year - 1 , data = dat4)
creg12 = lm(changepropertyrate ~ reform + unemp.rate + meanpropertyrate+ State - 1 + year - 1 , data = dat4)
creg13 = lm(changesocietyrate ~ reform + unemp.rate + meansocietyrate+ State - 1 + year - 1 , data = dat4)

#displays the results of each regression individually
summary(creg) #reform on crime change
summary(creg14) #reform on property crime change
summary(creg15) #reform on persons crime change
summary(creg16) #reform on society crime change
summary(creg2) #reform on crime w/ unemp.rate change
summary(creg3) #reform on property crime w/ unemp.rate change
summary(creg4) #reform on person crime w/ unemp.rate change
summary(creg5) #reform on society crime w/ unemp.rate change
summary(creg6) #reform on crime w/ unemp.rate and lagged crime change
summary(creg7) #reform on person crime w/ unemp.rate and lagged person crime change
summary(creg8) #reform on property crime w/ unemp.rate and lagged property crime change
summary(creg9) #reform on society crime w/ unemp.rate and lagged society crime change
summary(creg10) #reform on crime w/ unemp.rate and mean crime change
summary(creg11) #reform on person crime w/ unemp.rate and mean person crime change
summary(creg12) #reform on property crime w/ unemp.rate and mean property crime change
summary(creg13) #reform on society crime w/ unemp.rate and mean society crime change

#exports the results of all like regressions into a pleasant format
stargazer(reg, reg2,reg6, reg10, type = "html", out = "outv2.html" , omit = c("year","State"))
stargazer(reg14, reg3, reg8, reg12, type = "html", out = "out2v2.html", omit = c("year","State"))
stargazer(reg15, reg4, reg7, reg11, type = "html", out = "out3v2.html", omit = c("year","State"))
stargazer(reg16, reg5, reg9, reg13, type = "html", out = "out4v2.html", omit = c("year","State"))
stargazer(creg, creg2, creg10, type = "html", out = "out5v2.html", omit = c("year","State"))
stargazer(creg14, creg3, creg12, type = "html", out = "out6v2.html", omit = c("year","State"))
stargazer(creg15, creg4, creg11, type = "html", out = "out7v2.html", omit = c("year","State"))
stargazer(creg16, creg5, creg13, type = "html", out = "out8v2.html", omit = c("year","State"))

#displays the results of all like regressions into a pleasant format
stargazer(reg, reg2,reg6, reg10, type = "text" , omit = c("year","State"))
stargazer(reg14, reg3, reg8, reg12, type = "text", omit = c("year","State"))
stargazer(reg15, reg4, reg7, reg11, type = "text", omit = c("year","State"))
stargazer(reg16, reg5, reg9, reg13, type = "text", omit = c("year","State"))
stargazer(creg, creg2, creg10, type = "text", omit = c("year","State"))
stargazer(creg14, creg3, creg12, type = "text", omit = c("year","State"))
stargazer(creg15, creg4, creg11, type = "text", omit = c("year","State"))
stargazer(creg16, creg5, creg13, type = "text", omit = c("year","State"))

#generates .png files of each of the result tables
webshot(file = "crimerate.png", url = "file:///C:/Users/malon/OneDrive/Documents/GitHub/Advanced-Data-Project-Daniel-Margo/Data/Regression%20Outputs/outv2.html")
webshot(file = "propertyrate.png", url = "file:///C:/Users/malon/OneDrive/Documents/GitHub/Advanced-Data-Project-Daniel-Margo/Data/Regression%20Outputs/out2v2.html")
webshot(file = "personsrate.png", url = "file:///C:/Users/malon/OneDrive/Documents/GitHub/Advanced-Data-Project-Daniel-Margo/Data/Regression%20Outputs/out3v2.html")
webshot(file = "societyrate.png", url = "file:///C:/Users/malon/OneDrive/Documents/GitHub/Advanced-Data-Project-Daniel-Margo/Data/Regression%20Outputs/out4v2.html")
webshot(file = "changecrimerate.png", url = "file:///C:/Users/malon/OneDrive/Documents/GitHub/Advanced-Data-Project-Daniel-Margo/Data/Regression%20Outputs/out5v2.html")
webshot(file = "changepropertyrate.png", url = "file:///C:/Users/malon/OneDrive/Documents/GitHub/Advanced-Data-Project-Daniel-Margo/Data/Regression%20Outputs/out6v2.html")
webshot(file = "changepersonsrate.png", url = "file:///C:/Users/malon/OneDrive/Documents/GitHub/Advanced-Data-Project-Daniel-Margo/Data/Regression%20Outputs/out7v2.html")
webshot(file = "changesocietyrate.png", url = "file:///C:/Users/malon/OneDrive/Documents/GitHub/Advanced-Data-Project-Daniel-Margo/Data/Regression%20Outputs/out8v2.html")

#creates the final, more legible table sets, and saves them as .html files
stargazer(creg10, creg12, creg11, creg13, type = "html", omit = c("year","State"),
          covariate.labels = c("Reform", "Unemployment Rate", "Mean Total Crime Rate (Agency)", "Mean Crime Rate Against Property (Agency",
                             "Mean Crime Rate Against Persons (Agency)", "Mean Crime Rate Against Society (Agency)"),
          dep.var.labels = c("Change in Total Crime Rate", "Change in Crime Against Property Rate", "Change in Crime Against Persons Rate",
                             "Change in Crime Against Society Rate"), out = "Regression Outputs/htmls/finalregchangev2.html")
stargazer(reg10, reg12, reg11, reg13, type = "html", omit = c("year","State"),
          covariate.labels = c("Reform", "Unemployment Rate", "Mean Total Crime Rate (Agency)", "Mean Crime Rate Against Property (Agency",
                             "Mean Crime Rate Against Persons (Agency)", "Mean Crime Rate Against Society (Agency)"),
          dep.var.labels = c("Total Crime Rate", "Crime Against Property Rate", "Crime Against Persons Rate",
                             "Crime Against Society Rate"), out = "Regression Outputs/htmls/finalregv2.html")

#displays the regression tables to see what they look like
stargazer(creg10, creg12, creg11, creg13, type = "text", omit = c("year","State"),
          covariate.labels = c("Reform", "Unemployment Rate", "Mean Total Crime Rate (Agency)", "Mean Crime Rate Against Property (Agency",
                               "Mean Crime Rate Against Persons (Agency)", "Mean Crime Rate Against Society (Agency)"),
          dep.var.labels = c("Change in Total Crime Rate", "Change in Crime Against Property Rate", "Change in Crime Against Persons Rate",
                             "Change in Crime Against Society Rate"))
stargazer(reg10, reg12, reg11, reg13, type = "text", omit = c("year","State"),
          covariate.labels = c("Reform", "Unemployment Rate", "Mean Total Crime Rate (Agency)", "Mean Crime Rate Against Property (Agency",
                               "Mean Crime Rate Against Persons (Agency)", "Mean Crime Rate Against Society (Agency)"),
          dep.var.labels = c("Total Crime Rate", "Crime Against Property Rate", "Crime Against Persons Rate",
                             "Crime Against Society Rate"))

#converts the final regression tables into .png format
webshot(file = "Regression Outputs/pngs/finalregv2.png", url = "file:///C:/Users/malon/OneDrive/Documents/GitHub/Advanced-Data-Project-Daniel-Margo/Data/Regression%20Outputs/htmls/finalregv2.html")
webshot(file = "Regression Outputs/pngs/finalchangeregv2.png", url = "file:///C:/Users/malon/OneDrive/Documents/GitHub/Advanced-Data-Project-Daniel-Margo/Data/Regression%20Outputs/htmls/finalregchangev2.html")

#save the new .csv file containing the new transformations
write_csv(dat4, file = "Combined Unemp Crime Housing V2")
