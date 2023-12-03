library(tidyverse)
library(binsreg)
library(tidyverse)
library(readxl)
library(haven)
library(labelled)
library(dplyr)
library(tidyr)
library(writexl)
library(ggplot2)
library(RColorBrewer)

dat = read_csv(file = "Combined Unemp Crime Housing V2")

#convert reform into a factor, removes outliers from the graphs.  Note that none of the outliers are the reformed cities.
dat$reform = as.factor(dat$reform)
plotdat = dat|>
  filter(Total.Crime.Rate < 1000,
         Crimes.Society.Rate < 200,
         Crimes.Persons.Rate < 125,
         Crimes.Property.Rate < 200)|>
  mutate(opacity = ifelse(reform == 1, 1, .1))
         

reformcolor = "No reform"
adjustcolor("grey", alpha.f = .2) #Making 'grey' transparent for the plot
myColors <- brewer.pal(length(levels(plotdat$reform)),"Set1")
names(myColors) <- levels(dat$reform)
myColors[names(myColors)==reform] <- "#BEBEBE26" #If not a reform state, then assign grey color
colScale <- scale_colour_manual(name = "Reform States",values = myColors)

adjustcolor("grey", alpha.f = .2)
reformcolor = brewer.pal(length(levels(plotdat$reform)),"Set1")
names(reformcolor) = levels(plotdat$reform)
reformcolor[names(reformcolor)==0] = "#BEBEBE26"



totalcrimeplot = ggplot(plotdat)+
  geom_point(aes(x = Ln.Pop, y = Total.Crime.Rate, color = reform, alpha = opacity))+
  scale_alpha_identity()
changetotalplot = ggplot(plotdat)+
  geom_point(aes(x = Ln.Pop, y = changecrimerate, color = reform, alpha = opacity ))+
  scale_alpha_identity()
societycrimeplot = ggplot(plotdat)+
  geom_point(aes(x = Ln.Pop, y = Crimes.Society.Rate, color = reform, alpha = opacity))+
  scale_alpha_identity()
changesocietyplot = ggplot(plotdat)+
  geom_point(aes(x = Ln.Pop, y = changesocietyrate, color = reform, alpha = opacity))+
  scale_alpha_identity()
personscrimeplot = ggplot(plotdat)+
  geom_point(aes(x = Ln.Pop, y = Crimes.Persons.Rate, color = reform, alpha = opacity))+
  scale_alpha_identity()
changepersonsrate = ggplot(plotdat)+
  geom_point(aes(x = Ln.Pop, y = changepersonsrate, color = reform, alpha = opacity))+
  scale_alpha_identity()
propertycrimeplot = ggplot(plotdat)+
  geom_point(aes(x = Ln.Pop, y = Crimes.Property.Rate, color = reform, alpha = opacity))+
  scale_alpha_identity()
changepropertyplot = ggplot(plotdat)+
  geom_point(aes(x = Ln.Pop, y = changepropertyrate, color = reform, alpha = opacity))+
  scale_alpha_identity()


totalcrimeplot
changetotalplot
societycrimeplot
changesocietyplot
personscrimeplot
changepersonsrate
propertycrimeplot
changepropertyplot

ggsave(file = "Plots/TotalCrimeReform.png", plot = totalcrimeplot)
ggsave(file = "Plots/ChangeTotalCrimeReformPlot.png", plot = changetotalplot)
ggsave(file = "Plots/TotalSocietyReformPlot.png", plot = societycrimeplot)
ggsave(file = "Plots/ChangeSocietyCrimeReformPlot.png", plot = changesocietyplot)
ggsave(file = "Plots/TotalPersonsReformPlot.png", plot = personscrimeplot)
ggsave(file = "Plots/ChangePersonsCrimeReform Plot.png", plot = changepersonsrate)
ggsave(file = "Plots/PropertyCrimeReformPlot.png", plot = propertycrimeplot)
ggsave(file = "Plots/ChangePropertyCrimeReform Plot.png", plot = changepropertyplot)

