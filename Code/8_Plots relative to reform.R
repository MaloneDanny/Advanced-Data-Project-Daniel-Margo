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
library(ggpubr)

dat = read_csv(file = "Combined Unemp Crime Housing V2")

#convert reform into a factor, removes outliers from the graphs.  
#Note that none of the outliers are the reformed cities.
dat$reform = as.factor(dat$reform)
levels(dat$reform)
plotdat = dat|>
  filter(Total.Crime.Rate < 1000,
         Crimes.Society.Rate < 200,
         Crimes.Persons.Rate < 125,
         Crimes.Property.Rate < 200)|>
  mutate(opacity = ifelse(reform == 1, 1, .1))


#Create a series of plots
totalcrimeplot = ggplot(plotdat)+
  geom_point(aes(x = Ln.Pop, y = Total.Crime.Rate, color = reform, alpha = opacity))+
  scale_alpha_identity() + 
  xlab("Population (ln)") +
  ylab("Rate") + 
  labs(title = "Total") + 
  theme(plot.title = element_text(size = 10),
        plot.title.position = "plot",
        axis.text=element_text(size=5), 
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))

changetotalplot = ggplot(plotdat)+
  geom_point(aes(x = Ln.Pop, y = changecrimerate, color = reform, alpha = opacity ))+
  scale_alpha_identity() + 
  xlab("Population (ln)") +
  ylab("Rate Change") + 
  labs(title = "Total") + 
  theme(plot.title = element_text(size = 10),
        plot.title.position = "plot",
        axis.text=element_text(size=5), 
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))

societycrimeplot = ggplot(plotdat)+
  geom_point(aes(x = Ln.Pop, y = Crimes.Society.Rate, color = reform, alpha = opacity))+
  scale_alpha_identity() +
  xlab("Population (ln)") +
  ylab("Rate") + 
  labs(title = "Society") + 
  theme(plot.title = element_text(size = 10),
        plot.title.position = "plot",
        axis.text=element_text(size=5), 
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))

changesocietyplot = ggplot(plotdat)+
  geom_point(aes(x = Ln.Pop, y = changesocietyrate, color = reform, alpha = opacity))+
  scale_alpha_identity() + 
  xlab("Population (ln)") +
  ylab("Rate Change") + 
  labs(title = "Society") + 
  theme(plot.title = element_text(size = 10),
        plot.title.position = "plot",
        axis.text=element_text(size=5), 
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))

personscrimeplot = ggplot(plotdat)+
  geom_point(aes(x = Ln.Pop, y = Crimes.Persons.Rate, color = reform, alpha = opacity))+
  scale_alpha_identity() +
  xlab("Population (ln)") +
  ylab("Rate") + 
  labs(title = "Persons") + 
  theme(plot.title = element_text(size = 10),
        plot.title.position = "plot",
        axis.text=element_text(size=5), 
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))

changepersonsrate = ggplot(plotdat)+
  geom_point(aes(x = Ln.Pop, y = changepersonsrate, color = reform, alpha = opacity))+
  scale_alpha_identity() +
  xlab("Population (ln)") +
  ylab("Rate Change") + 
  labs(title = "Persons") + 
  theme(plot.title = element_text(size = 10),
        plot.title.position = "plot",
        axis.text=element_text(size=5), 
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))

propertycrimeplot = ggplot(plotdat)+
  geom_point(aes(x = Ln.Pop, y = Crimes.Property.Rate, color = reform, alpha = opacity))+
  scale_alpha_identity() +
  xlab("Population (ln)") +
  ylab("Rate") + 
  labs(title = "Property") + 
  theme(plot.title = element_text(size = 10),
        plot.title.position = "plot",
        axis.text=element_text(size=5), 
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))

changepropertyplot = ggplot(plotdat)+
  geom_point(aes(x = Ln.Pop, y = changepropertyrate, color = reform, alpha = opacity))+
  scale_alpha_identity() + 
  xlab("Population (ln)") +
  ylab("Rate Change") + 
  labs(title = "Property") + 
  theme(plot.title = element_text(size = 10),
        plot.title.position = "plot",
        axis.text=element_text(size=5), 
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))

#Arranging these plots into figures
figure1 <- ggarrange(totalcrimeplot, societycrimeplot, personscrimeplot, propertycrimeplot,
                     common.legend = TRUE, legend = "bottom",
                     ncol = 2, nrow = 2,
                     font.label=list(color="black",size=9))
figure1 <- annotate_figure(figure1,top = text_grob("Crime Rate vs. Population", 
                                                   color = "black", face = "bold", size = 10))

figure2 <- ggarrange(changetotalplot, changesocietyplot, changepersonsrate, changepropertyplot,
                     common.legend = TRUE, legend = "bottom",
                     ncol = 2, nrow = 2,
                     font.label=list(color="black",size=9))
figure2 <- annotate_figure(figure2,top = text_grob("Crime Rate Change vs. Population", 
                                                   color = "black", face = "bold", size = 10))

#Saving the individual plots
ggsave(file = "Plots/TotalCrimeReform.png", plot = totalcrimeplot)
ggsave(file = "Plots/ChangeTotalCrimeReformPlot.png", plot = changetotalplot)
ggsave(file = "Plots/TotalSocietyReformPlot.png", plot = societycrimeplot)
ggsave(file = "Plots/ChangeSocietyCrimeReformPlot.png", plot = changesocietyplot)
ggsave(file = "Plots/TotalPersonsReformPlot.png", plot = personscrimeplot)
ggsave(file = "Plots/ChangePersonsCrimeReform Plot.png", plot = changepersonsrate)
ggsave(file = "Plots/PropertyCrimeReformPlot.png", plot = propertycrimeplot)
ggsave(file = "Plots/ChangePropertyCrimeReform Plot.png", plot = changepropertyplot)

#Saving the two figures
ggsave(file = "Plots/Figure1.png", plot = figure1, width = 5, height = 5)
ggsave(file = "Plots/Figure2.png", plot = figure2, width = 5, height = 5)
