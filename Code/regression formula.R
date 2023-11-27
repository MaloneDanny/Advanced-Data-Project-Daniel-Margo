library(tidyverse)

dat = read.csv("Combined Transformed Vars.csv")

dat2 = dat|>
  group_by(Agency.Name)|>
  mutate(reform = ifelse(is.na(reform_mechanism) == TRUE, 0, 1))

dat2$year = as.factor(dat2$year)

reg = lm(Total.Crime.Rate ~ reform + State - 1 + year - 1, data = dat2)
summary(reg)         
