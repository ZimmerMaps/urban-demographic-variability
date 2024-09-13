rm(list = ls())

library(tidyvserse)
library(dplyr)

data = read.csv("/Users/andrewzimmer/Documents/Montana State - Postdoc/Research/Zimmer - Urban Demographic Variability/Zimmer - Paper 1 - Dem Var/sedacDataSubmission/worldpop_agesex_urban_all.csv")

dataAsia = data %>%
  filter(country_iso %in% c("IDN", "KHM", "LAO", "MYS", "THA", "VNM")) %>%
  filter(year %in% c(2000,2015)) %>%
  select(-c(city_name, country_name, continent_name, latitude, longitude))

dataSum = dataAsia %>%
  group_by(country_iso, age, sex, year) %>%
  summarise(TotalGroupPop = round(sum(sum)))

write.csv(dataSum, "/Users/andrewzimmer/Documents/Montana State - Postdoc/Research/Zimmer - Urban Demographic Variability/GitHub - Code and Data/data/SEAsia/SEAsia-Summary-AgeSex.csv")
