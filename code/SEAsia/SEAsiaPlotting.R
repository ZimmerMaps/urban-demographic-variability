#
# Global Urban Demographic Change and Migration Patterns
#
# Andrew Zimmer
#
# Last Updated: 15 February 2024
#

# load packages and set working directory ####
rm(list = ls())

library(tidyverse)
library(reshape2)
library(GGally)

setwd('/Users/andrewzimmer/Documents/Montana State - Postdoc/Research/Zimmer - Urban Demographic Variability/GitHub - Code and Data')

# load data ####
ucdb_seasia = read.csv('data/SEAsia/SEAsia_GHS_UCDB.csv')
worldpop_stats = read.csv('data/SEAsia/SEAsia_WP_Stats.csv')
worldpop_change = read.csv('data/SEAsia/SEAsia_WP_Change.csv')

# set city size factor levels
worldpop_stats$CitySize <- factor(worldpop_stats$CitySize, levels = c("<50k", "50-100k", "100-300k", "300-500k", "500k-1m", "1m-5m", "5m-10m", ">10m"))
worldpop_change$CitySize <- factor(worldpop_change$CitySize, levels = c("<50k", "50-100k", "100-300k", "300-500k", "500k-1m", "1m-5m", "5m-10m", ">10m"))


# filter out years of interest
Stats2020 = worldpop_stats %>%
  filter(year == 2020)

# Boxplots for Demographic variables and change

Plot2020 = ggplot(Stats2020, aes(x = CitySize,  y = TotalDR)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~country_iso)

Plot2020

PlotChange = ggplot(worldpop_change, aes(x = CitySize,  y = TotalDR_Delta)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~country_iso)

PlotChange

