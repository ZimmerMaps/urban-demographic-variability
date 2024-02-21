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

setwd('/Users/andrewzimmer/Documents/Montana State - Postdoc/Research/Zimmer - Urban Demographic Variability/GitHub - Code and Data')

# load data ####
worldpop_stats = read.csv('data/worldpop_ucdb_stats.csv')
worldpop_change = read.csv('data/worldpop_ucdb_change.csv')
ghs_ucdb = read.csv('data/UCDB/ghs_ucdb.csv')

# filter data ####

CountryList = c("THA", "LAO", "KHM", "VNM", "IDN", "MYS")

SEAsia_wp_stats = worldpop_stats %>%
  filter(country_iso %in% CountryList)

SEAsia_wp_change = worldpop_change %>%
  filter(country_iso %in% CountryList)

SEAsia_ghs_ucdb = ghs_ucdb %>%
  filter(CTR_MN_ISO %in% CountryList) %>%
  select(-X_sum)

# export ####
write.csv(SEAsia_wp_stats, 'data/SEAsia/SEAsia_WP_Stats.csv', row.names = FALSE)
write.csv(SEAsia_wp_change, 'data/SEAsia/SEAsia_WP_Change.csv', row.names = FALSE)
write.csv(SEAsia_ghs_ucdb, 'data/SEAsia/SEAsia_GHS_UCDB.csv', row.names = FALSE)