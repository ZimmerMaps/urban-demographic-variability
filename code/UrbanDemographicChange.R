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

city_details = dplyr::select(worldpop_stats, zone, city_name, country_iso, country_name, continent_name, latitude, longitude)
city_details = distinct(city_details) # keep only one set of observations per city


# filter variables to calculate change ####

interim_change = dplyr::select(worldpop_stats,
                               zone, year, 
                               YoungPop, WorkingPop, OldPop, TotalPop,
                               TotalDR, YoungDR, OldDR,
                               WomenCBA, WomanChildR,
                               YoungSR, WorkingSR, OldSR, TotalSR, GeneralFR)

# keep only 2000 and 2020 to calculate the change
interim_change = interim_change %>%
  filter(year %in% c(2000,2020))

# Define the range of columns to calculate differences (3 to 16)
column_range <- 3:15

# Group by two columns and calculate differences for columns in the specified range
ChangeVariables <- interim_change %>%
  group_by(zone) %>%
  reframe(across(column_range, ~ . - lag(.), .names = "{.col}_Delta")) %>%
  ungroup() %>%
  filter(complete.cases(.))


# calculate migration variables ####

SumVariables = worldpop_stats %>%
  group_by(zone) %>%
  summarise(SumBirths = sum(AnnualBirths),
            SumDeaths = sum(AnnualDeaths),
            NaturalChange = (SumBirths -SumDeaths))

#merge in to change dataset

ChangeVariables = merge(ChangeVariables, SumVariables, by = "zone")

ChangeVariables$TotalMigration = ChangeVariables$TotalPop_Delta - ChangeVariables$NaturalChange

# add total pop in 2020 to calculate proportion of growth from migration
TotalPop2020 = worldpop_stats %>%
  filter(year == '2020') %>%
  select(zone, TotalPop)

ChangeVariables = merge(ChangeVariables, TotalPop2020, by = "zone")

ChangeVariables$PercChangeFromMigration = ChangeVariables$TotalMigration / ChangeVariables$TotalPop_Delta * 100

# prepare for export ####
ChangeVariables = merge(ChangeVariables, city_details, by = "zone")

# keep only complete cases
ChangeVariables <- ChangeVariables[complete.cases(ChangeVariables), ]

# save
write.csv(ChangeVariables, 'data/worldpop_ucdb_change.csv', row.names = FALSE)
