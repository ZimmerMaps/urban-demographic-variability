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
worldpop_stats15 = worldpop_stats %>%
  filter(between(year, 2000, 2015))

city_details = dplyr::select(worldpop_stats, zone, city_name, country_iso, country_name, continent_name, latitude, longitude)
city_details = distinct(city_details) # keep only one set of observations per city


# filter variables to calculate change ####

interim_change = dplyr::select(worldpop_stats,
                               zone, year, 
                               YoungPop, WorkingPop, OldPop, TotalPop,
                               TotalDR, YoungDR, OldDR,
                               WomenCBA, WomanChildR,
                               YoungSR, WorkingSR, OldSR, TotalSR, GeneralFR)


# Calculate change between 2000 and 2020
interim_change20 = interim_change %>%
  filter(year %in% c(2000,2020))

# Define the range of columns to calculate differences (3 to 16)
column_range <- 3:15

# Group by two columns and calculate differences for columns in the specified range
ChangeVariables20 <- interim_change20 %>%
  group_by(zone) %>%
  reframe(across(column_range, ~ . - lag(.), .names = "{.col}_Delta")) %>%
  ungroup() 

column_range <- 3:14

ChangeVariables20 = ChangeVariables20 %>%
  filter(if_any(column_range, ~ !is.na(.)))

# calculate migration variables ####

SumVariables = worldpop_stats %>%
  group_by(zone) %>%
  summarise(SumBirths = sum(AnnualBirths),
            SumDeaths = sum(AnnualDeaths),
            NaturalChange = (SumBirths -SumDeaths))

#merge in to change dataset

ChangeVariables20 = merge(ChangeVariables20, SumVariables, by = "zone")

ChangeVariables20$TotalMigration = ChangeVariables20$TotalPop_Delta - ChangeVariables20$NaturalChange

# add total pop in 2020 to calculate proportion of growth from migration
TotalPop2020 = worldpop_stats %>%
  filter(year == '2020') %>%
  select(zone, TotalPop)

ChangeVariables20 = merge(ChangeVariables20, TotalPop2020, by = "zone")

ChangeVariables20$PercChangeFromMigration = ChangeVariables20$TotalMigration / ChangeVariables20$TotalPop_Delta * 100

# prepare for export ####
ChangeVariables20 = merge(ChangeVariables20, city_details, by = "zone")


# Calculate change between 2000 and 2015
interim_change15 = interim_change %>%
  filter(year %in% c(2000,2015))

# Define the range of columns to calculate differences (3 to 16)
column_range <- 3:15

# Group by two columns and calculate differences for columns in the specified range
ChangeVariables15 <- interim_change15 %>%
  group_by(zone) %>%
  reframe(across(column_range, ~ . - lag(.), .names = "{.col}_Delta")) %>%
  ungroup() 

column_range <- 3:14

ChangeVariables15 = ChangeVariables15 %>%
  filter(if_any(column_range, ~ !is.na(.)))

# calculate migration variables ####

SumVariables = worldpop_stats15 %>%
  group_by(zone) %>%
  summarise(SumBirths = sum(AnnualBirths),
            SumDeaths = sum(AnnualDeaths),
            NaturalChange = (SumBirths -SumDeaths))

#merge in to change dataset

ChangeVariables15 = merge(ChangeVariables15, SumVariables, by = "zone")

ChangeVariables15$TotalMigration = ChangeVariables15$TotalPop_Delta - ChangeVariables15$NaturalChange

# add total pop in 2015 to calculate proportion of growth from migration
TotalPop2015 = worldpop_stats %>%
  filter(year == '2015') %>%
  select(zone, TotalPop)

ChangeVariables15 = merge(ChangeVariables15, TotalPop2015, by = "zone")

ChangeVariables15$PercChangeFromMigration = ChangeVariables15$TotalMigration / ChangeVariables15$TotalPop_Delta * 100

# prepare for export ####
ChangeVariables15 = merge(ChangeVariables15, city_details, by = "zone")


# save
write.csv(ChangeVariables20, 'data/worldpop_ucdb_change_20.csv', row.names = FALSE)
write.csv(ChangeVariables15, 'data/worldpop_ucdb_change_15.csv', row.names = FALSE)
