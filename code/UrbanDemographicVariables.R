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

worldpop_all = read.csv('data/df_worldpop_agesex_all.csv')
deathrate_all = read.csv('data/GriddedDeathRate-UCDB.csv')

# clean and extract useful information for later ####

city_details = dplyr::select(worldpop_all, zone, city_name, country_iso, country_name, continent_name, latitude, longitude)
city_details = distinct(city_details) # keep only one set of observations per city

deathrate_all = dplyr::select(deathrate_all, -c(city_name))
deathrate_all_long <- melt(deathrate_all, id.vars = c("zone"), variable.name = "variable")
deathrate_all_long$variable <- sub("^DeathRate", "", deathrate_all_long$variable)
colnames(deathrate_all_long) = c("zone", "year", "DeathRate")

# calculate total, young, working-age and old population counts for each city and year ####

worldpop_all_wide = worldpop_all %>%
  select(year, age, sum, zone) %>%
  group_by(zone, year, age) %>%
  summarise_each(funs(sum))

worldpop_all_wide$age <- as.factor(worldpop_all_wide$age)
worldpop_all_wide <- spread(worldpop_all_wide, age, sum)

worldpop_all_wide <- worldpop_all_wide %>% 
  rowwise() %>% 
  mutate(YoungPop = sum(c(`0`,`1`,`5`,`10`))) %>%
  mutate(WorkingPop = sum(c(`15`, `20`,`25`,`30`,`35`,`40`,`45`,`50`,`55`,`60`))) %>%
  mutate(OldPop = sum(c(`65`,`70`,`75`,`80`))) %>%
  mutate(TotalPop = sum(c(`0`,`1`,`5`,`10`,`15`, `20`,`25`,`30`,`35`,`40`,`45`,`50`,`55`,`60`,`65`, `70`,`75`,`80`)))

# keep useful information
worldpop_all_wide <- dplyr::select(worldpop_all_wide, zone, year, YoungPop, WorkingPop, OldPop, TotalPop)

# calculate total, young and old dependency ratio for each city and year ####

worldpop_all_wide = worldpop_all_wide %>%
  mutate(TotalDR = ((YoungPop + OldPop)/WorkingPop),
         YoungDR = (YoungPop/WorkingPop),
         OldDR = (OldPop/WorkingPop))

# calculate total, young, working-age and old sex ratio for each city and year, and woman-child ratio####

sex_ratio = dplyr::select(worldpop_all, zone, year, sex, age, sum)

sex_ratio <- sex_ratio %>%
  tidyr::pivot_wider(
    names_from  = c(age, sex), 
    values_from = c(sum))

sex_ratio <- sex_ratio %>% 
  rowwise() %>% 
  mutate(child_sum = sum(c(`0_m`,`1_m`, `0_f`,`1_f`))) %>%
  mutate(women_childbirthingsum = sum(c(`15_f`, `20_f`,`25_f`,`30_f`,`35_f`,`40_f`,`45_f`))) %>%
  mutate(young_sum_male = sum(c(`0_m`,`1_m`,`5_m`,`10_m`,`15_m`))) %>%
  mutate(young_sum_female = sum(c(`0_f`,`1_f`,`5_f`,`10_f`,`15_f`))) %>%
  mutate(working_sum_male = sum(c(`20_m`,`25_m`,`30_m`,`35_m`,`40_m`,`45_m`,`50_m`,`55_m`,`60_m`,`65_m`))) %>%
  mutate(working_sum_female = sum(c(`20_f`,`25_f`,`30_f`,`35_f`,`40_f`,`45_f`,`50_f`,`55_f`,`60_f`,`65_f`))) %>%
  mutate(old_sum_male = sum(c(`70_m`,`75_m`,`80_m`))) %>%
  mutate(old_sum_female = sum(c(`70_f`,`75_f`,`80_f`))) %>%
  mutate(total_pop_male = sum(c(`0_m`,`1_m`,`5_m`,`10_m`,`15_m`, `20_m`,`25_m`,`30_m`,`35_m`,`40_m`,`45_m`,`50_m`,`55_m`,`60_m`,`65_m`, `70_m`,`75_m`,`80_m`))) %>%
  mutate(total_pop_female = sum(c(`0_f`,`1_f`,`5_f`,`10_f`,`15_f`, `20_f`,`25_f`,`30_f`,`35_f`,`40_f`,`45_f`,`50_f`,`55_f`,`60_f`,`65_f`, `70_f`,`75_f`,`80_f`))) %>%
  mutate(total_pop = sum(c(`0_m`,`1_m`,`5_m`,`10_m`,`15_m`, `20_m`,`25_m`,`30_m`,`35_m`,`40_m`,`45_m`,`50_m`,`55_m`,`60_m`,`65_m`, `70_m`,`75_m`,`80_m`,
                           `0_f`,`1_f`,`5_f`,`10_f`,`15_f`, `20_f`,`25_f`,`30_f`,`35_f`,`40_f`,`45_f`,`50_f`,`55_f`,`60_f`,`65_f`, `70_f`,`75_f`,`80_f`)))

sex_ratio <- dplyr::select(sex_ratio, zone, year,
                           child_sum, women_childbirthingsum,
                           young_sum_male, young_sum_female, 
                           working_sum_male, working_sum_female, 
                           old_sum_male, old_sum_female, 
                           total_pop_male, total_pop_female,
                           total_pop)

sex_ratio <- sex_ratio %>%
  mutate(WomenChildR = ((child_sum/women_childbirthingsum)*1000)) %>%
  mutate(YoungSR = ((young_sum_male/young_sum_female)*100)) %>%
  mutate(WorkingSR = ((working_sum_male/working_sum_female)*100)) %>%
  mutate(OldSR = ((old_sum_male/old_sum_female)*100)) %>%
  mutate(TotalSR = ((total_pop_male/total_pop_female)*100)) %>%
  select(zone, year, women_childbirthingsum, WomenChildR, YoungSR, WorkingSR, OldSR, TotalSR)

colnames(sex_ratio) = c("zone", "year", "WomenCBA", "WomanChildR", "YoungSR", "WorkingSR", "OldSR", "TotalSR")

worldpop_all_wide <- left_join(worldpop_all_wide, sex_ratio, by = c('zone', 'year'))

# remove rows with incomplete data. This removes those cities/years with NA in population categories 
worldpop_all_wide <- worldpop_all_wide[complete.cases(worldpop_all_wide), ]

# calculate births, deaths, migration ####
InterimTotalPop = worldpop_all %>%
  group_by(zone, year) %>%
  summarise(TotalPop = sum(sum))

InterimBirths = worldpop_all %>%
  filter(age == '0') %>%
  group_by(zone, country_iso, year) %>%
  summarize(SumZeros = sum(sum, na.rm = TRUE))

# number of deaths per year
InterimBirthsPop = merge(InterimBirths, InterimTotalPop, by = c("zone", "year"))
InterimBirthsDeathsPop = merge(InterimBirthsPop, deathrate_all_long, by = c("zone", "year"))

# annual deaths per year
InterimBirthsDeathsPop$AnnualDeaths = ((InterimBirthsDeathsPop$TotalPop/1000) * InterimBirthsDeathsPop$DeathRate)
InterimBirthsDeathsPop$AnnualDeathsChild = ((InterimBirthsDeathsPop$SumZeros/1000) * InterimBirthsDeathsPop$DeathRate)

# births without deaths
InterimBirthsDeathsPop$AnnualBirths = InterimBirthsDeathsPop$SumZeros - InterimBirthsDeathsPop$AnnualDeathsChild

MigrationData = InterimBirthsDeathsPop %>%
  arrange(zone, year) %>%
  group_by(zone) %>%
  mutate(PopChange = c(NA, diff(TotalPop))) %>%
  ungroup()

MigrationData$NaturalChangeInitial = MigrationData$AnnualBirths-MigrationData$AnnualDeaths
MigrationData$MigrationInitial = MigrationData$PopChange-MigrationData$NaturalChangeInitial

MigrationData$MigrationInitialPercentage = MigrationData$MigrationInitial / MigrationData$TotalPop

MigrationData$MigrationBabies = MigrationData$AnnualBirths*MigrationData$MigrationInitialPercentage
MigrationData$AnnualBirthsFinal = MigrationData$AnnualBirths - MigrationData$MigrationBabies

#now re calculate but accounting for children that may have moved, or died

MigrationData$NaturalChangeFinal = MigrationData$AnnualBirthsFinal - MigrationData$AnnualDeaths
MigrationData$MigrationFinal = MigrationData$PopChange-MigrationData$NaturalChangeFinal

# percentage of growth driven by migration
MigrationData$GrowthFromMigrationPercentage = MigrationData$MigrationFinal / MigrationData$PopChange * 100

# net migration rate
MigrationData$NetMigrationRate = MigrationData$MigrationFinal / (MigrationData$TotalPop / 1000)

# adding in births for 2000 to complete the dataset (doesn't account for migration etc.)
MigrationData <- MigrationData %>%
  mutate(AnnualBirthsFinalComplete = case_when(
    between(year, 2001, 2020) ~ AnnualBirthsFinal,
    year == 2000 ~ AnnualBirths,
    TRUE ~ NA_real_
  ))

# merge back together
MigrationDataFinal = dplyr::select(MigrationData,
                                   zone, year, DeathRate, 
                                   AnnualDeaths, AnnualBirthsFinalComplete, NaturalChangeFinal, MigrationFinal, 
                                   GrowthFromMigrationPercentage, NetMigrationRate)

colnames(MigrationDataFinal) = c("zone", "year", "DeathRate", "AnnualDeaths", "AnnualBirths", "NaturalChange", "Migration", "PercChangeFromMigration", "NetMigrationRate")

worldpop_all_wide = merge(worldpop_all_wide, MigrationDataFinal, by = c("zone", "year"))

# calculate general fertility rate (number of births / women 15-49) ####

worldpop_all_wide$GeneralFR = worldpop_all_wide$AnnualBirths / worldpop_all_wide$WomenCBA * 1000

# finalize data and prepare for export ####
# merge back in city details
worldpop_all_wide <- merge(worldpop_all_wide, city_details, by = c('zone'), all.x=TRUE)

write.csv(worldpop_all_wide, 'data/worldpop_ucdb_stats.csv', row.names = FALSE)
