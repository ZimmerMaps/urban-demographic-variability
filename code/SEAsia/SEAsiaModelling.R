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

totalpop_2015_data = worldpop_stats %>%
  filter(year %in% c(2015)) %>%
  select(zone, TotalPop) %>%
  rename(TotalPop2015 = TotalPop)

worldpop_change2015 = worldpop_stats %>%
  filter(year %in% c(2000, 2015)) %>%
  select(zone, year, TotalPop, TotalDR) %>%
  pivot_wider(names_from = year, values_from = c(TotalPop, TotalDR))

worldpop_change2015$TotalPopChange = worldpop_change2015$TotalPop_2015 - worldpop_change2015$TotalPop_2000
worldpop_change2015$TotalDRChange = worldpop_change2015$TotalDR_2015 - worldpop_change2015$TotalDR_2000

ucdb_seasia_selected =
  dplyr::select(ucdb_seasia, 
                ID_HDC_G0, #ID
                GCPNT_LAT, #Latitude
                GCPNT_LON, #Longitude
                AREA, #area km2
                CTR_MN_ISO, #ISO3 of main country
                CTR_MN_NM, #Name of main country
                UC_NM_MN, #Name of city
                EL_AV_ALS, #Average elevation
                E_WR_T_90, #mean temp 1990
                E_WR_T_14, #mean temp 2015
                E_WR_P_90, #mean precip 1990
                E_WR_P_14, #mean precip 2015
                B90, #total built up 1990
                B15, #total built up 2015
                NTL_AV, #Night time lights
                GDP90_SM, #sum gdp ppp 1990
                GDP15_SM, #sum gdp ppp 2015
                TT2CC, #travel time to country capital
                E_GR_AV90, #mean greenness 1990
                E_GR_AV14, #mean greenness 2014
                EX_FD_B90, #built up area exposed to floods in 1990
                EX_FD_B15, #built up area exposed to floods in 2015
                EX_HW_IDX #max magnitude of heatwaves (1980-2010)
  )

# rename variables
colnames(ucdb_seasia_selected) <- c("urbanid", 
                                    "latitude",
                                    "longitude",
                                    "area",
                                    "country_iso",
                                    "country_name",
                                    "city_name",
                                    "elevation",
                                    "mean_temp1990",
                                    "mean_temp2015",
                                    "mean_precip1990",
                                    "mean_precip2015", 
                                    "built1990",
                                    "built2015",
                                    "ntl",
                                    "sum_gdp1990",
                                    "sum_gdp2015",
                                    "tt_capital",
                                    "mean_green1990",
                                    "mean_green2015",
                                    "flood_exp1990",
                                    "flood_exp2015",
                                    "heatwave")
                

# calculate change in variables ####

# climate variables
ucdb_seasia_selected$mean_temp_change = ucdb_seasia_selected$mean_temp2015 - ucdb_seasia_selected$mean_temp1990
ucdb_seasia_selected$mean_precip_change = ucdb_seasia_selected$mean_precip2015 - ucdb_seasia_selected$mean_precip1990


# built up area
ucdb_seasia_selected$built_perc1990 = ucdb_seasia_selected$built1990 / ucdb_seasia_selected$area * 100
ucdb_seasia_selected$built_perc2015 = ucdb_seasia_selected$built2015 / ucdb_seasia_selected$area * 100

ucdb_seasia_selected$built_change = ucdb_seasia_selected$built_perc2015 - ucdb_seasia_selected$built_perc1990

# gdp
ucdb_seasia_selected = merge(ucdb_seasia_selected, totalpop_2015_data, by.x = "urbanid", by.y = "zone")

ucdb_seasia_selected$gdp_percap1990 = ucdb_seasia_selected$sum_gdp1990 / ucdb_seasia_selected$TotalPop2015
ucdb_seasia_selected$gdp_percap2015 = ucdb_seasia_selected$sum_gdp2015 / ucdb_seasia_selected$TotalPop2015

ucdb_seasia_selected$gdp_percap_change = ucdb_seasia_selected$gdp_percap2015 - ucdb_seasia_selected$gdp_percap1990

# greenness
ucdb_seasia_selected$mean_green_change = ucdb_seasia_selected$mean_green2015 - ucdb_seasia_selected$mean_green1990

# flood exposure
ucdb_seasia_selected$flood_exp_perc1990 = ucdb_seasia_selected$flood_exp1990 / ucdb_seasia_selected$area * 100
ucdb_seasia_selected$flood_exp_perc2015 = ucdb_seasia_selected$flood_exp2015 / ucdb_seasia_selected$area * 100

ucdb_seasia_selected$flood_exp_change = ucdb_seasia_selected$flood_exp_perc2015 - ucdb_seasia_selected$flood_exp_perc1990


# merge in change data
seasia_variables = merge(ucdb_seasia_selected, worldpop_change2015, by.x = "urbanid", by.y = "zone")

#ggplot to show distribution of variables
ggplot(seasia_variables, aes(x = TotalDRChange)) +
  geom_density() +
  theme_bw()

# set up regression model
lm1 = lm(TotalPopChange ~ 
           
           elevation +
           
           mean_temp2015 +
           mean_temp_change +
           
           mean_precip2015 +
           mean_precip_change +
           
           built2015 +
           built_change +
           
           ntl +
           
           gdp_percap2015 +
           gdp_percap_change +
           
           tt_capital +
           
           mean_green2015 +
           mean_green_change + 
           
           flood_exp2015 +
           flood_exp_change + 
           
           heatwave +
           
           TotalPop_2000 +
           TotalDR_2015 +
           
           country_iso, 
         data = seasia_variables)

summary(lm1)
predictions <- predict(lm1, newdata = seasia_variables)

plot_data <- data.frame(
  Actual = seasia_variables$TotalPopChange,
  Predicted = predictions
)

ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted",
       x = "Actual Values",
       y = "Predicted Values") +
  scale_x_log10() +
  scale_y_log10()




lm2 = lm(TotalDRChange ~ 
           elevation +
           
           mean_temp2015 +
           mean_temp_change +
           
           mean_precip2015 +
           mean_precip_change +
           
           built2015 +
           built_change +
           
           ntl +
           
           gdp_percap2015 +
           gdp_percap_change +
           
           tt_capital +
           
           mean_green2015 +
           mean_green_change + 
           
           flood_exp2015 +
           flood_exp_change + 
           
           heatwave +
           
           TotalPop_2015 +
           TotalDR_2000 +
           
           country_iso, 
         data = seasia_variables)

summary(lm2)

predictions <- predict(lm2, newdata = seasia_variables)

plot_data <- data.frame(
  Actual = seasia_variables$TotalDRChange,
  Predicted = predictions
)

ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted",
       x = "Actual Values",
       y = "Predicted Values") 















