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
library(ggplot2)
library(gganimate) # used for transition_time() and animate()
library(ggrepel)
library(viridis)
library(scales)

setwd('/Users/andrewzimmer/Documents/Montana State - Postdoc/Research/Zimmer - Urban Demographic Variability/GitHub - Code and Data')

# load data ####
ucdb_seasia = read.csv('data/SEAsia/SEAsia_GHS_UCDB.csv')
worldpop_stats = read.csv('data/SEAsia/SEAsia_WP_Stats.csv')
worldpop_change = read.csv('data/SEAsia/SEAsia_WP_Change.csv')

# set city size factor levels
worldpop_stats$CitySize <- factor(worldpop_stats$CitySize, levels = c("<50k", "50-100k", "100-300k", "300-500k", "500k-1m", "1m-5m", "5m-10m", ">10m"))
worldpop_change$CitySize <- factor(worldpop_change$CitySize, levels = c("<50k", "50-100k", "100-300k", "300-500k", "500k-1m", "1m-5m", "5m-10m", ">10m"))

DataVNM = worldpop_stats %>%
  filter(year >= 2000 & year <= 2015)

p = ggplot(DataVNM,
       aes(x = TotalPop, y = YoungDR, color = city_name, size = TotalPop)) +
  geom_hline(yintercept = 1, color = 'grey75', linetype = "dashed") +
  scale_color_viridis_d() +
  geom_point(alpha = 0.5) +
  scale_size(range = c(2, 14)) +
  scale_x_log10(labels = comma, limits = c(5000,50000000)) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  theme(legend.position = 'none',
        axis.title.x = element_text(margin = margin(t = 10), size = 15),
        axis.title.y = element_text(margin = margin(r = 10), size = 15),
        axis.text = element_text(color = "grey10", size = 12)) +
  labs(x = "Total Urban Population", y = "Young Dependency Ratio") +
  transition_time(DataVNM$year) + # this is where you set the variable that makes each frame
  labs(title = paste("Year: {frame_time}")) +
  shadow_mark(alpha = 0.3, size = 0.5) +
  facet_wrap(~country_iso)

animate(p, renderer = gifski_renderer(loop = T), width = 1000, height = 500)

DataVNM14 = DataVNM %>%
  filter(year >= 2000 & year <= 2014)
DataVNM15 = DataVNM %>%
  filter(year == 2015)

ggplot() +
  geom_point(DataVNM14, mapping = aes(x = TotalPop, y = YoungDR, color = city_name), size = 0.5, alpha = 0.2) +
  geom_point(DataVNM15, mapping = aes(x = TotalPop, y = YoungDR, color = city_name, size = TotalPop), alpha = 0.5) +
  geom_hline(yintercept = 1, color = 'grey75', linetype = "dashed") +
  scale_color_viridis_d() +
  geom_point(alpha = 0.5) +
  scale_size(range = c(2, 14)) +
  scale_x_log10(labels = comma, limits = c(5000,50000000)) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  theme(legend.position = 'none',
        axis.title.x = element_text(margin = margin(t = 10), size = 15),
        axis.title.y = element_text(margin = margin(r = 10), size = 15),
        axis.text = element_text(color = "grey10", size = 12)) +
  labs(x = "Total Urban Population", y = "Young Dependency Ratio") +
  facet_wrap(~country_iso)
  
