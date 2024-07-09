rm(list = ls())

# Load Packages ####
library(tidyverse)
library(rnaturalearth)
library(scales)
library(ggnewscale)

# Load Raw Data ####
setwd('/Users/andrewzimmer/Documents/Montana State - Postdoc/Research/Zimmer - Urban Demographic Variability/GitHub - Code and Data')

worldpop_all = read.csv('data/df_worldpop_agesex_all.csv')
worldpop_change15 = read.csv('data/worldpop_ucdb_change_15.csv')
worldpop_change20 = read.csv('data/worldpop_ucdb_change_20.csv')
worldpop_stats = read.csv('data/worldpop_ucdb_stats.csv')

# Filter out 2015
worldpop_stats15 = filter(worldpop_stats, year == 2015)

# Add map for plotting
world <- ne_countries(scale = "medium", returnclass = "sf")

# Figure 1 - Dependency Ratio & Change ####
colors <- c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#F4A582", "#D6604D", "#B2182B", "#67001F")

# TOTAL DR
worldpop_stats15 <- worldpop_stats15 %>%
  arrange(TotalDR)

dependency_ratio_map <- ggplot() +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_point(data = worldpop_stats15, aes(x = longitude, y = latitude, color = TotalDR),
             size = 0.2) +
  scale_y_continuous(limits = c(-55,90)) +
  scale_color_stepsn(colors = colors,
                     values = rescale(c(0.2,1,1.2)),
                     limits = c(0.2,1.2), n.breaks = 10) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Total Dependency Ratio", color = "") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80"))

dependency_ratio_map

# YOUNG DR

worldpop_stats15 <- worldpop_stats15 %>%
  arrange(YoungDR)

young_dependency_ratio_map <- ggplot() +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_point(data = worldpop_stats15, aes(x = longitude, y = latitude, color = YoungDR), 
             size = 0.2) +
  scale_y_continuous(limits = c(-55,90)) +
  scale_color_stepsn(colors = colors,
                     values = rescale(c(0.2,1,1.2)),
                     limits = c(0.2,1.2), n.breaks = 10) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Young Dependency Ratio", color = "") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80"))

young_dependency_ratio_map

# OLD DR

worldpop_stats15 <- worldpop_stats15 %>%
  arrange(OldDR)

old_dependency_ratio_map <- ggplot() +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_point(data = worldpop_stats15, aes(x = longitude, y = latitude, color = OldDR), 
             size = 0.2) +
  scale_y_continuous(limits = c(-55,90)) +
  scale_color_stepsn(colors = colors,
                     values = rescale(c(0.2,1,1.2)),
                     limits = c(0.2,1.2), n.breaks = 10) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Old Dependency Ratio", color = "") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80"))

old_dependency_ratio_map

# TOTAL DR CHANGE
colors <- c("#40004B", "#762A83", "#9970AB", "#C2A5CF", "#E7D4E8", "#D9F0D3", "#A6DBA0", "#5AAE61", "#1B7837", "#00441B")

worldpop_change15 <- worldpop_change15 %>%
  arrange(TotalDR_Delta)

total_d_ratio_delta_map <- ggplot(data = world) +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_point(data = worldpop_change15, aes(x = longitude, y = latitude, color = TotalDR_Delta), 
             size = 0.2) +
  scale_color_stepsn(colors = colors,
                     values = rescale(c(-0.4,0,0.4)),
                     limits = c(-0.4,0.4), n.breaks = 10) +
  scale_y_continuous(limits = c(-55,90)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Change in Young Dependency Ratio 2000-2020", color = "") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80"))

total_d_ratio_delta_map

# YOUNG DR CHANGE

worldpop_change15 <- worldpop_change15 %>%
  arrange(YoungDR_Delta)

young_d_ratio_delta_map <- ggplot(data = world) +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_point(data = worldpop_change15, aes(x = longitude, y = latitude, color = YoungDR_Delta), 
             size = 0.2) +
  scale_color_stepsn(colors = colors,
                     values = rescale(c(-0.4,0,0.4)),
                     limits = c(-0.4,0.4), n.breaks = 10) +
  scale_y_continuous(limits = c(-55,90)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Change in Young Dependency Ratio 2000-2020", color = "") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80"))

young_d_ratio_delta_map

# OLD DR CHANGE

worldpop_change15 <- worldpop_change15 %>%
  arrange(OldDR_Delta)

old_d_ratio_delta_map <- ggplot() +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_point(data = worldpop_change15, aes(x = longitude, y = latitude, color = OldDR_Delta), 
             size = 0.2) +
  scale_color_stepsn(colors = colors,
                     values = rescale(c(-0.4,0,0.4)),
                     limits = c(-0.4,0.4), n.breaks = 10) +
  scale_y_continuous(limits = c(-55,90)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Change in Old Dependency Ratio 2000-2020", color = "") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80"))

old_d_ratio_delta_map

# Figure 2 - Working Age Sex Ratio and Change ####

# WORKING AGE SEX RATIO

colors <- c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#F4A582", "#D6604D", "#B2182B", "#67001F")

worldpop_stats15 <- worldpop_stats15 %>%
  arrange(WorkingSR)

working_sex_ratio_map <- ggplot() +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_point(data = worldpop_stats15, aes(x = longitude, y = latitude, color = WorkingSR), 
             size = 0.2) + 
  scale_y_continuous(limits = c(-55,90)) +
  scale_color_stepsn(colors = colors,
                     values = rescale(c(50,100,400)),
                     limits = c(50,400), n.breaks = 10) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Working-Age Sex Ratio", color = "") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80"))

working_sex_ratio_map

working_sex_ratio_inset_map <- ggplot() +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_point(data = worldpop_stats15, aes(x = longitude, y = latitude, color = WorkingSR), 
             size = 1) +
  scale_y_continuous(limits = c(15,40)) +
  scale_x_continuous(limits = c(20,70)) +
  scale_color_stepsn(colors = colors,
                     values = rescale(c(50,100,400)),
                     limits = c(50,400), n.breaks = 10) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Working-Age Sex Ratio", color = "") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80"))

working_sex_ratio_inset_map

# WORKING AGE SEX RATIO CHANGE

colors <- c("#40004B", "#762A83", "#9970AB", "#C2A5CF", "#E7D4E8", "#D9F0D3", "#A6DBA0", "#5AAE61", "#1B7837", "#00441B")

worldpop_change15 <- worldpop_change15 %>%
  arrange(WorkingSR_Delta)

working_sex_ratio_over50 <- worldpop_change15 %>%
  filter(WorkingSR_Delta > 50)

working_sex_ratio_delta_map <- ggplot(data = world) +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_point(data = worldpop_change15, aes(x = longitude, y = latitude, color = WorkingSR_Delta), 
             size = 0.2) +
  geom_point(data = working_sex_ratio_over50, aes(x = longitude, y = latitude), 
             size = 0.3, color = '#ffc425', shape = 21) +
  scale_y_continuous(limits = c(-55,90)) +
  scale_color_stepsn(colors = colors,
                     values = rescale(c(-10,0,10)),
                     limits = c(-10,10), n.breaks = 12) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Change in Working-Age Sex Ratio 2000-2020", color = "") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80"))

working_sex_ratio_delta_map

working_sex_ratio_delta_inset_map <- ggplot() +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_point(data = worldpop_change15, aes(x = longitude, y = latitude, color = WorkingSR_Delta), 
             size = 1) +
  geom_point(data = working_sex_ratio_over50, aes(x = longitude, y = latitude), 
             size = 1, fill = '#ffc425', color = '#ffc425', ) +
  scale_y_continuous(limits = c(15,40)) +
  scale_x_continuous(limits = c(20,70)) +
  scale_color_stepsn(colors = colors,
                     values = rescale(c(-8,0,8)),
                     limits = c(-8,8), n.breaks = 10) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Working-Age Sex Ratio", color = "") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80"))

working_sex_ratio_delta_inset_map

# Figure 3 - Boxplots ####

boxplot_data <- dplyr::select(worldpop_stats, 
                              zone, year, 
                              YoungDR, OldDR, TotalDR, 
                              TotalPop, continent_name)

boxplot_data <- boxplot_data %>%
  filter(year %in% c(2000,2015))

boxplot_data <- boxplot_data %>% 
  mutate(CitySize = case_when(TotalPop <= 50000 ~ "<50k",
                              TotalPop > 50000 & TotalPop <= 300000 ~ "50-300k",
                              TotalPop > 300000 & TotalPop <= 1000000 ~ "300K-1M",
                              TotalPop > 1000000 & TotalPop <= 5000000 ~ "1-5M",
                              TotalPop > 5000000 ~ ">50M"))

boxplot_data$CitySize <- factor(boxplot_data$CitySize, levels = c("<50k", "50-300k", "300K-1M", "1-5M", ">50M"))
boxplot_data$year <- as.factor(boxplot_data$year)

young_boxplot_facet <- ggplot(boxplot_data, aes(x = CitySize, y = YoungDR, fill = year, color = year)) +
  geom_boxplot(position = "dodge", 
               size = 0.5, lwd = 0.3,
               outlier.shape = 4,
               outlier.colour = NULL) +
  theme_light() +
  scale_fill_manual(values = c("#E1AF00", "#3B9AB2")) +
  scale_color_manual(values = c("grey30", "grey30")) +
  scale_y_continuous(limits = c(0.0, 1.6), breaks = seq(0,1.6,0.2)) +
  facet_wrap(~continent_name) +
  labs(x = "", y = "Young Dependency Ratio", fill = "")  +
  guides(color = FALSE) +
  theme(legend.position = "none",
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
young_boxplot_facet

boxplot_data$overall = "All Urban Locations"
young_boxplot_all <- ggplot(boxplot_data, aes(x = CitySize, y = YoungDR, fill = year, color = year)) +
  geom_boxplot(position = "dodge", 
               size = 0.5, lwd = 0.3,
               outlier.shape = 4,
               outlier.colour = NULL) +
  theme_light() +
  scale_fill_manual(values = c("#E1AF00", "#3B9AB2")) +
  scale_color_manual(values = c("grey30", "grey30")) +
  labs(x = "", y = "", fill = "", title = "")  +
  guides(color = FALSE) +
  scale_y_continuous(limits = c(0.0, 1.6), breaks = seq(0,1.6,0.2)) +
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~overall) 
young_boxplot_all

panel3ab <- ggpubr::ggarrange(young_boxplot_all, young_boxplot_facet, 
                              nrow = 1, 
                              labels = c("A", "B"),
                              common.legend = T,
                              legend = "bottom")
panel3ab


# Figure 4 - Migration ####

# 4A Map of migration as percentage of 2015 pop

migration_map_data = worldpop_change15
migration_map_data$MigrationProportion = migration_map_data$TotalMigration / migration_map_data$TotalPop * 100

migration_map_data <- migration_map_data %>%
  arrange(MigrationProportion)

map_migration <- ggplot() +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_point(data = migration_map_data, aes(x = longitude, y = latitude, color = MigrationProportion),
             size = 0.2) +
  scale_color_viridis_b(
    limits = c(0,100), n.breaks = 10) +
  scale_y_continuous(limits = c(-55,90)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "", color = "") +
  guides(color = guide_colourbar(direction = "horizontal", 
                                 barwidth = 20, 
                                 barheight = 1.2,
                                 frame.color = "grey80",
                                 ticks.color = "grey80")) 
map_migration


# 4B - Stacked Boxplot

migration_map_data <- migration_map_data %>% 
  mutate(CitySize = case_when(TotalPop <= 50000 ~ "<50k",
                              TotalPop > 50000 & TotalPop <= 300000 ~ "50-300k",
                              TotalPop > 300000 & TotalPop <= 1000000 ~ "300K-1M",
                              TotalPop > 1000000 & TotalPop <= 5000000 ~ "1-5M",
                              TotalPop > 5000000 ~ ">50M"))
migration_map_data$CitySize <- factor(migration_map_data$CitySize, levels = c("<50k", "50-300k", "300K-1M", "1-5M", ">50M"))


stacked_plot_data <- migration_map_data %>%
  filter(! country_name %in% c("China", "India")) %>%
  select(continent_name, CitySize, TotalMigration) %>%
  group_by(CitySize, continent_name) %>%
  summarise(SumTotalMigration = sum(TotalMigration))

china_india_data <- migration_map_data %>%
  filter(country_name %in% c("China", "India")) %>%
  select(country_name, CitySize, TotalMigration) %>%
  group_by(CitySize, country_name) %>%
  summarise(SumTotalMigration = sum(TotalMigration))

colnames(china_india_data) <- c("CitySize", "continent_name", "SumTotalMigration")

stacked_plot_data <- rbind(stacked_plot_data, china_india_data)
stacked_plot_data$continent_name <- reorder(stacked_plot_data$continent_name, stacked_plot_data$SumTotalMigration)
stacked_plot_data$continent_name <- factor(stacked_plot_data$continent_name, levels=c("Oceania", "Northern America", "Europe", "Latin America and the Caribbean", "Africa", "Asia", "India", "China"))

stacked_plot_migration <- ggplot(stacked_plot_data, mapping = aes(x = CitySize, y = SumTotalMigration, fill = continent_name)) +
  geom_bar(stat = "identity", color = 'grey20', lwd = 0.4) +
  scale_fill_manual(values = c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#dd9e56", "#be874a")) +
  theme_bw() +
  scale_y_continuous(labels = comma, breaks = seq(0,500000000, by = 20000000), limits = c(0,140000000)) +
  theme(legend.position = "right",
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "City Size", y = "Total In-Migration", fill = "") 
stacked_plot_migration


# 4C - Map of contribution to change

MigrationProportion = worldpop_change15 %>%
  filter(PercChangeFromMigration < 100 & PercChangeFromMigration > 0)

nat_colors = c('#084081', '#0868ac', '#2b8cbe', '#4eb3d3', '#7bccc4', '#a8ddb5', '#ccebc5', '#e0f3db')
mig_colors = c('#fde0dd', '#fcc5c0', '#fa9fb5', '#f768a1', '#dd3497', '#ae017e', '#7a0177', '#49006a')
my_breaks = c(1000, 10000, 100000, 1000000, 10000000)

MigrationProportion <- MigrationProportion %>%
  mutate(PlotCategory = case_when(
    (TotalPop_Delta > 0 & TotalMigration < NaturalChange & TotalMigration < 0 ~ 1),
    (TotalPop_Delta > 0 & TotalMigration < NaturalChange & TotalMigration > 0 ~ 2),
    (TotalPop_Delta > 0 & TotalMigration > NaturalChange & NaturalChange > 0 ~ 3),
    (TotalPop_Delta > 0 & TotalMigration > NaturalChange & NaturalChange < 0 ~ 4),
    (TotalPop_Delta < 0 & TotalMigration > NaturalChange & TotalMigration > 0 ~ 5),
    (TotalPop_Delta < 0 & TotalMigration > NaturalChange & TotalMigration < 0 ~ 6),
    (TotalPop_Delta < 0 & TotalMigration < NaturalChange & NaturalChange < 0 ~ 7),
    (TotalPop_Delta < 0 & TotalMigration < NaturalChange & NaturalChange > 0 ~ 8)
    
  ))

MigrationProportion$PlotCategory <- as.factor(MigrationProportion$PlotCategory)

fig4d_data_migr <- MigrationProportion %>%
  filter(PlotCategory == 3) 

fig4d_data_nat <- MigrationProportion %>%
  filter(PlotCategory == 2)

mig_v_nat_p1 <- ggplot() +
  geom_point(data = fig4d_data_nat, aes(x = TotalMigration, y = NaturalChange, fill = PercChangeFromMigration), 
             color = 'black', stroke = 0.05, shape = 21) +
  scale_fill_gradientn(colors = nat_colors, limits = c(0,50), breaks = seq(0,50,10)) +
  new_scale_fill() +
  
  geom_point(data = fig4d_data_migr, aes(x = TotalMigration, y = NaturalChange, fill = PercChangeFromMigration),
             color = 'black', stroke = 0.05, shape = 21) +
  scale_fill_gradientn(colors = mig_colors, limits = c(50,100), breaks = seq(50,100,10)) +
  
  theme_bw() +
  scale_x_log10(limits = c(1000,10000000)) +
  scale_y_log10(limits = c(1000,10000000)) +
  geom_abline(slope = 1, intercept = 0) +
  labs(x = "In-Migration", y = "Natural Increase")
mig_v_nat_p1

mig_v_nat_p2 <- ggplot() +
  geom_sf(data = world, fill = "grey50", color = 'black', linewidth = 0.1) +
  geom_point(data = fig4d_data_nat, aes(x = longitude, y = latitude, color = PercChangeFromMigration),
             size = 0.2) +
  scale_color_gradientn(colors = nat_colors, limits = c(0,50), breaks = seq(0,50,10)) +
  new_scale_color() +
  geom_point(data = fig4d_data_migr, aes(x = longitude, y = latitude, color = PercChangeFromMigration),
             size = 0.2) +
  scale_color_gradientn(colors = mig_colors, limits = c(50,100), breaks = seq(50,100,10)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.background = element_rect(fill = "white", color = NA),
        legend.box.background = ) +
  labs(color = "")
mig_v_nat_p2




