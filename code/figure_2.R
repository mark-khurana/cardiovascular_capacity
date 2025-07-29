setwd(dir="~/Desktop/cardiovascular_capacity/analysis/data/")
library("scales") # to access nice colour palettes
library(ppcor)
library(corrr)
library(sensitivity)
library(ggplot2)
library(viridis)
library(epiR)
library(dplyr)
library(scales)
library(corrr)
library(sensitivity)
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyr)
library(patchwork)
library(countrycode)
library(forcats)


#iso3 codes for sub-saharan africa
iso3_subsaharan <- c(
  "BFA", "CAF", "STP", "COD", "ETH", "NGA", "SOM", "GAB", "MRT", "SWZ", "LSO",
  "AGO", "BEN", "BWA", "BDI", "CPV", "CMR", "TCD", "CIV", "DJI", "GNQ", "GHA", "GIN", "GNB",
  "KEN", "LBR", "MDG", "MWI", "MLI", "MOZ", "NAM", "RWA", "SEN", "SLE", "ZAF", "SSD", "COG",
  "GMB", "NER", "UGA", "ZMB", "ZWE", "ERI", "TGO", "COM", "TZA", "SYC", "SDN"
)
iso3_colors <- setNames(viridis(length(iso3_subsaharan), option = "D"), iso3_subsaharan)

# Cardiovascular burden data
setwd(dir="~/Desktop/cardiovascular_capacity/analysis/data/Burden_of_disease_2050/")
cardiovascular_burden <- read.csv("cardiovascular_DALYs_2050.csv", stringsAsFactors = FALSE)
unique_locations <- unique(cardiovascular_burden$Location)
iso3_all <- countrycode(unique_locations, origin = "country.name", destination = "iso3c")
location_iso3 <- data.frame(
  Location = unique_locations,
  ISO3 = iso3_all,
  stringsAsFactors = FALSE
)
cardiovascular_burden <- left_join(cardiovascular_burden, location_iso3, by = "Location")
cardiovascular_burden_ssa <- cardiovascular_burden %>%
  filter(ISO3 %in% iso3_subsaharan)

# Ischemic Heart Disease
ischemic_burden <- read.csv("ischaemic_heart_disease_DALYs_2050.csv", stringsAsFactors = FALSE)
unique_locations <- unique(ischemic_burden$Location)
iso3_all <- countrycode(unique_locations, origin = "country.name", destination = "iso3c")
location_iso3 <- data.frame(
  Location = unique_locations,
  ISO3 = iso3_all,
  stringsAsFactors = FALSE
)
ischemic_burden <- left_join(ischemic_burden, location_iso3, by = "Location")
ischemic_burden_ssa <- ischemic_burden %>%
  filter(ISO3 %in% iso3_subsaharan)

# Hypertensive heart disease
hypertensive_burden <- read.csv("hypertensive_heart_disease_DALYs_2050.csv", stringsAsFactors = FALSE)
unique_locations <- unique(hypertensive_burden$Location)
iso3_all <- countrycode(unique_locations, origin = "country.name", destination = "iso3c")
location_iso3 <- data.frame(
  Location = unique_locations,
  ISO3 = iso3_all,
  stringsAsFactors = FALSE
)
hypertensive_burden <- left_join(hypertensive_burden, location_iso3, by = "Location")
hypertensive_burden_ssa <- hypertensive_burden %>%
  filter(ISO3 %in% iso3_subsaharan)

# High LDL
high_LDL_burden <- read.csv("high_LDL_2050.csv", stringsAsFactors = FALSE)
unique_locations <- unique(high_LDL_burden$Location)
iso3_all <- countrycode(unique_locations, origin = "country.name", destination = "iso3c")
location_iso3 <- data.frame(
  Location = unique_locations,
  ISO3 = iso3_all,
  stringsAsFactors = FALSE
)
high_LDL_burden <- left_join(high_LDL_burden, location_iso3, by = "Location")
high_LDL_burden_ssa <- high_LDL_burden %>%
  filter(ISO3 %in% iso3_subsaharan)



# Line plots -----------
cardio_all <- bind_rows(
  cardiovascular_burden_ssa %>% mutate(Measure = "Cardiovascular burden (DALYs/100k)"),
  ischemic_burden_ssa %>% mutate(Measure = "Ischemic Heart Disease (DALYs/100k)"),
  hypertensive_burden_ssa %>% mutate(Measure = "Hypertensive Heart Disease (DALYs/100k)"),
  high_LDL_burden_ssa %>% mutate(Measure = "High LDL Exposure (per 100)")
)
cardio_all <- cardio_all %>%
  select(Location, ISO3, Value, Lower.bound, Upper.bound, Measure) %>%
  group_by(Measure) %>%
  mutate(ISO3 = fct_reorder(ISO3, Value)) %>%
  ungroup()

ggplot(cardio_all, aes(x = Value, y = ISO3, color = Measure)) +
  geom_point() +
  geom_errorbarh(aes(xmin = Lower.bound, xmax = Upper.bound), height = 0) +
  facet_wrap(~ Measure, scales = "free_x", nrow = 1) +
  theme_classic() +
  labs(
    x = "Burden in 2050",
    y = "Country"
  ) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(angle = 0, hjust = 1),
    strip.background = element_rect(fill = "lightgray", color = "black", size = 1),
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    panel.spacing = unit(1, "lines")  # adds space between facets
  )




# Map plots ----------
# Load SSA countries map
world <- ne_countries(scale = "medium", returnclass = "sf")

ssa_iso3 <- c(
  "BFA", "CAF", "STP", "COD", "ETH", "NGA", "SOM", "GAB", "MRT", "SWZ",
  "LSO", "AGO", "BEN", "BWA", "BDI", "CPV", "CMR", "TCD", "CIV", "DJI",
  "GNQ", "GHA", "GIN", "GNB", "KEN", "LBR", "MDG", "MWI", "MLI", "MOZ",
  "NAM", "RWA", "SEN", "SLE", "ZAF", "SSD", "COG", "GMB", "NER", "UGA",
  "ZMB", "ZWE", "ERI", "TGO", "COM", "TZA", "SYC", "SDN"
)

ssa_map <- world %>% filter(iso_a3 %in% ssa_iso3)

make_ssa_map <- function(data, varname, title) {
  map_data <- ssa_map %>%
    left_join(data, by = c("iso_a3" = "ISO3"))
  
  ggplot(map_data) +
    geom_sf(aes_string(fill = varname), color = "white") +
    scale_fill_gradient(
      low = "#f5f0e6",
      high = "#5c3a21",
      na.value = "grey80"
    ) +
    theme_classic() +
    labs(title = title, fill = varname) +
    theme(
      axis.line = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 11)
    )
}



p1 <- make_ssa_map(cardiovascular_burden_ssa, "Value", "Cardiovascular Burden (DALYs/100k)")
p2 <- make_ssa_map(ischemic_burden_ssa, "Value", "Ischemic Heart Disease (DALYs/100k)")
p3 <- make_ssa_map(hypertensive_burden_ssa, "Value", "Hypertensive Heart Disease (DALYs/100k)")
p4 <- make_ssa_map(high_LDL_burden_ssa, "Value", "High LDL Exposure (per 100)")

plot_grid(p1, p2, p3, p4, ncol = 2)


