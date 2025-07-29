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
library(stringr)
library(tidyverse)  # includes dplyr, ggplot2, stringr, forcats, etc.
library(countrycode)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(forcats)


#iso3 codes for sub-saharan africa
iso3_subsaharan <- c(
  "BFA", "CAF", "STP", "COD", "ETH", "NGA", "SOM", "GAB", "MRT", "SWZ", "LSO",
  "AGO", "BEN", "BWA", "BDI", "CPV", "CMR", "TCD", "CIV", "DJI", "GNQ", "GHA", "GIN", "GNB",
  "KEN", "LBR", "MDG", "MWI", "MLI", "MOZ", "NAM", "RWA", "SEN", "SLE", "ZAF", "SSD", "COG",
  "GMB", "NER", "UGA", "ZMB", "ZWE", "ERI", "TGO", "COM", "TZA", "SYC", "SDN"
)
iso3_colors <- setNames(viridis(length(iso3_subsaharan), option = "D"), iso3_subsaharan)

# MRI data
setwd(dir="~/Desktop/cardiovascular_capacity/analysis/data/diagnostic_capacity/")
MRI_data <- read.csv("Total_density_per_million_population_MRI.csv", stringsAsFactors = FALSE)
MRI_data_ssa <- MRI_data %>%
  mutate(ISO3 = countrycode(Location, origin = "country.name", destination = "iso3c")) %>%
  filter(ISO3 %in% iso3_subsaharan) %>%
  group_by(ISO3) %>%
  filter(Period == max(Period, na.rm = TRUE)) %>%
  ungroup()

# CT data
CT_data <- read.csv("Total_density_per_million_population_CT.csv", stringsAsFactors = FALSE)
CT_data_ssa <- CT_data %>%
  mutate(ISO3 = countrycode(Location, origin = "country.name", destination = "iso3c")) %>%
  filter(ISO3 %in% iso3_subsaharan) %>%
  group_by(ISO3) %>%
  filter(Period == max(Period, na.rm = TRUE)) %>%
  ungroup()

# PET data
PET_data <- read.csv("Total_density_per_million_population_PET.csv", stringsAsFactors = FALSE)
PET_data_ssa <- PET_data %>%
  mutate(ISO3 = countrycode(Location, origin = "country.name", destination = "iso3c")) %>%
  filter(ISO3 %in% iso3_subsaharan) %>%
  group_by(ISO3) %>%
  filter(Period == max(Period, na.rm = TRUE)) %>%
  ungroup()

# Combine data ----------
MRI_data_ssa$Measure <- "MRI Density"
CT_data_ssa$Measure <- "CT Density"

# Ensure is_latest exists
combined_data <- combined_data %>%
  group_by(ISO3, Measure) %>%
  mutate(is_latest = (Period == max(Period))) %>%
  ungroup()

# Define lighter and darker colors from your palette
# Assuming palette = c("#A0522D", "#CD853F", "#DEB887")
# Let’s use "#DEB887" (light beige) for MRI and "#A0522D" (sienna) for CT

color_MRI <- "#DEB887"  # lighter
color_CT <- "#A0522D"   # darker

# Filter and reorder MRI data
data_MRI <- combined_data %>%
  filter(Measure == "MRI Density") %>%
  mutate(ISO3_ordered = fct_rev(fct_reorder(ISO3, Value, .desc = TRUE)))

# Filter and reorder CT data
data_CT <- combined_data %>%
  filter(Measure == "CT Density") %>%
  mutate(ISO3_ordered = fct_rev(fct_reorder(ISO3, Value, .desc = TRUE)))

# Plot MRI Density with y-axis labels
plot_MRI <- ggplot(data_MRI, aes(x = Value, y = ISO3_ordered)) +
  geom_segment(aes(x = 0, xend = Value, y = ISO3_ordered, yend = ISO3_ordered), color = "gray80") +
  geom_point(color = color_MRI, size = 1, position = position_jitter(height = 0)) +
  geom_text(data = filter(data_MRI, is_latest),
            aes(label = Period), hjust = -0.3, vjust = 0.5, size = 3, color = "gray30") +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.2))) +
  theme_classic() +
  labs(x = "Total Density per Million Population, MRI", y = "Country") +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.border = element_rect(color = "gray50", fill = NA, size = 0.7),
    panel.background = element_rect(fill = "white")
  )

# Plot CT Density with y-axis labels (same as MRI)
plot_CT <- ggplot(data_CT, aes(x = Value, y = ISO3_ordered)) +
  geom_segment(aes(x = 0, xend = Value, y = ISO3_ordered, yend = ISO3_ordered), color = "gray80") +
  geom_point(color = color_CT, size = 1, position = position_jitter(height = 0)) +
  geom_text(data = filter(data_CT, is_latest),
            aes(label = Period), hjust = -0.3, vjust = 0.5, size = 3, color = "gray30") +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.2))) +
  theme_classic() +
  labs(x = "Total Density per Million Population, CT", y = "Country") +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.border = element_rect(color = "gray50", fill = NA, size = 0.7),
    panel.background = element_rect(fill = "white")
  )

# Combine side-by-side
figure_4_v1 <- plot_MRI + plot_CT + 
  plot_layout(ncol = 2, widths = c(1, 1)) +
  plot_annotation(tag_levels = "A")  # Adds tags A, B automatically
figure_4_v1








# Map ---------------
world <- ne_countries(scale = "medium", returnclass = "sf")

ssa_iso3 <- iso3_subsaharan

ssa_map <- world %>% filter(iso_a3 %in% ssa_iso3)

# Join combined data for each measure
join_map <- function(df, varname) {
  ssa_map %>%
    left_join(df %>% select(ISO3, Value), by = c("iso_a3" = "ISO3")) %>%
    mutate(across(all_of(varname), ~forcats::fct_explicit_na(.x, na_level = "Unknown")))
}

make_ssa_map <- function(data, varname, title, year) {
  map_data <- ssa_map %>%
    left_join(data %>% select(ISO3, Value), by = c("iso_a3" = "ISO3")) %>%
    mutate(Value = ifelse(is.na(Value), NA, Value))
  
  ggplot(map_data) +
    geom_sf(aes(fill = Value), color = "white") +
    scale_fill_gradient(
      low = "#f5f0e6",    # light beige
      high = "#5c3a21",   # dark brown
      na.value = "grey80"
    ) +
    theme_classic() +
    labs(
      title = paste0(title, " (", year, ")"),
      fill = "Density per\nMillion Population"
    ) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
}

# For each measure, filter the most recent year
make_map_for_measure <- function(df, measure_name) {
  recent_year <- max(df$Period, na.rm = TRUE)
  df_recent <- df %>% filter(Period == recent_year)
  make_ssa_map(df_recent, "Value", measure_name, recent_year)
}

map_MRI <- make_map_for_measure(MRI_data_ssa, "MRI Density")
map_CT  <- make_map_for_measure(CT_data_ssa, "CT Density")

library(patchwork)
(map_MRI | map_CT) + plot_layout(guides = "collect")






# Save Plots --------
setwd(dir="~/Desktop/cardiovascular_capacity/analysis/figures/")
ggsave("figure_4_v1.pdf",
       figure_4_v1,
       width = 8, height = 6, units = "in", device = cairo_pdf)




