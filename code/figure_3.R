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

#iso3 codes for sub-saharan africa
iso3_subsaharan <- c(
  "BFA", "CAF", "STP", "COD", "ETH", "NGA", "SOM", "GAB", "MRT", "SWZ", "LSO",
  "AGO", "BEN", "BWA", "BDI", "CPV", "CMR", "TCD", "CIV", "DJI", "GNQ", "GHA", "GIN", "GNB",
  "KEN", "LBR", "MDG", "MWI", "MLI", "MOZ", "NAM", "RWA", "SEN", "SLE", "ZAF", "SSD", "COG",
  "GMB", "NER", "UGA", "ZMB", "ZWE", "ERI", "TGO", "COM", "TZA", "SYC", "SDN"
)
iso3_colors <- setNames(viridis(length(iso3_subsaharan), option = "D"), iso3_subsaharan)

# Statin availability data
setwd(dir="~/Desktop/cardiovascular_capacity/analysis/data/statin_and_procedures/")
statin_data <- read.csv("public_sector_statin_availability.csv", stringsAsFactors = FALSE)
statin_data <- statin_data %>%
  group_by(Entity) %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  ungroup()
ssa_entity_to_iso3 <- c(
  "Angola" = "AGO",
  "Benin" = "BEN",
  "Botswana" = "BWA",
  "Burkina Faso" = "BFA",
  "Burundi" = "BDI",
  "Cabo Verde" = "CPV",               # Appears as "Cape Verde" in your data
  "Cameroon" = "CMR",
  "Central African Republic" = "CAF",
  "Chad" = "TCD",
  "Comoros" = "COM",
  "Congo" = "COG",                    # Means "Republic of the Congo"
  "Cote d'Ivoire" = "CIV",
  "Democratic Republic of Congo" = "COD",
  "Djibouti" = "DJI",
  "Equatorial Guinea" = "GNQ",
  "Eritrea" = "ERI",
  "Eswatini" = "SWZ",                 # Was Swaziland
  "Ethiopia" = "ETH",
  "Gabon" = "GAB",
  "Gambia" = "GMB",
  "Ghana" = "GHA",
  "Guinea" = "GIN",
  "Guinea-Bissau" = "GNB",
  "Kenya" = "KEN",
  "Lesotho" = "LSO",
  "Liberia" = "LBR",
  "Madagascar" = "MDG",
  "Malawi" = "MWI",
  "Mali" = "MLI",
  "Mauritania" = "MRT",
  "Mozambique" = "MOZ",
  "Namibia" = "NAM",
  "Niger" = "NER",
  "Nigeria" = "NGA",
  "Rwanda" = "RWA",
  "Sao Tome and Principe" = "STP",
  "Senegal" = "SEN",
  "Seychelles" = "SYC",
  "Sierra Leone" = "SLE",
  "Sudan" = "SDN",
  "Somalia" = "SOM",
  "South Africa" = "ZAF",
  "South Sudan" = "SSD",
  "Tanzania" = "TZA",
  "Togo" = "TGO",
  "Uganda" = "UGA",
  "Zambia" = "ZMB",
  "Zimbabwe" = "ZWE"
)
statin_ssa <- statin_data %>%
  filter(Entity %in% names(ssa_entity_to_iso3)) %>%
  mutate(iso3 = ssa_entity_to_iso3[Entity])



# Stroke, alteplase and coronary procedure data
setwd(dir="~/Desktop/cardiovascular_capacity/analysis/data/statin_and_procedures/")
procedure_data <- read.csv("procedure_availability_capacity_survey.csv", stringsAsFactors = FALSE)
ssa_location_to_iso3 <- c(
  "Angola" = "AGO",
  "Benin" = "BEN",
  "Botswana" = "BWA",
  "Burkina Faso" = "BFA",
  "Burundi" = "BDI",
  "Cabo Verde" = "CPV",
  "Cameroon" = "CMR",
  "Central African Republic" = "CAF",
  "Chad" = "TCD",
  "Comoros" = "COM",
  "Congo" = "COG",
  "Cote d'Ivoire" = "CIV",
  "Democratic Republic of the Congo" = "COD",
  "Equatorial Guinea" = "GNQ",
  "Eritrea" = "ERI",
  "Eswatini" = "SWZ",
  "Ethiopia" = "ETH",
  "Gabon" = "GAB",
  "Gambia" = "GMB",
  "Ghana" = "GHA",
  "Guinea" = "GIN",
  "Guinea-Bissau" = "GNB",
  "Kenya" = "KEN",
  "Lesotho" = "LSO",
  "Liberia" = "LBR",
  "Madagascar" = "MDG",
  "Malawi" = "MWI",
  "Mali" = "MLI",
  "Mauritania" = "MRT",
  "Mozambique" = "MOZ",
  "Namibia" = "NAM",
  "Niger" = "NER",
  "Nigeria" = "NGA",
  "Rwanda" = "RWA",
  "Sao Tome and Principe" = "STP",
  "Senegal" = "SEN",
  "Seychelles" = "SYC", 
  "Sierra Leone" = "SLE",
  "Sudan" = "SDN",
  "South Africa" = "ZAF",
  "South Sudan" = "SSD",
  "Togo" = "TGO",
  "Uganda" = "UGA",
  "United Republic of Tanzania" = "TZA",
  "Zambia" = "ZMB",
  "Zimbabwe" = "ZWE"
)
procedure_ssa <- procedure_data %>%
  filter(Location %in% names(ssa_location_to_iso3)) %>%
  mutate(iso3 = ssa_location_to_iso3[Location])

# Alteplase data
alteplase_data <- procedure_ssa %>%
  filter(Dim2ValueCode == "NCDCCS_QUESTION_NCD_CCS_ALTEPLASE") %>%
  group_by(iso3) %>%
  filter(Period == max(Period, na.rm = TRUE)) %>%
  ungroup()

# Coronary data
coronary_data <- procedure_ssa %>%
  filter(Dim2ValueCode == "NCDCCS_QUESTION_NCD_CCS_CORONARY") %>%
  group_by(iso3) %>%
  filter(Period == max(Period, na.rm = TRUE)) %>%
  ungroup()

# Stroke data
stroke_data <- procedure_ssa %>%
  filter(Dim2ValueCode == "NCDCCS_QUESTION_NCD_CCS_STROKE") %>%
  group_by(iso3) %>%
  filter(Period == max(Period, na.rm = TRUE)) %>%
  ungroup()

thrombolysis_data <- procedure_ssa %>%
  filter(Dim2ValueCode == "NCDCCS_QUESTION_NCD_CCS_THROMBOLYTIC") %>%
  group_by(iso3) %>%
  filter(Period == max(Period, na.rm = TRUE)) %>%
  ungroup()


#Prepare data to be plotted
clean_response <- function(x) {
  recode(x,
         "Yes" = "Yes",
         "No" = "No",
         "Don't know" = "Unknown",
         "No response" = "Unknown",
         .default = "Unknown")
}

#Extract and rename each variable
statins <- statin_ssa %>%
  select(iso3, statins = General.availability.of.statins.in.the.public.health.sector) %>%
  mutate(statins = clean_response(statins))

alteplase <- alteplase_data %>%
  select(iso3, alteplase = Value) %>%
  mutate(alteplase = clean_response(alteplase))

coronary <- coronary_data %>%
  select(iso3, coronary = Value) %>%
  mutate(coronary = clean_response(coronary))

stroke <- stroke_data %>%
  select(iso3, stroke = Value) %>%
  mutate(stroke = clean_response(stroke))

thrombolysis <- thrombolysis_data %>%
  select(iso3, thrombolysis = Value) %>%
  mutate(thrombolysis = clean_response(thrombolysis))

#Merge all into a single wide dataframe
availability_all <- full_join(statins, alteplase, by = "iso3") %>%
  full_join(coronary, by = "iso3") %>%
  full_join(thrombolysis, by = "iso3") %>%
  full_join(stroke, by = "iso3")

#Convert to long format
availability_long <- availability_all %>%
  pivot_longer(cols = -iso3, names_to = "Service", values_to = "Availability")

#Optional: order by frequency of Yes/No, or region
availability_long <- availability_long %>%
  mutate(
    Availability = case_when(
      is.na(Availability) ~ "Unknown",
      Availability %in% c("Don't know", "No response") ~ "Unknown",
      TRUE ~ Availability
    ),
    Availability = factor(Availability, levels = c("Yes", "No", "Unknown")),
    Service = tools::toTitleCase(Service),
    iso3 = fct_rev(fct_relevel(iso3, sort(unique(iso3))))
  )
palette <- c(
  "Yes" = "wheat4",      # Dark green (deep forest green)
  "No" = "wheat2",       # Light green (soft spring green)
  "Unknown" = "snow3"    # Neutral grey for unknown/missing
)
#palette <- c("Yes" = "#2E5736", "No" = "#A56441", "Unknown" = "grey50")

# Plot
figure_3_v1 <- ggplot(availability_long, aes(x = Service, y = iso3, fill = Availability)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_manual(values = palette, drop = FALSE) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.6, "lines")
  ) +
  labs(
    x = "Service",
    y = "Country",
    fill = "General Public Sector Availability"
  )
figure_3_v1

#General availability of alteplase for acute stroke management in the public health system
#General availability of coronary bypass or stenting in the public health system
#Provision for care of acute stroke and rehabilitation in more than 50% of public sector health facilities
#General availability of statins in the public health sector
#General availability of thrombolytic therapy in the public health system






# Map Style --------

library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)


# Load world map and filter SSA countries -
world <- ne_countries(scale = "medium", returnclass = "sf")

ssa_iso3 <- c(
  "BFA", "CAF", "STP", "COD", "ETH", "NGA", "SOM", "GAB", "MRT", "SWZ",
  "LSO", "AGO", "BEN", "BWA", "BDI", "CPV", "CMR", "TCD", "CIV", "DJI",
  "GNQ", "GHA", "GIN", "GNB", "KEN", "LBR", "MDG", "MWI", "MLI", "MOZ",
  "NAM", "RWA", "SEN", "SLE", "ZAF", "SSD", "COG", "GMB", "NER", "UGA",
  "ZMB", "ZWE", "ERI", "TGO", "COM", "TZA", "SYC", "SDN"
)

ssa_map <- world %>% filter(iso_a3 %in% ssa_iso3)

# Join each availability to the map -
join_map <- function(df, varname) {
  ssa_map %>%
    left_join(df, by = c("iso_a3" = "iso3")) %>%
    mutate(across(all_of(varname), ~fct_explicit_na(.x, na_level = "Unknown")))
}

ssa_map_statins <- join_map(statins, "statins")
ssa_map_alteplase <- join_map(alteplase, "alteplase")
ssa_map_coronary <- join_map(coronary, "coronary")
ssa_map_stroke <- join_map(stroke, "stroke")
ssa_map_thrombolysis <- join_map(thrombolysis, "thrombolysis")

#  Define palette 
palette <- c("Yes" = "#2E5736", "No" = "#A56441", "Unknown" = "grey50")

# Make maps --
make_map <- function(map_data, fill_var, title) {
  ggplot(map_data) +
    geom_sf(aes_string(fill = fill_var), color = "white") +
    scale_fill_manual(values = palette, drop = FALSE) +
    theme_minimal() +
    labs(title = title, fill = "General Public Sector\nAvailability") +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 8)
    )
}

p_statins <- make_map(ssa_map_statins, "statins", "Statins")
p_alteplase <- make_map(ssa_map_alteplase, "alteplase", "Alteplase for acute stroke management")
p_coronary <- make_map(ssa_map_coronary, "coronary", "Coronary bypass or stenting")
p_stroke <- make_map(ssa_map_stroke, "stroke", "Provision for care of acute stroke and rehabilitation")
p_thrombolysis <- make_map(ssa_map_thrombolysis, "thrombolysis", "Thrombolytic therapy")

# Extract legend from one plot 
p_legend <- p_statins + theme(legend.position = "right")
legend_grob <- ggplotGrob(p_legend)$grobs
legend <- legend_grob[[which(sapply(legend_grob, function(x) x$name) == "guide-box")]]

# Remove legends from individual plots
p_statins <- p_statins + theme(legend.position = "none")
p_alteplase <- p_alteplase + theme(legend.position = "none")
p_coronary <- p_coronary + theme(legend.position = "none")
p_stroke <- p_stroke + theme(legend.position = "none")
p_thrombolysis <- p_thrombolysis + theme(legend.position = "none")

# Arrange plots with common legend on top 
figure_3_v2 <- plot_grid(
  p_statins, p_alteplase, p_coronary,
  p_stroke, p_thrombolysis, legend,
  ncol = 3,
  rel_heights = c(1, 1)
)
figure_3_v2







# Save Plots --------
setwd(dir="~/Desktop/cardiovascular_capacity/analysis/figures/")
ggsave("figure_3_v1.pdf",
       figure_3_v1,
       width = 6, height = 8, units = "in", device = cairo_pdf)
ggsave("figure_3_v2.pdf",
       figure_3_v2,
       width = 7, height = 5, units = "in", device = cairo_pdf)






