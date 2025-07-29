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

#iso3 codes for sub-saharan africa
iso3_subsaharan <- c(
  "BFA", "CAF", "STP", "COD", "ETH", "NGA", "SOM", "GAB", "MRT", "SWZ", "LSO",
  "AGO", "BEN", "BWA", "BDI", "CPV", "CMR", "TCD", "CIV", "DJI", "GNQ", "GHA", "GIN", "GNB",
  "KEN", "LBR", "MDG", "MWI", "MLI", "MOZ", "NAM", "RWA", "SEN", "SLE", "ZAF", "SSD", "COG",
  "GMB", "NER", "UGA", "ZMB", "ZWE", "ERI", "TGO", "COM", "TZA"
)
iso3_colors <- setNames(viridis(length(iso3_subsaharan), option = "D"), iso3_subsaharan)


# Figure 1: Burden and Resource Comparison (Correlation Scatter plot or Alluvial Plot) ------------------

#Burden data
setwd(dir="~/Desktop/cardiovascular_capacity/analysis/data/Burden_of_disease_2021/")
burden_2021 <- read.csv("Burden_2021.csv", stringsAsFactors = FALSE)
iso3_lookup <- tibble::tibble(
  location_name = c(
    "Burkina Faso",
    "Central African Republic",
    "Democratic Republic of Sao Tome and Principe",
    "Democratic Republic of the Congo",
    "Federal Democratic Republic of Ethiopia",
    "Federal Republic of Nigeria",
    "Federal Republic of Somalia",
    "Gabonese Republic",
    "Islamic Republic of Mauritania",
    "Kingdom of Eswatini",
    "Kingdom of Lesotho",
    "Republic of Angola",
    "Republic of Benin",
    "Republic of Botswana",
    "Republic of Burundi",
    "Republic of Cabo Verde",
    "Republic of Cameroon",
    "Republic of Chad",
    "Republic of Côte d'Ivoire",
    "Republic of Djibouti",
    "Republic of Equatorial Guinea",
    "Republic of Ghana",
    "Republic of Guinea",
    "Republic of Guinea-Bissau",
    "Republic of Kenya",
    "Republic of Liberia",
    "Republic of Madagascar",
    "Republic of Malawi",
    "Republic of Mali",
    "Republic of Mozambique",
    "Republic of Namibia",
    "Republic of Rwanda",
    "Republic of Senegal",
    "Republic of Sierra Leone",
    "Republic of South Africa",
    "Republic of South Sudan",
    "Republic of the Congo",
    "Republic of the Gambia",
    "Republic of the Niger",
    "Republic of Uganda",
    "Republic of Zambia",
    "Republic of Zimbabwe",
    "State of Eritrea",
    "Togolese Republic",
    "Union of the Comoros",
    "United Republic of Tanzania"
  ),
  iso3 = c(
    "BFA", "CAF", "STP", "COD", "ETH", "NGA", "SOM", "GAB", "MRT", "SWZ", "LSO",
    "AGO", "BEN", "BWA", "BDI", "CPV", "CMR", "TCD", "CIV", "DJI", "GNQ", "GHA", "GIN", "GNB",
    "KEN", "LBR", "MDG", "MWI", "MLI", "MOZ", "NAM", "RWA", "SEN", "SLE", "ZAF", "SSD", "COG",
    "GMB", "NER", "UGA", "ZMB", "ZWE", "ERI", "TGO", "COM", "TZA"
  )
)

burden_2021 <- burden_2021 %>%
  left_join(iso3_lookup, by = "location_name")

burden_2021 <- burden_2021 %>%
  filter(
    measure_name == "DALYs (Disability-Adjusted Life Years)",
    location_name != "Sub-Saharan Africa",
    metric_name == "Rate"
  )

burden_cardio_2021 <- burden_2021 %>%
  filter(cause_name == "Cardiovascular diseases")
burden_ischaemic_2021 <- burden_2021 %>%
  filter(cause_name == "Ischemic heart disease")


#Cardiovascular resource data
setwd(dir="~/Desktop/cardiovascular_capacity/analysis/data/cardiovascular_services/")
cardiovascular_resources <- read.csv("iAHO_v2_expanded_cardiovascular_services_survey_results_AFRO.csv", stringsAsFactors = FALSE)
cardiovascular_resources <- cardiovascular_resources %>%
  filter(Indicator == "Availability of Cardiovascular diseases services")
cardiovascular_resources <- cardiovascular_resources %>%
  filter(Country != "AFRO Region")
iso3_cardiovascular <- tibble::tibble(
  Country = c(
    "BENIN",
    "BURKINA FASO",
    "BURUNDI",
    "CHAD",
    "ETHIOPIA",
    "LIBERIA",
    "MAURITANIA",
    "NIGER",
    "SIERRA LEONE",
    "SOUTH SUDAN",
    "UGANDA",
    "UNITED REPUBLIC OF TANZANIA",
    "ZAMBIA",
    "ZIMBABWE"
  ),
  iso3 = c(
    "BEN", "BFA", "BDI", "TCD", "ETH", "LBR", "MRT",
    "NER", "SLE", "SSD", "UGA", "TZA", "ZMB", "ZWE"
  )
)
cardiovascular_resources <- cardiovascular_resources %>%
  left_join(iso3_cardiovascular, by = "Country")
cardiovascular_resources <- cardiovascular_resources %>%
  filter(iso3 %in% iso3_subsaharan)
cardiovascular_resources <- cardiovascular_resources %>%
  mutate(Value = as.numeric(Value))


# CVD Management Guidelines data
setwd(dir="~/Desktop/cardiovascular_capacity/analysis/data/NCD_management_guidelines/")
CVD_guidelines <- read.csv("NCD_country_capacity_survey.csv", stringsAsFactors = FALSE)
CVD_guidelines <- CVD_guidelines %>%
  filter(Dim2ValueCode == "NCDCCS_QUESTION_NCD_CCS_CVD_GUIDE")
iso3_ncd <- tibble::tibble(
  Location = c(
    "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde",
    "Cameroon", "Central African Republic", "Chad", "Comoros", "Congo", "Cote d'Ivoire",
    "Democratic Republic of the Congo", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia",
    "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia",
    "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Mozambique", "Namibia", "Niger",
    "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone",
    "South Africa", "South Sudan", "Togo", "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe", "Seychelles"
  ),
  iso3 = c(
    "DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", "TCD", "COM", "COG", "CIV",
    "COD", "GNQ", "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR",
    "MDG", "MWI", "MLI", "MRT", "MUS", "MOZ", "NAM", "NER", "NGA", "RWA", "STP", "SEN", "SYC",
    "SLE", "ZAF", "SSD", "TGO", "UGA", "TZA", "ZMB", "ZWE", "SYC"
  )
)
CVD_guidelines <- CVD_guidelines %>%
  left_join(iso3_ncd, by = "Location")
CVD_guidelines <- CVD_guidelines %>%
  filter(ParentLocationCode == "AFR")
CVD_guidelines <- CVD_guidelines %>%
  filter(iso3 %in% iso3_subsaharan)
CVD_guidelines <- CVD_guidelines %>%
  filter(IsLatestYear == "true")


# Panel a: Cardiovascular burden vs. cardiovascular services
cardio_plot_data <- burden_cardio_2021 %>%
  inner_join(cardiovascular_resources, by = "iso3")

panel_a_plot <- ggplot(cardio_plot_data, aes(x = Value, y = val, color = iso3)) +
  geom_point(size = 4, alpha = 0.9) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5, alpha = 0.6) +
  
  # ISO3 label above the point
  geom_text(aes(label = iso3), vjust = -1, size = 3) +
  
  # Year label below the point
  geom_text(aes(label = Period), vjust = 2, size = 2.5, color = "black") +
  
  scale_color_manual(values = iso3_colors) +
  scale_y_continuous(limits = c(0, 5000)) +
  labs(x = "Availability of Cardiovascular Services (%)",
    y = "DALYs per 100,000 (2021), Cardiovascular Disease",
    color = "Country (ISO3)"
  ) +
  theme_classic() +
  theme(legend.position = "none")
panel_a_plot


# Panel b: Ischemic burden vs. cardiovascular services
ischemic_plot_data <- burden_ischaemic_2021 %>%
  inner_join(cardiovascular_resources, by = "iso3")

panel_b_plot <- ggplot(ischemic_plot_data, aes(x = Value, y = val, color = iso3)) +
  geom_point(size = 4, alpha = 0.9) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5, alpha = 0.6) +
  
  # ISO3 label above the point
  geom_text(aes(label = iso3), vjust = -1, size = 3) +
  
  # Year label below the point
  geom_text(aes(label = Period), vjust = 2, size = 2.5, color = "black") +
  
  scale_color_manual(values = iso3_colors) +
  scale_y_continuous(limits = c(0, 1500)) +
  labs(
    x = "Availability of Cardiovascular Services (%)",
    y = "DALYs per 100,000 (2021), Ischemic Heart Disease",
    color = "Country (ISO3)"
  ) +
  theme_classic() +
  theme(legend.position = "none")
panel_b_plot


# Panel c: Cardiovascular burden vs. cardiovascular management guidelines
cardio_guidelines_plot_data <- burden_cardio_2021 %>%
  filter(iso3 %in% CVD_guidelines$iso3) %>%
  left_join(
    CVD_guidelines %>% select(iso3, Value_guidelines = Value),
    by = "iso3"
  ) %>%
  filter(!is.na(Value_guidelines)) %>%
  mutate(
    Value_guidelines = trimws(Value_guidelines),
    Value_guidelines = case_when(
      grepl("^Yes$", Value_guidelines, ignore.case = TRUE) ~ "Yes",
      grepl("^No response$", Value_guidelines, ignore.case = TRUE) ~ "No response",
      TRUE ~ "No"
    )
  )
guidelines_colors <- c(
  "Yes" = "forestgreen",
  "No" = "firebrick",
  "No response" = "grey")
panel_c_plot <- ggplot(cardio_guidelines_plot_data, aes(x = reorder(iso3, val), y = val, color = Value_guidelines)) +
  geom_point(size = 4, alpha = 0.9) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3, alpha = 0.6) +
  scale_color_manual(values = guidelines_colors) +
  labs(
    x = "Country",
    y = "DALYs per 100,000 (2021), Cardiovascular Disease",
    color = "National CVD Guidelines (2023)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.65, 0.85),    # place legend inside plot, top-right corner
    legend.background = element_rect(fill = alpha('white', 0.7), color = NA)
  )
panel_c_plot



# Panel d: Cardiovascular burden vs. cardiovascular management guidelines
cardio_guidelines_ischemic_plot_data <- burden_ischaemic_2021 %>%
  filter(iso3 %in% CVD_guidelines$iso3) %>%
  left_join(
    CVD_guidelines %>% select(iso3, Value_guidelines = Value),
    by = "iso3"
  ) %>%
  filter(!is.na(Value_guidelines)) %>%
  mutate(
    Value_guidelines = trimws(Value_guidelines),
    Value_guidelines = case_when(
      grepl("^Yes$", Value_guidelines, ignore.case = TRUE) ~ "Yes",
      grepl("^No response$", Value_guidelines, ignore.case = TRUE) ~ "No response",
      TRUE ~ "No"
    )
  )
guidelines_colors <- c(
  "Yes" = "forestgreen",
  "No" = "firebrick",
  "No response" = "grey")
panel_d_plot <- ggplot(cardio_guidelines_ischemic_plot_data, aes(x = reorder(iso3, val), y = val, color = Value_guidelines)) +
  geom_point(size = 4, alpha = 0.9) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3, alpha = 0.6) +
  scale_color_manual(values = guidelines_colors) +
  labs(
    x = "Country",
    y = "DALYs per 100,000 (2021), Ischemic Heart Disease",
    color = "National CVD Guidelines (2023)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.65, 0.85),    # place legend inside plot, top-right corner
    legend.background = element_rect(fill = alpha('white', 0.7), color = NA)
  )
panel_d_plot


# Combine
figure_1_v1 <- (panel_a_plot) / (panel_c_plot) +
  plot_annotation(tag_levels = "A") &  # Adds labels A, B, C, D automatically
  theme(plot.tag = element_text(face = "bold", size = 14, hjust = -0.5, vjust = 0.8))
figure_1_v1

figure_1_v2 <- (panel_a_plot + panel_b_plot) / (panel_c_plot + panel_d_plot) +
  plot_annotation(tag_levels = "A") &  # Adds labels A, B, C, D automatically
  theme(plot.tag = element_text(face = "bold", size = 14, hjust = 0, vjust = 1))
figure_1_v2

# Save
setwd(dir="~/Desktop/cardiovascular_capacity/analysis/figures/")





