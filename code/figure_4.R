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
