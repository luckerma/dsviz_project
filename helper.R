library(dplyr)
library(lubridate)
library(readr)
library(sf)

# Function to load and clean data
load_combined_data <- function() {
    combined_nb_vald <- readRDS("data/cleaned_data/combined_nb_vald.rds")
    combined_profil <- readRDS("data/cleaned_data/combined_profil.rds")

    return(list(combined_nb_vald = combined_nb_vald, combined_profil = combined_profil))
}

# Load spatial data
load_spatial_data <- function() {
    zones_spatial <- readRDS("data/cleaned_data/zones_spatial.rds")

    return(zones_spatial)
}
