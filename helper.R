library(dplyr)
library(lubridate)
library(readr)
library(sf)

# Function to load and clean data
load_data <- function(year) {
    nb_vald_df <- readRDS(sprintf("data/cleaned_data/%s_nb_vald.rds", year))
    profil_df <- readRDS(sprintf("data/cleaned_data/%s_profil.rds", year))

    return(list(nb_vald_df = nb_vald_df, profil_df = profil_df))
}

# Load spatial data
load_spatial_data <- function() {
    zones_spatial <- readRDS("data/cleaned_data/zones_spatial.rds")

    return(zones_spatial)
}
