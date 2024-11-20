library(dplyr)
library(readr)
library(lubridate)
library(sf)

# Function to load and clean data
load_data <- function(year) {
    nb_vald_df <- read_delim(sprintf("data/cleaned_data/%s_nb_vald.csv", year), delim = ",", na = c("NA", "ND", "ND", "?"), locale = locale(decimal_mark = ","), col_types = cols(.default = col_character()))
    nb_vald_df <- nb_vald_df |>
        mutate(
            jour = ymd(jour),
            nb_vald = as.numeric(nb_vald)
        ) |>
        na.omit()

    profil_df <- read_delim(sprintf("data/cleaned_data/%s_profil.csv", year), delim = ",", na = c("NA", "ND", "ND", "?"), locale = locale(decimal_mark = ","), col_types = cols(.default = col_character()))
    profil_df <- profil_df |>
        mutate(
            pourc_validations = as.numeric(gsub(",", ".", pourc_validations)),
            jour = year
        ) |>
        na.omit()

    return(list(nb_vald_df = nb_vald_df, profil_df = profil_df))
}

# Load spatial data
load_spatial_data <- function() {
    zones <- read_delim("data/zones.csv", delim = ";")
    zones_spatial <- st_read("data/REF_ZdA/PL_ZDL_R_14_11_2024.shp", crs = 2154)

    names(zones) <- tolower(names(zones))

    zones_spatial <- zones_spatial |>
        st_make_valid() |>
        st_transform(4326) |>
        mutate(idrefa_lda = as.character(idrefa_lda)) |>
        full_join(zones, by = c("id_refa" = "zdaid"))

    centroids <- st_centroid(zones_spatial)
    zones_spatial$longitude <- st_coordinates(centroids)[, 1]
    zones_spatial$latitude <- st_coordinates(centroids)[, 2]

    return(zones_spatial)
}
