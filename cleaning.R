library(dplyr)
library(readr)
library(sf)
library(stringr)
library(lubridate)

read_data <- function(year, base_path = ".") {
    # Data paths
    nb_s1 <- sprintf("%s/data/data-rf-%s/%s_S1_NB_FER.txt", base_path, year, year)
    profil_s1 <- sprintf("%s/data/data-rf-%s/%s_S1_PROFIL_FER.txt", base_path, year, year)
    nb_s2 <- sprintf("%s/data/data-rf-%s/%s_S2_NB_FER.txt", base_path, year, year)
    profil_s2 <- sprintf("%s/data/data-rf-%s/%s_S2_Profil_FER.txt", base_path, year, year)

    if (!file.exists(nb_s1) || !file.exists(profil_s1)) {
        stop("Data files not found for year ", year)
    }

    # Load the data
    delim <- if (year == "2023") ";" else "\t"
    nb_s1_df <- read_delim(nb_s1, delim = delim, col_types = cols(.default = col_character()), guess_max = 10000, locale = locale(decimal_mark = ","))
    profil_s1_df <- read_delim(profil_s1, delim = delim, col_types = cols(.default = col_character()), guess_max = 10000, locale = locale(decimal_mark = ","))

    # Standardize column names
    colnames(nb_s1_df) <- tolower(colnames(nb_s1_df))
    colnames(profil_s1_df) <- tolower(colnames(profil_s1_df))
    nb_s1_df <- nb_s1_df |>
        rename_with(~ gsub("^lda$", "id_refa_lda", .x), everything())
    profil_s1_df <- profil_s1_df |>
        rename_with(~ gsub("^lda$", "id_refa_lda", .x), everything())

    # Convert date format
    if (year == "2023") {
        nb_s1_df$jour <- ymd(nb_s1_df$jour)
    } else {
        nb_s1_df$jour <- dmy(nb_s1_df$jour)
    }

    # Convert numeric columns
    nb_s1_df <- nb_s1_df |>
        mutate(nb_vald = as.numeric(nb_vald))
    profil_s1_df <- profil_s1_df |>
        mutate(
            pourc_validations = as.numeric(gsub(",", ".", pourc_validations)),
            jour = year
        )

    # Remove missing values
    nb_s1_df <- na.omit(nb_s1_df)
    profil_s1_df <- na.omit(profil_s1_df)

    # Check for second semester data
    if (!file.exists(nb_s2) || !file.exists(profil_s2)) {
        message("Second semester data not found for year: ", year)

        nb_s1_df <- nb_s1_df |>
            select(jour, id_refa_lda, libelle_arret, nb_vald) |>
            mutate(libelle_arret = toupper(str_trim(libelle_arret))) |>
            group_by(jour, id_refa_lda, libelle_arret) |>
            summarise(nb_vald = sum(nb_vald, na.rm = TRUE), .groups = "drop")

        profil_s1_df <- profil_s1_df |>
            select(jour, id_refa_lda, libelle_arret, cat_jour, trnc_horr_60, pourc_validations) |>
            mutate(libelle_arret = toupper(str_trim(libelle_arret))) |>
            group_by(jour, id_refa_lda, libelle_arret, cat_jour, trnc_horr_60) |>
            summarise(pourc_validations = mean(pourc_validations, na.rm = TRUE), .groups = "drop")

        return(list(nb_vald = nb_s1_df, profil = profil_s1_df))
    }

    # Same for second semester
    delim <- if (year == "2022" || year == "2023") ";" else "\t"
    nb_s2_df <- read_delim(nb_s2, delim = delim, col_types = cols(.default = col_character()), guess_max = 10000, locale = locale(decimal_mark = ","))
    profil_s2_df <- read_delim(profil_s2, delim = delim, col_types = cols(.default = col_character()), guess_max = 10000, locale = locale(decimal_mark = ","))

    colnames(nb_s2_df) <- tolower(colnames(nb_s2_df))
    colnames(profil_s2_df) <- tolower(colnames(profil_s2_df))
    nb_s1_df <- nb_s1_df |>
        rename_with(~ gsub("^lda$", "id_refa_lda", .x), everything())
    profil_s1_df <- profil_s1_df |>
        rename_with(~ gsub("^lda$", "id_refa_lda", .x), everything())

    if (year == "2023") {
        nb_s2_df$jour <- ymd(nb_s2_df$jour)
    } else {
        nb_s2_df$jour <- dmy(nb_s2_df$jour)
    }

    nb_s2_df <- nb_s2_df |>
        mutate(
            nb_vald = as.numeric(nb_vald)
        )
    profil_s2_df <- profil_s2_df |>
        mutate(
            pourc_validations = as.numeric(gsub(",", ".", pourc_validations)),
            jour = year
        )

    original_nb_vald_rows <- nrow(nb_s2_df)
    original_profil_rows <- nrow(profil_s2_df)
    nb_s2_df <- na.omit(nb_s2_df)
    profil_s2_df <- na.omit(profil_s2_df)
    removed_nb_vald_rows <- original_nb_vald_rows - nrow(nb_s2_df)
    removed_profil_rows <- original_profil_rows - nrow(profil_s2_df)
    message("NA removed from 'nb_vald' for '", year, "': ", removed_nb_vald_rows, " (", round(removed_nb_vald_rows / original_nb_vald_rows * 100, 2), "%)")
    message("NA removed from 'profil' for '", year, "': ", removed_profil_rows, " (", round(removed_profil_rows / original_profil_rows * 100, 2), "%)")

    # Merge first and second semester data
    combined_nb_vald <- bind_rows(nb_s1_df, nb_s2_df)
    combined_profil <- bind_rows(profil_s1_df, profil_s2_df)

    combined_nb_vald <- combined_nb_vald |>
        select(jour, id_refa_lda, libelle_arret, nb_vald) |>
        mutate(libelle_arret = toupper(str_trim(libelle_arret))) |>
        group_by(jour, id_refa_lda, libelle_arret) |>
        summarise(nb_vald = sum(nb_vald, na.rm = TRUE), .groups = "drop")

    combined_profil <- combined_profil |>
        select(jour, id_refa_lda, libelle_arret, cat_jour, trnc_horr_60, pourc_validations) |>
        mutate(libelle_arret = toupper(str_trim(libelle_arret))) |>
        group_by(jour, id_refa_lda, libelle_arret, cat_jour, trnc_horr_60) |>
        summarise(pourc_validations = mean(pourc_validations, na.rm = TRUE), .groups = "drop")

    return(list(nb_vald = combined_nb_vald, profil = combined_profil))
}

remove_outliers <- function(data) {
    for (col in colnames(data)) {
        # Only remove outliers for numeric columns
        if (is.numeric(data[[col]])) {
            # Calculate IQR
            q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
            q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
            iqr <- q3 - q1
            lower_bound <- q1 - 1.5 * iqr
            upper_bound <- q3 + 1.5 * iqr

            # Filter outliers
            unique_arret <- unique(data$libelle_arret)
            data <- data[data[[col]] >= lower_bound & data[[col]] <= upper_bound, ]
            removed_arret <- setdiff(unique_arret, unique(data$libelle_arret))
            if (length(removed_arret) > 0) {
                message("Removed outliers for '", col, "': ", removed_arret)
            }
        }
    }

    return(data)
}

read_clean_data <- function(years, base_path) {
    combined_nb_vald <- list()
    combined_profil <- list()

    for (year in years) {
        data <- read_data(year, base_path)

        original_nb_vald_rows <- nrow(data$nb_vald)
        original_profil_rows <- nrow(data$profil)

        # TODO: Fix outlier removal, it's removing too many rows (Chatelet, Gare de L'Est, Nord, Lyon, Montparnasse, Defense, ...)
        # data$nb_vald <- remove_outliers(data$nb_vald)
        # data$profil <- remove_outliers(data$profil)

        combined_nb_vald[[year]] <- data$nb_vald
        combined_profil[[year]] <- data$profil

        removed_nb_vald_rows <- original_nb_vald_rows - nrow(data$nb_vald)
        removed_profil_rows <- original_profil_rows - nrow(data$profil)
        message("Rows removed from 'nb_vald' for '", year, "': ", removed_nb_vald_rows, " (", round(removed_profil_rows / original_profil_rows * 100, 2), "%)")
        message("Rows removed from 'profil' for '", year, "': ", removed_profil_rows, " (", round(removed_profil_rows / original_profil_rows * 100, 2), "%)")
    }

    # Combine data across years
    combined_nb_vald <- bind_rows(combined_nb_vald)
    combined_profil <- bind_rows(combined_profil)

    return(list(nb_vald = combined_nb_vald, profil = combined_profil))
}

read_spatial_data <- function(base_path = ".") {
    path <- sprintf("%s/data/zones.csv", base_path)
    zones <- read_delim(path, delim = ";")
    path <- sprintf("%s/data/REF_ZdA/PL_ZDL_R_14_11_2024.shp", base_path)
    zones_spatial <- st_read(path, crs = 2154)

    names(zones) <- tolower(names(zones))

    zones_spatial <- zones_spatial |>
        st_make_valid() |>
        st_transform(4326) |>
        mutate(idrefa_lda = as.character(idrefa_lda)) |>
        full_join(zones, by = c("id_refa" = "zdaid"))

    centroids <- st_centroid(zones_spatial)
    zones_spatial$longitude <- st_coordinates(centroids)[, 1]
    zones_spatial$latitude <- st_coordinates(centroids)[, 2]

    zones_spatial <- zones_spatial |>
        st_drop_geometry() |>
        rename_with(~ gsub("^idrefa_lda$", "id_refa_lda", .x), everything()) |>
        mutate(nom_lda = toupper(str_trim(nom_lda))) |>
        select(id_refa_lda, nom_lda, longitude, latitude)

    return(zones_spatial)
}

export_data <- function(combined_data, base_path = ".") {
    export_path <- sprintf("%s/data/cleaned_data/combined_nb_vald.rds", base_path)
    saveRDS(combined_data$nb_vald, export_path)

    export_path <- sprintf("%s/data/cleaned_data/combined_profil.rds", base_path)
    saveRDS(combined_data$profil, export_path)
}

export_spatial_data <- function(spatial_data, base_path = ".") {
    export_path <- sprintf("%s/data/cleaned_data/zones_spatial.rds", base_path)
    saveRDS(spatial_data, export_path)
}

main <- function(years, base_path) {
    export_data(read_clean_data(years, base_path), base_path)
    export_spatial_data(read_spatial_data(base_path), base_path)
}

main(YEARS, BASE_PATH)
message("Data cleaning and export done: '/data/cleaned_data'")
