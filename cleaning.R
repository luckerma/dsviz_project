library(dplyr)
library(readr)
library(lubridate)


read_data <- function(year) {
    # Data paths
    nb_s1 <- sprintf("data/data-rf-%s/%s_S1_NB_FER.txt", year, year)
    profil_s1 <- sprintf("data/data-rf-%s/%s_S1_PROFIL_FER.txt", year, year)
    nb_s2 <- sprintf("data/data-rf-%s/%s_S2_NB_FER.txt", year, year)
    profil_s2 <- sprintf("data/data-rf-%s/%s_S2_Profil_FER.txt", year, year)

    if (!file.exists(nb_s1) || !file.exists(profil_s1)) {
        stop("Data files not found")
    }

    # Load the data
    delim <- if (year == "2023") ";" else "\t"
    nb_s1_df <- read_delim(nb_s1, delim = delim, col_types = cols(.default = col_character()), guess_max = 10000, locale = locale(decimal_mark = ","))
    profil_s1_df <- read_delim(profil_s1, delim = delim, col_types = cols(.default = col_character()), guess_max = 10000, locale = locale(decimal_mark = ","))

    # Standardize column names
    colnames(nb_s1_df) <- tolower(colnames(nb_s1_df))
    colnames(profil_s1_df) <- tolower(colnames(profil_s1_df))

    # TODO: Columns are not matching!!!
    if (year == "2023") {
        # 22: CODE_STIF_TRNS;CODE_STIF_RES;CODE_STIF_ARRET;LIBELLE_ARRET;ID_REFA_LDA;CAT_JOUR;TRNC_HORR_60;pourc_validations
        # 23: CODE_STIF_TRNS;CODE_STIF_RES;CODE_STIF_LIGNE;LIBELLE_LIGNE;ID_GROUPOFLIGNE;CAT_JOUR;TRNC_HORR_60;pourc_validations

        mapping <- c(
            "CODE_STIF_TRNS" = "CODE_STIF_TRNS",
            "CODE_STIF_RES" = "CODE_STIF_RES",
            "CODE_STIF_ARRET" = "CODE_STIF_LIGNE",
            "LIBELLE_ARRET" = "LIBELLE_LIGNE",
            "ID_REFA_LDA" = "ID_GROUPOFLIGNE",
            "CAT_JOUR" = "CAT_JOUR",
            "TRNC_HORR_60" = "TRNC_HORR_60",
            "pourc_validations" = "pourc_validations"
        )
        mapping <- setNames(tolower(names(mapping)), tolower(mapping))
        profil_s1_df <- profil_s1_df |>
            rename_with(~ mapping[.x], everything())
    }

    # "jour" to date format
    if (year == "2023") {
        nb_s1_df$jour <- ymd(nb_s1_df$jour)
    } else {
        nb_s1_df$jour <- dmy(nb_s1_df$jour)
    }

    # "nb_vald" to numeric
    nb_s1_df <- nb_s1_df |>
        mutate(
            nb_vald = as.numeric(nb_vald)
        )

    # "puorc_validations" to numeric
    profil_s1_df <- profil_s1_df |>
        mutate(
            pourc_validations = as.numeric(gsub(",", ".", pourc_validations))
        )

    # Handle missing values (drop rows with missing values) / TODO: handle missing values
    nb_s1_df <- na.omit(nb_s1_df)
    profil_s1_df <- na.omit(profil_s1_df)

    if (!file.exists(nb_s2) || !file.exists(profil_s2)) {
        print("Second semester data not found")
        return(list(nb_vald = nb_s1_df, profil = profil_s1_df))
    }

    # Same for second semester
    delim <- if (year == "2022" || year == "2023") ";" else "\t"
    nb_s2_df <- read_delim(nb_s2, delim = delim, col_types = cols(.default = col_character()), guess_max = 10000, locale = locale(decimal_mark = ","))
    profil_s2_df <- read_delim(profil_s2, delim = delim, col_types = cols(.default = col_character()), guess_max = 10000, locale = locale(decimal_mark = ","))

    colnames(nb_s2_df) <- tolower(colnames(nb_s2_df))
    colnames(profil_s2_df) <- tolower(colnames(profil_s2_df))

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

    nb_s2_df <- na.omit(nb_s2_df)
    profil_s2_df <- na.omit(profil_s2_df)

    # Merge first and second semester
    nb_vald <- bind_rows(nb_s1_df, nb_s2_df)
    profil <- bind_rows(profil_s1_df, profil_s2_df)

    return(list(nb_vald = nb_vald, profil = profil))
}

remove_outliers <- function(data) {
    for (col in colnames(data)) {
        if (is.numeric(data[[col]])) {
            q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
            q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
            iqr <- q3 - q1
            lower_bound <- q1 - 1.5 * iqr
            upper_bound <- q3 + 1.5 * iqr

            # Filter outliers
            data <- data[data[[col]] >= lower_bound & data[[col]] <= upper_bound, ]
        }
    }

    return(data)
}

read_clean_data <- function(year) {
    data <- read_data(year)

    original_nb_vald_rows <- nrow(data$nb_vald)
    original_profil_rows <- nrow(data$profil)

    message(paste("Summary for year", year, "before cleaning:"))
    print(summary(data$nb_vald))
    print(summary(data$profil))

    data$nb_vald <- remove_outliers(data$nb_vald)
    data$profil <- remove_outliers(data$profil)

    cleaned_nb_vald_rows <- nrow(data$nb_vald)
    cleaned_profil_rows <- nrow(data$profil)

    message(paste("Summary for year", year, "after cleaning:"))
    print(summary(data$nb_vald))
    print(summary(data$profil))

    message(paste("Rows removed from nb_vald for year", year, ":", original_nb_vald_rows - cleaned_nb_vald_rows))
    message(paste("Rows removed from profil for year", year, ":", original_profil_rows - cleaned_profil_rows))

    return(data)
}

export_data <- function(data, year) {
    saveRDS(data$nb_vald, sprintf("data/cleaned_data/%s_nb_vald.rds", year))
    saveRDS(data$profil, sprintf("data/cleaned_data/%s_profil.rds", year))
}

years <- c("2018", "2019", "2020", "2021", "2022", "2023")
for (year in years) {
    export_data(read_clean_data(year), year)
}
print("Data cleaning and export done: /data/cleaned_data)")
