# install.packages(c("dplyr", "readr", "lubridate"))

library(dplyr)
library(readr)
library(lubridate)

process_year_data <- function(year) {
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

    # "Jour" to date format
    if (year == "2023") {
        nb_s1_df$jour <- ymd(nb_s1_df$jour)
    } else {
        nb_s1_df$jour <- dmy(nb_s1_df$jour)
    }

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
        nb_s1_df$jour <- ymd(nb_s1_df$jour)
    } else {
        nb_s1_df$jour <- dmy(nb_s1_df$jour)
    }

    nb_s2_df <- na.omit(nb_s2_df)
    profil_s2_df <- na.omit(profil_s2_df)

    # Merge first and second semester
    nb_vald <- bind_rows(nb_s1_df, nb_s2_df)
    profil <- bind_rows(profil_s1_df, profil_s2_df)

    return(list(nb_vald = nb_vald, profil = profil))
}

# Process data for 2018-2022
# data_2018 <- process_year_data("2018")
# print(data_2018)
# data_2019 <- process_year_data("2019")
# print(data_2019)
# data_2020 <- process_year_data("2020")
# print(data_2020)
# data_2021 <- process_year_data("2021")
# print(data_2021)
# data_2022 <- process_year_data("2022")
# print(data_2022)
data_2023 <- process_year_data("2023")
print(data_2023$nb_vald)
print(data_2023$profil)
