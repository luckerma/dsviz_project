library(dplyr)
library(readr)
library(lubridate)

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
            pourc_validations = as.numeric(gsub(",", ".", pourc_validations))
        ) |>
        na.omit()

    return(list(nb_vald_df = nb_vald_df, profil_df = profil_df))
}

# Function to get holidays and vacation days
get_holidays <- function() {
    holidays <- as.Date(c(
        "2018-01-01", "2018-05-01", "2018-05-08", "2018-07-14", "2018-08-15",
        "2018-11-01", "2018-11-11", "2018-12-25",
        "2019-01-01", "2019-05-01", "2019-05-08", "2019-07-14", "2019-08-15",
        "2019-11-01", "2019-11-11", "2019-12-25",
        "2020-01-01", "2020-05-01", "2020-05-08", "2020-07-14", "2020-08-15",
        "2020-11-01", "2020-11-11", "2020-12-25",
        "2021-01-01", "2021-05-01", "2021-05-08", "2021-07-14", "2021-08-15",
        "2021-11-01", "2021-11-11", "2021-12-25",
        "2022-01-01", "2022-05-01", "2022-05-08", "2022-07-14", "2022-08-15",
        "2022-11-01", "2022-11-11", "2022-12-25",
        "2023-01-01", "2023-05-01", "2023-05-08", "2023-07-14", "2023-08-15",
        "2023-11-01", "2023-11-11", "2023-12-25"
    ))
    school_breaks <- list(
        winter_2018 = seq(as.Date("2018-02-17"), as.Date("2018-03-04"), by = "day"),
        spring_2018 = seq(as.Date("2018-04-14"), as.Date("2018-04-29"), by = "day"),
        summer_2018 = seq(as.Date("2018-07-01"), as.Date("2018-09-02"), by = "day"),
        autumn_2018 = seq(as.Date("2018-10-20"), as.Date("2018-11-04"), by = "day"),
        christmas_2018 = seq(as.Date("2018-12-22"), as.Date("2019-01-06"), by = "day"),
        winter_2019 = seq(as.Date("2019-02-16"), as.Date("2019-03-03"), by = "day"),
        spring_2019 = seq(as.Date("2019-04-13"), as.Date("2019-04-28"), by = "day"),
        summer_2019 = seq(as.Date("2019-07-01"), as.Date("2019-09-01"), by = "day"),
        autumn_2019 = seq(as.Date("2019-10-19"), as.Date("2019-11-03"), by = "day"),
        christmas_2019 = seq(as.Date("2019-12-21"), as.Date("2020-01-05"), by = "day"),
        winter_2020 = seq(as.Date("2020-02-15"), as.Date("2020-03-01"), by = "day"),
        spring_2020 = seq(as.Date("2020-04-11"), as.Date("2020-04-26"), by = "day"),
        summer_2020 = seq(as.Date("2020-07-01"), as.Date("2020-08-31"), by = "day"),
        autumn_2020 = seq(as.Date("2020-10-17"), as.Date("2020-11-01"), by = "day"),
        christmas_2020 = seq(as.Date("2020-12-19"), as.Date("2021-01-03"), by = "day"),
        winter_2021 = seq(as.Date("2021-02-13"), as.Date("2021-02-28"), by = "day"),
        spring_2021 = seq(as.Date("2021-04-10"), as.Date("2021-04-25"), by = "day"),
        summer_2021 = seq(as.Date("2021-07-01"), as.Date("2021-08-31"), by = "day"),
        autumn_2021 = seq(as.Date("2021-10-16"), as.Date("2021-10-31"), by = "day"),
        christmas_2021 = seq(as.Date("2021-12-18"), as.Date("2022-01-02"), by = "day"),
        winter_2022 = seq(as.Date("2022-02-12"), as.Date("2022-02-27"), by = "day"),
        spring_2022 = seq(as.Date("2022-04-09"), as.Date("2022-04-24"), by = "day"),
        summer_2022 = seq(as.Date("2022-07-01"), as.Date("2022-08-31"), by = "day"),
        autumn_2022 = seq(as.Date("2022-10-15"), as.Date("2022-10-30"), by = "day"),
        christmas_2022 = seq(as.Date("2022-12-17"), as.Date("2023-01-01"), by = "day"),
        winter_2023 = seq(as.Date("2023-02-11"), as.Date("2023-02-26"), by = "day"),
        spring_2023 = seq(as.Date("2023-04-08"), as.Date("2023-04-23"), by = "day"),
        summer_2023 = seq(as.Date("2023-07-01"), as.Date("2023-08-31"), by = "day"),
        autumn_2023 = seq(as.Date("2023-10-14"), as.Date("2023-10-29"), by = "day"),
        christmas_2023 = seq(as.Date("2023-12-16"), as.Date("2024-01-01"), by = "day")
    )
    vacation_days <- unique(as.Date(unlist(school_breaks)))

    return(list(holidays = holidays, vacation_days = vacation_days))
}
