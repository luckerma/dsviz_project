library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

zones <- read_delim("data/zones.csv", delim = ";")
zones_spatial <- st_read("data/REF_ZdA/PL_ZDL_R_14_11_2024.shp", crs = 2154)

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

### DATA ###

# Load and combine data for multiple years
years <- c("2018", "2019", "2020", "2021", "2022")
# years <- c("2018", "2019")

nb_vald_list <- list()
profil_list <- list()

for (year in years) {
    data <- load_data(year)
    nb_vald_list[[year]] <- data$nb_vald_df
    profil_list[[year]] <- data$profil_df
}

# Combined data
combined_nb_vald <- bind_rows(nb_vald_list)
combined_profil <- bind_rows(profil_list)

# Export
write_csv(combined_nb_vald, "data/cleaned_data/combined_nb_vald.csv")
write_csv(combined_profil, "data/cleaned_data/combined_profil.csv")

# Aggregate data
aggregated_nb_vald <- combined_nb_vald |>
    group_by(id_refa_lda) |>
    summarize(total_nb_vald = sum(nb_vald, na.rm = TRUE))

aggregated_profil <- combined_profil |>
    group_by(id_refa_lda) |>
    summarize(avg_pourc_validations = mean(pourc_validations, na.rm = TRUE))

aggregated_data <- left_join(aggregated_nb_vald, aggregated_profil, by = "id_refa_lda")

zones_spatial <- zones_spatial |>
    mutate(
        idrefa_lda = as.character(idrefa_lda)
    )
spatial_data <- left_join(zones_spatial, aggregated_data, by = c("idrefa_lda" = "id_refa_lda"))

### EDA ###

# Trend analysis
yearly_trend <- combined_nb_vald |>
    mutate(year = year(jour)) |>
    group_by(year) |>
    summarize(total_nb_vald = sum(nb_vald, na.rm = TRUE))
yearly_trend_plot <- ggplot(yearly_trend, aes(x = year, y = total_nb_vald)) +
    geom_line() +
    labs(title = "Yearly Trend (Total)", x = "Year", y = "Total Validations") +
    theme_minimal()
ggsave("plots/yearly_validations.png", yearly_trend_plot)

monthly_trend <- combined_nb_vald |>
    mutate(month = floor_date(jour, "month")) |>
    group_by(month) |>
    summarize(total_nb_vald = sum(nb_vald, na.rm = TRUE))
monthly_trend_plot <- ggplot(monthly_trend, aes(x = month, y = total_nb_vald)) +
    geom_line() +
    labs(title = "Monthly Trend (Total)", x = "Month", y = "Total Validations") +
    theme_minimal()
ggsave("plots/monthly_validations.png", monthly_trend_plot)

# Seasonality analysis
weekday_trend <- combined_nb_vald |>
    mutate(weekday = wday(jour, label = TRUE, week_start = 1)) |>
    group_by(weekday) |>
    summarize(avg_ridership = mean(nb_vald, na.rm = TRUE))
weekday_trend_plot <- ggplot(weekday_trend, aes(x = weekday, y = avg_ridership)) +
    geom_bar(stat = "identity") +
    labs(title = "Weekday Trend (2018-2023)", x = "Weekday", y = "Average Validations") +
    theme_minimal()
ggsave("plots/weekday_trend.png", weekday_trend_plot)

### NORMS ###

# Holidays and school breaks
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

# Filter data
normal_data <- combined_nb_vald |>
    filter(!(jour %in% holidays) & !(jour %in% vacation_days))

# Baseline (normal days)
baseline_week <- normal_data |>
    mutate(weekday = wday(jour, label = TRUE, week_start = 1)) |>
    group_by(weekday) |>
    summarize(avg_ridership = mean(nb_vald, na.rm = TRUE))

# Holidays
holiday_data <- combined_nb_vald |>
    filter(jour %in% holidays) |>
    mutate(weekday = wday(jour, label = TRUE, week_start = 1)) |>
    group_by(weekday) |>
    summarize(avg_ridership = mean(nb_vald, na.rm = TRUE))

# Vacations
vacation_data <- combined_nb_vald |>
    filter(jour %in% vacation_days) |>
    mutate(weekday = wday(jour, label = TRUE, week_start = 1)) |>
    group_by(weekday) |>
    summarize(avg_ridership = mean(nb_vald, na.rm = TRUE))

# Combined data
baseline_week$type <- "Normal"
holiday_data$type <- "Holiday"
vacation_data$type <- "Vacation"
combined_trend <- bind_rows(baseline_week, holiday_data, vacation_data)

# Yearly trends
normal_yearly_trend <- normal_data |>
    mutate(year = year(jour)) |>
    group_by(year) |>
    summarize(avg_vald = mean(nb_vald, na.rm = TRUE))
normal_yearly_trend$type <- "Normal"

holiday_yearly_trend <- combined_nb_vald |>
    filter(jour %in% holidays) |>
    mutate(year = year(jour)) |>
    group_by(year) |>
    summarize(avg_vald = mean(nb_vald, na.rm = TRUE))
holiday_yearly_trend$type <- "Holiday"

vacation_yearly_trend <- combined_nb_vald |>
    filter(jour %in% vacation_days) |>
    mutate(year = year(jour)) |>
    group_by(year) |>
    summarize(avg_vald = mean(nb_vald, na.rm = TRUE))
vacation_yearly_trend$type <- "Vacation"

combined_yearly_trend <- bind_rows(normal_yearly_trend, holiday_yearly_trend, vacation_yearly_trend)

yearly_trend_plot <- ggplot(combined_yearly_trend, aes(x = year, y = avg_vald, color = type, group = type)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    labs(
        title = "Yearly Trend Comparison: Normal vs Holidays vs Vacations",
        x = "Year",
        y = "Average Validations"
    ) +
    theme_minimal()
ggsave("plots/yearly_trend_comparison.png", yearly_trend_plot)

# Monthly trends
normal_monthly_trend <- normal_data |>
    mutate(month = floor_date(jour, "month")) |>
    group_by(month) |>
    summarize(mean_vald = mean(nb_vald, na.rm = TRUE))
normal_monthly_trend$type <- "Normal"

holiday_monthly_trend <- combined_nb_vald |>
    filter(jour %in% holidays) |>
    mutate(month = floor_date(jour, "month")) |>
    group_by(month) |>
    summarize(mean_vald = mean(nb_vald, na.rm = TRUE))
holiday_monthly_trend$type <- "Holiday"

vacation_monthly_trend <- combined_nb_vald |>
    filter(jour %in% vacation_days) |>
    mutate(month = floor_date(jour, "month")) |>
    group_by(month) |>
    summarize(mean_vald = mean(nb_vald, na.rm = TRUE))
vacation_monthly_trend$type <- "Vacation"

combined_monthly_trend <- bind_rows(normal_monthly_trend, holiday_monthly_trend, vacation_monthly_trend)

monthly_trend_plot <- ggplot(combined_monthly_trend, aes(x = month, y = mean_vald, color = type, group = type)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    labs(
        title = "Monthly Trend Comparison: Normal vs Holidays vs Vacations",
        x = "Month",
        y = "Average Validations"
    ) +
    theme_minimal()
ggsave("plots/monthly_trend_comparison.png", monthly_trend_plot)

# Weekday trends
weekdays_trend_plot <- ggplot(combined_trend, aes(x = weekday, y = avg_ridership, color = type, group = type)) +
    geom_bar(stat = "identity") +
    labs(
        title = "Weekday Trend Comparison (2018-2023): Normal vs Holidays vs Vacations",
        x = "Weekday",
        y = "Average Validations"
    ) +
    theme_minimal()
ggsave("plots/weekday_trend_comparison.png", weekdays_trend_plot)
