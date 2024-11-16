library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

source("helper.R")

zones <- read_delim("data/zones.csv", delim = ";")
zones_spatial <- st_read("data/REF_ZdA/PL_ZDL_R_14_11_2024.shp", crs = 2154)

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
    geom_line(size = 1) +
    labs(title = "Yearly Trend (Total)", x = "Year", y = "Total Validations") +
    theme_minimal()
ggsave("plots/yearly_validations.png", yearly_trend_plot)

monthly_trend <- combined_nb_vald |>
    mutate(month = floor_date(jour, "month")) |>
    group_by(month) |>
    summarize(total_nb_vald = sum(nb_vald, na.rm = TRUE))
monthly_trend_plot <- ggplot(monthly_trend, aes(x = month, y = total_nb_vald)) +
    geom_line(size = 1) +
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
holiday_data <- get_holidays()
holidays <- holiday_data$holidays
vacation_days <- holiday_data$vacation_days

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
