library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(leaflet)
library(sf)

source("helper.R")

### DATA ###

# Yearly Data
years <- c("2018", "2019", "2020", "2021", "2022")
# years <- c("2018")
yearly_data <- lapply(years, function(year) load_data(year))
combined_nb_vald <- bind_rows(lapply(yearly_data, `[[`, "nb_vald_df"))
combined_profil <- bind_rows(lapply(yearly_data, `[[`, "profil_df"))

# Spatial Data
zones_spatial <- load_spatial_data()

# Yearly data for nb_vald_df
yearly_nb_vald <- combined_nb_vald |>
    mutate(year = year(jour)) |>
    group_by(id_refa_lda, year) |>
    summarise(nb_vald = sum(nb_vald, na.rm = TRUE), .groups = "drop")

### UI ###
ui <- fluidPage(
    titlePanel("DSViz: IDF Ridership Analysis"),
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(
                condition = "input.tabs == 'Comparison'",
                dateInput("start_date1", "Starting Date for Period 1:", value = "2018-01-01"),
                dateInput("start_date2", "Starting Date for Period 2:", value = "2022-01-01"),
                sliderInput("period_days", "Comparison Period (Days):", value = 7, min = 1, max = 365)
            ),
            conditionalPanel(
                condition = "input.tabs == 'Stations Map'",
                sliderInput("min_vald", "Minimum Validations:", min = min(yearly_nb_vald$nb_vald), max = max(yearly_nb_vald$nb_vald), value = mean(yearly_nb_vald$nb_vald), step = max(yearly_nb_vald$nb_vald) / 100),
                selectInput("station", "Select Station:", choices = unique(combined_nb_vald$libelle_arret), selected = "NOISY-CHAMPS"),
                selectInput("year", "Select Year:", choices = years)
            )
        ),
        mainPanel(
            tabsetPanel(
                id = "tabs",
                tabPanel(
                    "Total Validations",
                    fluidRow(
                        column(12, plotOutput("yearlySumPlot")),
                        column(12, plotOutput("monthlySumPlot"))
                    )
                ),
                tabPanel(
                    "Trends",
                    fluidRow(
                        column(12, plotOutput("yearlyTrendPlot")),
                        column(12, plotOutput("monthlyTrendPlot")),
                        column(12, plotOutput("weekdayTrendPlot"))
                    )
                ),
                tabPanel(
                    "Comparison",
                    fluidRow(
                        column(12, plotOutput("comparisonDaysPlot")),
                        column(12, plotOutput("comparisonWeekdayPlot")),
                        column(12, plotOutput("comparisonDaysPlotTotal")),
                        column(12, plotOutput("comparisonWeekdayPlotTotal"))
                    )
                ),
                tabPanel(
                    "Stations Map",
                    fluidRow(
                        column(12, leafletOutput("stationMap")),
                        column(12, verbatimTextOutput("stationStats")),
                        column(12, plotOutput("profilHourlyPlot")),
                        column(12, plotOutput("profilDayTypePlot"))
                    )
                )
            )
        )
    )
)

### SERVER ###
server <- function(input, output, session) {
    # Comparison Data
    comparison_data <- reactive({
        days <- as.integer(input$period_days)
        period1 <- seq.Date(as.Date(input$start_date1), as.Date(input$start_date1) + days - 1, by = "day")
        period2 <- seq.Date(as.Date(input$start_date2), as.Date(input$start_date2) + days - 1, by = "day")

        period1_data <- combined_nb_vald |>
            filter(jour %in% period1) |>
            mutate(
                period = "Period 1",
                relative_day = as.numeric(jour - min(jour)) + 1
            )
        period2_data <- combined_nb_vald |>
            filter(jour %in% period2) |>
            mutate(
                period = "Period 2",
                relative_day = as.numeric(jour - min(jour)) + 1
            )

        bind_rows(period1_data, period2_data)
    })

    # Yearly Total Validations
    output$yearlySumPlot <- renderPlot({
        yearly_sum <- combined_nb_vald |>
            mutate(year = year(jour)) |>
            group_by(year) |>
            summarize(total_validations = sum(nb_vald, na.rm = TRUE))

        ggplot(yearly_sum, aes(x = year, y = total_validations)) +
            geom_col() +
            labs(title = "Total Validations (Yearly)", x = "Year", y = "Total Validations") +
            theme_minimal()
    })

    # Monthly Total Validations
    output$monthlySumPlot <- renderPlot({
        monthly_sum <- combined_nb_vald |>
            mutate(month = floor_date(jour, "month")) |>
            group_by(month) |>
            summarize(total_validations = sum(nb_vald, na.rm = TRUE))

        ggplot(monthly_sum, aes(x = month, y = total_validations)) +
            geom_col() +
            labs(title = "Total Validations (Monthly)", x = "Month", y = "Total Validations") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    # Yearly Trend Plot
    output$yearlyTrendPlot <- renderPlot({
        yearly_trend <- combined_nb_vald |>
            mutate(year = year(jour)) |>
            group_by(year) |>
            summarize(avg_vald = mean(nb_vald, na.rm = TRUE))

        ggplot(yearly_trend, aes(x = year, y = avg_vald)) +
            geom_line(size = 1) +
            geom_point(size = 3) +
            labs(title = "Yearly Trend", x = "Year", y = "Average Validations") +
            theme_minimal()
    })

    # Monthly Trend Plot
    output$monthlyTrendPlot <- renderPlot({
        monthly_trend <- combined_nb_vald |>
            mutate(month = floor_date(jour, "month")) |>
            group_by(month) |>
            summarize(mean_vald = mean(nb_vald, na.rm = TRUE))

        ggplot(monthly_trend, aes(x = month, y = mean_vald)) +
            geom_line(size = 1) +
            geom_point(size = 3) +
            labs(title = "Monthly Trend", x = "Month", y = "Average Validations") +
            theme_minimal()
    })

    # Weekday Trend Plot
    output$weekdayTrendPlot <- renderPlot({
        weekday_trend <- combined_nb_vald |>
            mutate(weekday = wday(jour, label = TRUE, week_start = 1)) |>
            group_by(weekday) |>
            summarize(mean_vald = mean(nb_vald, na.rm = TRUE))

        ggplot(weekday_trend, aes(x = weekday, y = mean_vald)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = "Weekday Trend", x = "Weekday", y = "Average Validations") +
            theme_minimal()
    })

    # Comparison Days Plot
    output$comparisonDaysPlot <- renderPlot({
        data <- comparison_data()

        daily_trend <- data |>
            group_by(relative_day, period, original_date = jour) |>
            summarize(avg_vald = mean(nb_vald, na.rm = TRUE), .groups = "drop")

        ggplot(daily_trend, aes(x = relative_day, y = avg_vald, color = period, group = period)) +
            geom_line(size = 1) +
            geom_point(size = 3) +
            labs(
                title = paste("Comparison of Two Periods (", input$period_days, " Days)", sep = ""),
                x = "Time",
                y = "Average Validations"
            ) +
            theme_minimal()
    })

    # Comparison Weekday Plot
    output$comparisonWeekdayPlot <- renderPlot({
        data <- comparison_data()

        weekday_trend <- data |>
            group_by(weekday = wday(jour, label = TRUE, week_start = 1), period) |>
            summarize(avg_vald = mean(nb_vald, na.rm = TRUE), .groups = "drop")

        ggplot(weekday_trend, aes(x = weekday, y = avg_vald, fill = period)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(
                title = paste("Weekday Comparison of Two Periods (", input$period_days, " Days)", sep = ""),
                x = "Weekday",
                y = "Average Validations"
            ) +
            theme_minimal()
    })

    # Comparison Days Plot - Total
    output$comparisonDaysPlotTotal <- renderPlot({
        data <- comparison_data()

        daily_trend <- data |>
            group_by(relative_day, period, original_date = jour) |>
            summarize(total_validations = sum(nb_vald, na.rm = TRUE), .groups = "drop")

        ggplot(daily_trend, aes(x = relative_day, y = total_validations, color = period, group = period)) +
            geom_line(size = 1) +
            geom_point(size = 3) +
            labs(
                title = paste("Comparison of Two Periods (", input$period_days, " Days)", sep = ""),
                x = "Time",
                y = "Total Validations"
            ) +
            theme_minimal()
    })

    # Comparison Weekday Plot - Total
    output$comparisonWeekdayPlotTotal <- renderPlot({
        data <- comparison_data()

        weekday_trend <- data |>
            group_by(weekday = wday(jour, label = TRUE, week_start = 1), period) |>
            summarize(total_validations = sum(nb_vald, na.rm = TRUE), .groups = "drop")

        ggplot(weekday_trend, aes(x = weekday, y = total_validations, fill = period)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(
                title = paste("Weekday Comparison of Two Periods (", input$period_days, " Days)", sep = ""),
                x = "Weekday",
                y = "Total Validations"
            ) +
            theme_minimal()
    })

    filtered_stations <- reactive({
        filtered_data <- combined_nb_vald |>
            filter(year(jour) == as.numeric(input$year)) |>
            group_by(libelle_arret, id_refa_lda) |>
            summarize(total_validations = sum(nb_vald, na.rm = TRUE), .groups = "drop") |>
            filter(total_validations >= input$min_vald)
    })

    # Update station dropdown
    observe({
        updateSelectInput(
            session,
            "station",
            choices = filtered_stations()$libelle_arret
        )
    })

    # Station Map
    output$stationMap <- renderLeaflet({
        selected_station <- input$station

        selected_station_ids <- combined_nb_vald |>
            filter(libelle_arret == selected_station) |>
            distinct(id_refa_lda) |>
            pull(id_refa_lda)

        yearly_validations <- combined_nb_vald |>
            group_by(id_refa_lda) |>
            summarise(total_validations = sum(nb_vald, na.rm = TRUE), .groups = "drop")

        filtered_zones_spatial <- zones_spatial |>
            filter(idrefa_lda %in% filtered_stations()$id_refa_lda) |>
            left_join(yearly_validations, by = c("idrefa_lda" = "id_refa_lda")) |>
            mutate(
                scaled_size = ifelse(is.na(total_validations), 5, total_validations / max(total_validations, na.rm = TRUE) * 20)
            )

        leaflet(filtered_zones_spatial) |>
            addTiles() |>
            addCircleMarkers(
                lng = ~longitude,
                lat = ~latitude,
                popup = ~ paste("Station:", nom_lda),
                layerId = ~idrefa_lda,
                color = ~ ifelse(idrefa_lda %in% selected_station_ids, "green", "red"),
                radius = ~scaled_size
            )
    })

    observeEvent(input$stationMap_marker_click, {
        click <- input$stationMap_marker_click
        selected_station_id <- click$id

        selected_station_name <- combined_nb_vald |>
            filter(id_refa_lda == selected_station_id) |>
            distinct(libelle_arret) |>
            pull(libelle_arret)

        if (!is.null(selected_station_name) && length(selected_station_name) > 0) {
            updateSelectInput(
                session,
                inputId = "station",
                selected = selected_station_name
            )
        }
    })

    # Station Stats
    output$stationStats <- renderPrint({
        selected_station <- input$station
        if (is.null(selected_station) || selected_station == "") {
            return("No station selected.")
        }

        selected_station_ids <- combined_nb_vald |>
            filter(libelle_arret == selected_station) |>
            distinct(id_refa_lda) |>
            pull(id_refa_lda)

        if (length(selected_station_ids) == 0) {
            return(paste("No data found for station:", selected_station))
        }

        station_data <- combined_nb_vald |>
            filter(id_refa_lda %in% selected_station_ids)

        total_validations_summary <- station_data |>
            filter(year(jour) == as.numeric(input$year)) |>
            summarise(
                TotalValidations = sum(nb_vald, na.rm = TRUE),
                AvgDailyValidations = mean(nb_vald, na.rm = TRUE),
                MinDailyValidations = min(nb_vald, na.rm = TRUE),
                MaxDailyValidations = max(nb_vald, na.rm = TRUE)
            )

        yearly_validations_summary <- station_data |>
            mutate(Year = year(jour)) |>
            group_by(Year) |>
            summarise(
                TotalValidations = sum(nb_vald, na.rm = TRUE),
                AvgDailyValidations = mean(nb_vald, na.rm = TRUE)
            )

        cat("Station Statistics for: ", selected_station, "\n")

        cat("\n1. Validation Summary for the Year:", input$year, "\n")
        print(total_validations_summary)

        cat("\n2. Validation Summary by Years:\n")
        print(yearly_validations_summary)
    })

    # Hourly Trends Plot (Day Type)
    output$profilHourlyPlot <- renderPlot({
        selected_station <- input$station
        if (is.null(selected_station) || selected_station == "") {
            return(NULL)
        }

        selected_station_ids <- combined_nb_vald |>
            filter(libelle_arret == selected_station) |>
            distinct(id_refa_lda) |>
            pull(id_refa_lda)
        if (length(selected_station_ids) == 0) {
            return(NULL)
        }

        hourly_levels <- c(
            "0H-1H", "1H-2H", "2H-3H", "3H-4H", "4H-5H", "5H-6H",
            "6H-7H", "7H-8H", "8H-9H", "9H-10H", "10H-11H", "11H-12H",
            "12H-13H", "13H-14H", "14H-15H", "15H-16H", "16H-17H",
            "17H-18H", "18H-19H", "19H-20H", "20H-21H", "21H-22H",
            "22H-23H", "23H-0H"
        )
        station_profil <- combined_profil |>
            filter(as.numeric(jour) == as.numeric(input$year)) |>
            filter(id_refa_lda %in% selected_station_ids) |>
            mutate(trnc_horr_60 = factor(trnc_horr_60, levels = hourly_levels, ordered = TRUE))
        if (nrow(station_profil) == 0) {
            return(NULL)
        }

        day_type_descriptions <- paste(
            "DIJFP: Sundays, public holidays, and bridge days",
            "JOHV: Weekdays outside school holidays",
            "JOVS: Weekdays during school holidays",
            "SAHV: Saturdays outside school holidays",
            "SAVS: Saturdays during school holidays",
            sep = "\n"
        )

        ggplot(station_profil, aes(x = trnc_horr_60, y = pourc_validations, group = cat_jour, color = cat_jour)) +
            geom_line(size = 1) +
            geom_point(size = 3) +
            scale_color_brewer(palette = "Set2") +
            labs(
                title = paste("Hourly Validation Profiles for:", selected_station, "(", input$year, ")", sep = " "),
                caption = day_type_descriptions,
                x = "Hourly Interval",
                y = "Percentage of Daily Validations",
                color = "Day Type"
            ) +
            theme_minimal() +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "bottom",
                plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                plot.caption = element_text(size = 10, hjust = 0, face = "italic")
            )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
