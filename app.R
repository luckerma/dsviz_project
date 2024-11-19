library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(leaflet)
library(sf)

source("helper.R")

### DATA ###
zones <- read_delim("data/zones.csv", delim = ";")
zones_spatial <- st_read("data/REF_ZdA/PL_ZDL_R_14_11_2024.shp", crs = 2154)

# Load and combine yearly data
years <- c("2018", "2019", "2020", "2021", "2022")
nb_vald_list <- lapply(years, function(year) load_data(year)$nb_vald_df)
combined_nb_vald <- bind_rows(nb_vald_list)

# Prepare Spatial Data
zones_spatial <- zones_spatial |>
    st_transform(4326) |>
    mutate(idrefa_lda = as.character(idrefa_lda))
centroids <- st_centroid(zones_spatial)
zones_spatial$longitude <- st_coordinates(centroids)[, 1]
zones_spatial$latitude <- st_coordinates(centroids)[, 2]

# Holidays and School Breaks
holiday_data <- get_holidays()
holidays <- holiday_data$holidays
vacation_days <- holiday_data$vacation_days

combined_nb_vald <- combined_nb_vald |>
    mutate(
        type = case_when(
            jour %in% holidays ~ "Holiday",
            jour %in% vacation_days ~ "Vacation",
            !jour %in% holidays & !jour %in% vacation_days ~ "Normal",
        )
    )

### UI ###
ui <- fluidPage(
    titlePanel("DSViz: IDF Ridership Analysis"),
    sidebarLayout(
        sidebarPanel(
            dateInput("start_date1", "Starting Date for Period 1:", value = "2018-01-01"),
            dateInput("start_date2", "Starting Date for Period 2:", value = "2022-01-01"),
            numericInput("period_days", "Number of Days for Comparison:", value = 7, min = 1, step = 1),
            checkboxInput("show_holidays", "Show Holidays", value = TRUE),
            checkboxInput("show_vacations", "Show Vacations", value = TRUE),
            selectInput("station", "Select Station:", choices = unique(combined_nb_vald$libelle_arret))
        ),
        mainPanel(
            tabsetPanel(
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
                        column(12, verbatimTextOutput("stationStats"))
                    )
                )
            )
        )
    )
)

### SERVER ###
server <- function(input, output, session) {
    # Filtered Data
    filtered_data <- reactive({
        data <- combined_nb_vald

        if (!input$show_holidays) {
            data <- data |> filter(type != "Holiday")
        }
        if (!input$show_vacations) {
            data <- data |> filter(type != "Vacation")
        }

        data
    })

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
            geom_col(fill = "skyblue") +
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
            geom_col(fill = "lightgreen") +
            labs(title = "Total Validations (Monthly)", x = "Month", y = "Total Validations") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    # Yearly Trend Plot
    output$yearlyTrendPlot <- renderPlot({
        yearly_trend <- filtered_data() |>
            mutate(year = year(jour)) |>
            group_by(year, type) |>
            summarize(avg_vald = mean(nb_vald, na.rm = TRUE))

        ggplot(yearly_trend, aes(x = year, y = avg_vald, color = type, group = type)) +
            geom_line(size = 1) +
            geom_point(size = 3) +
            labs(title = "Yearly Trend", x = "Year", y = "Average Validations") +
            theme_minimal()
    })

    # Monthly Trend Plot
    output$monthlyTrendPlot <- renderPlot({
        monthly_trend <- filtered_data() |>
            mutate(month = floor_date(jour, "month")) |>
            group_by(month, type) |>
            summarize(mean_vald = mean(nb_vald, na.rm = TRUE))

        ggplot(monthly_trend, aes(x = month, y = mean_vald, color = type, group = type)) +
            geom_line(size = 1) +
            geom_point(size = 3) +
            labs(title = "Monthly Trend", x = "Month", y = "Average Validations") +
            theme_minimal()
    })

    # Weekday Trend Plot
    output$weekdayTrendPlot <- renderPlot({
        weekday_trend <- filtered_data() |>
            mutate(weekday = wday(jour, label = TRUE, week_start = 1)) |>
            group_by(weekday, type) |>
            summarize(avg_ridership = mean(nb_vald, na.rm = TRUE))

        ggplot(weekday_trend, aes(x = weekday, y = avg_ridership, fill = type)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = "Weekday Trend", x = "Weekday", y = "Average Validations") +
            theme_minimal()
    })

    # Comparison Days Plot
    output$comparisonDaysPlot <- renderPlot({
        data <- comparison_data()

        daily_trend <- data |>
            group_by(relative_day, period, original_date = jour) |>
            summarize(avg_vald = mean(nb_vald, na.rm = TRUE))

        ggplot(daily_trend, aes(x = relative_day, y = avg_vald, color = period, group = period)) +
            geom_line(size = 1) +
            geom_point(size = 3) +
            scale_x_continuous(
                breaks = daily_trend$relative_day,
                labels = daily_trend$original_date
            ) +
            labs(
                title = paste("Comparison of Two Periods (", input$period_days, " Days)", sep = ""),
                x = "Time",
                y = "Average Validations"
            ) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    # Comparison Weekday Plot
    output$comparisonWeekdayPlot <- renderPlot({
        data <- comparison_data()

        weekday_trend <- data |>
            group_by(weekday = wday(jour, label = TRUE, week_start = 1), period) |>
            summarize(avg_vald = mean(nb_vald, na.rm = TRUE))

        ggplot(weekday_trend, aes(x = weekday, y = avg_vald, fill = period)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(
                title = paste("Weekday Comparison of Two Periods (", input$period_days, " Days)", sep = ""),
                x = "Weekday",
                y = "Average Validations"
            ) +
            theme_minimal()
    })

    # Comparison Days Plot Total
    output$comparisonDaysPlotTotal <- renderPlot({
        data <- comparison_data()

        daily_trend <- data |>
            group_by(relative_day, period, original_date = jour) |>
            summarize(total_validations = sum(nb_vald, na.rm = TRUE))

        ggplot(daily_trend, aes(x = relative_day, y = total_validations, color = period, group = period)) +
            geom_line(size = 1) +
            geom_point(size = 3) +
            scale_x_continuous(
                breaks = daily_trend$relative_day,
                labels = daily_trend$original_date
            ) +
            labs(
                title = paste("Comparison of Two Periods (", input$period_days, " Days)", sep = ""),
                x = "Time",
                y = "Total Validations"
            ) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    # Comparison Weekday Plot Total
    output$comparisonWeekdayPlotTotal <- renderPlot({
        data <- comparison_data()

        weekday_trend <- data |>
            group_by(weekday = wday(jour, label = TRUE, week_start = 1), period) |>
            summarize(total_validations = sum(nb_vald, na.rm = TRUE))

        ggplot(weekday_trend, aes(x = weekday, y = total_validations, fill = period)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(
                title = paste("Weekday Comparison of Two Periods (", input$period_days, " Days)", sep = ""),
                x = "Weekday",
                y = "Total Validations"
            ) +
            theme_minimal()
    })

    # Station Map
    output$stationMap <- renderLeaflet({
        leaflet(zones_spatial) |>
            addTiles() |>
            addCircleMarkers(
                lng = ~longitude,
                lat = ~latitude,
                popup = ~ paste("Station:", nom_lda),
                layerId = ~nom_lda,
                color = "red"
            )
    })

    observeEvent(input$stationMap_marker_click, {
        click <- input$stationMap_marker_click
        selected_station <- click$id

        if (!is.null(selected_station)) {
            updateSelectInput(
                session,
                inputId = "station",
                selected = selected_station
            )
        }
    })

    # Station Stats
    output$stationStats <- renderPrint({
        selected_station <- input$station
        station_data <- combined_nb_vald |>
            filter(libelle_arret == selected_station)

        summary(station_data$nb_vald)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
