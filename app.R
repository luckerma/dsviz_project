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

# Load and combine data for multiple years
years <- c("2018", "2019", "2020", "2021", "2022")
nb_vald_list <- list()
profil_list <- list()

for (year in years) {
    data <- load_data(year)
    nb_vald_list[[year]] <- data$nb_vald_df
    profil_list[[year]] <- data$profil_df
}

combined_nb_vald <- bind_rows(nb_vald_list)
combined_profil <- bind_rows(profil_list)

colnames(zones) <- tolower(colnames(zones))

zones_spatial <- zones_spatial |>
    st_transform(4326) |>
    mutate(idrefa_lda = as.character(idrefa_lda))
centroids <- st_centroid(zones_spatial)
zones_spatial$longitude <- st_coordinates(centroids)[, 1]
zones_spatial$latitude <- st_coordinates(centroids)[, 2]

# Aggregate data
aggregated_nb_vald <- combined_nb_vald |>
    group_by(id_refa_lda) |>
    summarize(total_nb_vald = sum(nb_vald, na.rm = TRUE))

aggregated_profil <- combined_profil |>
    group_by(id_refa_lda) |>
    summarize(avg_pourc_validations = mean(pourc_validations, na.rm = TRUE))

combined_full <- left_join(combined_nb_vald, combined_profil, by = "id_refa_lda")
spatial_data <- left_join(zones_spatial, zones, by = c("idrefa" = "zdaid"))

# Holidays and school breaks
holiday_data <- get_holidays()
holidays <- holiday_data$holidays
vacation_days <- unique(as.Date(unlist(holiday_data$school_breaks)))

### UI ###
ui <- fluidPage(
    titlePanel("DSViz: IDF Ridership Analysis"),
    sidebarLayout(
        sidebarPanel(
            selectInput("year", "Select Year:", choices = 2018:2023, selected = 2023),
            checkboxInput("show_holidays", "Show Holidays", value = TRUE),
            checkboxInput("show_vacations", "Show Vacations", value = TRUE),
            selectInput("station", "Select Station:", choices = unique(combined_nb_vald$libelle_arret)),
            actionButton("update_map", "Update Map")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Yearly Trend", plotOutput("yearlyTrendPlot")),
                tabPanel("Monthly Trend", plotOutput("monthlyTrendPlot")),
                tabPanel("Weekday Trend", plotOutput("weekdayTrendPlot")),
                tabPanel("Map", leafletOutput("stationMap")),
                tabPanel("Station Stats", verbatimTextOutput("stationStats"))
            )
        )
    )
)

### SERVER ###
server <- function(input, output, session) {
    filtered_data <- reactive({
        combined_nb_vald |>
            filter(year(jour) == input$year)
    })

    output$yearlyTrendPlot <- renderPlot({
        yearly_trend <- combined_nb_vald |>
            mutate(year = year(jour)) |>
            group_by(year) |>
            summarize(total_nb_vald = sum(nb_vald, na.rm = TRUE))

        ggplot(yearly_trend, aes(x = year, y = total_nb_vald)) +
            geom_line(size = 1, color = "blue") +
            labs(title = "Yearly Trend (Total Validations)", x = "Year", y = "Total Validations") +
            theme_minimal()
    })

    output$monthlyTrendPlot <- renderPlot({
        monthly_trend <- combined_nb_vald |>
            mutate(month = floor_date(jour, "month")) |>
            group_by(month) |>
            summarize(total_nb_vald = sum(nb_vald, na.rm = TRUE))

        ggplot(monthly_trend, aes(x = month, y = total_nb_vald)) +
            geom_line(size = 1, color = "green") +
            labs(title = "Monthly Trend (Total Validations)", x = "Month", y = "Total Validations") +
            theme_minimal()
    })

    output$weekdayTrendPlot <- renderPlot({
        weekday_trend <- combined_nb_vald |>
            mutate(weekday = wday(jour, label = TRUE, week_start = 1)) |>
            group_by(weekday) |>
            summarize(avg_ridership = mean(nb_vald, na.rm = TRUE))

        ggplot(weekday_trend, aes(x = weekday, y = avg_ridership)) +
            geom_bar(stat = "identity", fill = "skyblue") +
            labs(title = "Weekday Trend", x = "Weekday", y = "Average Validations") +
            theme_minimal()
    })

    output$stationMap <- renderLeaflet({
        leaflet() |>
            addTiles() |>
            setView(lng = 2.3522, lat = 48.8566, zoom = 10)
    })

    observeEvent(input$update_map, {
        selected_station <- input$station
        station_data <- spatial_data |>
            filter(libelle_arret == selected_station)

        leafletProxy("stationMap") |>
            clearMarkers() |>
            addCircleMarkers(
                data = station_data,
                lng = ~longitude, lat = ~latitude,
                popup = ~ paste("Station:", libelle_arret, "<br>Average Ridership:", mean(nb_vald, na.rm = TRUE)),
                color = "red"
            )
    })

    output$stationStats <- renderPrint({
        selected_station <- input$station
        station_data <- combined_nb_vald |>
            filter(libelle_arret == selected_station)

        summary(station_data$nb_vald)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
