library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(rsconnect)

source("helper.R")

### DATA ###

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

# Holidays and school breaks
holiday_data <- get_holidays()
holidays <- holiday_data$holidays
vacation_days <- holiday_data$vacation_days

### SHINY APP ###

# UI
ui <- fluidPage(
    titlePanel("DSViz: IDF Visualization"),
    sidebarLayout(
        sidebarPanel(
            selectInput("year", "Select Year:", choices = 2018:2023, selected = 2023),
            checkboxInput("show_holidays", "Show Holidays", value = TRUE),
            checkboxInput("show_vacations", "Show Vacations", value = TRUE)
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Overall Trends", plotOutput("trendPlot")),
                tabPanel("Weekly Trends", plotOutput("weeklyPlot")),
                tabPanel("Comparison", plotOutput("comparisonPlot"))
            )
        )
    )
)

# Server
server <- function(input, output) {
    filtered_data <- reactive({
        combined_nb_vald |>
            filter(year(jour) == input$year)
    })

    output$trendPlot <- renderPlot({
        data <- filtered_data()
        ggplot(data, aes(x = jour, y = nb_vald)) +
            geom_line(color = "blue") +
            labs(title = "Total Validations Over Time", x = "Date", y = "Total Validations")
    })

    output$weeklyPlot <- renderPlot({
        data <- filtered_data() |>
            mutate(weekday = wday(jour, label = TRUE)) |>
            group_by(weekday) |>
            summarize(avg_ridership = mean(nb_vald, na.rm = TRUE))

        ggplot(data, aes(x = weekday, y = avg_ridership)) +
            geom_bar(stat = "identity", fill = "skyblue") +
            labs(title = "Average Validations by Weekday", x = "Weekday", y = "Average Validations")
    })

    output$comparisonPlot <- renderPlot({
        data <- filtered_data() |>
            mutate(
                type = case_when(
                    jour %in% holidays ~ "Holiday",
                    TRUE ~ "Normal"
                )
            )

        comparison_data <- data |>
            group_by(type) |>
            summarize(avg_ridership = mean(nb_vald, na.rm = TRUE))

        ggplot(comparison_data, aes(x = type, y = avg_ridership, fill = type)) +
            geom_bar(stat = "identity") +
            labs(title = "Average Validations: Normal vs Holidays", x = "Type", y = "Average Validations")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
