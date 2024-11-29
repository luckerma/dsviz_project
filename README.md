# Analyzing and Visualizing Ridership Patterns in Île-de-France Rail Network

## Report

https://luckerma.github.io/dsviz_project/

## Shiny App

https://luckerma.shinyapps.io/DSViz_Project/

## Data Collection

### Validations & Profiles

2018-2022:

https://data.iledefrance-mobilites.fr/explore/dataset/histo-validations-reseau-ferre/information/

2023:

https://data.iledefrance-mobilites.fr/explore/dataset/validations-reseau-ferre-nombre-validations-par-jour-1er-semestre/information/

https://data.iledefrance-mobilites.fr/explore/dataset/validations-reseau-ferre-profils-horaires-par-jour-type-1er-semestre/information/

### Zones & Spatial Data

Zones: https://data.iledefrance-mobilites.fr/explore/dataset/zones-d-arrets/information/

Spatial Data: https://eu.ftp.opendatasoft.com/stif/Reflex/REF_ZdA.zip

## R

### Required Packages

```R
install.packages(c("dplyr", "forecast", "ggplot2", "leaflet", "lubridate", "readr", "rsconnect", "sf", "shiny", "stringr"))
```

### Data Cleaning

```R
source("cleaning.R")
```

### Shiny

#### Run Local Server

```R
library(shiny)

shiny::runApp("./")
```

http://127.0.0.1:{port}

#### Deploy on Remote Server

```R
library(rsconnect)

rsconnect::deployApp(
    appDir="./",
    appName="DSViz_Project",
    forceUpdate=TRUE,
    appFiles=c(
        "app.R",
        "helper.R",
        list.files("data/cleaned_data", full.names=TRUE, recursive=TRUE)
    )
)
```

https://{username}.shinyapps.io/DSViz_Project/
