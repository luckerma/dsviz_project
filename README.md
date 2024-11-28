# Analyzing and Visualizing Ridership Patterns in Île-de-France Rail Network

## Report

https://luckerma.github.io/dsviz_project/

## Data Collection

### Yearly Data

2018: https://data.iledefrance-mobilites.fr/api/explore/v2.1/catalog/datasets/histo-validations-reseau-ferre/files/e1ef1b42c0e0ff7ea62ac76937ff0a60

2019: https://data.iledefrance-mobilites.fr/api/explore/v2.1/catalog/datasets/histo-validations-reseau-ferre/files/6d7e7b859e6acac7bebad18bdb37bfc3

2020: https://data.iledefrance-mobilites.fr/api/explore/v2.1/catalog/datasets/histo-validations-reseau-ferre/files/e6bcf4c994951fc086e31db6819a3448

2021: https://data.iledefrance-mobilites.fr/api/explore/v2.1/catalog/datasets/histo-validations-reseau-ferre/files/e35b9ec0a183a8f2c7a8537dd43b124c

2022: https://data.iledefrance-mobilites.fr/api/explore/v2.1/catalog/datasets/histo-validations-reseau-ferre/files/2a6afefb59a0ccd657ba46962c96d90b

2023: https://data.iledefrance-mobilites.fr/api/explore/v2.1/catalog/datasets/validations-reseau-ferre-nombre-validations-par-jour-1er-semestre/exports/csv?lang=fr&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B

### Validations

Validations: https://data.iledefrance-mobilites.fr/explore/dataset/histo-validations-reseau-ferre/information/

### Zones & Spatial Data

Zones: https://data.iledefrance-mobilites.fr/explore/dataset/zones-d-arrets/information/
Spatial Data: https://eu.ftp.opendatasoft.com/stif/Reflex/REF_ZdA.zip

## R

### Required Packages

```R
install.packages(c("dplyr", "readr", "lubridate", "shiny", "sf", "rsconnect", "leaflet", "shinylive", "httpuv"))
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

https://luckerma.shinyapps.io/DSViz_Project/

#### Export Static Server & Run (Work in Progress)

```R
library(shinylive)
library(httpuv)

shinylive::export(
    appdir="./",
    destdir="./docs/"
)
httpuv::runStaticServer("./docs/")
```

http://127.0.0.1:{port}
