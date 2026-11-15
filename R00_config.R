paquetes <- c("rvest", "xml2", "httr", "dplyr", "stringr", "stringi", "purrr",
              "tibble", "readr", "ggplot2", "igraph", "ggraph", "tidygraph",
              "lubridate", "shiny", "bslib", "plotly", "jsonlite")
nuevos <- setdiff(paquetes, rownames(installed.packages()))
if (length(nuevos)) install.packages(nuevos)
invisible(lapply(paquetes, library, character.only = TRUE))

for (d in c("datos", "salidas", "graficos")) dir.create(d, showWarnings = FALSE)
UA <- "Mozilla/5.0 (proyecto academico UMSA - Mineria Web)"
cat("Config lista. Carpeta:", getwd(), "\n")
