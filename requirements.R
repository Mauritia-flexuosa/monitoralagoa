# requirements.R - Instala dependências
list_of_packages <- c(
  "shiny",
  "shinydashboard",
  "plotly",
  "DT",
  "dplyr",
  "lubridate",
  "corrplot",
  "reshape2",
  "httr",
  "jsonlite"
)

new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, repos = "http://cran.rstudio.com/")

# Carrega pacotes
lapply(list_of_packages, require, character.only = TRUE)
