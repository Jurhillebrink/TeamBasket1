required_packages = c(
  "shiny",
  "readr", 
  "tidyr", 
  "stringr", 
  "dplyr", 
  "ggplot2",
  "grid",
  "jpeg",
  "RCurl",
  "DBI",
  "png",
  "data.table",
  "mongolite",
  "shinydashboard",
  "shinyWidgets",
  "shinythemes",
  "plotly",
  "markdown",
  "shinyjs",
  "RJDBC",
  "RODBC",
  "openssl",
  "V8",
  "DT",
  "shinyWidgets",
  
  #optional
  "hexbin",
  "httr",
  "jsonlite"
)


packages_to_install = required_packages[!(required_packages %in% installed.packages()[, 1])]

if (length(packages_to_install) > 0) {
  install.packages(packages_to_install, repos = "https://cran.rstudio.com")
}

