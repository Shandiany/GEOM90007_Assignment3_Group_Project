# Install missing packages required by the dashboard

cran <- "https://cloud.r-project.org"

needed <- c(
  # Core app
  "shiny", "plotly", "dplyr", "shinyjs", "httr", "jsonlite",
  "lubridate", "readxl", "htmltools", "leaflet"
)

installed <- rownames(installed.packages())
missing <- setdiff(needed, installed)

if (length(missing)) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = cran, dependencies = TRUE)
} else {
  message("All required packages are already installed.")
}

message("\nDone. You can now run the app with:\n  shiny::runApp('Project3_Dashboard.R')\n")