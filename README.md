Melbourne Explorer — R Shiny Dashboard
=====================================

Run locally
-----------
Requirements:

- R 4.2+ (or recent)
- Packages: `shiny`, `plotly`, `dplyr`, `shinyjs`, `httr`, `jsonlite`, `lubridate`, `readxl`, `htmltools`, `leaflet`.

Launch the app from the repository root:

```r
shiny::runApp('Project3_Dashboard.R')
```

Data layout
-----------
Place preprocessed data files in `data_processed/` (required):

- `microclimate.xlsx`
- `melbourne_on_a_budget.csv`
- `exchange_rates_2024_9_to_2025_9.csv`
- `fares_for_bi_2020_2025_melbourne.csv`
- `australian_city_coordinates.csv`

Static assets (images) are served from `www/`. For the built‑in cards and creator photos, the app expects:

- `www/images/weather.jpg`, `www/images/landmark.jpg`, `www/images/budget.jpg`, `www/images/support.jpg`
- `www/images/creators/p1.png` … `p4.png`
- `www/images/unimelb_logo.png`


Tableau integration
-------------------
This app embeds a Tableau Public visualization on the Landmarks page using a helper library.

- Helper: `tableau-in-shiny-v1.2.R` (loaded at startup via `source("tableau-in-shiny-v1.2.R")`)
- The `tableau/` folder (e.g., `tableau/landmarks_restaurants.md`) is for notes/assets. Runtime embedding is done directly from a Tableau Public URL.


Data sources
------------
- City of Melbourne Open Data – Microclimate Sensors Data: https://data.melbourne.vic.gov.au/explore/dataset/microclimate-sensors-data/information/
- Open‑Meteo API: https://open-meteo.com/en/docs
- City of Melbourne Open Data – Pedestrian Counting System: https://data.melbourne.vic.gov.au/explore/dataset/pedestrian-counting-system-monthly-counts-per-hour/information/
- City of Melbourne Open Data – Landmarks and Places of Interest: https://data.melbourne.vic.gov.au/explore/dataset/landmarks-and-places-of-interest-including-schools-theatres-health-services-spor/information/
- 500 of Melbourne's most acclaimed restaurants (TripAdvisor, 2024) – Tripadvisor API: https://www.tripadvisor.com/developers
- Bureau of Infrastructure and Transport Research Economics – Domestic Air Fares: https://www.bitre.gov.au/statistics/aviation/air_fares
- ExchangeRate‑API: https://app.exchangerate-api.com/dashboard/confirmed
- BudgetYourTrip – Melbourne: https://www.budgetyourtrip.com/australia/melbourne
- YouTube: Coxcomb Chart in Tableau: https://youtu.be/YokyjNsyNZ4?si=xTtBAOFjbVrX382l
- Markers (Emoji): https://twemoji-cheatsheet.vercel.app/
- Image sources: 1) http://xhslink.com/o/5hbQV0lkb0v  2) http://xhslink.com/o/Afdqdr0DLw1  3) http://xhslink.com/o/7KoX7nnSgrN  4) http://xhslink.com/o/59RkNailTaq
