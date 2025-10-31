Melbourne Explorer Dashboard
=====================================
Melbourne Explorer was developed as part of GEOM90007 group project at the University of Melbourne. Our chosen target audience is tourists visiting Melbourne, and the theme focuses on trip planning and city exploration,including weather insights, major attractions, and budget estimation. The dashboard was built using R Shiny and Tableau, combining the strengths of both platforms to deliver dynamic and user-friendly data exploration.


Run locally
-----------
Requirements:

- R 4.2+ (or recent)
- Packages: `shiny`, `plotly`, `dplyr`, `shinyjs`, `httr`, `jsonlite`, `lubridate`, `readxl`, `htmltools`, `leaflet`.
```r
source('install_packages.R')
```

Launch the app from the repository root:

```r
shiny::runApp('Project3_Dashboard.R')
```

Group Member
-----------
- TianxiChen 1585095
- Yushu Hou 1575948
- Zeyu Wang 1255384
- Zhihan Wang 1116250


Tableau integration
-------------------
This app embeds a Tableau Public visualization on the Landmarks page using a helper library.

- Tableau Public URL: https://public.tableau.com/views/melbourne_restaurant_landmarks/Dashboard1?:language=en-US&publish=yes&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link
- Helper: `tableau-in-shiny-v1.2.R` (loaded at startup via `source("tableau-in-shiny-v1.2.R")`)
- The `tableau/` folder (e.g., `tableau/landmarks_restaurants.md`) is for notes/assets. Runtime embedding is done directly from a Tableau Public URL.


Data sources
------------
Place preprocessed data files in `data_processed/` (required):

- `microclimate.xlsx`
- `melbourne_on_a_budget.csv`
- `exchange_rates_2024_9_to_2025_9.csv`
- `fares_for_bi_2020_2025_melbourne.csv`
- `australian_city_coordinates.csv`

Static assets (images) are served from `www/`. For the built‑in cards and creator photos, the app expects:

- `www/images/weather.jpg`, `www/images/landmark.jpg`, `www/images/budget.jpg`, `www/images/support.jpg`
- `www/images/creators/p1.png` to `p4.png`
- `www/images/unimelb_logo.png`
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
- ABS – Overseas Arrivals and Departures, Australia: https://www.abs.gov.au/statistics/industry/tourism-and-transport/overseas-arrivals-and-departures-australia/latest-release


Declaration 
------------

- I acknowledge the use of ChatGPT [https://chat.openai.com/] to assist this assessment.

- I entered prompts such as:
  - Cleaning and simplifying Shiny code and identifying possible issues
  -  Explain how this works ????
  - What does this CSS do?
  -  Rewriting comments 
  -  Please teach me how this chart is implemented based on this screenshot

- I used the output to:
  - Clean and refactor my code to improve readability and reduce redundancy
  - Detect and resolve logic or syntax problems
  -  Learn new Shiny features and visualization techniques
  - Rewrite code comments
