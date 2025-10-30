# ============================================================================
# Melbourne Explorer - R Shiny Dashboard
# ============================================================================

# ============================================================================
# 1. Libraries
# ============================================================================

library(shiny)         
library(plotly)       
library(dplyr)         
library(shinyjs)       
library(httr)          
library(jsonlite)      
library(lubridate)     
library(readxl)        
library(htmltools)    
library(leaflet)      
source("tableau-in-shiny-v1.2.R") 

# ============================================================================
# 2. Data loading helpers
# ============================================================================

# -------------------------
# 2.1 Weather helpers
# -------------------------

# Load weather data from data_processed
load_weather_data <- function() {
  Sys.setlocale("LC_TIME", "C")
  tryCatch({
    data_path <- file.path("data_processed", "microclimate.xlsx")
    if (!file.exists(data_path)) {
      stop("Required file not found: data_processed/microclimate.xlsx")
    }
    data <- read_excel(data_path)
    
    # Data file already contains the desired site; keep all rows
    data <- data %>%
      na.omit()
    
    # Convert time column
    data$Time <- as.POSIXct(data$Time, tz = "Australia/Melbourne", origin = "1970-01-01")
    
    # Keep records after 2024-11-01
    cutoff_date <- as.POSIXct("2024-11-01", tz = "Australia/Melbourne")
    data <- data %>% filter(Time >= cutoff_date)
    
    # Add month and day fields
    data$Month <- month(data$Time)
    data$MonthName <- month(data$Time, label = TRUE, abbr = TRUE)
    data$Day <- as.Date(data$Time)
    
    return(data)
  }, error = function(e) {
    message("Error loading weather data: ", e$message)
    return(NULL)
  })
}

# Fetch 7-day forecast from Open-Meteo
get_weather_forecast <- function() {
  tryCatch({
    lat <- -37.8136  # Melbourne latitude
    lon <- 144.9631  # Melbourne longitude
    
    url <- paste0(
      "https://api.open-meteo.com/v1/forecast?",
      "latitude=", lat,
      "&longitude=", lon,
      "&daily=temperature_2m_max,temperature_2m_min",
      "&timezone=Australia/Melbourne",
      "&forecast_days=7"
    )
    
    response <- GET(url)
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text", encoding = "UTF-8"))
      
      forecast_df <- data.frame(
        Date = as.Date(data$daily$time),
        TempMax = data$daily$temperature_2m_max,
        TempMin = data$daily$temperature_2m_min
      )
      
      return(forecast_df)
    } else {
      return(NULL)
    }
  }, error = function(e) {
    message("Error fetching forecast data: ", e$message)
    return(NULL)
  })
}

# -------------------------
# 2.2 Budget helpers
# -------------------------

# Country to currency map
country_currency <- list(
  "Australia" = "AUD",
  "China" = "CNY", 
  "New Zealand" = "NZD",
  "India" = "INR",
  "Singapore" = "SGD",
  "Hong Kong" = "HKD",
  "Malaysia" = "MYR",
  "Japan" = "JPY",
  "USA" = "USD",
  "UK" = "GBP"
)

# Currency symbols
currency_symbols <- list(
  "AUD" = "$",
  "CNY" = "¥",
  "NZD" = "$",
  "INR" = "₹",
  "SGD" = "S$",
  "HKD" = "HK",
  "MYR" = "RM",
  "JPY" = "¥",
  "USD" = "$",
  "GBP" = "£"
)

# ============================================================================
# 3. UI definitions
# ============================================================================

# -------------------------
# 3.1 Home UI
# -------------------------
home_page_ui <- function() {
  div(id = "home-page",
    # Hero section
    div(class = "hero-section",
      h1(class = "hero-title", "Plan Your Perfect Melbourne Trip"),
      p(class = "hero-subtitle", 
        "Plan your visit, explore landmarks, manage your budget, and travel with confidence!")
    ),
    
    # Feature cards
    div(class = "feature-cards",
      div(class = "cards-container",
        # Weather card
        div(class = "feature-card green clickable", id = "weather-card",
          div(class = "card-header-row",
              div(class = "card-icon", "🌧"),
              div(class = "card-title", "Weather")
          ),
          div(class = "card-description",
              "Plan your visit based on Melbourne's unique four-seasons-in-one-day weather. Discover the best months for outdoor and indoor activities."),
          div(class = "card-bottom",
              div(class = "card-media", tags$img(src = "images/weather.jpg", alt = "Weather")),
              actionButton("go_weather", "Open Weather", class = "chart-button")
          )
        ),
        
        # Landmarks card
        div(class = "feature-card white clickable", id = "landmarks-card",
          div(class = "card-header-row",
              div(class = "card-icon", "🏙"),
              div(class = "card-title", "Landmarks & Culture")
          ),
          div(class = "card-description",
              "Discover Melbourne's icons and cultural experiences, from vibrant street art to historic laneways and landmarks."),
          div(class = "card-bottom",
              div(class = "card-media", tags$img(src = "images/landmark.jpg", alt = "Landmarks & Culture")),
              actionButton("go_landmarks", "Open Landmarks", class = "chart-button")
          )
        ),
        
        # Budget card
        div(class = "feature-card green clickable", id = "budget-card",
          div(class = "card-header-row",
              div(class = "card-icon", "💰"),
              div(class = "card-title", "Budget")
          ),
          div(class = "card-description",
              "Estimate your expenses with our interactive budget planner. Compare travel costs in your own currency across hotels, food, transport, and more."),
          div(class = "card-bottom",
              div(class = "card-media", tags$img(src = "images/budget.jpg", alt = "Budget")),
              actionButton("go_budget", "Open Budget", class = "chart-button")
          )
        ),
        
        # FAQs card
        div(class = "feature-card white clickable", id = "faq-card",
          div(class = "card-header-row",
              div(class = "card-icon", "👥"),
              div(class = "card-title", "FAQs & Support")
          ),
          div(class = "card-description",
              "Browse quick answers, data notes, and meet the team!"),
          div(class = "card-bottom",
              div(class = "card-media", tags$img(src = "images/support.jpg", alt = "FAQs & Support")),
              actionButton("go_faq", "Open FAQs", class = "chart-button")
          )
        )
      )
    )
  )
}

# -------------------------
# 3.2 Weather page UI
# -------------------------
weather_page_ui <- function() {
  div(id = "weather-page", style = "display: none;",
    div(class = "page-content",
      h1("Weather"),
      div(class = "dashboard-grid",
        div(class = "dashboard-card",
          style = "grid-column: span 2;",
          h3("Annual Temperature Trends"),
          p(style = "font-size: 1.2em; color: #666; margin-top: -10px;",
            "Click on a month to view daily temperatures"),
          plotlyOutput("temp_plot", height = "450px", width = "100%")
        ),
        div(class = "dashboard-card",
          style = "grid-column: span 1;",
          h3("Daily Weather Lookup"),
          div(style = "margin-top: 20px;",
            selectInput(
              "select_month",
              "Select Month:",
              choices = setNames(1:12, month.name),
              selected = as.numeric(format(Sys.Date(), "%m"))
            ),
            uiOutput("date_selector_ui"),
            actionButton(
              "lookup_weather",
              "Look Up Weather",
              class = "chart-button",
              style = paste(
                "width: 100%;",
                "margin-top: 10px;",
                "background-color: #28A27C;",
                "color: white;",
                "border: none;",
                "border-radius: 5px;",
                "padding: 8px 0;"
              )
            )
          ),
          div(style = "margin-top: 30px;",
            uiOutput("selected_weather_info")
          )
        ),
        div(class = "dashboard-card",
          h3("Annual Humidity Trends"),
          plotlyOutput("humidity_plot", height = "300px")
        ),
        div(class = "dashboard-card",
          h3("Wind Rose Chart"),
          plotlyOutput("wind_rose_plot", height = "300px")
        ),
        div(class = "dashboard-card",
          h3("7-Day Weather Forecast"),
          plotlyOutput("forecast_plot", height = "300px")
        ),
      # Sources note
      div(style = "margin-top: 1rem; color: #6b7280;",
          tags$small(
            HTML(
              "Sources: City of Melbourne Open Data – Microclimate Sensors Data ",
              "<a href='https://data.melbourne.vic.gov.au/explore/dataset/microclimate-sensors-data/information/' target='_blank'>link</a>",
              " &nbsp;|&nbsp; Open‑Meteo API ",
              "<a href='https://open-meteo.com/en/docs' target='_blank'>link</a>"
            )
          )
      )
      )
    )
  )
}

# -------------------------
# 3.3 Landmarks page UI
# -------------------------
landmarks_page_ui <- function() {
  div(id = "landmarks-page", style = "display: none;",
    div(class = "page-content",
      h1("Landmarks & Culture"),
      div(id = "landmarks-viz-container", uiOutput("landmarks_viz_ui")),
      # Sources note
      div(style = "margin-top: 1rem; color: #6b7280;",
          tags$small(
            HTML(
              "Sources: ",
              "City of Melbourne Open Data – Pedestrian Counting System ",
              "<a href='https://data.melbourne.vic.gov.au/explore/dataset/pedestrian-counting-system-monthly-counts-per-hour/information/' target='_blank'>link</a>",
              " &nbsp;|&nbsp; ",
              "City of Melbourne Open Data – Landmarks and Places of Interest ",
              "<a href='https://data.melbourne.vic.gov.au/explore/dataset/landmarks-and-places-of-interest-including-schools-theatres-health-services-spor/information/' target='_blank'>link</a>",
              " &nbsp;|&nbsp; ",
              "500 of Melbourne's most acclaimed restaurants (TripAdvisor, 2024) – Tripadvisor API ",
              "<a href='https://www.tripadvisor.com/developers' target='_blank'>link</a>",
              " &nbsp;|&nbsp; ",
              "YouTube: Coxcomb Chart in Tableau ",
              "<a href='https://youtu.be/YokyjNsyNZ4?si=xTtBAOFjbVrX382l' target='_blank'>link</a>",
              " &nbsp;|&nbsp; ",
              "Markers (Emoji) ",
              "<a href='https://twemoji-cheatsheet.vercel.app/' target='_blank'>link</a>"
            )
          )
      )
   )
  )
}

# -------------------------
# 3.4 Budget page UI
# -------------------------
budget_page_ui <- function() {
  div(id = "budget-page", style = "display: none;",
    div(class = "page-content",
      h1("Budget Planner"),
      
      div(class = "budget-layout",
        # Top controls
        div(class = "budget-top-controls",
          div(class = "control-row",
            div(class = "control-group",
              selectInput("country", "Country / Region", 
                         choices = c("Australia", "China", "New Zealand", "India", 
                            "Singapore", "Hong Kong", "Malaysia", "Japan", 
                            "USA", "UK"), selected = "Australia")
            ),
            div(class = "control-group budget-category",
              radioButtons(
                "budget_category", "Budget Category",
                choices = c("Essential", "Comfort", "Deluxe"), inline = TRUE, selected = "Essential"
              )
            )
          )
        ),
        
        # Two columns: donut (left), map (right)
        div(class = "budget-mid-grid",
          div(class = "chart-container",
            h3("Daily Budget Breakdown"),
            div(id = "currency_info", class = "currency-info", "All prices shown in AUD"),
            plotlyOutput("budget_donut", height = "450px")
          ),
          div(class = "chart-container",
            h3("Flight Routes to Melbourne"),
            leafletOutput("flight_map", height = "450px")
          )
        ),
        # Sources note
        div(style = "margin-top: 1rem; color: #6b7280;",
            tags$small(
              HTML(
                "Sources: Bureau of Infrastructure and Transport Research Economics – Domestic Air Fares · ",
                "<a href='https://www.bitre.gov.au/statistics/aviation/air_fares' target='_blank'>link</a>",
                " &nbsp;|&nbsp; ExchangeRate‑API · ",
                "<a href='https://app.exchangerate-api.com/dashboard/confirmed' target='_blank'>link</a>",
                " &nbsp;|&nbsp; BudgetYourTrip – Melbourne · ",
                "<a href='https://www.budgetyourtrip.com/australia/melbourne' target='_blank'>link</a>"
              )
            )
        )
      )
    )
  )
}

# -------------------------
# 3.5 FAQ page UI
# -------------------------
faq_page_ui <- function() {
  div(id = "faq-page", style = "display: none;",
    div(class = "page-content faq-page",
      h1("FAQs & Support"),
      div(class = "faq-grid",
        div(class = "faq-column",
          div(class = "faq-card",
            h2("Data Sources"), # Data Sources
            tags$ul(class = "faq-list",
              tags$li(HTML(
                "City of Melbourne Open Data – Microclimate Sensors Data · ",
                "<a href='https://data.melbourne.vic.gov.au/explore/dataset/microclimate-sensors-data/information/' target='_blank'>link</a>"
              )),
              tags$li(HTML(
                "Open‑Meteo API · ",
                "<a href='https://open-meteo.com/en/docs' target='_blank'>link</a>"
              )),
              tags$li(HTML(
                "City of Melbourne Open Data – Pedestrian Counting System · ",
                "<a href='https://data.melbourne.vic.gov.au/explore/dataset/pedestrian-counting-system-monthly-counts-per-hour/information/' target='_blank'>link</a>"
              )),
              tags$li(HTML(
                "City of Melbourne Open Data – Landmarks and Places of Interest · ",
                "<a href='https://data.melbourne.vic.gov.au/explore/dataset/landmarks-and-places-of-interest-including-schools-theatres-health-services-spor/information/' target='_blank'>link</a>"
              )),
              tags$li(HTML(
                "500 of Melbourne's most acclaimed restaurants (TripAdvisor, 2024) – Tripadvisor API · ",
                "<a href='https://www.tripadvisor.com/developers' target='_blank'>link</a>"
              )),
              tags$li(HTML(
                "Bureau of Infrastructure and Transport Research Economics – Domestic Air Fares · ",
                "<a href='https://www.bitre.gov.au/statistics/aviation/air_fares' target='_blank'>link</a>"
              )),
              tags$li(HTML(
                "ExchangeRate‑API · ",
                "<a href='https://app.exchangerate-api.com/dashboard/confirmed' target='_blank'>link</a>"
              )),
              tags$li(HTML(
                "BudgetYourTrip – Melbourne · ",
                "<a href='https://www.budgetyourtrip.com/australia/melbourne' target='_blank'>link</a>"
              )),
              tags$li(HTML(
                "YouTube: Coxcomb Chart in Tableau · ",
                "<a href='https://youtu.be/YokyjNsyNZ4?si=xTtBAOFjbVrX382l' target='_blank'>link</a>"
              )),
              tags$li(HTML(
                "Markers (Emoji) · ",
                "<a href='https://twemoji-cheatsheet.vercel.app/' target='_blank'>link</a>"
              )),
              tags$li(HTML(
                "ABS – Overseas Arrivals and Departures, Australia · ",
                "<a href='https://www.abs.gov.au/statistics/industry/tourism-and-transport/overseas-arrivals-and-departures-australia/latest-release' target='_blank'>link</a>"
              )),
              tags$li(HTML(
                "Image sources · ",
                "<a href='http://xhslink.com/o/5hbQV0lkb0v' target='_blank'>1</a>",
                " &nbsp; ",
                "<a href='http://xhslink.com/o/Afdqdr0DLw1' target='_blank'>2</a>",
                " &nbsp; ",
                "<a href='http://xhslink.com/o/7KoX7nnSgrN' target='_blank'>3</a>",
                " &nbsp; ",
                "<a href='http://xhslink.com/o/59RkNailTaq' target='_blank'>4</a>"
              ))
            )
          ),
          div(class = "faq-card",
            h2("Contact"),
            p(HTML("Email: <a href='mailto:zhihanw1@unimelb.edu.au'>zhihanw1@unimelb.edu.au</a>")),
          )
        ),
        # Project Card
        div(class = "faq-column",
          div(class = "faq-card",
            h2("About the Project"),
            p("Melbourne Explorer was developed as part of GEOM90007 group project at the University of Melbourne. Our chosen target audience is 
            tourists visiting Melbourne, and the theme focuses on trip planning and city exploration, including weather insights, major 
            attractions, and budget estimation. The dashboard was built using R Shiny and Tableau, combining the strengths
             of both platforms to deliver dynamic and user-friendly data exploration."),
            tags$img(src = "images/unimelb_logo.png",
                     alt = "The University of Melbourne",
                     style = "width: 100%; height: auto; margin-top: 0.5rem;")
          ),
          # Creators card
          div(class = "faq-card creators-card",
            h2("Creators"),
            div(class = "creators-grid",
              tagList(
                div(class = "creator-card",
                  div(class = "creator-avatar", tags$img(src = "images/creators/p1.png", alt = "Tianxi Chen")),
                  div(class = "creator-meta",
                    div(class = "creator-name", "Tianxi Chen"),
                    div(class = "creator-id", "Student id: 1585095")
                  )
                ),
                div(class = "creator-card",
                  div(class = "creator-avatar", tags$img(src = "images/creators/p2.png", alt = "Yushu Hou")),
                  div(class = "creator-meta",
                    div(class = "creator-name", "Yushu Hou"),
                    div(class = "creator-id", "Student id: 1575948")
                  )
                ),
                div(class = "creator-card",
                  div(class = "creator-avatar", tags$img(src = "images/creators/p3.png", alt = "Zeyu Wang")),
                  div(class = "creator-meta",
                    div(class = "creator-name", "Zeyu Wang"),
                    div(class = "creator-id", "Student id: 1255384")
                  )
                ),
                div(class = "creator-card",
                  div(class = "creator-avatar", tags$img(src = "images/creators/p4.png", alt = "Zhihan Wang")),
                  div(class = "creator-meta",
                    div(class = "creator-name", "Zhihan Wang"),
                    div(class = "creator-id", "Student id: 1116250")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

# -------------------------
# 3.6 Footer UI
# -------------------------
footer_ui <- function() {
  div(class = "footer",
    div(class = "footer-content",
      div(class = "footer-title", "Melbourne Explorer"),
      div(class = "footer-description", 
          "Your ultimate guide to discovering the best of Melbourne!"),
    )
  )
}

# ============================================================================
# 4. Main UI
# ============================================================================

ui <- fluidPage(
  # Enable shinyjs
  useShinyjs(),
  
  # Include external CSS
  tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
  
  # Layout structure
  div(class = "main-container",
    # Header nav
    div(class = "header",
      div(class = "logo", "Melbourne Explorer"),
      div(class = "nav-menu",
        actionLink("nav_home", "Home", class = "nav-item"),
        actionLink("nav_weather", "Weather", class = "nav-item"),
        actionLink("nav_landmarks", "Landmarks", class = "nav-item"),
        actionLink("nav_budget", "Budget", class = "nav-item"),
        actionLink("nav_faq", "FAQs", class = "nav-item")
      )
    ),
    
    # Tableau embed init
    setUpTableauInShiny(),
    
    # Main content
    div(id = "main-content",
      home_page_ui(),
      weather_page_ui(),
      landmarks_page_ui(),
      budget_page_ui(),
      faq_page_ui()
    ),
    
    # Footer
    footer_ui()
  )
)

# ============================================================================
# 5. Server logic
# ============================================================================

server <- function(input, output, session) {
  
  # ==========================================================================
  # 5.1 Init and state
  # ==========================================================================
  
  # Current page
  current_page <- reactiveVal("home")
  
  # Weather module state
  selected_lookup_date <- reactiveVal(NULL)
  temp_view <- reactiveVal("yearly")
  selected_month <- reactiveVal(NULL)
  
  # Exchange rate cache
  exchange_rates <- reactiveValues(cache = list(), last_update = NULL)
  
  # Lazy init for Landmarks viz (avoid loading while hidden)
  landmarks_viz_inited <- reactiveVal(FALSE)
  ensure_landmarks_viz <- function() {
    if (!isTRUE(landmarks_viz_inited())) {
      output$landmarks_viz_ui <- renderUI({
        tableauPublicViz(
          id = "landmarks_viz",
          url = "https://public.tableau.com/views/melbourne_restaurant_landmarks/Dashboard1?:showVizHome=no&:embed=true",
          height = "610px",
          style = "width: 100%; border-radius: 8px;",
          device = "desktop"
        )
      })
      landmarks_viz_inited(TRUE)
    }
  }
  
  # ==========================================================================
  # 5.2 Data loading (reactive)
  # ==========================================================================
  
  # Weather data
  weather_data <- reactive({
    load_weather_data()
  })
  
  # Budget data
  budget_data <- reactive({
    csv_path <- "data_processed/melbourne_on_a_budget.csv"
    if (!file.exists(csv_path)) {
      stop("Required file not found: data_processed/melbourne_on_a_budget.csv")
    }
    df <- read.csv(
      csv_path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(df) <- trimws(names(df))
    df
  })
  
  # Flight fares data
  flight_fares_data <- reactive({
    csv_path <- "data_processed/fares_for_bi_2020_2025_melbourne.csv"
    if (!file.exists(csv_path)) {
      stop("Required file not found: data_processed/fares_for_bi_2020_2025_melbourne.csv")
    }
    df <- read.csv(
      csv_path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(df) <- trimws(names(df))
    df
  })
  
  # AU city coordinates
  city_coordinates_data <- reactive({
    csv_path <- "data_processed/australian_city_coordinates.csv"
    if (!file.exists(csv_path)) {
      stop("Required file not found: data_processed/australian_city_coordinates.csv")
    }
    df <- read.csv(
      csv_path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(df) <- trimws(names(df))
    df
  })
  
  # Compute average fares to Melbourne
  flight_routes_data <- reactive({
    fares <- flight_fares_data()
    coords <- city_coordinates_data()
    
    # Filter routes to Melbourne (port2 = Melbourne)
    melbourne_routes <- fares %>%
      filter(port2 == "Melbourne") %>%
      group_by(port1) %>%
      summarise(
        avg_fare = mean(real, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      rename(city = port1)
    
    # Merge coordinates data
    result <- melbourne_routes %>%
      left_join(coords, by = "city") %>%
      filter(!is.na(lat) & !is.na(lon)) %>%
      mutate(
        melbourne_lat = -37.8136,
        melbourne_lon = 144.96332
      )
    
    return(result)
  })
  
  # ==========================================================================
  # 5.3 Helpers
  # ==========================================================================
  
  # Exchange rate function
  get_exchange_rate <- function(from_currency, to_currency = "AUD") {
    if (from_currency == to_currency) return(1)
    
    cache_key <- paste(from_currency, to_currency, sep = "_")
    today <- Sys.Date()
    
    # Check cache
    if (!is.null(exchange_rates$last_update) && 
        exchange_rates$last_update == today && 
        !is.null(exchange_rates$cache[[cache_key]])) {
      return(exchange_rates$cache[[cache_key]])
    }
    
    # API call
    api_key <- "dc447c6177e0b7ae5b15936d"
    url <- paste0("https://v6.exchangerate-api.com/v6/", api_key, "/latest/", from_currency)
    
    tryCatch({
      response <- GET(url)
      if (status_code(response) == 200) {
        data <- content(response, "parsed")
        rate <- data$conversion_rates[[to_currency]]
        
        # Update cache
        exchange_rates$cache[[cache_key]] <- rate
        exchange_rates$last_update <- today
        
        return(rate)
      } else {
        return(1) # default 1 (show AUD)
      }
    }, error = function(e) {
      return(1) # default 1 on error
    })
  }
  
  # Compute converted prices
  converted_prices <- reactive({
    data <- budget_data()
    selected_category <- input$budget_category
    selected_country <- input$country
    
    # Map display categories to original columns
    category_mapping <- list(
      "Essential" = "Budget",
      "Comfort" = "Mid-Range", 
      "Deluxe" = "Luxury"
    )
    
    # Resolve original column name
    original_category <- category_mapping[[selected_category]]
    if (is.null(original_category)) {
      original_category <- "Mid-Range"  # default
    }
    
    # Selected budget column
    category_data <- data[[original_category]]
    
    # FX to local currency
    from_currency <- country_currency[[selected_country]]
    rate <- 1 / get_exchange_rate(from_currency, "AUD")
    
    # Convert price (AUD -> local)
    converted_prices <- category_data * rate
    
    # Build result frame
    result <- data.frame(
      Category = data$Category,
      Price_AUD = category_data,
      Price_Converted = converted_prices,
      Currency = from_currency,
      Rate = rate
    )
    
    return(result)
  })
  
  # ==========================================================================
  # 5.4 Weather visuals
  # ==========================================================================
  
  # Annual temperature trend
  output$temp_plot <- renderPlotly({
    data <- weather_data()
    req(data)
    
    temp_col <- "AirTemperature"
    
    if (temp_view() == "yearly") {
      # Yearly view: monthly avg
      monthly_temp <- data %>%
        group_by(Month, MonthName) %>%
        summarise(AvgTemp = mean(.data[[temp_col]], na.rm = TRUE), .groups = "drop") %>%
        arrange(Month)

      p <- plot_ly(
        monthly_temp,
        source = "temp_chart",
        x = ~MonthName,
        y = ~AvgTemp,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#28A27C", width = 3),
        marker = list(size = 8, color = "#28A27C"),
        customdata = ~Month,
        hovertemplate = paste(
          "<b>%{x}</b><br>",
          "Avg Temperature: %{y:.1f}°C<br>",
          "<extra></extra>"
        )
      ) %>%
        layout(
          xaxis = list(title = "Month"),
          yaxis = list(title = "Average Temperature (°C)"),
          margin = list(l = 70, r = 50, t = 60, b = 60)
        )
      
      p <- plotly::event_register(p, 'plotly_click')
      p
    } else {
      # Monthly view: daily avg
      month_num <- selected_month()
      req(month_num)
      
      daily_temp <- data %>%
        filter(Month == month_num) %>%
        group_by(Day) %>%
        summarise(AvgTemp = mean(.data[[temp_col]], na.rm = TRUE), .groups = "drop") %>%
        arrange(Day)
      
      month_name <- if (!is.null(month_num) && month_num >= 1 && month_num <= 12) {
        month.name[month_num]
      } else {
        "Selected Month"
      }

      p <- plot_ly(
        daily_temp,
        source = "temp_chart",
        x = ~Day,
        y = ~AvgTemp,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#FF6B6B", width = 2),
        marker = list(size = 6, color = "#FF6B6B"),
        hovertemplate = paste(
          "<b>%{x}</b><br>",
          "Avg Temperature: %{y:.1f}°C<br>",
          "<extra></extra>"
        )
      ) %>%
        layout(
          title = list(
            text = paste("Daily Temperatures in", month_name),
            font = list(size = 14)
          ),
          xaxis = list(title = "Date"),
          yaxis = list(title = "Average Temperature (°C)"),
          margin = list(l = 50, r = 20, t = 60, b = 50)
        )
      
      p <- plotly::event_register(p, 'plotly_click')
      p
    }
  })
  
  observeEvent(plotly::event_data("plotly_click", source = "temp_chart"), {
    click_data <- plotly::event_data("plotly_click", source = "temp_chart")
    req(click_data)
    
    if (temp_view() == "yearly") {
      # Month clicked -> switch to monthly view
      month_clicked <- click_data$x
      month_num <- which(month.abb == month_clicked)
      
      if (length(month_num) > 0) {
        selected_month(month_num)
        temp_view("monthly")
      }
    } else {
      # Click to return to yearly view
      temp_view("yearly")
      selected_month(NULL)
    }
  }, ignoreInit = TRUE)
  
  # Annual humidity trend
  output$humidity_plot <- renderPlotly({
    data <- weather_data()
    req(data)
    
    monthly_humidity <- data %>%
      group_by(Month, MonthName) %>%
      summarise(AvgHumidity = mean(RelativeHumidity, na.rm = TRUE), .groups = "drop") %>%
      arrange(Month)
    
    plot_ly(monthly_humidity, x = ~MonthName) %>%
      add_bars(
        y = ~AvgHumidity,
        marker = list(color = "#A3D5FF"),
        showlegend = FALSE
      ) %>%
      add_trace(
        y = ~AvgHumidity,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#28A27C", shape = "spline", width = 2),
        marker = list(size = 8, color = "#28A27C"),
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(
          title = "",
          tickvals = c(60, 65, 70, 75),
          ticktext = c("Dry", "Moderate", "Mild", "Humid"),
          range = c(60, 78)
        ),
        annotations = list(
          list(
            x = -0.02,
            y = 1.08,
            xref = "paper",
            yref = "paper",
            text = "Humidity Level",
            showarrow = FALSE,
            xanchor = "left",
            yanchor = "top",
            font = list(size = 14)
          )
        ),
        yaxis2 = list(
          title = "Average Humidity (%)",
          overlaying = "y",
          side = "right",
          tickvals = c(60, 65, 70, 75),
          ticktext = c("60", "65", "70", "75")
        ),
        bargap = 0,
        margin = list(l = 70, r = 10, t = 40, b = 50)
      )
  })
  
  # Wind rose
  output$wind_rose_plot <- renderPlotly({
    data <- weather_data()
    req(data)
    
    # Split wind directions into 8 categories
    data$WindDir8 <- cut(
      data$AverageWindDirection,
      breaks = c(0, 45, 90, 135, 180, 225, 270, 315, 360),
      labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
      include.lowest = TRUE
    )
    
    wind_summary <- data %>%
      group_by(WindDir8) %>%
      summarise(AvgSpeed = mean(AverageWindSpeed, na.rm = TRUE), .groups = "drop") %>%
      na.omit()
    
    plot_ly(
      wind_summary,
      theta = ~WindDir8,
      r = ~AvgSpeed,
      type = "barpolar",
      marker = list(color = "#28A27C"),
      hovertemplate = paste(
        "<b>%{theta}</b><br>",
        "Avg Speed: %{r:.2f} m/s<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        polar = list(
          radialaxis = list(title = "Wind Speed (m/s)", visible = TRUE),
          angularaxis = list(direction = "clockwise")
        ),
        margin = list(l = 50, r = 20, t = 40, b = 50)
      )
  })
  
  # 7-day forecast
  output$forecast_plot <- renderPlotly({
    forecast_data <- get_weather_forecast()
    req(forecast_data)
    
    plot_ly(forecast_data) %>%
      add_trace(
        x = ~Date,
        y = ~TempMax,
        type = "scatter",
        mode = "lines+markers",
        name = "Max Temp",
        line = list(color = "#FF6B6B", width = 2),
        marker = list(size = 6),
        hovertemplate = paste(
          "<b>%{x}</b><br>",
          "Max: %{y:.1f}°C<br>",
          "<extra></extra>"
        )
      ) %>%
      add_trace(
        x = ~Date,
        y = ~TempMin,
        type = "scatter",
        mode = "lines+markers",
        name = "Min Temp",
        line = list(color = "#4ECDC4", width = 2),
        marker = list(size = 6),
        hovertemplate = paste(
          "<b>%{x}</b><br>",
          "Min: %{y:.1f}°C<br>",
          "<extra></extra>"
        )
      ) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Temperature (°C)"),
        legend = list(x = 0.1, y = 0.9),
        margin = list(l = 50, r = 20, t = 40, b = 50)
      )
  })
  
  # Date selector UI
  output$date_selector_ui <- renderUI({
    data <- weather_data()
    req(data)
    
    month_num <- as.numeric(input$select_month)
    
    available_dates <- data %>%
      filter(Month == month_num) %>%
      pull(Day) %>%
      unique() %>%
      sort()
    
    if (length(available_dates) > 0) {
      date_choices <- setNames(
        as.character(available_dates),
        format(available_dates, "%B %d, %Y")
      )
      
      selectInput(
        "select_date",
        "Select Date:",
        choices = date_choices,
        selected = date_choices[[1]]
      )
    } else {
      p("No data available for this month", style = "color: #999; font-style: italic;")
    }
  })
  
  # Lookup button
  observeEvent(input$lookup_weather, {
    req(input$select_date)
    selected_lookup_date(as.Date(input$select_date))
  })
  
  # Selected date summary
  output$selected_weather_info <- renderUI({
    lookup_date <- selected_lookup_date()
    req(lookup_date)
    
    data <- weather_data()
    req(data)
    
    month_num <- lubridate::month(lookup_date)
    
    # Day data
    day_data <- data %>%
      filter(Day == lookup_date)
    
    # Monthly averages (fallback)
    month_data <- data %>%
      filter(Month == month_num) %>%
      summarise(
        AvgTemp = mean(AirTemperature, na.rm = TRUE),
        AvgHumidity = mean(RelativeHumidity, na.rm = TRUE),
        AvgWindSpeed = mean(AverageWindSpeed, na.rm = TRUE),
        AvgWindDir = mean(AverageWindDirection, na.rm = TRUE),
        .groups = "drop"
      )
    
    # If no monthly data, create empty stub
    if (nrow(month_data) == 0) {
      month_data <- data.frame(
        AvgTemp = NA_real_,
        AvgHumidity = NA_real_,
        AvgWindSpeed = NA_real_,
        AvgWindDir = NA_real_
      )
    }
    
    # Use day data else monthly avg
    if (nrow(day_data) > 0) {
      temp_val <- mean(day_data$AirTemperature, na.rm = TRUE)
      humidity_val <- mean(day_data$RelativeHumidity, na.rm = TRUE)
      wind_speed_val <- mean(day_data$AverageWindSpeed, na.rm = TRUE)
      wind_dir_val <- mean(day_data$AverageWindDirection, na.rm = TRUE)
      
      temp_source <- ""
      humidity_source <- ""
      wind_speed_source <- ""
      wind_dir_source <- ""
    } else {
      # Use monthly avg
      temp_val <- month_data$AvgTemp
      humidity_val <- month_data$AvgHumidity
      wind_speed_val <- month_data$AvgWindSpeed
      wind_dir_val <- month_data$AvgWindDir
      
      temp_source <- "*"
      humidity_source <- "*"
      wind_speed_source <- "*"
      wind_dir_source <- "*"
    }
    
    # If NA, fallback to monthly avg
    if (is.na(temp_val)) {
      temp_val <- month_data$AvgTemp
      temp_source <- "*"
    }
    if (is.na(humidity_val)) {
      humidity_val <- month_data$AvgHumidity
      humidity_source <- "*"
    }
    if (is.na(wind_speed_val)) {
      wind_speed_val <- month_data$AvgWindSpeed
      wind_speed_source <- "*"
    }
    if (is.na(wind_dir_val)) {
      wind_dir_val <- month_data$AvgWindDir
      wind_dir_source <- "*"
    }
    
    # Convert wind degrees to cardinal text
    wind_dir_text <- ""
    if (!is.na(wind_dir_val)) {
      directions <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N")
      index <- round(wind_dir_val / 45) + 1
      wind_dir_text <- directions[pmax(pmin(index, length(directions)), 1)]
    }
    
    footnote_needed <- any(c(temp_source, humidity_source, wind_speed_source, wind_dir_source) == "*")
    
    tagList(
      h4(
        format(lookup_date, "%B %d, %Y"),
        style = "color: #28A27C; margin-bottom: 15px;"
      ),
      div(
        style = "padding: 15px; background: #f8f9fa; border-radius: 5px;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
          span(style = "font-weight: 600; color: #666;", "🌡️ Temperature:"),
          span(
            style = "font-size: 1.1em; color: #333;",
            paste0(round(temp_val, 1), "°C", temp_source)
          )
        ),
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
          span(style = "font-weight: 600; color: #666;", "💧 Humidity:"),
          span(
            style = "font-size: 1.1em; color: #333;",
            paste0(round(humidity_val, 1), "%", humidity_source)
          )
        ),
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
          span(style = "font-weight: 600; color: #666;", "💨 Wind Speed:"),
          span(
            style = "font-size: 1.1em; color: #333;",
            paste0(round(wind_speed_val, 2), " m/s", wind_speed_source)
          )
        ),
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          span(style = "font-weight: 600; color: #666;", "🧭 Wind Direction:"),
          span(
            style = "font-size: 1.1em; color: #333;",
            paste0(wind_dir_text, " (", round(wind_dir_val, 0), "°)", wind_dir_source)
          )
        )
      ),
      if (footnote_needed) {
        p(
          "* Monthly average (daily data not available)",
          style = "font-size: 0.85em; color: #999; font-style: italic; margin-top: 15px;"
        )
      }
    )
  })
  
  # ==========================================================================
  # 5.5 Budget visuals
  # ==========================================================================
  
  # Update currency info
  observe({
    prices <- converted_prices()
    if (!is.null(prices) && nrow(prices) > 0) {
      currency <- prices$Currency[1]
      rate <- prices$Rate[1]
      symbol <- currency_symbols[[currency]]
      
      if (currency == "AUD") {
        currency_text <- "All prices shown in AUD"
      } else {
        currency_text <- paste0("All prices shown in ", currency, " (1 AUD = ", symbol, round(rate, 2), ")")
      }
      
      shinyjs::html("currency_info", currency_text)
    }
  })
  
  # Budget donut chart
  output$budget_donut <- renderPlotly({
    prices <- converted_prices()
    if (is.null(prices) || nrow(prices) == 0) return(NULL)
    
    currency <- prices$Currency[1]
    symbol <- currency_symbols[[currency]]
    
    plot_data <- prices %>%
      dplyr::mutate(
        value_text = paste0(
          Category, ": ",
          symbol,
          format(round(Price_Converted, 0), big.mark = ",", trim = TRUE)
        )
      )
    
    # 为分类生成稳定的配色方案
    base_palette <- c('#005f60', '#F78104', '#FAAB36', '#249EA0', '#FD5901')
    cats <- unique(plot_data$Category)
    color_map <- setNames(rep(base_palette, length.out = length(cats)), cats)
    colors_vec <- unname(color_map[plot_data$Category])

    # 创建环形图
    fig <- plot_ly(
      data = plot_data,
      labels = ~Category,
      values = ~Price_Converted,
      type = 'pie',
      hole = 0.6,
      text = ~value_text,
      textinfo = 'text',
      textposition = 'outside',
      hoverinfo = 'skip',
      marker = list(
        colors = colors_vec,
        line = list(color = '#FFFFFF', width = 2)
      )
    ) %>%
      layout(
        showlegend = FALSE,
        legend = list(
          orientation = "v",
          x = 1.05,
          y = 0.5,
          font = list(size = 12)
        ),
        margin = list(l = 50, r = 50, t = 50, b = 50),
        paper_bgcolor = '#ffffff',
        plot_bgcolor = '#ffffff'
      )  %>%
      plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
    
    return(fig)
  })
  
  # Flight routes map
  output$flight_map <- renderLeaflet({
    routes <- flight_routes_data()
    req(nrow(routes) > 0)
    
    selected_country <- input$country
    from_currency <- country_currency[[selected_country]]
    rate <- 1 / get_exchange_rate(from_currency, "AUD")
    symbol <- currency_symbols[[from_currency]]
    
    # Convert fares
    routes$converted_fare <- routes$avg_fare * rate
    
    # Create map
    map <- leaflet() %>%
      addTiles(
        urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
        attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>'
      ) %>%
      setView(lng = 136.855609, lat = -28.718546, zoom = 4)
    
    # Add city markers and routes
    for (i in seq_len(nrow(routes))) {
      city <- routes[i, ]
      
      # City marker
      map <- map %>%
        addCircleMarkers(
          lng = city$lon,
          lat = city$lat,
          radius = 6,
          color = "#28A27C",
          fillColor = "#28A27C",
          fillOpacity = 0.8,
          weight = 2,
          popup = paste0(
            "<b>", city$city, "</b><br>",
            "Average Fare: ", symbol, round(city$converted_fare, 2)
          )
        )
      
      # Route polyline (slight bend)
      mid_lat <- (city$lat + city$melbourne_lat) / 2
      mid_lon <- (city$lon + city$melbourne_lon) / 2
      
      # Offset midpoint for slight curve
      offset_factor <- 0.1
      bend_lat <- mid_lat + (city$lat - city$melbourne_lat) * offset_factor
      bend_lon <- mid_lon + (city$lon - city$melbourne_lon) * offset_factor
      
      map <- map %>%
        addPolylines(
          lng = c(city$lon, bend_lon, city$melbourne_lon),
          lat = c(city$lat, bend_lat, city$melbourne_lat),
          color = "#28A27C",
          weight = 2,
          opacity = 0.7,
          popup = paste0(
            "<b>", city$city, " → Melbourne</b><br>",
            "Average Fare: ", symbol, round(city$converted_fare, 2)
          )
        )
    }
    
    # Melbourne marker
    map <- map %>%
      addCircleMarkers(
        lng = 144.96332,
        lat = -37.8136,
        radius = 8,
        color = "#F59E0B",
        fillColor = "#F59E0B",
        fillOpacity = 1,
        weight = 3,
        popup = "<b>Melbourne</b><br>Destination City"
      )
    
    return(map)
  })
  
  # ==========================================================================
  # 5.6 Navigation
  # ==========================================================================
  
  # Page switch
  show_page <- function(page_name) {
    # Hide all pages
    shinyjs::hide("home-page")
    shinyjs::hide("weather-page")
    shinyjs::hide("landmarks-page")
    shinyjs::hide("budget-page")
    shinyjs::hide("faq-page")
    
    # Show target page
    shinyjs::show(paste0(page_name, "-page"))
    current_page(page_name)
  }
  
  # Navbar click handlers
  observeEvent(input$nav_home, {
    show_page("home")
  })
  
  observeEvent(input$nav_weather, {
    temp_view("yearly")
    selected_month(NULL)
    selected_lookup_date(NULL)
    show_page("weather")
  })
  
  observeEvent(input$nav_landmarks, {
    show_page("landmarks")
    ensure_landmarks_viz()
  })
  
  observeEvent(input$nav_budget, {
    show_page("budget")
  })
  
  observeEvent(input$nav_faq, {
    show_page("faq")
  })
  
  # Home card click handlers
  observeEvent(input$go_weather, {
    temp_view("yearly")
    selected_month(NULL)
    selected_lookup_date(NULL)
    show_page("weather")
  })
  
  observeEvent(input$go_landmarks, {
    show_page("landmarks")
    ensure_landmarks_viz()
  })
  
  observeEvent(input$go_budget, {
    show_page("budget")
  })
  
  observeEvent(input$go_faq, {
    show_page("faq")
  })
  
  # Show home on init
  observe({
    show_page("home")
  })
}

# ============================================================================
# 6. Run app
# ============================================================================

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
