# Melbourne Explorer - R Shiny Dashboard
# 重构后的多页面导航系统

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(ggplot2)
library(shinyjs)
library(httr)
library(jsonlite)
library(lubridate)
library(tidyr)
library(readxl)
library(htmltools)
library(leaflet)
source("tableau-in-shiny-v1.2.R")

##################
# WEATHER HELPERS #
##################

load_weather_data <- function() {
  Sys.setlocale("LC_TIME", "C")
  tryCatch({
    data_path <- file.path("data_processed", "microclimate.xlsx")
    if (!file.exists(data_path)) {
      stop("Required file not found: data_processed/microclimate.xlsx")
    }
    data <- read_excel(data_path)
    
    data <- data %>%
      filter(SensorLocation == "101 Collins St L11 Rooftop") %>%
      na.omit()
    
    data$Time <- as.POSIXct(data$Time, tz = "Australia/Melbourne", origin = "1970-01-01")
    
    cutoff_date <- as.POSIXct("2024-11-01", tz = "Australia/Melbourne")
    data <- data %>% filter(Time >= cutoff_date)
    
    data$Month <- month(data$Time)
    data$MonthName <- month(data$Time, label = TRUE, abbr = TRUE)
    data$Day <- as.Date(data$Time)
    
    return(data)
  }, error = function(e) {
    message("Error loading weather data: ", e$message)
    return(NULL)
  })
}

get_weather_forecast <- function() {
  tryCatch({
    lat <- -37.8136
    lon <- 144.9631
    
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

##################
# PAGE FUNCTIONS #
##################

# 首页UI函数
home_page_ui <- function() {
  div(id = "home-page",
    # Hero Section
    div(class = "hero-section",
      h1(class = "hero-title", "Plan Your Perfect Melbourne Trip"),
      p(class = "hero-subtitle", 
        "Plan your visit, explore landmarks, manage your budget, and travel with confidence")
    ),
    
    # Feature Cards Section
    div(class = "feature-cards",
      div(class = "cards-container",
        # Weather Card
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
        
        # Landmarks Card
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
        
        # Budget Card
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
        
        # FAQs Card
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

# Weather页面UI函数
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
      # Data source note
      div(style = "margin-top: 1rem; color: #6b7280;",
          tags$small(
            HTML("Source: City of Melbourne Open Data – Microclimate Sensors Data · ",
                 "<a href='https://data.melbourne.vic.gov.au/explore/dataset/microclimate-sensors-data/information/' target='_blank'>link</a>")
          )
      )
      )
    )
  )
}

# Landmarks页面UI函数
landmarks_page_ui <- function() {
  div(id = "landmarks-page", style = "display: none;",
    div(class = "page-content",
      h1("Landmarks & Culture"),
      div(id = "landmarks-viz-container", uiOutput("landmarks_viz_ui")),
      # Data source note
      div(style = "margin-top: 1rem; color: #6b7280;",
          tags$small(
            HTML(
              "Sources: ",
              "City of Melbourne Open Data – Pedestrian Counting System · ",
              "<a href='https://data.melbourne.vic.gov.au/explore/dataset/pedestrian-counting-system-monthly-counts-per-hour/information/' target='_blank'>link</a>",
              " &nbsp;|&nbsp; ",
              "City of Melbourne Open Data – Landmarks and Places of Interest · ",
              "<a href='https://data.melbourne.vic.gov.au/explore/dataset/landmarks-and-places-of-interest-including-schools-theatres-health-services-spor/information/' target='_blank'>link</a>",
              " &nbsp;|&nbsp; ",
              "500 of Melbourne's most acclaimed restaurants (TripAdvisor, 2024) – Tripadvisor API · ",
              "<a href='https://www.tripadvisor.com/developers' target='_blank'>link</a>",
              " &nbsp;|&nbsp; ",
              "YouTube: Coxcomb Chart in Tableau · ",
              "<a href='https://youtu.be/YokyjNsyNZ4?si=xTtBAOFjbVrX382l' target='_blank'>link</a>",
              " &nbsp;|&nbsp; ",
              "Markers (Emoji) · ",
              "<a href='https://twemoji-cheatsheet.vercel.app/' target='_blank'>link</a>"
            )
          )
      )
   )
  )
}

# Budget页面UI函数
budget_page_ui <- function() {
  div(id = "budget-page", style = "display: none;",
    div(class = "page-content",
      h1("Budget Planner"),
      
      # 新的三层布局：顶部控件 / 中部两列（左 Donut，右占位）/ 底部占位
      div(class = "budget-layout",
        # 顶部控件（并列）
        div(class = "budget-top-controls",
          div(class = "control-row",
            div(class = "control-group",
              selectInput("country", "Country / Region", 
                         choices = c("Australia", "China", "New Zealand", "India", 
                                   "Singapore", "Hong Kong", "Malaysia", "Japan", 
                                   "USA", "UK"),
                         selected = "Australia")
            ),
            div(class = "control-group budget-category",
              radioButtons(
                "budget_category", "Budget Category",
                choices = c("Essential", "Comfort", "Deluxe"),
                inline = TRUE, selected = "Essential"
              )
            )
          )
        ),
        
        # 中部两列：左侧 Donut，右侧地图（均为白底）
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
        # Sources footnote for budget
        div(style = "margin-top: 1rem; color: #6b7280;",
            tags$small(
              HTML(
                "Sources: Bureau of Infrastructure and Transport Research Economics – Domestic Air Fares · ",
                "<a href='https://www.bitre.gov.au/statistics/aviation/air_fares' target='_blank'>link</a>",
                " &nbsp;|&nbsp; ExchangeRate‑API · ",
                "<a href='https://app.exchangerate-api.com/dashboard/confirmed' target='_blank'>link</a>"
              )
            )
        )
      )
    )
  )
}

# FAQS页面UI函数
faq_page_ui <- function() {
  div(id = "faq-page", style = "display: none;",
    div(class = "page-content faq-page",
      h1("FAQs & Support"),
      div(class = "faq-grid",
        div(class = "faq-column",
          div(class = "faq-card",
            h2("Data Sources"),
            tags$ul(class = "faq-list",
              tags$li(HTML(
                "City of Melbourne Open Data – Microclimate Sensors Data · ",
                "<a href='https://data.melbourne.vic.gov.au/explore/dataset/microclimate-sensors-data/information/' target='_blank'>link</a>"
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
                "YouTube: Coxcomb Chart in Tableau · ",
                "<a href='https://youtu.be/YokyjNsyNZ4?si=xTtBAOFjbVrX382l' target='_blank'>link</a>"
              )),
              tags$li(HTML(
                "Markers (Emoji) · ",
                "<a href='https://twemoji-cheatsheet.vercel.app/' target='_blank'>link</a>"
              ))
            )
          ),
          div(class = "faq-card",
            h2("Contact"),
            p(HTML("Email: <a href='mailto:zhihanw1@unimelb.edu.au'>zhihanw1@unimelb.edu.au</a>")),
            tags$img(src = "images/unimelb_logo.png",
                     alt = "The University of Melbourne",
                     style = "width: 100%; height: auto; margin-top: 0.5rem;")
          )
        ),
        div(class = "faq-column",
          div(class = "faq-card",
            h2("About the Project"),
            p("Melbourne Explorer was developed as part of GEOM90007 group project at the University of Melbourne. Our chosen target audience is 
            tourists visiting Melbourne, and the theme focuses on trip planning and city exploration, including weather insights, major 
            attractions, and budget estimation. The dashboard was built using R Shiny and Tableau, combining the strengths
             of both platforms to deliver dynamic and user-friendly data exploration.")
          ),
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

# 通用Footer函数
footer_ui <- function() {
  div(class = "footer",
    div(class = "footer-content",
      div(class = "footer-title", "Melbourne Explorer"),
      div(class = "footer-description", 
          "Your ultimate guide to discovering the best of Melbourne."),
    )
  )
}

##################
# USER INTERFACE #
##################

# 主UI界面
ui <- fluidPage(
  # 启用shinyjs
  useShinyjs(),

  
  # 引入外部CSS样式（www/custom.css）
  tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
  
  # 主界面结构
  div(class = "main-container",
    # Header导航栏
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
    
    # 主内容区域
    header <- setUpTableauInShiny(),
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

################
# SHINY SERVER #
################

# 定义服务器逻辑
server <- function(input, output, session) {
  # Weather module state
  selected_lookup_date <- reactiveVal(NULL)
  temp_view <- reactiveVal("yearly")
  selected_month <- reactiveVal(NULL)
  weather_data <- reactive({
    load_weather_data()
  })
  
  # Landmarks viz lazy init (avoid loading while hidden)
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
  
  # 当前页面状态
  current_page <- reactiveVal("home")
  
  # 汇率缓存
  exchange_rates <- reactiveValues(cache = list(), last_update = NULL)

  # Tableau 嵌入配置（支持环境变量或 Markdown 文件）
  tableau_embed <- reactiveValues(url = NULL, html = NULL)

  observe({
    tableau_embed$url <- NULL
    tableau_embed$html <- NULL
    
    # 优先使用环境变量
    env_url <- Sys.getenv("TABLEAU_LANDMARKS_URL", unset = NA)
    if (!is.na(env_url) && nzchar(env_url)) {
      tableau_embed$url <- env_url
      return()
    }
    
    # 其次尝试从文档中提取 Tableau Public 嵌入代码
    md_path <- file.path("tableau", "landmarks_restaurants.md")
    if (file.exists(md_path)) {
      content <- tryCatch(readLines(md_path, warn = FALSE), error = function(e) "")
      content_combined <- paste(content, collapse = "\n")
      
      # 尝试根据 host_url + name 构建标准嵌入链接
      host_match <- regexec("name='host_url' value='([^']+)'", content_combined)
      host_res <- regmatches(content_combined, host_match)[[1]]
      name_match <- regexec("name='name' value='([^']+)'", content_combined)
      name_res <- regmatches(content_combined, name_match)[[1]]
      if (length(host_res) >= 2 && length(name_res) >= 2) {
        host_url <- utils::URLdecode(host_res[2])
        viz_name <- name_res[2]
        embed_url <- paste0(host_url, if (!grepl("/$", host_url)) "/" else "", "views/", viz_name, "?:showVizHome=no&:embed=true")
        tableau_embed$url <- embed_url
        return()
      }
    
    # 若无法构建链接但存在完整 HTML，则直接嵌入
    if (grepl("tableauPlaceholder", content_combined, fixed = TRUE)) {
      tableau_embed$html <- content_combined
      return()
    }
  }
  
  default_public_url <- "https://public.tableau.com/views/melbourne_restaurant_landmarks/Dashboard1?:showVizHome=no&:embed=true"
  tableau_embed$url <- default_public_url
  # 默认使用提供的公开链接
  })
  
  # ==================
  # Weather visualisations
  # ==================
  output$temp_plot <- renderPlotly({
    data <- weather_data()
    req(data)
    
    temp_col <- "AirTemperature"
    
    if (temp_view() == "yearly") {
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
      
      p <- plotly::event_register(p, "plotly_click")
      p
    } else {
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
      
      p <- plotly::event_register(p, "plotly_click")
      p
    }
  })
  
  observeEvent(plotly::event_data("plotly_click", source = "temp_chart"), ignoreInit = TRUE, {
    click_data <- plotly::event_data("plotly_click", source = "temp_chart")
    req(click_data)
    
    if (temp_view() == "yearly") {
      month_clicked <- click_data$x
      month_num <- which(month.abb == month_clicked)
      
      if (length(month_num) > 0) {
        selected_month(month_num)
        temp_view("monthly")
      }
    } else {
      temp_view("yearly")
      selected_month(NULL)
    }
  })
  
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
  
  output$wind_rose_plot <- renderPlotly({
    data <- weather_data()
    req(data)
    
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
  
  observeEvent(input$lookup_weather, {
    req(input$select_date)
    selected_lookup_date(as.Date(input$select_date))
  })
  
  output$selected_weather_info <- renderUI({
    lookup_date <- selected_lookup_date()
    req(lookup_date)
    
    data <- weather_data()
    req(data)
    
    month_num <- lubridate::month(lookup_date)
    
    day_data <- data %>%
      filter(Day == lookup_date)
    
    month_data <- data %>%
      filter(Month == month_num) %>%
      summarise(
        AvgTemp = mean(AirTemperature, na.rm = TRUE),
        AvgHumidity = mean(RelativeHumidity, na.rm = TRUE),
        AvgWindSpeed = mean(AverageWindSpeed, na.rm = TRUE),
        AvgWindDir = mean(AverageWindDirection, na.rm = TRUE),
        .groups = "drop"
      )
    
    if (nrow(month_data) == 0) {
      month_data <- data.frame(
        AvgTemp = NA_real_,
        AvgHumidity = NA_real_,
        AvgWindSpeed = NA_real_,
        AvgWindDir = NA_real_
      )
    }
    
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
      temp_val <- month_data$AvgTemp
      humidity_val <- month_data$AvgHumidity
      wind_speed_val <- month_data$AvgWindSpeed
      wind_dir_val <- month_data$AvgWindDir
      
      temp_source <- "*"
      humidity_source <- "*"
      wind_speed_source <- "*"
      wind_dir_source <- "*"
    }
    
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
  
  # 国家货币映射
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
  
  # 货币符号映射
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
  
  # 获取汇率函数
  get_exchange_rate <- function(from_currency, to_currency = "AUD") {
    if (from_currency == to_currency) return(1)
    
    cache_key <- paste(from_currency, to_currency, sep = "_")
    today <- Sys.Date()
    
    # 检查缓存
    if (!is.null(exchange_rates$last_update) && 
        exchange_rates$last_update == today && 
        !is.null(exchange_rates$cache[[cache_key]])) {
      return(exchange_rates$cache[[cache_key]])
    }
    
    # API调用
    api_key <- "dc447c6177e0b7ae5b15936d"
    url <- paste0("https://v6.exchangerate-api.com/v6/", api_key, "/latest/", from_currency)
    
    tryCatch({
      response <- GET(url)
      if (status_code(response) == 200) {
        data <- content(response, "parsed")
        rate <- data$conversion_rates[[to_currency]]
        
        # 更新缓存
        exchange_rates$cache[[cache_key]] <- rate
        exchange_rates$last_update <- today
        
        return(rate)
      } else {
        return(1) # 默认返回1，显示AUD价格
      }
    }, error = function(e) {
      return(1) # 出错时返回1，显示AUD价格
    })
  }
  
  # 读取预算数据
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
  
  # 读取历史汇率数据（1 AUD -> 目标货币）
  exchange_rates_data <- reactive({
    csv_path <- "data_processed/exchange_rates_2024_9_to_2025_9.csv"
    if (!file.exists(csv_path)) {
      stop("Required file not found: data_processed/exchange_rates_2024_9_to_2025_9.csv")
    }
    df <- read.csv(
      csv_path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(df) <- trimws(tolower(names(df)))
    # 列 "units" 为日期
    if ("units" %in% names(df)) {
      df$units <- as.Date(df$units)
    }
    df
  })
  
  # 读取航班票价数据
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
  
  # 读取澳洲城市坐标数据
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
  
  # 计算各城市到墨尔本的平均票价
  flight_routes_data <- reactive({
    fares <- flight_fares_data()
    coords <- city_coordinates_data()
    
    # 筛选出到墨尔本的航线（port2为Melbourne）
    melbourne_routes <- fares %>%
      filter(port2 == "Melbourne") %>%
      group_by(port1) %>%
      summarise(
        avg_fare = mean(real, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      rename(city = port1)
    
    # 合并坐标数据
    result <- melbourne_routes %>%
      left_join(coords, by = "city") %>%
      filter(!is.na(lat) & !is.na(lon)) %>%
      mutate(
        # 添加墨尔本坐标
        melbourne_lat = -37.8136,
        melbourne_lon = 144.96332
      )
    
    return(result)
  })

  # 计算转换后的价格
  converted_prices <- reactive({
    data <- budget_data()
    selected_category <- input$budget_category
    selected_country <- input$country
    
    # 映射新的类别名称到原始列名
    category_mapping <- list(
      "Essential" = "Budget",
      "Comfort" = "Mid-Range", 
      "Deluxe" = "Luxury"
    )
    
    # 获取对应的原始列名
    original_category <- category_mapping[[selected_category]]
    if (is.null(original_category)) {
      original_category <- "Mid-Range"  # 默认值
    }
    
    # 获取选中的预算类别数据
    category_data <- data[[original_category]]
    
    # 获取汇率（显示为本币）
    from_currency <- country_currency[[selected_country]]
    rate <- 1 / get_exchange_rate(from_currency, "AUD")
    
    # 转换价格（AUD -> 本币）
    converted_prices <- category_data * rate
    
    # 创建数据框
    result <- data.frame(
      Category = data$Category,
      Price_AUD = category_data,
      Price_Converted = converted_prices,
      Currency = from_currency,
      Rate = rate
    )
    
    return(result)
  })
  
  # 更新货币信息显示
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
  
  # Donut Chart输出
  output$budget_donut <- renderPlotly({
    prices <- converted_prices()
    if (is.null(prices) || nrow(prices) == 0) return(NULL)
    
    currency <- prices$Currency[1]
    symbol <- currency_symbols[[currency]]
    
    plot_data <- prices %>%
      dplyr::mutate(
        # 格式化金额，带千位分隔符
        value_text = paste0(
          Category, ": ",
          symbol,
          format(round(Price_Converted, 0), big.mark = ",", trim = TRUE)
        )
      )
    
    # 为分类生成稳定的绿-橙-青色系配色，保证类别顺序变化时颜色不乱
    base_palette <- c('#2E7D32', '#FF9800', '#C66900', '#0097A7', '#81C784')
    cats <- unique(plot_data$Category)
    color_map <- setNames(rep(base_palette, length.out = length(cats)), cats)
    colors_vec <- unname(color_map[plot_data$Category])

    # 创建donut chart
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
      ) %>%
      # 添加平滑的动画效果
      animation_opts(
        frame = 200,   # 每帧持续时间（毫秒）
        transition = 100,  # 过渡时间（毫秒）
        easing = "cubic-in-out",  # 缓动函数
        redraw = FALSE  # 不重绘整个图表
      ) %>%
      plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
    
    return(fig)
  })

  # # Tableau 可视化嵌入（Landmarks 页面）
  # output$landmarks_tableau <- renderUI({
  #   embed_url <- tableau_embed$url
  #   embed_html <- tableau_embed$html
  #   if (!is.null(embed_url) && nzchar(embed_url)) {
  #     tags$div(
  #       class = "tableau-embed-container",
  #       tags$iframe(
  #         src = embed_url,
  #         width = "100%",
  #         height = "600",
  #         style = "border:none;",
  #         allowfullscreen = NA,
  #         sandbox = "allow-scripts allow-popups allow-same-origin allow-forms"
  #       )
  #     )
  #   } else if (!is.null(embed_html) && nzchar(embed_html)) {
  #     htmltools::HTML(embed_html)
  #   } else {
  #     tags$div(
  #       class = "tableau-placeholder",
  #       tags$p(
  #         "Publish the Tableau workbook and set \"TABLEAU_LANDMARKS_URL\" or update",
  #         tags$code("tableau/landmarks_restaurants.md"),
  #         "with the embed link.",
  #         tags$br(),
  #         "Current placeholder file: tableau/melbourne_restaurant_landmarks.twb"
  #       )
  #     )
  #   }
  # })
  
  # 航班地图输出
  output$flight_map <- renderLeaflet({
    routes <- flight_routes_data()
    req(nrow(routes) > 0)
    
    selected_country <- input$country
    from_currency <- country_currency[[selected_country]]
    rate <- 1 / get_exchange_rate(from_currency, "AUD")
    symbol <- currency_symbols[[from_currency]]
    
    # 转换票价
    routes$converted_fare <- routes$avg_fare * rate
    
    # 创建地图
    map <- leaflet() %>%
      addTiles(
        urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
        attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a>'
      ) %>%
      setView(lng = 136.855609, lat = -28.718546, zoom = 4)
    
    # 添加城市标记和航线
    for (i in seq_len(nrow(routes))) {
      city <- routes[i, ]
      
      # 添加城市标记
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
      
      # 添加航线（带轻微弯曲）
      # 计算航线中点，用于弯曲效果
      mid_lat <- (city$lat + city$melbourne_lat) / 2
      mid_lon <- (city$lon + city$melbourne_lon) / 2
      
      # 添加轻微偏移创建弯曲效果
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
    
    # 添加墨尔本标记
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
  
  # 导航函数
  show_page <- function(page_name) {
    # 隐藏所有页面
    shinyjs::hide("home-page")
    shinyjs::hide("weather-page")
    shinyjs::hide("landmarks-page")
    shinyjs::hide("budget-page")
    shinyjs::hide("faq-page")
    
    # 显示目标页面
    shinyjs::show(paste0(page_name, "-page"))
    current_page(page_name)
  }
  
  # 导航菜单点击事件
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
  
  # Feature card buttons -> navigate to pages
  observeEvent(input$go_weather, {
    temp_view("yearly")
    selected_month(NULL)
    selected_lookup_date(NULL)
    show_page("weather")
  })
  observeEvent(input$go_landmarks, { show_page("landmarks"); ensure_landmarks_viz() })
  observeEvent(input$go_budget,    { show_page("budget") })
  observeEvent(input$go_faq,       { show_page("faq") })
  
       
  # 初始化时显示首页
  observe({
    show_page("home")
  })
}

# 运行应用
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
# runApp('Project3_Dashboard.R')
