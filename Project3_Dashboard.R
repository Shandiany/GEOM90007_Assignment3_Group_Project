# Melbourne Explorer - R Shiny Dashboard
# 重构后的多页面导航系统

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(ggplot2)
library(shinyjs)

##################
# PAGE FUNCTIONS #
##################

# 首页UI函数
home_page_ui <- function() {
  div(id = "home-page",
    # Hero Section
    div(class = "hero-section",
      h1(class = "hero-title", "Discover Melbourne Like a Local"),
      p(class = "hero-subtitle", 
        "Find the best time to visit, explore attractions, plan your budget, and avoid the crowds.")
    ),
    
    # Feature Cards Section
    div(class = "feature-cards",
      div(class = "cards-container",
        # Weather Card
        div(class = "feature-card green clickable", id = "weather-card",
          div(class = "card-header-row",
              div(class = "card-icon", "☁️"),
              div(class = "card-title", "Weather")
          ),
          div(class = "card-description",
              "Plan your visit based on Melbourne's unique four-seasons-in-one-day weather. Discover the best months for outdoor and indoor activities."),
          div(class = "card-chart", "Mini chart placeholder"),
          div(style = "margin-top: 0.75rem;",
              actionButton("go_weather", "Read More", class = "chart-button"))
        ),
        
        # Landmarks Card
        div(class = "feature-card white clickable", id = "landmarks-card",
          div(class = "card-header-row",
              div(class = "card-icon", "🏛"),
              div(class = "card-title", "Landmarks & Culture")
          ),
          div(class = "card-description",
              "Explore iconic locations and cultural experiences — from street art in Hosier Lane to the historic Flinders Street Station."),
          div(class = "card-chart", "Mini chart placeholder"),
          div(style = "margin-top: 0.75rem;",
              actionButton("go_landmarks", "Read More", class = "chart-button"))
        ),
        
        # Budget Card
        div(class = "feature-card green clickable", id = "budget-card",
          div(class = "card-header-row",
              div(class = "card-icon", "💰"),
              div(class = "card-title", "Budget")
          ),
          div(class = "card-description",
              "Estimate your expenses with our interactive budget planner. Compare average costs for accommodation, dining, transport, and attractions."),
          div(class = "card-chart", "Mini chart placeholder"),
          div(style = "margin-top: 0.75rem;",
              actionButton("go_budget", "Read More", class = "chart-button"))
        ),
        
        # Crowd Card
        div(class = "feature-card white clickable", id = "crowd-card",
          div(class = "card-header-row",
              div(class = "card-icon", "👥"),
              div(class = "card-title", "Crowd")
          ),
          div(class = "card-description",
              "Avoid peak times at popular spots. Use crowd predictions to explore Melbourne with ease."),
          div(class = "card-chart", "Mini chart placeholder"),
          div(style = "margin-top: 0.75rem;",
              actionButton("go_crowd", "Read More", class = "chart-button"))
        )
      )
    )
  )
}

# Weather页面UI函数
weather_page_ui <- function() {
  div(id = "weather-page", style = "display: none;",
    div(class = "page-content",
      h1("Weather Dashboard"),
      p("Plan your visit based on Melbourne's unique four-seasons-in-one-day weather."),
      div(class = "dashboard-grid"
      )
    )
  )
}

# Landmarks页面UI函数
landmarks_page_ui <- function() {
  div(id = "landmarks-page", style = "display: none;",
    div(class = "page-content",
      h1("Landmarks & Culture Dashboard"),
      p("Explore iconic locations and cultural experiences in Melbourne."),
      div(class = "dashboard-grid"
      )
   )
  )
}

# Budget页面UI函数
budget_page_ui <- function() {
  div(id = "budget-page", style = "display: none;",
    div(class = "page-content",
      h1("Budget Planner Dashboard"),
      p("Estimate your expenses with our interactive budget planner."),
      div(class = "dashboard-grid"
      )
    )
  )
}

# Crowd页面UI函数
crowd_page_ui <- function() {
  div(id = "crowd-page", style = "display: none;",
    div(class = "page-content",
      h1("Crowd Analysis Dashboard"),
      p("Avoid peak times at popular spots with crowd predictions."),
      div(class = "dashboard-grid"
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
          "Your ultimate guide to discovering the best of Melbourne. Plan your trip like a local with our curated recommendations and tools."),
      div(class = "social-links",
        a(class = "social-link", href = "#", "Facebook"),
        a(class = "social-link", href = "#", "Twitter"),
        a(class = "social-link", href = "#", "Instagram"),
        a(class = "social-link", href = "#", "Pinterest")
      )
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
        actionLink("nav_crowd", "Crowd", class = "nav-item")
      )
    ),
    
    # 主内容区域
    div(id = "main-content",
      home_page_ui(),
      weather_page_ui(),
      landmarks_page_ui(),
      budget_page_ui(),
      crowd_page_ui()
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
  
  # 当前页面状态
  current_page <- reactiveVal("home")
  
  # 导航函数
  show_page <- function(page_name) {
    # 隐藏所有页面
    shinyjs::hide("home-page")
    shinyjs::hide("weather-page")
    shinyjs::hide("landmarks-page")
    shinyjs::hide("budget-page")
    shinyjs::hide("crowd-page")
    
    # 显示目标页面
    shinyjs::show(paste0(page_name, "-page"))
    current_page(page_name)
  }
  
  # 导航菜单点击事件
  observeEvent(input$nav_home, {
    show_page("home")
  })
  
  observeEvent(input$nav_weather, {
    show_page("weather")
  })
  
  observeEvent(input$nav_landmarks, {
    show_page("landmarks")
  })
  
  observeEvent(input$nav_budget, {
    show_page("budget")
  })
  
  observeEvent(input$nav_crowd, {
    show_page("crowd")
  })
  
  # Feature card buttons -> navigate to pages
  observeEvent(input$go_weather,   { show_page("weather") })
  observeEvent(input$go_landmarks, { show_page("landmarks") })
  observeEvent(input$go_budget,    { show_page("budget") })
  observeEvent(input$go_crowd,     { show_page("crowd") })
  
       
  # 初始化时显示首页
  observe({
    show_page("home")
  })
}

# 运行应用
shinyApp(ui = ui, server = server)