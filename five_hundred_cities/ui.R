# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title = "500 Cities"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Interactive Map", tabName = "map", icon = icon("dashboard")),
        #menuItem("Inputs", icon = icon("th"),
                 selectInput("States", "States", choices = states),
                 selectInput("Cities", "Cities", choices = cities),
                 selectInput("Categories", "Category", choices = categories),
                 conditionalPanel(
                   condition = "input.Categories == 'Prevention'",
                   selectInput("Measures", "Measures", choices = prevention_measures)),
                 conditionalPanel(
                   condition = "input.Categories == 'Health Outcomes'",
                   selectInput("Measures", "Measures", choices = outcome_measures)),
                 conditionalPanel(
                   condition = "input.Categories == 'Unhealthy Behaviors'",
                   selectInput("Measures", "Measures", choices = behavior_measures)),
                 conditionalPanel(
                   condition = "input.Measures == 'High Blood Pressure' ||
                   input.Measures == 'Taking BP Medication' ||
                   input.Measures == 'Cholesterol Screening' ||
                   input.Measures == 'High Cholesterol'",
                   selectInput("Year", "Year", choices = odd_years)),
                 conditionalPanel(
                   condition = "input.Measures != 'High Blood Pressure &
                   input.Measures != 'Taking BP Medication' &
                   input.Measures != 'Cholesterol Sreening &
                   input.Measures != 'High Cholesterol'",
                   selectInput("Year", "Year", choices = even_years)),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("th"))
        )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "map", 
                h2("Interactive Map"),
                box(
                  height = '100%',
                  width = '100%',
                  leafletOutput("map")
                ),
                box(
                  height = '100%',
                  width = '100%',
                  dataTableOutput("table")
                )
                ),
        tabItem(tabName = "dashboard",
                h2("Dashboard"))
      )
      #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      #leafletOutput("map")
    )
)
)


