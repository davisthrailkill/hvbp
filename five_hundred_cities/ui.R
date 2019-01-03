# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title = "500 Cities"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Inputs", icon = icon("bar-chart-o"),
                 selectInput("States", "States", choices = states),
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
                   condition = "input.Measures %in% odd_measures",
                   selectInput("Year", "Year", choices = odd_years)),
                 conditionalPanel(
                   condition = "!input.Measures %in% odd_measures",
                   selectInput("Year", "Year", choices = even_years)))
        )
    ),
    
    dashboardBody(
      tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      leafletOutput("map")
    )
)
)


