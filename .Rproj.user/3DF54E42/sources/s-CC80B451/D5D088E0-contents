# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title = "500 Cities"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Inputs", icon = icon("bar-chart-o"),
                 selectInput("States", "States", choices = states),
                 selectInput("Categories", "Category", choices = categories),
                 selectInput("Measures", "Measures", choices = measures),
                 selectInput("Year", "Year", choices = year))
        )
    ),
    
    dashboardBody(
      tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      leafletOutput("map")
    )
))


