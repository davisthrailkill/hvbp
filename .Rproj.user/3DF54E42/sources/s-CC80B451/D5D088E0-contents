# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title = "500 Cities"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Inputs", icon = icon("bar-chart-o"),
                 selectInput("States", "States", choices = states))
        )
    ),
    
    dashboardBody(
      tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      leafletOutput("map")
    )
))


