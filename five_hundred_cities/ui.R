# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title = "500 Cities"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Interactive Map", tabName = "map", icon = icon("map-marked")),
        selectInput("States", "States", choices = states),
        uiOutput("cityselection"),
        #selectInput("Cities", "Cities", choices = cities),
        selectInput("Categories", "Category", choices = categories),
        uiOutput("measureselection"),
        radioButtons("radio", "City vs. Census Tract", choices = c("City", "Census Tract")),
        actionButton("go", "Go"),
        downloadButton("download", "Download"),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Data", tabName = "data", icon = icon("database"))
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
                  dataTableOutput("table"),
                  height = "100%",
                  width = "100%"
                )),
        tabItem(tabName = "dashboard",
                h2("Dashboard")),
        tabItem(tabName = "data",
                h2("Data"))
      )
      #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      #leafletOutput("map")
    )
)
)


