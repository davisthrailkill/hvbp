# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title = "500 Cities"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Interactive Map", tabName = "map", icon = icon("map-marked")),
        # radioButtons("radio", "City vs. Census Tract", choices = c("City", "Census Tract")),
        # selectInput("States", "States", choices = states),
        # uiOutput("cityselection"),
        # selectInput("Categories", "Category", choices = categories),
        # uiOutput("measureselection"),
        # actionButton("go", "Go"),
        downloadButton("download", "Download"),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Data", tabName = "data", icon = icon("database"))
        )
    ),
    
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "map", 
                #h2("Interactive Map"),
                column(
                  width = 3,
                  box(
                    status = "primary",
                    title = "Inputs",
                    width = NULL,
                    radioButtons("radio", "City vs. Census Tract", choices = c("City", "Census Tract")),
                    selectInput("States", "States", choices = states),
                    uiOutput("cityselection"),
                    selectInput("Categories", "Category", choices = categories),
                    uiOutput("measureselection"),
                    actionButton("go", "Go")
                  )
                ),
                column(
                  width = 9,
                  box(
                    status = "primary",
                    width = NULL,
                    #leafletOutput("map", height = 500)
                    plotlyOutput("map", height = 500)
                  )
                )
                
                # box(
                #   dataTableOutput("table"),
                #   height = "100%",
                #   width = "100%"
                # )),
        ),
        tabItem(tabName = "dashboard",
                h2("Dashboard")),
        tabItem(tabName = "data",
                h2("Data"),
                box(
                  dataTableOutput("table"),
                  height = "100%",
                  width = "100%"
                ))
      )
      #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      #leafletOutput("map")
    )
)
)


