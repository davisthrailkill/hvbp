# Define UI for application
shinyUI(
  dashboardPage(
    dashboardHeader(title = "500 Cities"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Interactive Map", tabName = "map", icon = icon("map-marked")),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Data", tabName = "data", icon = icon("database")),
        radioButtons("geo", "Geographic Level",
                     choices = c("City" = "City", "Census Tract" = "Census Tract")),
        selectInput("States", "States", choices = states),
        uiOutput("cityselection"),
        # conditionalPanel(
        #   condition = "input.geo == 'Census Tract'",
        #   uiOutput("tractselection")
        # ),
        selectInput("Categories", "Category", choices = categories),
        uiOutput("measureselection"),
        actionButton("go", "Go"),
        downloadButton("download", "Download")
        )
    ),
    
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "map", 
                column(
                  width = 12,
                  box(
                    status = "primary",
                    width = NULL,
                    leafletOutput("map", height = 500)
                    #plotlyOutput("map", height = 500)
                  )
                )
                
                # box(
                #   dataTableOutput("table"),
                #   height = "100%",
                #   width = "100%"
                # )),
        ),
        tabItem(tabName = "dashboard",
                h2("Dashboard"),
                column(
                  width = 8,
                  box(
                    status = "primary",
                    width = NULL,
                    plotOutput("barplot")
                  )
                )),
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


