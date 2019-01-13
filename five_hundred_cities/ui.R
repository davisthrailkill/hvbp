# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title = "500 Cities"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Interactive Map", tabName = "map", icon = icon("map-marked")),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Data", tabName = "data", icon = icon("database")),
        #selectInput("geo", "Geographic Level", choices = c("City", "Census Tract")),
        selectInput("States", "States", choices = states),
        uiOutput("cityselection"),
        selectInput("Categories", "Category", choices = categories),
        uiOutput("measureselection"),
        actionButton("go", "Go"),
        downloadButton("download", "Download")
        )
    ),
    
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "map", 
                #h2("Interactive Map"),
                # column(
                #   width = 3,
                #   box(
                #     status = "primary",
                #     title = "Inputs",
                #     width = NULL,
                #     radioButtons("radio", "City vs. Census Tract", choices = c("City", "Census Tract")),
                #     selectInput("States", "States", choices = states),
                #     uiOutput("cityselection"),
                #     selectInput("Categories", "Category", choices = categories),
                #     uiOutput("measureselection"),
                #     actionButton("go", "Go")
                #   )
                # ),
                fluidRow(
                  valueBoxOutput("estimate"),
                  valueBoxOutput("population")
                ),
                column(
                  width = 5,
                  box(
                    status = "primary",
                    width = NULL,
                    dataTableOutput('maptable')
                  )
                ),
                column(
                  width = 7,
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
                  width = 4,
                  box(
                    status = "primary",
                    width = NULL,
                    selectInput("in_msr", "Input Measure", choices = in_msrs),
                    selectInput("out_msr", "Outcome Measure", choices = out_msrs)
                  )
                ),
                column(
                  width = 8,
                  box(
                    status = "primary",
                    width = NULL,
                    plotOutput("scatter")
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


