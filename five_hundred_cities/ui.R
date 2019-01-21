# Define UI for application
shinyUI(
  dashboardPage(
    dashboardHeader(title = "500 Cities"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Map", tabName = "map", icon = icon("map")),
        menuItem("Measure Explorer", tabName = "measure_explorer", icon = icon("dashboard")),
        #menuItem("Census Dashboard", tabName = "census_dashboard", icon = icon("dashboard")),
        menuItem("Data", tabName = "data", icon = icon("data")),
        # radioButtons("geo", "Geographic Level",
        #              choices = c("City" = "City", "Census Tract" = "Census Tract")),
        # selectInput("States", "States", choices = states, selected = "Tennessee"),
        # conditionalPanel(
        #   condition = "input.geo == 'Census Tract'",
        #   uiOutput("cityselection")
        # ),
        # conditionalPanel(
        #   condition = "input.geo == 'Census Tract'",
        #   uiOutput("tractselection")
        # ),
        # selectInput("Categories", "Category", choices = categories),
        # uiOutput("measureselection"),
        #uiOutput("yearselection"),
        # actionButton("go", "Go"),
        # actionButton("reset", "Reset Map"),
        downloadButton("download", "Download")
        )),
    dashboardBody(
      tabItems(
        tabItem("map",
                fluidRow(
                  box(title = "Inputs", solidHeader = TRUE,
                      status = "primary", 
                      width = 3,
                      radioButtons("geo", "Geographic Level",
                                   choices = c("City" = "City", "Census Tract" = "Census Tract"),
                                   inline = TRUE),
                      selectInput("States", "States", choices = states, selected = "Tennessee"),
                      conditionalPanel(
                        condition = "input.geo == 'Census Tract'",
                        uiOutput("cityselection")
                      ),
                      selectInput("Categories", "Category", choices = categories),
                      uiOutput("measureselection")),
                  box(title = "Interactive Map", solidHeader = TRUE,
                    status = "primary",
                    width = 9,
                    leafletOutput("map", height = 350)
                  )
                ),
                fluidRow(
                  box(title = "Estimate by City", solidHeader = TRUE,
                    status = "primary",
                    width = 6,
                    plotlyOutput("barplot_cities")
                  ),
                  box(title = "Table", solidHeader = TRUE, status = "primary",
                    # valueBoxOutput("state_estimateBox"),
                    # valueBoxOutput("state_popBox")
                    dataTableOutput("table")
                  )
                )
              ),
        tabItem("measure_explorer",
                box(title = "Inputs", solidHeader = TRUE,
                    status = "primary",
                    width = 3,
                    selectInput("scatterStates", "State", choices = scatter_states,
                                selected = "Tennessee"),
                    selectInput("in_msr", "Input Measure", choices = in_msrs),
                    selectInput("out_msr", "Health Outcome", choices = out_msrs)),
                box(title = "Measure Explorer", solidHeader = TRUE,
                    status = "primary",
                    width = 9,
                    plotlyOutput("scatter"))),
        #tabItem("census_dashboard", plotOutput("barplot_tracts")),
        tabItem("data",
                box(title = "Data Table", solidHeader = TRUE,
                    status = "primary",
                    width = 12,
                    dataTableOutput("dataTable")))
      )
    )
  )
)

    
    # ("Interactive Map", tabName = "map", icon = icon("map-marked")),
    # menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    # menuItem("Data", tabName = "data", icon = icon("database")),
    
    
      #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      #leafletOutput("map")



