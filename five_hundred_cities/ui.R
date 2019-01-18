# Define UI for application
shinyUI(
  fluidPage(
    titlePanel(title = "500 Cities"),
    
    sidebarLayout(
      sidebarPanel(
        
        radioButtons("geo", "Geographic Level",
                     choices = c("City" = "City", "Census Tract" = "Census Tract")),
        selectInput("States", "States", choices = states, selected = "Tennessee"),
        conditionalPanel(
          condition = "input.geo == 'Census Tract'",
          uiOutput("cityselection")
        ),
        # conditionalPanel(
        #   condition = "input.geo == 'Census Tract'",
        #   uiOutput("tractselection")
        # ),
        selectInput("Categories", "Category", choices = categories),
        uiOutput("measureselection"),
        # actionButton("go", "Go"),
        # actionButton("reset", "Reset Map"),
        downloadButton("download", "Download")
        ),
      mainPanel(
        tabsetPanel(
          tabPanel("Map", leafletOutput("map", height = 500)),
          tabPanel("City Dashboard",
                   valueBoxOutput("estimateBox"),
                   plotOutput("barplot_cities")),
          tabPanel("Census Dashboard", plotOutput("barplot_tracts")),
          tabPanel("Data", dataTableOutput("table"))
        )
      )
    )
  )
)
    
    # ("Interactive Map", tabName = "map", icon = icon("map-marked")),
    # menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    # menuItem("Data", tabName = "data", icon = icon("database")),
    
    
      #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      #leafletOutput("map")



