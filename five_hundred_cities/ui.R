# Define UI for application
shinyUI(
  dashboardPage(
    dashboardHeader(title = "SDOH Explorer"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Overview", tabName = "overview", icon = icon("fas fa-book")),
        menuItem("Map", tabName = "map", icon = icon("fas fa-map-marked-alt")),
        menuItem("Measure Explorer", tabName = "measure_explorer", 
                 icon = icon("fas fa-chart-line")),
        #menuItem("Census Dashboard", tabName = "census_dashboard", icon = icon("dashboard")),
        menuItem("Data", tabName = "data", icon = icon("fas fa-database"))
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
        # downloadButton("download", "Download")
        )),
    dashboardBody(
      tabItems(
        tabItem("overview",
                tags$h1("Welcome to the Social Determinants of Health Explorer"),
                tags$h4("The United States spends a higher percentage of its gross
                domestic product on medical care expenditure than many other developed
                countries, but we tend to have poorer health outcomes.
                According to a study published in the Journal of the American Medical Association
                (JAMA), the U.S. spent 17.8% of its GDP on healthcare in 2016 while other 
                countries' spending ranged from 9.6% in Australia to 12.4% in Switzerland. 
                Yet, ife expectancy in the U.S. was the lowest of the 10 countries in the 
                study at 78.8 years."),
                tags$br(),
                tags$h4("We can therefore definitively say that medicare care alone is insufficient
                for ensuring higher quality health. In fact, another recent study suggests that
                only about 10-20% of a population's health outcomes can be directly attributed
                to medical care. The other 80-90% can be attributed to what are known as
                Social Determinants of Health (SDOH), which are defined by the World Health
                Organization (WHO) as 'the conditions in which people are born, grow, live,
                work, and age.' These conditions can be more broadly placed into one of three
                main categories: health behaviors, socioeconomic factors, and environmental
                factors.")),
        tabItem("map",
                fluidRow(
                  box(title = "Inputs", solidHeader = TRUE,
                      status = "primary", 
                      width = 3,
                      "Use the inputs below to filter the visualizations on this page.",
                      tags$hr(),
                      # radioButtons("geo", "Geographic Level",
                      #              choices = c("City" = "City", "Census Tract" = "Census Tract"),
                      #              inline = TRUE),
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
                h2("Measure Explorer"),
                h4("The Measure Explorer allows you to identify correlations between social
                determinants and health outcomes in cities across the United States. You can 
                do this by using the inputs below to filter the scatterplot. Choose a state and
                two measures that you wish to explore. The Input Measures will render along
                the x-axis while the Health Outcome Measures will render along the y-axis."),
                tags$hr(),
                box(title = "Inputs", solidHeader = TRUE,
                    status = "primary",
                    width = 3,
                    selectInput("scatterStates", "State", choices = scatter_states,
                                selected = "Tennessee"),
                    selectInput("in_msr", "Input Measure", choices = in_msrs,
                                selected = "Current Smoking"),
                    selectInput("out_msr", "Health Outcome", choices = out_msrs,
                                selected = "COPD")),
                box(title = "Measure Explorer", solidHeader = TRUE,
                    status = "primary",
                    width = 9,
                    plotlyOutput("scatter"))),
        #tabItem("census_dashboard", plotOutput("barplot_tracts")),
        tabItem("data",
                box(title = "Raw Data", solidHeader = TRUE,
                    status = "primary",
                    width = 12,
                    dataTableOutput("dataTable"),
                    downloadButton("download", "Download")))
      )
    )
  )
)

    
    # ("Interactive Map", tabName = "map", icon = icon("map-marked")),
    # menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    # menuItem("Data", tabName = "data", icon = icon("database")),
    
    
      #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      #leafletOutput("map")



