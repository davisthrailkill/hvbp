# Define UI for application
shinyUI(
  dashboardPage(
    dashboardHeader(title = "SDOH Explorer"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Overview", tabName = "overview", icon = icon("fas fa-book")),
        menuItem("Dashboard", tabName = "map", icon = icon("dashboard")),
        menuItem("Measure Explorer", tabName = "measure_explorer", 
                 icon = icon("fas fa-chart-line")),
        menuItem("Data", tabName = "data", icon = icon("fas fa-database")),
        menuItem("Measure Specs", icon = icon("far fa-newspaper"), href = "https://www.cdc.gov/500cities/measure-definitions.htm")
        )),
    dashboardBody(
      tabItems(
        tabItem("overview",
                tags$h2("Welcome to the Social Determinants of Health Explorer"),
                tags$div(list(
                  tags$h4(
                    "The United States spends a higher percentage of its gross
                    domestic product on medical care expenditure than many other developed
                    countries, but we tend to have poorer health outcomes.
                    According to a recent study published in the Journal of the American 
                    Medical Association (JAMA), the U.S. spent 17.8% of its GDP on 
                    healthcare in 2016 while other countries' spending ranged from 9.6% in 
                    Australia to 12.4% in Switzerland. Yet, life expectancy in the U.S. was 
                    the lowest of the 10 countries in the study at 78.8 years."
                  )
                ),
                tags$br(),
                tags$h4("We can therefore predict that medical care alone is insufficient
                for ensuring higher quality health. In fact, another recent study suggests that
                only about 10-20% of a population's health outcomes can be directly attributed
                to medical care. The other 80-90% can be attributed to what are known as",
                tags$b("Social Determinants of Health (SDOH),"), "which are defined by the World Health
                Organization (WHO) as", tags$i('"the conditions in which people are born, grow, live,
                work, and age."'), "These conditions can be more broadly placed into one of three
                main categories:", tags$b("health behaviors,"), tags$b("socioeconomic factors,"), "and", 
                tags$b("environmental factors.")))),
        tabItem("map",
                fluidRow(
                  box(title = "Inputs", solidHeader = TRUE,
                      status = "primary", 
                      width = 3,
                      "Use the inputs below to filter the visualizations on this page.",
                      tags$hr(),
                      selectInput("States", "State", choices = states, selected = "Tennessee"),
                      conditionalPanel(
                        condition = "input.geo == 'Census Tract'",
                        uiOutput("cityselection")
                      ),
                      selectInput("Categories", "Category", choices = categories, 
                                  selected = "Unhealthy Behaviors"),
                      uiOutput("measureselection")),
                  box(title = "Interactive Map", solidHeader = TRUE,
                    status = "primary",
                    width = 9,
                    leafletOutput("map", height = 350)
                  )
                ),
                fluidRow(
                  box(title = "Estimate of Selected Measure by City", solidHeader = TRUE,
                    status = "primary",
                    width = 6,
                    "Hover over the bars to view the estimates per city.",
                    plotlyOutput("barplot_cities")
                  ),
                  box(title = "List of Cities, Associated Estimates, and Population", 
                      solidHeader = TRUE, status = "primary",
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
                the x-axis while the Health Outcomes will render along the y-axis."),
                tags$hr(),
                fluidRow(
                  column(width = 3,
                    box(title = "Inputs", solidHeader = TRUE,
                        status = "primary",
                        width = NULL,
                        selectInput("scatterStates", "State", choices = scatter_states,
                                    selected = "Tennessee"),
                        selectInput("in_msr", "Input Measure", choices = in_msrs,
                                    selected = "Current Smoking"),
                        selectInput("out_msr", "Health Outcome", choices = out_msrs,
                                    selected = "COPD")),
                    valueBoxOutput("correlation", width = NULL)),
                  column(width = 9,
                    box(title = "Measure Explorer", solidHeader = TRUE,
                        status = "primary",
                        width = NULL,
                        "Hover over each dot to view a city's estimates.",
                        plotlyOutput("scatter"))
                    )
                  )
                ),
        tabItem("data",
                box(title = "Raw Data", solidHeader = TRUE,
                    status = "primary",
                    width = 12,
                    dataTableOutput("dataTable"),
                    downloadButton("download", "Download"))),
        tabItem("measure_specs")
      )
    )
  )
)

