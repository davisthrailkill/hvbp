# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    leafletOutput("map"),
    selectInput("States", "States", choices = states)
  )
)


