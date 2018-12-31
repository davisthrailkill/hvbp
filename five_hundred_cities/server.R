# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  filteredData <- reactive({
    x <- cities_frame[cities_frame$State == input$States,]
  })

   
  output$map <- renderLeaflet({
    df <- filteredData()
    #mapdata <- cities_frame %>% 
      #filter(State == input$States)
    
    leaflet(data = df) %>% 
      addTiles() %>% 
      setView(-98.5795, 39.8283, zoom = 4) %>% 
      addCircleMarkers(~Long, ~Lat, popup = df$City)
  })
  
  #observe({
    #leafletProxy("map") %>% 
      #addCircles(data = filteredData(), 
                 #lng = ~Long,
                 #lat = ~Lat)
  #})
})
