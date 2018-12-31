# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #filteredData <- reactive({
    #cities_frame[cities_frame$State == input$state,]
  #})
   
  output$map <- renderLeaflet({
    mapdata <- cities_frame %>% 
      filter(State == input$States)
    
    leaflet(mapdata) %>% 
      addTiles() %>% 
      setView(-98.5795, 39.8283, zoom = 4) %>% 
      addCircleMarkers(~Long, ~Lat)
  })
  
  #observe({
    #leafletProxy("map", data = mapdata) %>% 
      #clearShapes() %>% 
      #addCircles(~Long, ~Lat)
  #})
})
