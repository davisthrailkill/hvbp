# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(-98.5795, 39.8283, zoom = 4)
  })
  
  filteredData <- observeEvent(input$go, {
    x <- cities_frame[cities_frame$State == input$States &
                        cities_frame$City == input$Cities &
                        cities_frame$Category == input$Categories &
                        cities_frame$Short_Question_Text == input$Measures,]
                        #cities_frame$Year == input$Year,]
    leafletProxy("map") %>% 
      clearMarkers() %>% 
      addCircleMarkers(data = x, ~Long, ~Lat, layerId = ~City)
    
    output$table <- renderDataTable({
      cities_frame_table <- x %>% 
        #filter(State == input$States & 
                 #City == input$Cities & 
                 #Category == input$Categories & 
                 #Short_Question_Text == input$Measures) %>% 
        select(State, City, Category, Short_Question_Text, Data_Value)
      })
    })
  })
    
  #output$map <- renderLeaflet({
    #df <- filteredData()
      
    #leaflet(data = filteredData()) %>% 
      #addTiles() %>% 
      #setView(-98.5795, 39.8283, zoom = 4) %>% 
      #addCircleMarkers(~Long, ~Lat, layerId = ~City)
      
    #output$table <- renderDataTable({
      #cities_frame_table <- cities_frame %>% 
        #filter(State == input$States & 
                  #City == input$Cities & 
                  #Category == input$Categories & 
                  #Short_Question_Text == input$Measures) %>% 
        #select(State, City, Category, Short_Question_Text, Data_Value)
    #})
  #})
  
  #output$map <- renderLeaflet({
    #df <- filteredData()
    
    #leaflet(data = df) %>% 
      #addTiles() %>% 
      #setView(-98.5795, 39.8283, zoom = 4) %>% 
      #addCircleMarkers(~Long, ~Lat, layerId = ~City)
  #})
  
  #showCityPopup <- function(city, lat, long){
    #selectedCity <- cities_frame[cities_frame$City == city,]
    #content <- as.character(tagList(
      #tags$h4("City: ", as.character(selectedCity$City))
    #))
    #leafletProxy("map") %>% addPopups(long, lat, layerId = city)
  #}
  
  #observe({
    #leafletProxy("map") %>% clearPopups()
    #click <- input$map_marker_click
    #if (is.null(click))
      #return()
    
    #isolate({
      #showCityPopup(event$id, event$lat, event$long)
    #})
    #text <-
      #paste(click$id)
    
    #leafletProxy(mapId = "map") %>%
      #clearPopups() %>%
      #addPopups(dat = click, lat = ~lat, lng = ~lng, popup = text)
    
    #output$table <- renderDataTable({
      #cities_frame_table <- cities_frame %>% 
        #filter(State == input$States & 
                 #City == input$Cities & 
                 #Category == input$Categories & 
                 #Short_Question_Text == input$Measures) %>% 
        #select(State, City, Category, Short_Question_Text)
      #cities_frame[cities_frame$City == click$id,]
    #})
  #})
  #output$table <- renderDataTable({
    #cities_frame_table <- cities_frame %>% 
      #filter(State == input$States & 
               #City == input$Cities & 
               #Category == input$Categories & 
               #Short_Question_Text == input$Measures) %>% 
      #select(State, City, Category, Short_Question_Text, Data_Value)
    #})
#})