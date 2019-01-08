# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$cityselection <- renderUI({
    cities_available <- cities_frame[cities_frame$State == input$States, "City"]
    
    selectInput("Cities", "City", choices = unique(cities_available))
  })
  
  output$measureselection <- renderUI({
    measures_available <- cities_frame[cities_frame$Category == input$Categories, 
                                       "Short_Question_Text"]
    
    selectInput("Measures", "Measures", choices = unique(measures_available))
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(data = cities_geo, ~Long, ~Lat) %>% 
      setView(-98.5795, 39.8283, zoom = 4)
  })
  
  output$table <- renderDataTable({
    cities_frame_table <- cities_frame %>% 
      select(State, City, Year, Category, Short_Question_Text, `Crude prevalence`)
  },rownames = FALSE, extensions = 'Buttons', options = list(dom = 'Bfrtip',
                                                             buttons = list('copy', 'print', list(
                                                               extend = 'collection',
                                                               buttons = c('csv', 'excel', 'pdf'),
                                                               text = 'Download'
                                                             ))))
  
  filteredData <- observeEvent(input$go, {
    x <- subset(tracts_frame, State == input$States &
                        City == input$Cities &
                        Category == input$Categories &
                        Short_Question_Text == input$Measures)
    
    leafletProxy("map") %>% 
      clearPopups() %>% 
      clearMarkers()
      #addCircleMarkers(data = x, ~Long, ~Lat, layerId = ~City)
    
    output$table <- renderDataTable({
      cities_frame_table <- x %>% 
        select(State, City, TractFIPS, Year, Category, Short_Question_Text, `Crude prevalence`)
      },rownames = FALSE, extensions = 'Buttons', options = list(dom = 'Bfrtip',
                                                                 buttons = list('copy', 'print', list(
                                                                   extend = 'collection',
                                                                   buttons = c('csv', 'excel', 'pdf'),
                                                                   text = 'Download'
                                                                 ))))
    
    observe({
      click <- input$map_marker_click
      if (is.null(click))
        return()
      
      text <-
        paste(click$id)
      
      leafletProxy(mapId = "map") %>%
        clearPopups() %>%
        addPopups(data = click, lat = ~lat, lng = ~lng, popup = text) %>% 
        setView(lng = click$lng, lat = click$lat, zoom = 6)
      })
    
    output$download <- downloadHandler(
      filename = function(){
        paste("data_", Sys.Date(), ".csv", sep = "")
        }, content = function(file){
          write.csv(x, file)
        }
      )
    })
  })

