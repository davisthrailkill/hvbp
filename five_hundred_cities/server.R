# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$cityselection <- renderUI({
    cities_available <- combined_tract_metrics[combined_tract_metrics$State == input$States, "City"]
    
    selectInput("Cities", "City", choices = unique(cities_available))
  })
  
  output$measureselection <- renderUI({
    measures_available <- combined_tract_metrics[combined_tract_metrics$Category == input$Categories, 
                                       "Measure"]
    
    selectInput("Measures", "Measures", choices = unique(measures_available))
  })
  
  output$estimate <- renderValueBox({
    valueBox(formatC(combined_city_metrics$Estimate, digits = 1, format = "f"),
             subtitle = "Estimate")
  })
  
  output$population <- renderValueBox({
    valueBox(formatC(combined_city_metrics$Population, digits = 0, format = "f"),
             subtitle = "Population")
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = cities_geo, ~Long, ~Lat) %>%
      setView(-98.5795, 39.8283, zoom = 4)
  })
  # output$map <- renderPlotly({
  #   g <- list(
  #     scope = 'usa',
  #     projection = list(type = 'albers usa'),
  #     lakecolor = toRGB('white')
  #     )
  # 
  #   p <- plot_geo(data = cities_geo, lat = ~Lat, lon = ~Long) %>%
  #     add_markers(
  #       text = ~paste(City)
  #       ) %>%
  #     layout(geo = g)
  # })
  
  output$maptable <- renderDataTable({
    cities_frame_table <- combined_tract_metrics %>% 
      select(TractFIPS, Estimate, Population)
  }, rownames = FALSE)
  
  output$table <- renderDataTable({
    cities_frame_table <- combined_tract_metrics %>% 
      select(State, City, TractFIPS, Year, Category, Measure, Estimate)
  },rownames = FALSE, extensions = 'Buttons', options = list(dom = 'Bfrtip',
                                                             buttons = list('copy', 'print', list(
                                                               extend = 'collection',
                                                               buttons = c('csv', 'excel', 'pdf'),
                                                               text = 'Download'
                                                             ))))
  
  
  filteredData <- observeEvent(input$go, {
    # if(input$radio == "City"){
      x <- subset(combined_city_metrics,
                  State == input$States &
                    City == input$Cities &
                    Category == input$Categories &
                    Measure == input$Measures)
      
      y <- subset(combined_tract_metrics,
                  State == input$States &
                    City == input$Cities &
                    Category == input$Categories &
                    Measure == input$Measures)
      
      z <- subset(combined_tract_metrics,
                  State == input$States,
                  City == input$Cities)
      
      output$estimate <- renderValueBox({
        valueBox(formatC(x$Estimate, digits = 1, format = "f"),
                 subtitle = "Estimate")
      })
      
      output$population <- renderValueBox({
        valueBox(formatC(x$Population, digits = 0, format = "f"),
                 subtitle = "Population")
      })
    
      leafletProxy("map") %>% 
        clearPopups() %>%
        clearMarkers() %>%
        addCircleMarkers(data = x, ~Long, ~Lat, layerId = ~City)
        # plot_geo(data = x, lat = ~Lat, lon = ~Long) %>% 
        # add_markers(
        #   text = ~paste(City)
        # )
      
      output$maptable <- renderDataTable({
        cities_frame_table <- y %>% 
          select(TractFIPS, Estimate, Population)
      }, rownames = FALSE)
    
      output$table <- renderDataTable({
        cities_frame_table <- y %>% 
          select(State, City, TractFIPS, Year, Category, Measure, Estimate)
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
      
      output$scatter <- renderPlot({
        ggplot(z, aes(x = z$Measure == input$in_msr, y = z$Measure == input$out_msr)) +
          geom_point()
      })
    
      output$download <- downloadHandler(
        filename = function(){
          paste("data_", Sys.Date(), ".csv", sep = "")
        }, content = function(file){
          write.csv(y, file)
        }
      )
      })
    # x <- subset(combined_tract_metrics, State == input$States &
    #               City == input$Cities &
    #               Category == input$Categories &
    #               Measure == input$Measures)
    # 
    # output$estimate <- renderValueBox({
    #   valueBox(formatC(x$Estimate, digits = 1, format = "f"),
    #            subtitle = "Estimate")
    # })
    # 
    # output$population <- renderValueBox({
    #   valueBox(formatC(x$Population, digits = 0, format = "f"),
    #            subtitle = "Population")
    # })
    # 
    # 
    # # plotlyOutput("map") %>% 
    # #   plot_geo(data = x, lat = ~Lat, lon = ~Long) %>% 
    # #   add_markers(
    # #     text = ~paste(City)
    # #   )
    # 
    # leafletProxy("map") %>%
    #   clearPopups() %>%
    #   clearMarkers() %>%
    #   # addPolygons(data = x, ~Long, ~Lat, fillColor = pal(x$Estimate),
    #   #             fillOpacity = 0.8,
    #   #             color = "#BDBDC3",
    #   #             weight = 2)
    #   addCircleMarkers(data = x, ~Long, ~Lat, layerId = ~City)
    # 
    # output$table <- renderDataTable({
    #   tracts_frame_table <- x %>% 
    #     select(State, City, TractFIPS, Year, Category, Measure, Estimate)
    # },rownames = FALSE, extensions = 'Buttons', options = list(dom = 'Bfrtip',
    #                                                            buttons = list('copy', 'print', list(
    #                                                              extend = 'collection',
    #                                                              buttons = c('csv', 'excel', 'pdf'),
    #                                                              text = 'Download'
    #                                                            ))))
    # 
    # observe({
    #   click <- input$map_marker_click
    #   if (is.null(click))
    #     return()
    #   
    #   text <-
    #     paste(click$id)
    #   
    #   leafletProxy(mapId = "map") %>%
    #     clearPopups() %>%
    #     addPopups(data = click, lat = ~lat, lng = ~lng, popup = text) %>% 
    #     setView(lng = click$lng, lat = click$lat, zoom = 6)
    # })
    # 
    # output$download <- downloadHandler(
    #   filename = function(){
    #     paste("data_", Sys.Date(), ".csv", sep = "")
    #   }, content = function(file){
    #     write.csv(x, file)
    #   }
    # )
  # })
})

