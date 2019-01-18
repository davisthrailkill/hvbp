# Define server logic
shinyServer(function(input, output) {
  
  output$cityselection <- renderUI({
    cities_available <- combined_tract_metrics[combined_tract_metrics$State == input$States, "City"]

    selectInput("Cities", "City", choices = unique(cities_available))
  })
  
  geo_level <- reactive({
    if(input$geo == "City"){
      data <- combined_city_metrics
    } else if(input$geo == "Census Tract"){
      data <- combined_tract_metrics
    }
  })
  
  output$measureselection <- renderUI({
    measures_available <- geo_level()[geo_level()$Category == input$Categories, 
                                       "Measure"]
    
    selectInput("Measures", "Measures", choices = unique(measures_available))
  })
  
  # output$tractselection <- renderUI({
  #   tracts_available <- combined_tract_metrics[combined_tract_metrics$City == input$Cities,
  #                                              "TractFIPS"]
  # 
  #   selectInput("Tracts", "Census Tract", choices = unique(tracts_available))
  # })
  
  # output$estimate <- renderValueBox({
  #   valueBox(formatC(combined_tract_metrics$Estimate, digits = 1, format = "f"),
  #            subtitle = "Estimate")
  # })
  # 
  # output$population <- renderValueBox({
  #   valueBox(formatC(combined_tract_metrics$Population, digits = 0, format = "f"),
  #            subtitle = "Population")
  # })
  
  # output$map <- renderLeaflet({
  #   leaflet() %>% 
  #     clearPopups() %>% 
  #     clearMarkers() %>%
  #     addTiles()%>% 
  #     addCircleMarkers(data = cities_geo, ~Long, ~Lat,
  #                      radius = 5,
  #                      fillOpacity = 0.5)
  # })
  # output$map <- renderPlotly({
  #   g <- list(
  #     scope = 'usa',
  #     projection = list(type = 'albers usa'),
  #     lakecolor = toRGB('white')
  #     )
  # 
  #   p <- plot_geo(data = cities_geo, x = ~Long, y = ~Lat) %>%
  #     add_markers(
  #       text = ~paste(City)
  #       ) %>%
  #     layout(geo = g)
  # })
  
  # output$maptable <- renderDataTable({
  #   cities_frame_table <- combined_tract_metrics %>% 
  #     select(FIPS, Year, Estimate, Population)
  # }, rownames = FALSE)
  
  output$table <- renderDataTable({
    cities_frame_table <- combined_city_metrics %>% 
      dplyr::select(State, City, Year, Category, Measure, Estimate)
  },rownames = FALSE, extensions = 'Buttons', options = list(dom = 'Bfrtip',
                                                             buttons = list('copy', 'print', list(
                                                               extend = 'collection',
                                                               buttons = c('csv', 'excel', 'pdf'),
                                                               text = 'Download'
                                                             ))))
  

  
  
  filteredData_cities <- reactive({#input$go, {
    # if(input$radio == "City"){
      x <- subset(combined_city_metrics, #geo_level(),
                  State == input$States &
                    #City == input$Cities &
                    #FIPS == input$Tracts &
                    Category == input$Categories &
                    Measure == input$Measures)
  })
  
  filteredData_tracts <- reactive({
    x <- subset(combined_tract_metrics,
                State == input$States &
                  City == input$Cities &
                  Category == input$Categories &
                  Measure == input$Measures)
  })
      
     
      # output$estimate <- renderValueBox({
      #   valueBox(formatC(x$Estimate, digits = 1, format = "f"),
      #            subtitle = "Estimate")
      # })
      # 
      # output$population <- renderValueBox({
      #   valueBox(formatC(x$Population, digits = 0, format = "f"),
      #            subtitle = "Population")
      # })

    
  output$map <- renderLeaflet({
    leaflet("map") %>% 
      clearPopups() %>%
      clearMarkers() %>%
      addTiles() %>% 
      addCircleMarkers(data = filteredData_cities(), ~Long, ~Lat, layerId = ~City,
                       radius = filteredData_cities()$Estimate/2,
                       fillOpacity = 0.3)
    })
        
      # g <- list(
      #   scope = 'usa',
      #   projection = list(type = 'albers usa'),
      #   lakecolor = toRGB('white')
      # )
      # plotlyProxy("map", session) %>%
      #   plotlyProxyInvoke(
      #     "addTraces", marker = 
      #   )
      #   add_markers(
      #     text = ~paste(City, Estimate)
      #   ) %>%
      #   layout(geo = g)
      
      # output$maptable <- renderDataTable({
      #   cities_frame_table <- x %>% 
      #     select(Year, FIPS, Estimate, Population)
      # }, rownames = FALSE)
  
  output$estimateBox <- renderValueBox({
    valueBox(formatC(mean(filteredData_cities()$Estimate, na.rm = TRUE), 
                     digits = 1, format = "f"), subtitle = "Estimate")
  })
      
  output$barplot_cities <- renderPlot({
    ggplot(data = filteredData_cities(), aes(x = reorder(factor(City), -Estimate), y = Estimate, fill = as.factor(Year))) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_errorbar(aes(ymax=mean(filteredData_cities()$Estimate), ymin=mean(filteredData_cities()$Estimate))) +
      labs(x = "City", y = "Estimate", title = "Estimate per City")
  })
  
  output$barplot_tracts <- renderPlot({
    ggplot(data = filteredData_tracts(), aes(x = reorder(factor(TractFIPS), -Estimate), y = Estimate, fill = as.factor(Year))) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_errorbar(aes(ymax=mean(filteredData_tracts()$Estimate), ymin=mean(filteredData_tracts()$Estimate))) +
      labs(x = "City", y = "Estimate", title = "Estimate per City")
  })
  
  tableData <- reactive({
    if(input$geo == "City"){
      table_data <- filteredData_cities()
    } else if(input$geo == "Census Tract"){
      table_data <- filteredData_tracts()
    }
  })

  output$table <- renderDataTable({
    cities_frame_table <- tableData() %>% 
      dplyr::select(State, City, Year, Category, Measure, Estimate)
  },rownames = FALSE, extensions = 'Buttons', options = list(dom = 'Bfrtip',
                                                           buttons = list('copy', 'print', list(
                                                             extend = 'collection',
                                                             buttons = c('csv', 'excel', 'pdf'),
                                                             text = 'Download'
                                                           ))))
  
  # NEED TO FIX - likely will need to take out list geolocation variable in datasets
  output$download <- downloadHandler(
    filename = function(){
      paste("data_", Sys.Date(), ".csv", sep = "")
    }, content = function(file){
      write.csv(tableData(), file)
    }
  )
  
  # show_popup <- function(city, lat, long){
  #   selectedCity <- filteredData()$City == city
  #   content <- as.character("Estimate: ", selectedCity$Estimate)
  #   leafletProxy("map") %>%
  #     addPopups(long, lat, content, layerId = City)
  # }
      
  
  observe({
    leafletProxy("map") %>% clearPopups()
    click <- input$map_marker_mouseover
    if (is.null(click))
      return()
  
    text <-
      paste(click$id)
    
    # isolate({
    #   show_popup(click$id, click$lat, click$lng)
    # })
    
    leafletProxy(mapId = "map") %>%
      clearPopups() %>%
      addPopups(data = click, lat = ~lat, lng = ~lng, popup = text)
  })
})

