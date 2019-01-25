# Define server logic
shinyServer(function(input, output) {
  
  output$cityselection <- renderUI({
    cities_available <- combined_tract_metrics[combined_tract_metrics$State == input$States, "City"]

    selectInput("Cities", "City", choices = unique(cities_available))
  })
  
  # geo_level <- reactive({
  #   if(input$geo == "City"){
  #     data <- combined_city_metrics
  #   } else if(input$geo == "Census Tract"){
  #     data <- combined_tract_metrics
  #   }
  # })
  
  output$measureselection <- renderUI({
    measures_available <- combined_city_metrics[combined_city_metrics$Category == input$Categories, 
                                       "Measure"]
    
    selectInput("Measures", "Measure", choices = unique(measures_available))
  })
  
  # output$yearselection <- renderUI({
  #   years_available <- geo_level()[geo_level()$Measure == input$Measures, "Year"]
  #   
  #   selectInput("Year", "Year", choices = unique(years_available))
  # })
  
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
  
  # output$table <- renderDataTable({
  #   cities_frame_table <- combined_city_metrics %>% 
  #     dplyr::select(State, City, Category, Measure, Estimate)
  # },rownames = FALSE)
  

  
  
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
  
  output$state_estimateBox <- renderValueBox({
    valueBox(formatC(mean(filteredData_cities()$Estimate, na.rm = TRUE), 
                     digits = 1, format = "f"), subtitle = "State Estimate")
  })
  
  output$state_popBox <- renderValueBox({
    valueBox(formatC(mean(filteredData_cities()$Population, na.rm = TRUE), 
                     digits = 0, format = "f"), subtitle = "State Population")
  })
      
  output$barplot_cities <- renderPlotly({
    ggplotly({
      ggplot(data = head(arrange(filteredData_cities(), desc(Estimate)),10),
                           aes(x = reorder(factor(City), Estimate), y = Estimate)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        theme_light() +
        #geom_errorbar(aes(ymax=mean(filteredData_cities()$Estimate), ymin=mean(filteredData_cities()$Estimate))) +
        labs(x = "City", y = "Estimate")
    })
  })
  
  output$barplot_tracts <- renderPlot({
    ggplot(data = filteredData_tracts(), aes(x = reorder(factor(TractFIPS), Estimate), y = Estimate)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_light() +
      #geom_errorbar(aes(ymax=mean(filteredData_tracts()$Estimate), ymin=mean(filteredData_tracts()$Estimate))) +
      labs(x = "City", y = "Estimate", title = "Estimate per City")
  })
  
  # tableData <- reactive({
  #   if(input$geo == "City"){
  #     table_data <- filteredData_cities()
  #   } else if(input$geo == "Census Tract"){
  #     table_data <- filteredData_tracts()
  #   }
  # })

  output$table <- renderDataTable({
    cities_frame_table <- filteredData_cities() %>% 
      dplyr::select(City, Estimate, Population) %>% 
      arrange(desc(Estimate))
  },rownames = FALSE)
  
  # NEED TO FIX - likely will need to take out list geolocation variable in datasets
  output$download <- downloadHandler(
    filename = function(){
      paste("data_", Sys.Date(), ".csv", sep = "")
    }, content = function(file){
      write.csv(downloadTable, file)
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
  
  scatter_filter <- reactive({
    x <- subset(cities_wide,
                State == input$scatterStates)
  })
  
  output$scatter <- renderPlotly({
    if(input$in_msr == "Health Insurance"){
      i <- "Health Insurance"
    }
    if(input$in_msr == "Taking BP Medication"){
      i <- "Taking BP Medication"
    }
    if(input$in_msr == "Annual Checkup"){
      i <- "Annual Checkup"
    }
    if(input$in_msr == "Cholesterol Screening"){
      i <- "Cholesterol Screening"
    }
    if(input$in_msr == "Colorectal Cancer Screening"){
      i <- "Colorectal Cancer Screening"
    }
    if(input$in_msr == "Core preventive services for older men"){
      i <- "Core preventive services for older men"
    }
    if(input$in_msr == "Core preventive services for older women"){
      i <- "Core preventive services for older women"
    }
    if(input$in_msr == "Dental Visit"){
      i <- "Dental Visit"
    }
    if(input$in_msr == "Mammography"){
      i <- "Mammography"
    }
    if(input$in_msr == "Pap Smear Test"){
      i <- "Pap Smear Test"
    }
    if(input$in_msr == "Binge Drinking"){
      i <- "Binge Drinking"
    }
    if(input$in_msr == "Current Smoking"){
      i <- "Current Smoking"
    }
    if(input$in_msr == "Physical Inactivity"){
      i <- "Physical Inactivity"
    }
    if(input$in_msr == "Obesity"){
      i <- "Obesity"
    }
    if(input$in_msr == "Sleep <7 hours"){
      i <- "Sleep <7 hours"
    }
    if(input$in_msr == "Absenteeism"){
      i <- "Absenteeism"
    }
    if(input$in_msr == "High school graduation"){
      i <- "High school graduation"
    }
    if(input$in_msr == "Housing cost, excessive"){
      i <- "Housing cost, excessive"
    }
    if(input$in_msr == "Neighborhood racial/ethnic segregation"){
      i <- "Neighborhood racial/ethnic segregation"
    }
    if(input$in_msr == "Racial/ethnic diversity"){
      i <- "Racial/ethnic diversity"
    }
    if(input$in_msr == "Third-grade reading proficiency"){
      i <- "Third-grade reading proficiency"
    }
    if(input$in_msr == "Unemployment"){
      i <- "Unemployment"
    }
    if(input$out_msr == "Arthritis"){
      o <- "Arthritis"
    }
    if(input$out_msr == "High Blood Pressure"){
      o <- "High Blood Pressure"
    }
    if(input$out_msr == "Stroke"){
      o <- "Stroke"
    }
    if(input$out_msr == "Cancer (except skin)"){
      o <- "Cancer (except skin)"
    }
    if(input$out_msr == "Current Asthma"){
      o <- "Current Asthma"
    }
    if(input$out_msr == "Coronary Heart Disease"){
      o <- "Coronary Heart Disease"
    }
    if(input$out_msr == "COPD"){
      o <- "COPD"
    }
    if(input$out_msr == "Diabetes"){
      o <- "Diabetes"
    }
    if(input$out_msr == "High Cholesterol"){
      o <- "High Cholesterol"
    }
    if(input$out_msr == "Chronic Kidney Disease"){
      o <- "Chronic Kidney Disease"
    }
    if(input$out_msr == "Mental Health"){
      o <- "Mental Health"
    }
    if(input$out_msr == "Physical Health"){
      o <- "Physical Health"
    }
    if(input$out_msr == "Teeth Loss"){
      o <- "Teeth Loss"
    }
    
    x_var <- scatter_filter()[, i]
    y_var <- scatter_filter()[, o]
    
    ggplotly({
      ggplot(data = scatter_filter(), aes(x = x_var, y = y_var)) +
        geom_point(aes(col = City)) +
        geom_smooth(method = "lm") +
        theme_light() +
        labs(x = input$in_msr, y = input$out_msr)
    })
  })
  
  output$correlation <- renderValueBox({
    data <- scatter_filter()
    
    if(input$in_msr == "Health Insurance"){
      i <- "Health Insurance"
    }
    if(input$in_msr == "Taking BP Medication"){
      i <- "Taking BP Medication"
    }
    if(input$in_msr == "Annual Checkup"){
      i <- "Annual Checkup"
    }
    if(input$in_msr == "Cholesterol Screening"){
      i <- "Cholesterol Screening"
    }
    if(input$in_msr == "Colorectal Cancer Screening"){
      i <- "Colorectal Cancer Screening"
    }
    if(input$in_msr == "Core preventive services for older men"){
      i <- "Core preventive services for older men"
    }
    if(input$in_msr == "Core preventive services for older women"){
      i <- "Core preventive services for older women"
    }
    if(input$in_msr == "Dental Visit"){
      i <- "Dental Visit"
    }
    if(input$in_msr == "Mammography"){
      i <- "Mammography"
    }
    if(input$in_msr == "Pap Smear Test"){
      i <- "Pap Smear Test"
    }
    if(input$in_msr == "Binge Drinking"){
      i <- "Binge Drinking"
    }
    if(input$in_msr == "Current Smoking"){
      i <- "Current Smoking"
    }
    if(input$in_msr == "Physical Inactivity"){
      i <- "Physical Inactivity"
    }
    if(input$in_msr == "Obesity"){
      i <- "Obesity"
    }
    if(input$in_msr == "Sleep <7 hours"){
      i <- "Sleep <7 hours"
    }
    if(input$in_msr == "Absenteeism"){
      i <- "Absenteeism"
    }
    if(input$in_msr == "High school graduation"){
      i <- "High school graduation"
    }
    if(input$in_msr == "Housing cost, excessive"){
      i <- "Housing cost, excessive"
    }
    if(input$in_msr == "Neighborhood racial/ethnic segregation"){
      i <- "Neighborhood racial/ethnic segregation"
    }
    if(input$in_msr == "Racial/ethnic diversity"){
      i <- "Racial/ethnic diversity"
    }
    if(input$in_msr == "Third-grade reading proficiency"){
      i <- "Third-grade reading proficiency"
    }
    if(input$in_msr == "Unemployment"){
      i <- "Unemployment"
    }
    if(input$out_msr == "Arthritis"){
      o <- "Arthritis"
    }
    if(input$out_msr == "High Blood Pressure"){
      o <- "High Blood Pressure"
    }
    if(input$out_msr == "Stroke"){
      o <- "Stroke"
    }
    if(input$out_msr == "Cancer (except skin)"){
      o <- "Cancer (except skin)"
    }
    if(input$out_msr == "Current Asthma"){
      o <- "Current Asthma"
    }
    if(input$out_msr == "Coronary Heart Disease"){
      o <- "Coronary Heart Disease"
    }
    if(input$out_msr == "COPD"){
      o <- "COPD"
    }
    if(input$out_msr == "Diabetes"){
      o <- "Diabetes"
    }
    if(input$out_msr == "High Cholesterol"){
      o <- "High Cholesterol"
    }
    if(input$out_msr == "Chronic Kidney Disease"){
      o <- "Chronic Kidney Disease"
    }
    if(input$out_msr == "Mental Health"){
      o <- "Mental Health"
    }
    if(input$out_msr == "Physical Health"){
      o <- "Physical Health"
    }
    if(input$out_msr == "Teeth Loss"){
      o <- "Teeth Loss"
    }
    
    x_var <- scatter_filter()[, i]
    y_var <- scatter_filter()[, o]
    
    corr <- cor(x_var, y_var, method = "pearson")
    
    valueBox(formatC(corr, digits = 2,
                     format = "f"), subtitle = "Correlation Coefficient", color = "blue",
             icon = icon("far fa-chart-bar"))
  })
  
  output$dataTable <- renderDataTable({
    dataTable <- combined_city_metrics %>% 
      dplyr::select(State, City, Category, Measure, Estimate, Population) %>% 
      arrange(State, City, Category, desc(Estimate))
  }, rownames = FALSE, filter = "top", options = list(autoWidth = TRUE))
})

