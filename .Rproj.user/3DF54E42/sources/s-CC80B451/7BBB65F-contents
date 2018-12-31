# Define server logic
shinyServer(function(input, output) {
   
  output$providers <- renderValueBox({
    
    # filter providers dataset by state and year
    state_data <- providers %>% 
      filter(nppes_provider_state == input$state & specialty_description == input$specialty & 
               year == input$year)
    
    # create a line plot from filtered state data
    valueBox(formatC(state_data$count_npi, digits = 0, format = "f"),
             subtitle = "Count of Opioid Prescribers")
  })
  output$beneficiaries <- renderValueBox({
    state_data <- providers %>%
      filter(nppes_provider_state == input$state & specialty_description == input$specialty &
               year == input$year)
    
    valueBox(formatC(state_data$opioid_beneficiaries, digits = 0, format = "f"),
             subtitle = "Part D Opioid Beneficiaries")
  })
  output$`30_day_fills` <- renderValueBox({
    state_data <- providers %>% 
      filter(nppes_provider_state == input$state & specialty_description == input$specialty &
               year == input$year)
    
    valueBox(formatC(state_data$total_30_day_fills, digits = 0, format = "f"),
             subtitle = "Total 30 Day Fills")
  })
  output$rate <- renderValueBox({
    
    # filter providers dataset by state and year
    state_data <- providers %>% 
      filter(nppes_provider_state == input$state & specialty_description == input$specialty & 
               year == input$year)
    
    # create a line plot from filtered state data
    valueBox(formatC(state_data$opioid_prescription_rate, digits = 1, format = "f"),
             subtitle = "Prescription Rate")
  })
  output$opioid_claims <- renderValueBox({
    
    # filter providers dataset by state and year
    state_data <- providers %>% 
      filter(nppes_provider_state == input$state & specialty_description == input$specialty & 
               year == input$year)
    
    # create a line plot from filtered state data
    valueBox(formatC(state_data$opioid_claims, digits = 0, format = "f"),
             subtitle = "Opioid Claims")
  })
  output$total_claims <- renderValueBox({
    
    # filter providers dataset by state and year
    state_data <- providers %>% 
      filter(nppes_provider_state == input$state & specialty_description == input$specialty & 
               year == input$year)
    
    # create a line plot from filtered state data
    valueBox(formatC(state_data$total_claims, digits = 0, format = "f"),
             subtitle = "Total Claims")
  })
  output$map <- renderPlot({
    mapdata <- prescription_rate_map %>% 
      filter(year == input$year)
    
    map0 <- ggplot(mapdata, aes(x = long, y = lat, group = group,
                                              fill = opioid_prescription_rate))
    
    map1 <- map0 + geom_polygon(color = "gray90", size = 0.05) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45)
    
    map2 <- map1 + scale_fill_viridis_c(option = "viridis", direction = -1)
    
    map2 + theme_map() + #facet_wrap(~ year, ncol = 2) +
      theme(legend.position = "bottom",
            strip.background = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      labs(fill = "Opioid Prescription Rate")
  })
  output$opioid_providers_table <- renderDataTable({
    providers_table_data <- providers_table %>% 
      filter(nppes_provider_state == input$state & specialty_description == input$specialty &
               year == input$year) %>% 
      select("NPI" = npi, "Last Name" = nppes_provider_last_org_name,
             "First Name" = nppes_provider_first_name,
             "Specialty" = specialty_description,
             "City" = nppes_provider_city,
             "State" = nppes_provider_state,
             "Total Claims" = total_claim_count,
             "Opioid Claims" = opioid_claim_count,
             "Opioid Beneficiaries" = opioid_bene_count,
             "Prescription Rate" = opioid_prescriber_rate
             ) %>% 
      arrange(desc(`Prescription Rate`))
  }, rownames = FALSE, extensions = 'Buttons', options = list(dom = 'Bfrtip', 
                   buttons = list('copy', 'print', list(
                     extend = 'collection',
                     buttons = c('csv', 'excel', 'pdf'),
                     text = 'Download'
                   ))))
})
