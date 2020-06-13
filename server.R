

shinyServer(function(input, output, session){
  
    #Uncomment to see click information
    
    
    # selection <- observe({
    #
    #   event <- input$torontoREmap_shape_click
    #   if (!is.null(event)){
    #
    #   print(event$id)
    #   }
    #   else{
    #   return()
    #   }
    # })
    
    #Manipulate data to required groupings
    
    Group <- reactive({
      if (input$hometype == "Detached/Townhouse") {
        gpdata = resdat
      }
      else if (input$hometype == "Condo") {
        gpdata = workingdat
      }
      
      Group <-
        Vowparse(
          dat = gpdata,
          dated = input$date,
          BR = input$bedroom,
          WR = input$Washrooms,
          SL = input$SL
        )
      Group <- Group %>%
        mutate(Community.ID = str_replace_all(Community, "[:punct:]|[:blank:]+", "."))
    })
    
    #output render map
    output$torontoREmap = renderLeaflet({
      Sale_data <- left_join(citymap, Group())
      
      fig <- tm_shape(Sale_data) +
        tm_polygons(
          col = "median_sold",
          id = "Community",
          title = "Median Price",
          palette = "viridis" ,
          borders = "lightgrey",
          alpha = 0.7,
          layerID = "Sale Data",
          popup.var = c(
            "Highest:" = "max",
            "Median:" = "median_sold",
            "Lowest:" = "min",
            "Sold" = "Total"
          )
          
        ) +
        tm_layout(title = paste(
          input$bedroom,
          "Bedroom",
          input$SL,
          "price",
          "in Toronto",
          sep = " "
        ))
      print('map')
      tmap_leaflet(fig)
    })
    
    #graphing function update when something is clicked
    
    graphing <- reactive({
      event <- input$torontoREmap_shape_click$id
      
      if (input$SL == "Sale") {
        textmod = 1000
        ylab = " Median Price\n(in thousands)"
      }
      else if (input$SL == "Lease")
      {
        textmod = 1
        ylab = " Median Price"
      }
      
      dat <- Group()
      
      print('Graph')
      graph <- ggplot(dat,
                      aes(
                        x = Community,
                        y =  avg_DOM,
                        fill = Area,
                        horiz = T
                      )) + geom_bar(stat = "identity") +
        
        theme(axis.text.x = element_text(
          size = 8,
          angle = 90,
          vjust = 0.1,
          hjust = 1
        )) +
        labs (x = "Neighbourhood",
              y = "Days on Market",
              title = "Number of days on market"
              # title = paste(
              #   input$date[2],
              #   " ",
              #   input$SL ,
              #   " median price",
              #   input$bedroom ,
              #   " bedroom condos",
              #   sep = ""))
              
        )
      
      if (!is.null(event)) {
        if (is.na(Group()$median_sold[match(event, Group()$Community.ID)])) {
          graph
        }
        else {
          print("event")
          graph = graph  + gghighlight(Community.ID == event)  #Doesn't work can't find Community object
        }
      }
      
      else {
        graph
      }
      
    })
    
    
    #render graph
    output$torontoREgraph <- renderPlotly({
      ggplotly(graphing(), tooltip = "text")
      
    })
    
    
    
    #render census data change to layer
    output$census <- renderLeaflet({
      a <- input$censusdat
      print(a)
      fig2 <- tm_shape(hood.dat) +
        tm_polygons(
          col = input$censusdat,
          id = "Community",
          palette = "plasma" ,
          borders = "lightgrey",
          alpha = 0.7
        )
      tmap_leaflet(fig2)
    })
    
  })
