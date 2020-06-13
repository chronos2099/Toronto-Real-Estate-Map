rm(list = ls())#clear workspace

####
#Remote
####
HDD <- "//C-147/"
dropbox <- "C:/Users/Chronos/Dropbox/Real estate/graphs/"
####


###
#Desktop
###
HDD <- "E:/RE/"
dropbox <- "E:/Dropbox/Real estate/graphs/"
###

folder <- "TREB data/"


calc <- "Vow data/R calc"

app <- "app"


wd <- paste(HDD, folder, app, sep = "")

setwd(wd)

#######################
#Libraries used in R
#######################

library(shiny)
library(tidyverse)#dataframe library
library(lubridate)#date library
library(gghighlight)#ggplot highlight function

library(tmap)
library(plotly)
library(leaflet)
#library(ggthemes)#extra themes from ggplot library


source("functions.R")

##################################
#Read data
##################################
workingdat <<- read_rds('./data/workingdat.rds')
resdat <<- read_rds('./data/resdat.rds')
print("loaded workingdat ui")
communitylist <<- read_rds('./data/communitylist.rds')
print("loaded communitylist ui")
citymap <<- read_rds('./data/citymap.rds')
print("loaded citymap ui")
cleanhood <<- read_rds("./data/cleanhood.rds")



hood.dat <<- left_join(citymap, cleanhood[1:142, ], by = 'Num')

##############################
#UI 

# 2 tab page
# 1st tab RE data and graph
# 2nd tab Census data
###############################

ui <- fluidPage(headerPanel("Toronto Real Estate Data"),
                
                tabsetPanel(
                  #Map panel
                  tabPanel(
                    "Map",
                    fluid = TRUE,
                    mainPanel(leafletOutput("torontoREmap")),
                    sidebarPanel(
                      position = "right",
                      selectInput(
                        "hometype",
                        label = "Type of home",
                        choices = c("Condo", "Detached/Townhouse"),
                        selected = "Condo"
                      ),
                      dateRangeInput(
                        "date",
                        label = "Find Sold data between Date:",
                        min   = "2018-01-01",
                        max   = "2020-05-01",
                        start = '2019-01-01',
                        end = '2020-01-01'
                      ),
                      selectInput(
                        "SL",
                        label = "Sale or Lease data",
                        choices = c("Sale", "Lease"),
                        selected = "Sale"
                      ),
                      numericInput(
                        "bedroom",
                        label = "Number of Bedrooms",
                        min = 1,
                        max = 6,
                        value = 1
                      ),
                      numericInput(
                        "Washrooms",
                        label = "Number of Washrooms",
                        value = 1,
                        max = 4
                      )
                      
                    ),
                    
                    #Graphs
                    fluidRow(column(
                      12, plotlyOutput("torontoREgraph",  height = "auto")
                    ))
                  ),
                  
                  #Census Tab
                  tabPanel(
                    "Census",
                    fluid = TRUE,
                    mainPanel(fluidRow(column(
                      12, leafletOutput("census")
                    ))),
                    sidebarPanel(
                      position = "right",
                      selectInput(
                        "censusdat",
                        label = "Census data",
                        choices = names(cleanhood)[-3:-1],
                        selected = "population_2016"
                      )
                    )
                  )
                ))





##############################
#Server 


###############################

server <- function(input, output, session) {
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
                    )) + 
      geom_bar(stat = "identity") +

      theme(axis.text.x = element_text(
        size = 8,
        angle = 90,
        vjust = 0.1,
        hjust = 1
      )) +
      labs (x = "Neighbourhood",
            y = "Days on Market",
            title = "Average number of days on market"
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
    
}

shinyApp(ui = ui, server = server)
