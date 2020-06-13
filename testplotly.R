rm(list=ls())#clear workspace

####
#Remote
####
HDD <- "//C-147/"
dropbox<- "C:/Users/Chronos/Dropbox/Real estate/graphs/"
####


###
#Desktop
###
HDD<- "E:/RE/"
dropbox<- "E:/Dropbox/Real estate/graphs/"
###

folder<- "TREB data/"


calc<- "Vow data/R calc"

app<-"app"

wd<- paste(HDD,folder,app,sep="")

setwd(wd)

#######################
#Libraries used in R
#######################

library(shiny)
library(tidyverse)#dataframe library
library(lubridate)#date library
library(gghighlight)#ggplot highlight function
library(plotly)
library(broom)
#library(ggthemes)#extra themes from ggplot library


source("functions.R")

##################################  
#Read data
##################################  
workingdat<<-read_rds('./data/workingdat.rds')
resdat<<-read_rds('./data/resdat.rds')
print("loaded workingdat ui")
communitylist<<-read_rds('./data/communitylist.rds')
print("loaded communitylist ui")
citymap <<-read_rds('./data/citymap.rds')
print("loaded citymap ui")
cleanhood<<-read_rds("./data/cleanhood.rds")


hood.dat<<- mapjoin(cleanhood[1:142,],citymap)



ui <- fluidPage(
  headerPanel("Toronto Real Estate Data"),
  
  tabsetPanel(
    tabPanel(
      "Map",
      fluid = TRUE,
      
      mainPanel(fluidRow(column(
        12, plotlyOutput("torontoREmap", width = "100%")
      ))),
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
        
      )
    ),
    tabPanel(
      "Graph",
      fluid = TRUE,
      
      mainPanel(fluidRow(column(
        12, plotlyOutput("torontoREgraph",width = "100%")
      ))),
      sidebarPanel(
        position = "right",
        selectInput(
          "hometype1",
          label = "Type of home",
          choices = c("Condo", "Detached/Townhouse"),
          selected = "Condo"
        ),
        dateRangeInput(
          "date1",
          label = "Find Sold data between Date:",
          min   = "2018-01-01",
          max   = "2020-05-01",
          start = '2019-01-01',
          end = '2020-01-01'
        ),
        selectInput(
          "SL1",
          label = "Sale or Lease data",
          choices = c("Sale", "Lease"),
          selected = "Sale"
        ),
        numericInput(
          "bedroom1",
          label = "Number of Bedrooms",
          min = 1,
          max = 6,
          value = 1
        ),
        numericInput(
          "Washrooms1",
          label = "Number of Washrooms",
          value = 1,
          max = 4
        )
        
      )
      
    ),
    tabPanel(
      "Census",
      fluid = TRUE,
      mainPanel(fluidRow(column(
        12, plotlyOutput("census", width = "100%")
      ))),
      sidebarPanel(
        position = "right",
        selectInput("census", label = "Census data", choices = names(cleanhood)[-3:-1])
      )
    )
  )
  #    selectInput("AN", label = "Annual or Monthly", choices = c("Annual","Monthly"),selected = "Annual"),
  # selectInput("graph", label = "Graph or Map", choices = c("Graph","Map"), selected = "Graph")
  #    selectInput("DN", label = "Den", choices = c("yes" = 1,"no" = 0,""),selected = "delta"),
)
          
      
    



server<- function(input, output, session){
  

  output$torontoREmap <- renderPlotly({
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
    print('map')
    mapdat <- mapjoin(Group, citymap)
    
    ggplot() + #plot data
      geom_polygon(
        data = mapdat,
        aes(
          x = long,
          y = lat,
          group = Community,
          fill = median_sold
        ),
        color = "grey",
        size = 0.1
      ) +
      scale_fill_viridis_c(option = "C", name = "Median Price") +
      theme_void() +
      theme(
        axis.text = element_blank(),
        # change the theme options
        axis.title = element_blank(),
        # remove axis titles
        axis.ticks = element_blank()
      ) # remove axis ticks
  
  })

  
  output$torontoREgraph <- renderPlotly({
    if (input$hometype1 == "Detached/Townhouse") {
      gpdata = resdat
    }
    else if (input$hometype1 == "Condo") {
      gpdata = workingdat
    }
    
    

    
    Group <-
      Vowparse(
        dat = gpdata,
        dated = input$date1,
        BR = input$bedroom1,
        WR = input$Washrooms1,
        SL = input$SL1
      )
    print('done')
    ggplot(Group,
           aes(
             x = Community,
             y =  median_sold,
             fill = Area,
             horiz = T
           )) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(
        size = 10,
        angle = 90,
        vjust = 0.1,
        hjust = 1
      )) + #angle = 90,
      labs (
        x = "Neighbourhood",
        y = ylab,
        title = paste(
          input$date1[2],
          " ",
          input$SL1 ,
          " median price",
          input$bedroom1 ,
          " bedroom condos",
          sep = ""
        )
      )
    
  
  })
  
  output$census <- renderPlotly({

  fig<-  ggplot() + #plot data
      geom_polygon(
        data = hood.dat,
        aes(
          x = long,
          y = lat,
          group = Community,
          fill = get(input$census),
          text = paste("Census data", ": "," test","\nx: ", long)
        ),
        color = "grey",
        size = 0.1
      ) +
      scale_fill_viridis_c(option = "C", name = "Data") +
      theme_void() +
      theme(
        axis.text = element_blank(),
        # change the theme options
        axis.title = element_blank(),
        # remove axis titles
        axis.ticks = element_blank()
      ) # remove axis ticks
  ggplotly(fig, tooltip = "text")
  })
  
}

shinyApp(ui = ui, server = server)



tm_shape(citymap)+
  tm_polygons(citymap)
