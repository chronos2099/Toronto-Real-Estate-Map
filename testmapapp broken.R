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
library(broom)#map data frame library
library(rgdal)#mapping library
library(plotly)
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

hood.dat<- mapjoin(cleanhood,citymap)





censuslist<- names(cleanhood)[c(-1,-3)]


ggplot() + #plot data
  geom_polygon(data = hood.dat, aes(x=long,y=lat,group = Community, fill = population_2016),color = "grey", size = 0.1) +
  scale_fill_viridis_c(option = "C") +
  theme_void() +
  theme(axis.text = element_blank(), # change the theme options
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank()) # remove axis ticks

ui<- fluidPage(
 
   headerPanel("Toronto Real Estate data"),
   
   
  
   tabsetPanel(
    
     tabPanel("Map", fluid = TRUE,
              sidebarPanel(
                
                selectInput("hometype", label = "Type of home", choices = c("Condo","Detached/Townhouse"), selected = "Condo"),
                dateRangeInput("date", label = "Find Sold data between Date:",min   = "2018-01-01",max   = "2020-05-01",start = '2019-01-01', end = '2020-01-01'),
                selectInput("SL", label = "Sale or Lease data", choices = c("Sale","Lease"), selected = "Sale"),
                numericInput("bedroom", label = "Number of Bedrooms", min = 1,max = 6,value =1),
                numericInput("Washrooms", label = "Number of Washrooms", value =1, max = 4)
              
              ),
              
              mainPanel(fluidRow(column(12,plotOutput("torontoREmap",height = "auto")))
                                 )
     ),
     tabPanel("Graph", fluid = TRUE,
              sidebarPanel(
                selectInput("census", label = "Census data", choices = censuslist),
                selectInput("hometype", label = "Type of home", choices = c("Condo","Detached/Townhouse"), selected = "Condo"),
                dateRangeInput("date", label = "Find Sold data between Date:",min   = "2018-01-01",max   = "2020-05-01",start = '2019-01-01', end = '2020-01-01'),
                selectInput("SL", label = "Sale or Lease data", choices = c("Sale","Lease"), selected = "Sale"),
                numericInput("bedroom", label = "Number of Bedrooms", min = 1,max = 6,value =1),
                numericInput("Washrooms", label = "Number of Washrooms", value =1, max = 4)
              
              ),
              mainPanel(
              fluidRow(column(12,plotOutput("torontoREgraph",height = "auto")))
     )
     ),
     tabPanel("Census", fluid = TRUE,
              sidebarPanel(
              sidebarPanel(selectInput("census", label = "Census data", choices = censuslist)),
              mainPanel(fluidRow(column(12,plotOutput("census",height = "auto"))))
     ))
     
  
  

      

   #    selectInput("AN", label = "Annual or Monthly", choices = c("Annual","Monthly"),selected = "Annual"),
   # selectInput("graph", label = "Graph or Map", choices = c("Graph","Map"), selected = "Graph")
   #    selectInput("DN", label = "Den", choices = c("yes" = 1,"no" = 0,""),selected = "delta"),
              )
          )
      
    



server<- function(input, output, session){
  print(input)
  output$census <- renderPlot({
    ggplot() + #plot data
      geom_polygon(data = hood.dat, aes(x=long,y=lat,group = Community, fill = get(input$census)),color = "grey", size = 0.1) +
      scale_fill_viridis_c(option = "C", name="Data" ) +
      theme_void() +
      theme(axis.text = element_blank(), # change the theme options
            axis.title = element_blank(), # remove axis titles
            axis.ticks = element_blank()) # remove axis ticks
     
  }, height = function() {
    session$clientData$output_torontoREmap_width*0.55
  },width = function() {
    session$clientData$output_torontoREmap_width
  } )
  
  output$torontoREgraph <- renderPlot({

    if(input$hometype == "Detached/Townhouse"){
      Group<-Vowparse(dat = resdat,dated =input$date,BR = input$bedroom,WR = input$Washrooms,SL = input$SL)
    }
    else if(input$hometype == "Condo"){
      Group<-Vowparse(dat = workingdat,dated =input$date,BR = input$bedroom,WR = input$Washrooms,SL = input$SL)
    }
    print('done')
 
      if(input$SL == "Sale"){
        textmod = 1000
        ylab = " Median Price\n(in thousands)"
      }
      else if (input$SL == "Lease")
      {
        textmod = 1
        ylab = " Median Price"
      }
      

      ggplot(Group, aes( x = Community, y=  median_sold/textmod,  fill = Area, horiz = T)) +
        geom_bar(stat ="identity") +
        theme(axis.text.x = element_text(size = 12,angle = 90,  vjust =0.1,hjust = 1)) + #angle = 90,
        labs (x = "Neighbourhood",y = ylab, title = paste(input$date[2]," ",input$SL ," median price", input$bedroom ," bedroom condos",sep ="" )) 
        #scale_y_continuous(breaks = seq(0,textmod*1000,textmod))
        # coord_flip()
        #geom_text(aes(y = median_sold + textmod* sign(median_sold),label= Total, group = Community),position = position_dodge(width = 1), size = 2.5) #+
      # gghighlight(Community %in% c('Moss Park',"Church-Yonge Corridor"))
      
  }, height = function() {
    session$clientData$output_torontoREgraph_width
    })
  
  
  output$torontoREmap <- renderPlot({
    if(input$SL == "Sale"){
      textmod = 1000
      ylab = " Median Price\n(in thousands)"
    }
    else if (input$SL == "Lease")
    {
      textmod = 1
      ylab = " Median Price"
    }
    if(input$hometype == "Detached/Townhouse"){
      Group<-Vowparse(dat = resdat,dated =input$date,BR = input$bedroom,WR = input$Washrooms,SL = input$SL)
    }
    else if(input$hometype == "Condo"){
    Group<-Vowparse(dat = workingdat,dated =input$date,BR = input$bedroom,WR = input$Washrooms,SL = input$SL)
    }
      print('map')
      mapdat<-mapjoin(Group,citymap)
      
      ggplot() + #plot data
        geom_polygon(data = mapdat, aes(x=long,y=lat,group = Community.x, fill = median_sold/textmod),color = "grey", size = 0.1) +
        scale_fill_viridis_c(option = "C", name = ylab) +
        theme_void() +
        theme(axis.text = element_blank(), # change the theme options
              axis.title = element_blank(), # remove axis titles
              axis.ticks = element_blank()) # remove axis ticks
    }, height = function() {
      session$clientData$output_torontoREmap_width*0.55
  },width = function() {
    session$clientData$output_torontoREmap_width
  } )
    
  
}

shinyApp(ui = ui, server = server)
