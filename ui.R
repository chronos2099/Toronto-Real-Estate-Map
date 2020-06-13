
library(shiny)
library(shinydashboard)
library(tidyverse)#dataframe library
library(lubridate)#date library
library(gghighlight)#ggplot highlight function
library(sf)
# library(broom)#map data frame library
# library(rgdal)#mapping library
library(tmap)
library(plotly)
library(leaflet)
#######################
#Libraries used in R
#######################

workingdat<<-read_rds('./data/workingdat.rds')
print("loaded workingdat ui")
resdat<<-read_rds('./data/resdat.rds')
communitylist<<-read_rds('./data/communitylist.rds')
print("loaded communitylist ui")
citymap <<-read_rds('./data/citymap.rds')
print("loaded citymap ui")
cleanhood<<-read_rds("./data/cleanhood.rds")




source("functions.R")

st_crs(citymap) <- 4326 #Server uses old PROJ UBUNTU 18 need to explicitly state crs
st_crs(citymap$geometry)<<- 4326

hood.dat <<- left_join(citymap, cleanhood[1:142, ], by = 'Num')

shinyUI(fluidPage(
  headerPanel("Toronto Real Estate Data" ),
  
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
)
