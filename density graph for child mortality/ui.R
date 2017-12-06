library("shiny")
library("dplyr")
library("ggplot2")

shinyUI(fluidPage(
  
  #title
  titlePanel("Global Child Mortality"),
  
  #Layout
  sidebarLayout(
    # side panel
    sidebarPanel(
      
      sliderInput('years',                # key this value will be assigned to
                  "Years",                # label
                  min = 1800,             # minimum slider value
                  max = 2015,             # maximum slider value
                  value = c(1800, 2015)   # starting value
      )
    ),
    
    #main panel
    mainPanel(
      #plotting output
      plotlyOutput("rolePlot")
    )
  )
  
))