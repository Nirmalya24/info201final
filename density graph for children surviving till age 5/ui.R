library("shiny")
library("dplyr")
library("ggplot2")

data <- read.csv("../data/Children-woman-death-vs-survival.csv", stringsAsFactors = FALSE)
countries <- unique(data$Entity, incomparables = FALSE, fromLast = FALSE,
                    nmax = NA)
shinyUI(fluidPage(
  
  #title
  titlePanel("How many children did a woman give birth to that died before their 5th birthday?"),
  
  #Layout
  sidebarLayout(
    # side panel
    sidebarPanel(
      
      sliderInput('years',                # key this value will be assigned to
                  "Years",                # label
                  min = 1800,             # minimum slider value
                  max = 2015,             # maximum slider value
                  value = c(1800, 2015)   # starting value
      ),
      #country
      selectInput(inputId = "country",
                  label = "Country:",
                  choices = countries,
                  selected = "Sweden")
    ),
    
    #main panel
    mainPanel(
      #plotting output
      plotlyOutput("rolePlot")
    )
  )
  
))