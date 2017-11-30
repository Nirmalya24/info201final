
library(shiny)
library(plotly)
library(shinyjs)

#setwd("/Users/aviralsharma/Desktop/INFO201/info201final")

raw.data <- read.csv("./data/child-mortality-by-sex.csv")
map.data <- read.csv("./data/child-mortality.csv", stringsAsFactors = FALSE)

#This line removes all the rows which contains null values inthe male and female column
raw.data <- na.omit(raw.data)


#Renaming all the column names so as to make the column names much shorter an dreadable
colnames(raw.data)[1] <- "Country"
colnames(raw.data)[4] <- "Female-Ratio"
colnames(raw.data)[5] <- "Male-Ratio"
colnames(raw.data)[6] <- "Total-Population"

colnames(map.data)[4] <- "MortalityRate"


my.server <- function(input, output) {
  #A variable which keeps the track of whether the button was clicked or not
  counter <- reactiveValues(countervalue = 0)
  
  #This function, brings into view, another widget when the button is clicked and then hides it so that it can't be clicked anymore
  observeEvent(input$button, {
    shinyjs::show("ChildCountry2")
    shinyjs::hide("button")
    counter$countervalue <- counter$countervalue + 1 
  })
  
  output$worldmap <- renderPlotly({
    #removes all the uears that weren't equal to the user input
    map.data <- map.data[map.data$Year == input$years, ]
    l <- list(color = toRGB("grey"), width = 0.5)
    
    # specify map projection/options
    g <- list(
      showframe = TRUE,
      showcoastlines = TRUE,
      projection = list(type = 'Mercator')
    )
    p <- plot_geo(map.data) %>%
      add_trace(
        z = ~map.data$MortalityRate, color = ~map.data$MortalityRate, colors = 'Greens',
        text = ~map.data$Entity, locations = ~map.data$Code, marker = list(line = l)
      ) %>%
      colorbar(title = 'Child-Mortality (per 1,000 live births)', ticksuffix = '%') %>%
      layout(
        title = 'Child Mortality',
        geo = g
      )
    return(p)
  })
  
  #Takes the user input of the country name and gender for eithe rone or two countries and creates a scatter plot to show the child mortality rate
  #based on gender
  output$plot <- renderPlotly({
    #Getting the first country name
    result.one <- raw.data %>% filter(raw.data$Country == input$ChildCountry)
    #Getting the type of gender that needs to be displayed: Male/Female/Both
    gender.selection <- input$gender
    #Checks if the button was pressed and if it was takes in the second country name
    if(counter$countervalue > 0) {
      result.two <- raw.data %>% filter(raw.data$Country == input$ChildCountry2)
      if(gender.selection == "Male") {
        chart <- plot_ly(data = result.one, y = result.one$`Male-Ratio`, x = result.one$Year, type = 'scatter', mode = 'lines', name = input$ChildCountry)  %>% 
          add_trace( y = result.two$`Male-Ratio`, x = result.two$Year, name = input$ChildCountry2 ) 
      } else if(gender.selection == "Female") {
        chart <- plot_ly(data = result.one, y = result.one$`Female-Ratio`, x = result.one$Year, type = 'scatter', mode = 'lines', name = input$ChildCountry)  %>% 
          add_trace( y = result.two$`Female-Ratio`, x = result.two$Year, name = input$ChildCountry2 )
      } else {
        chart <- plot_ly(data = result.one, y = result.one$`Female-Ratio`, x = result.one$Year, type = 'scatter', mode = 'lines', name = paste( input$ChildCountry, "(Female)"))  %>% 
          add_trace( y = result.two$`Female-Ratio`, x = result.two$Year, name = paste( input$ChildCountry2, "(Female)") ) %>%
          add_trace( y = result.two$`Male-Ratio`, x = result.two$Year, name = paste( input$ChildCountry2, "(Male)")  )  %>%
          add_trace( y = result.one$`Male-Ratio`, x = result.one$Year, name = paste( input$ChildCountry, "(Male)") ) 
      }
    } else {
      if(gender.selection == "Male") {
        chart <- plot_ly(data = result.one, y = result.one$`Male-Ratio`, x = result.one$Year, type = 'scatter', mode = 'lines', name ="Country One")  
      } else if(gender.selection == "Female") {
        chart <- plot_ly(data = result.one, y = result.one$`Female-Ratio`, x = result.one$Year, type = 'scatter', mode = 'lines', name ="Country One")
      } else {
        chart <- plot_ly(data = result.one, y = result.one$`Female-Ratio`, x = result.one$Year, type = 'scatter', mode = 'lines', name ="Country One (Female)")  %>%
          add_trace( y = result.one$`Male-Ratio`, x = result.one$Year, name = "Country One (Male)" ) 
      }
    }
    return(chart)
  })
  
  #returns the brief overview of the data collected in the form of a table for country 1
  output$statitics1 <- renderTable ({
    result.one <- raw.data %>% filter(raw.data$Country == input$ChildCountry)
    statitics <- result.one
    return(statitics)
  })   
  
  #returns the brief overview of the data collected in the form of a table for country 2
  output$statitics2 <- renderTable ({
    statitics <- NULL
    if(counter$countervalue > 0) {
      result.two <- raw.data %>% filter(raw.data$Country == input$ChildCountry2)
      statitics <- result.two
    }
    return(statitics)
  })   
  
  output$table.info <- renderText({
    return("SUMMARY STATISTICS OF CHILD MORTALITY RATE BY GENDER")
  })
  
}

shinyServer(my.server)