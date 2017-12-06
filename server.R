library(shiny)
library(plotly)
library(shinyjs)
library(tidyr)
library(dplyr)
library(readr)
library(sm)

#setwd("/Users/aviralsharma/Desktop/INFO201/info201final")

raw.data <- read.csv("./data/child-mortality-by-sex.csv")
map.data <- read.csv("./data/child-mortality.csv", stringsAsFactors = FALSE)
causes.data <- read.csv("./data/global-child-deaths-by-cause.csv", stringsAsFactors = FALSE)
intro.data <- read.csv("./data/global-child-mortality-timeseries.csv", stringsAsFactors = FALSE)
data1 <- read.csv("./data/Children-woman-death-vs-survival.csv", stringsAsFactors = FALSE)
#This line removes all the rows which contains null values inthe male and female column
raw.data <- na.omit(raw.data)


#Renaming all the column names so as to make the column names much shorter and readable
#Raw data rename
colnames(raw.data)[1] <- "Country"
colnames(raw.data)[4] <- "Female-Ratio"
colnames(raw.data)[5] <- "Male-Ratio"
colnames(raw.data)[6] <- "Total-Population"
#Map data rename
colnames(map.data)[4] <- "MortalityRate"
#causes data rename
colnames(causes.data) <- c("Entity", "Code", "Year", "Deaths", "Population")


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
      projection = list(type = 'mollweide')
    )
    p <- plot_geo(map.data) %>%
      add_trace(
        z = ~map.data$MortalityRate, color = ~map.data$MortalityRate, colors = 'Reds',
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
  
  output$areaGraph <- renderPlotly({
    
    df <- causes.data %>% select(Entity, Year, Deaths) %>% filter(Deaths >= 0)
    df <- spread(df, Year, Deaths) #converting it from long to wide
    
    p <- plot_ly(x = df$Entity, y = df$'2015', type = 'scattergl', mode = 'lines', name = '2015', fill = 'tozeroy') %>%
      add_trace(x = df$Entity, y = df$'1990', name = '1990', fill = 'tozeroy') %>%
      layout(xaxis = list(title = "Leading Causes", showticklabels = FALSE),
             yaxis = list(title = 'Number of Deaths'), hovermode = 'compare')
    
    return(p)
  })
  
  output$introPlot <- renderPlotly({
    #x axis will contain years based on user input
    #y axis will contain percent of population
    data <- intro.data %>% filter(Year >= input$introYears[1] & Year <= input$introYears[2])
    plot_ly(data, x = ~Year, y = ~Share.surviving.first.5.years.of.life....+ Share.dying.in.first.5.years.... , name = 'Share surviving ',
            type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = '#F5FF8D',
            text = ~paste0(Year, '<br>Share surviving: ', Share.surviving.first.5.years.of.life....,
                           '<br>Share dying: ', Share.dying.in.first.5.years....)) %>%
      add_trace(y = ~Share.dying.in.first.5.years...., name = 'Share dying', fillcolor = '#50CB86') %>%
      
      layout(title = 'Global Child Mortality',
             xaxis = list(title = "",
                          showgrid = FALSE),
             yaxis = list(title = "Percent",
                          showgrid = FALSE,
                          ticksuffix = '%'))
    
  })

  output$rolePlot <- renderPlotly({
    #x axis will contain years based on user input
    #y axis will contain percent of population
    data2 <- data1 %>% filter(Year >= input$roleYears[1] & Year <= input$roleYears[2])  %>% filter(Entity == input$country)
    plot_ly(data2, x = ~Year, y = ~Children.that.survived.past.their.5th.birthday.per.woman..children.
            + Children.that.died.before.5.years.of.age.per.woman..Children.that.died.before.5.years.of.age.per.woman. ,
            name = 'died ', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = 'red',
            text = ~paste0(Year, ' ', Children.that.survived.past.their.5th.birthday.per.woman..children.
                           + Children.that.died.before.5.years.of.age.per.woman..Children.that.died.before.5.years.of.age.per.woman.,
                           '<br>Children that died before five years of age per woman: ', Children.that.died.before.5.years.of.age.per.woman..Children.that.died.before.5.years.of.age.per.woman.,
                           '<br>Children that survived after five years of age per woman: ',
                           Children.that.survived.past.their.5th.birthday.per.woman..children.)) %>%
      add_trace(y = ~Children.that.survived.past.their.5th.birthday.per.woman..children., name = 'survived', fillcolor = '#50CB86') %>%
      
      layout(title = 'How many children did a woman give birth to that died before their 5th birthday?',
             xaxis = list(title = "",
                          showgrid = FALSE),
             yaxis = list(title = "Children per woman",
                          showgrid = FALSE))
    
  })
  
}

shinyServer(my.server)