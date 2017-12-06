#require the following
library("shiny")
library("dplyr")
library("ggplot2")
library("readr")
library("sm")


#read in data
data1 <- read.csv("../data/Children-woman-death-vs-survival.csv", stringsAsFactors = FALSE)

shinyServer(function(input, output) {
  
  #plotName same as plot name in the UI
  
  output$rolePlot <- renderPlotly({
    #x axis will contain years based on user input
    #y axis will contain percent of population
    data2 <- data1 %>% filter(Year >= input$years[1] & Year <= input$years[2])  %>% filter(Entity == input$country)
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
  
})