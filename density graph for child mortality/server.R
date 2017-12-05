#require the following
library("shiny")
library("dplyr")
library("ggplot2")
library("readr")
library("sm")


#read in data
data1 <- read.csv("../data/global-child-mortality-timeseries.csv", stringsAsFactors = FALSE)
data2 <- data1 %>% mutate(Share.surviving.first.5.years.of.life.... = Share.surviving.first.5.years.of.life.... +
                            Share.dying.in.first.5.years....)

shinyServer(function(input, output) {
  
  #plotName same as plot name in the UI
  output$rolePlot <- renderPlot({
    #x axis will contain years based on user input
    #y axis will contain percent of population
    data <- data1 %>% filter(Year >= input$years[1] & Year <= input$years[2])
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
  
})