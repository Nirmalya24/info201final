
library(shiny)
library(plotly)
library(shinyjs)
library(readr)
library(sm)
            
#setwd("/Users/aviralsharma/Desktop/INFO201/info201final")

raw.data <- read.csv("./data/child-mortality-by-sex.csv")
data <- read.csv("./data/Children-woman-death-vs-survival.csv", stringsAsFactors = FALSE)
countries <- unique(data$Entity, incomparables = FALSE, fromLast = FALSE, nmax = NA)
#This line removes all the rows which contains null values inthe male and female column
raw.data <- na.omit(raw.data)
#Renaming all the column names so as to make the column names much shorter an dreadable
colnames(raw.data)[1] <- "Country"
colnames(raw.data)[4] <- "Female-Ratio"
colnames(raw.data)[5] <- "Male-Ratio"
colnames(raw.data)[6] <- "Total-Population"



ui <- navbarPage(
  #Application 
  "Child Mortality Viz",
  
  tabPanel("Introduction",
           fluidPage(
             fluidRow(
               column(12,
                      mainPanel(
                                 h1("A visual representation of Child Mortality Rates"),
                                 br(),
                                 plotlyOutput("introPlot"),
                                 sliderInput('introYears',           # key this value will be assigned to
                                             "Years",                # label
                                             min = 1800,             # minimum slider value
                                             max = 2015,             # maximum slider value
                                             sep = "",
                                             value = c(1800, 2015)   # starting value
                                 )
                                 
                          )
               )
             ),
             br(),
             textOutput("intro")
             )
  ),
  tabPanel("Map",
           fluidRow(
             column(11,
                    h2("Our first visualisation depicts the magnitude of decline of Global Child Mortality Rate."),
                    br(),
                    textOutput("map.info"),
                    br(),
                    mainPanel(plotlyOutput("worldmap")),
                    sliderInput("years", "Choose year",
                                min = 1751,
                                max = 2015,
                                sep = "",
                                value = 1950,
                                animate =
                                  animationOptions(interval = 300, loop = FALSE))
             )
           )
  ),
  tabPanel("Infant Mortality",
           #Layout
           sidebarLayout(
             # side panel
             sidebarPanel(
               
               sliderInput('roleYears',                # key this value will be assigned to
                           "Years",                # label
                           min = 1800,             # minimum slider value
                           max = 2015,             # maximum slider value
                           sep="",
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
               plotlyOutput("rolePlot"),
               br(),
               strong(textOutput("fertility.intro")),
               br(),
               textOutput("fertility.summary")
             )
           )
  ),
  useShinyjs(),
  tabPanel("Child mortality by sex",
           
           sidebarLayout(
             sidebarPanel(
               h2("Please Make Selections"),
               radioButtons("gender", label = h5("Select the Gender"),
                            choices = c("Male" , "Female" , "Both" ), selected = "Male"),
               br(),
               selectInput("ChildCountry", label = h5("Select Country"), 
                           choices = raw.data$Country, width = '75%',
                           selected = 1),
               
               hidden (
                 selectInput("ChildCountry2", label = h5("Second Country"), 
                             choices = raw.data$Country, width = '75%',
                             selected = raw.data$Country[4])
               ),
               actionButton("button", "Add country +", width = '75%'),
               width = 2
             ),
             mainPanel(
               plotlyOutput("plot"),
               br(),
               strong(textOutput("table.info")),
               br(),
               tableOutput("statitics1"),
               br(),
               tableOutput("statitics2"),
               br(),
               textOutput("gender")
             )
           )
  ),
  
  tabPanel("Global child death causes",
           
           fluidPage(
           mainPanel(
             fluidRow(
             column(8,h1("Causes Behind Child Mortality"),
                    plotlyOutput("areaGraph"),
                    br(),
                    textOutput("causes"),
                    br()
                    
             ),
             fluidRow(
               column(4, 
                      br(),
                      h3("Some of the diseases that cause child mortality:"),
                      tableOutput("diseases")
               )
             )
           )
           )
  )
)
)



