
library(shiny)
library(plotly)
library(shinyjs)
            
#setwd("/Users/aviralsharma/Desktop/INFO201/info201final")

raw.data <- read.csv("../data/child-mortality-by-sex.csv")

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
  useShinyjs(),
  tabPanel("Introduction",
           mainPanel(
             #strong(textOutput("summary2"))
           )
  ),
  tabPanel("Map",
           fluidPage(
             # Show a plot of the generated distribution
             fluidRow(
               column(12, div(style = "height:200px"),
                      mainPanel(plotlyOutput("worldmap")),
                      fluidRow(
                        column(12, div(style = "height:100px"), 
                               hr(), 
                               fluidRow(column(12, sidebarPanel(
                                 sliderInput("years", "Choose year",
                                             min = 1751,
                                             max = 2015,
                                             sep = "",
                                             value = 1950,
                                             animate =
                                               animationOptions(interval = 300, loop = FALSE))))
                               )
                        )
                      )
               )
             )
           )
  ),
  tabPanel("Infant Mortality",
           sidebarLayout(
             sidebarPanel(
               selectInput("infantCountry", label = h3("Select Country"), 
                           choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                           selected = 1)
               
             ),
             mainPanel(
               #strong(textOutput("summary2"))
             )
           )
  ),
  
  tabPanel("Child mortality by sex",
           sidebarLayout(
             sidebarPanel(
               h2("Please Make Selections"),
               radioButtons("gender", label = h5("Select the Gender"),
                            choices = c("Male" , "Female" , "Both" ), selected = "Male"),
               br(),
               selectInput("ChildCountry", label = h5("Select Country"), 
                           choices = raw.data$Country, width = '50%',
                           selected = 1),
               
               hidden (
                 selectInput("ChildCountry2", label = h5("Second Country"), 
                             choices = raw.data$Country, width = '50%',
                             selected = raw.data$Country[4])
               ),
               actionButton("button", "Add country +", width = '50%'),
               width = 2
             ),
             mainPanel(
               plotlyOutput("plot"),
               br(),
               strong(textOutput("table.info")),
               br(),
               tableOutput("statitics1"),
               br(),
               tableOutput("statitics2")
             )
           )
  ),
  
  tabPanel("Global child death causes",
           mainPanel(
             plotlyOutput("areaGraph")
           )
  )
)

#onclick("button", show("infantCountry2"))

