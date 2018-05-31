library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(twitteR)
library(stringr)
library(lubridate)
library(ggrepel)
library(ggmap)
library(maps)
source("retrieve.twitter.user.R")
source("retrieve.twitter.term.R")
source("score.R")
source("retrieve.twitter.DA.R")




diseaseDB = "data/Corn_diseases.lst"


ui <- fluidPage(
  titlePanel("Corn Diseases Detected on Twitter", windowTitle = "Corn Disease detected on Twitter"),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = 'consumer_key',
                label = 'Your Twitter Consumer key'),
      textInput(inputId = 'consumer_secret',
                label = 'Your Twitter Consumer Secret'),
      textInput(inputId = 'access_token',
                label = 'Your Twitter Access Token'),
      textInput(inputId = 'access_secret',
                label = 'Your Twitter Access Secret'),
      selectInput(inputId = "scn",
                label = "Twitter Screen Name:",
                choices = c("@corndisease", "@soydisease")),
      numericInput(inputId = 'yr',
                   label = 'Year',
                   value = 2017,
                   min = 2016,
                   max = 2017),
      numericInput(inputId = 'mn',
                   label = 'Month',
                   value = 9,
                   min = 1,
                   max = 12),
      numericInput(inputId = 'ntweet',
                   label = "Number of Tweets",
                   value = 455,
                   min = 1, 
                   max = 455),
      radioButtons(inputId = 'IncRt',
                   label = "Include Retweet?",
                   choices = c(TRUE, FALSE)),
      actionButton("plot", "Plot")
    ),
    mainPanel(
      plotOutput("onmap")
    )
  )
)



server <- function(input, output, session) {
  
  
  tw <- reactive({
    
    # req() function checks start point if there is no input, hault it
    req(input$consumer_key, input$consumer_secret, input$access_token, input$access_secret)
    
    return(retrieve.twitter.user(input$consumer_key, input$consumer_secret, input$access_token, input$access_secret, input$scn, input$ntweet, input$IncRt))
    })
  
  mx = reactive({
    return(retrieve.twitter.term(tw(), diseaseDB, input$yr, input$mn))
  })
  
  output$onmap = renderPlot({
    req(tw(), mx())
    input$plot
    isolate(
      retrieve.twitter.DA(mx()))
  })
}

shinyApp(ui, server)