library(shiny)
#install.packages("shinythemes")
library(shinythemes)
library(DT)
require(XML)
library(dplyr)
library(plyr)

ui <- fluidPage(
  tagList(
    tags$head(
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "style.css"),
      tags$script(type = "text/javascript",
                  src = "busy.js")
    )
  ),
  
  mainPanel(
    headerPanel("Shiny Blast"),
    textAreaInput(inputId = 'query', 
                  label = 'Input sequence:', 
                  value = "",
                  placeholder = "",
                  width = "600px",
                  height = "200px"),
    selectInput(inputId = "db",
                label = "Database:",
                choices = c("Oregon Local Plant trnL", "Oregon Local plant ITS2"),
                width = "100px"),
    div(style='display:inline-block',
        selectInput(inputId = "eval",
                    label = "e-value:", choices = c(1, 0.001, 1e-4, 1e-5, 1e-10),
                    width = "120px"),
        actionButton(inputId = "blast",
                     label = "Blast!"))
    
  ),
  
  # this snippet generates a 'busy' indicator for long Blasts
  div(class = 'busy',
      p('calculation in progress..'),
      img(src='https://i.stack.imgur.com/8pui0.gif'), height = 100, width = 100, align = 'center'),
  
  mainPanel(
    h4('Results'),
    DT::dataTableOutput("BlastResults"),
    p("Alignment:", tableOutput("clicked")),
    verbatimTextOutput('alignment')
  )
  
)

server <- function(input, output, session) {
  blastresults <- eventReactive(input$blast, {
    # gather input and set up temp file
    query = input$query
    
    tmp = tempfile(fileext = ".fa")
    
    # if else chooses the right data base
    # here you could add more database
    if(input$db == "Oregon Local Plant trnL"){
      db = c("database/blast_db/")
    }
  }
}

shinyApp(ui, server)