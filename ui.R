library(shiny)


shinyUI(navbarPage(
        title = 'NBA 2014-2015 Prediction',
        tabPanel('League Playoffs',      dataTableOutput('mytable1')),       
        tabPanel('Miscellaneous Stats',      dataTableOutput('mytable'))
        
))