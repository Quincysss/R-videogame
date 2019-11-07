library(wordcloud)
library(shiny)
library(datasets)
library(shinydashboard)
function(input, output) {
  output$aDataPlot <- renderPlot({
    types <- switch(input$selection,
                     "Publisher" = gameDB$Publisher,
                     "Platform" = gameDB$Platform,
                     "Genre" = gameDB$Genre
                    )
  wordcloud(words = types, scale=c(4,1),min.freq = input$freq,
              max.words=input$max, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
  
 
  }
