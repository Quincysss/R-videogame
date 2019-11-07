library(shiny)
library(wordcloud)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(date)
library(dplyr)
library(ggplot2)
library(datasets)
library(rAmCharts)
library(radarchart)
gameDB <- read.csv("Video_Game_Sales.csv",header = T)
gameDB[gameDB == 0] <-NA
noNAgameDBSale <- gameDB[complete.cases(gameDB[,6:10]),]
CrnoNAgameDB <- gameDB[complete.cases(gameDB[,11:15]),]
noAllNaDB <- gameDB[complete.cases(gameDB[,1:15]),]
labs <- c("North America","Europe","Japan","Other")
header <- dashboardHeader(title = span(tagList(icon("truck"), "Electronic Game!")))
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("wordcloud", tabName = "word"),
    menuItem("sale Rank", tabName = "saleRank"),
    menuItem("detail Information", tabName = "detail"),
    menuItem("User Number", tabName = "number")
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "word",
            fluidRow(
              mainPanel(
                selectInput("selection", "Choose a type:",
                            list("Publisher",
                                 "Platform",
                                 "Genre")),
                sliderInput("freq",
                            "Minimum Frequency:",
                            min = 1,  max = 10, value = 15),
                sliderInput("max",
                            "Maximum Number of Words:",
                            min = 1,  max = 600,  value = 200),
                
                plotOutput("aDataPlot")
              )
            )
    ),
    tabItem(tabName = "saleRank",
            fluidRow(
              absolutePanel(top = 490,right = 600,fixed = FALSE,actionButton("showall", "showAllGenre")),
              absolutePanel(top = 25,right = 410, fixed = FALSE,
                            selectInput("selectGenre", "Choose a Genre:", choices = levels(gameDB$Genre),width = 200) 
              ),             
              absolutePanel(top = 25,right = 210, fixed = FALSE,
                            selectInput("selectArea", "Choose a Area:",c( "North America",
                                                                          "Europe",
                                                                          "Japan",
                                                                          "Other Area",
                                                                          "Global Amount"),width = 200)),
              amChartsOutput("rank"),
              hr(),
              hr(),
              hr(),
              amChartsOutput("allRank"),
              absolutePanel(top = 470,right = 210, fixed = FALSE,
                            selectInput("selectArea2", "Choose a Area:",c( "North America" ,
                                                                           "Europe",
                                                                           "Japan",
                                                                           "Other Area",
                                                                           "Global Amount"),width = 200)),
              
              absolutePanel(top = 450, right = 20, fixed = FALSE,
                            knobInput("rankyear", "", min = 1983, max = 2016, value = 2016,
                                      displayPrevious = FALSE, 
                                      lineCap = "round",
                                      fgColor = "#DC143C",
                                      bgColor = "FFFFFF",
                                      inputColor = "#DC143C",
                                      width = 100,
                                      height = 100,
                                      immediate = FALSE)
                            
              )
            )
    ),
    tabItem(tabName = "detail",
            fluidRow(
              absolutePanel(top = 55,right = 300, fixed = FALSE,
                            selectInput("selectGenre1", "Choose a Genre:", choices = levels(noAllNaDB$Genre),width = 200) 
              ),
              absolutePanel(top = 125,right = 300, fixed = FALSE,
                            selectInput("selectplatForm", "Choose a platform:", choices = levels(noAllNaDB$Platform),width = 200) 
              ),
              absolutePanel(top = 185,right = 300, fixed = FALSE,
                            uiOutput("choiceGame"),width = 300) 
              ),
            absolutePanel( top=300,right = 240, chartJSRadarOutput("radarPlot", width = "350", height = "350") )
            ),
    tabItem(tabName = "number",
            fluidRow(
              sliderInput("PieYear",
                          "Select Year for PIE:",
                          min = 1970,  max = 2020, value = 2015),
              selectInput("selectUsers", "Select Users:", c(
                "Player",
                "Critic"
              )),
              selectInput("selectplatFormP", "Choose a platform:", choices = levels(noAllNaDB$Platform),width = 200), 
              
              top=300,right = 640,amChartsOutput("PiePlot")
            ))
    )
  
  )

ui <- dashboardPage(header, sidebar, body, skin = "red")
server <- function(input, output) {
  output$aDataPlot <- renderPlot({
    types <- switch(input$selection,
                    "Publisher" = gameDB$Publisher,
                    "Platform" = gameDB$Platform,
                    "Genre" = gameDB$Genre
    )
    wordcloud(words = types, scale=c(3,0.5),min.freq = input$freq,
              max.words=input$max, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
  
  gameSale_Rank <- reactive({
    
    noNAgameDBSale %>% group_by(Year_of_Release)
    
  })
  output$rank <- renderAmCharts({
   switch(input$selectArea,
          "North America" =amBarplot(x=("Name"),y=("NA_Sales"),data = gameSale_Rank()%>%filter(Year_of_Release %in% input$rankyear,Genre %in% input$selectGenre )%>%arrange(desc(NA_Sales)) ,horiz = TRUE, zoom = TRUE),
          "Europe" =amBarplot(x=("Name"),y=("EU_Sales"),data = gameSale_Rank()%>%filter(Year_of_Release %in% input$rankyear,Genre %in% input$selectGenre )%>%arrange(desc(EU_Sales)) ,horiz = TRUE, zoom = TRUE),
          "Japan" =amBarplot(x=("Name"),y=("JP_Sales"),data = gameSale_Rank()%>%filter(Year_of_Release %in% input$rankyear,Genre %in% input$selectGenre )%>%arrange(desc(JP_Sales)) ,horiz = TRUE, zoom = TRUE),
          "Other Area" = amBarplot(x=("Name"),y=("Other_Sales"),data = gameSale_Rank()%>%filter(Year_of_Release %in% input$rankyear,Genre %in% input$selectGenre )%>%arrange(desc(Other_Sales)) ,horiz = TRUE, zoom = TRUE),
          "Global Amount" = amBarplot(x=("Name"),y=("Global_Sales"),data = gameSale_Rank()%>%filter(Year_of_Release %in% input$rankyear,Genre %in% input$selectGenre )%>%arrange(desc(Global_Sales)) ,horiz = TRUE, zoom = TRUE) 
          )
    #amBarplot(x=("Name"),y=(input$selectArea),data = gameSale_Rank()%>%filter(Year_of_Release %in% input$rankyear,Genre %in% input$selectGenre ) ,horiz = TRUE, zoom = TRUE)
  })
  output$allRank <- renderAmCharts({
    if(input$showall == 1){
      switch(input$selectArea2,
             "North America" =amBarplot(x=("Name"),y=("NA_Sales"),data = gameSale_Rank()%>%filter(Year_of_Release %in% input$rankyear)%>%arrange(desc(NA_Sales)) ,horiz = TRUE, zoom = TRUE),
             "Europe" =amBarplot(x=("Name"),y=("EU_Sales"),data = gameSale_Rank()%>%filter(Year_of_Release %in% input$rankyear)%>%arrange(desc(EU_Sales)) ,horiz = TRUE, zoom = TRUE),
             "Japan" =amBarplot(x=("Name"),y=("JP_Sales"),data = gameSale_Rank()%>%filter(Year_of_Release %in% input$rankyear)%>%arrange(desc(JP_Sales)) ,horiz = TRUE, zoom = TRUE),
             "Other Area" = amBarplot(x=("Name"),y=("Other_Sales"),data = gameSale_Rank()%>%filter(Year_of_Release %in% input$rankyear)%>%arrange(desc(Other_Sales)) ,horiz = TRUE, zoom = TRUE),
             "Global Amount" = amBarplot(x=("Name"),y=("Global_Sales"),data = gameSale_Rank()%>%filter(Year_of_Release %in% input$rankyear)%>%arrange(desc(Global_Sales)) ,horiz = TRUE, zoom = TRUE) 
      )
     # amBarplot(x=("Name"),y=(input$selectArea2),data = gameSale_Rank()%>%filter(Year_of_Release %in% input$rankyear),horiz = TRUE, zoom = TRUE)
    }
  })
  
  output$choiceGame <- renderUI({
    selectInput("selectName", "Select Game:", choices = as.character(noAllNaDB[which(noAllNaDB$Genre %in% input$selectGenre1 & noAllNaDB$Platform %in% input$selectplatForm),"Name"]))
  }) 
  
   
    
 
  output$radarPlot<-renderChartJSRadar ({
    radarSaleDB <- noAllNaDB %>% filter(Name %in% input$selectName,Platform %in% input$selectplatForm) %>% select(NA_Sales,EU_Sales,JP_Sales,Other_Sales)
    radarNum <- as.numeric(radarSaleDB)
    tmpradarList <- as.list(radarNum)
    gameName <- input$selectName
    radarlist <- list("gameName" = c(tmpradarList))
    
    chartJSRadar(scores = radarlist, labs = labs, maxScale = 50)
  })
  
  output$PiePlot <- renderAmCharts({
    UserPieDB <- noAllNaDB %>% filter(Year_of_Release %in% input$PieYear,Platform %in% input$selectplatFormP) %>% select(Name,User_Count)
    CriticPieDB <- noAllNaDB %>% filter(Year_of_Release %in% input$PieYear,Platform %in% input$selectplatFormP) %>% select(Name,Critic_Count)
      userPieCh <- as.character(UserPieDB[,1])
      userPieNum <- as.numeric(UserPieDB[,2])
      newUserPieDB <- data.frame(label = c(userPieCh),value = c(userPieNum))
      criticPieCh <- as.character(CriticPieDB[,1])
      criticPieNum <- as.numeric(CriticPieDB[,2])
      newCriticPieDB <- data.frame(label = c(criticPieCh),value = c(criticPieNum))
      switch(input$selectUsers,
             "Player"= amPie(data = newCriticPieDB,inner_radius = 50),
             "Critic"= amPie(data = newUserPieDB,inner_radius = 50)
      )
  })
  
}
shinyApp(ui = ui, server = server)
