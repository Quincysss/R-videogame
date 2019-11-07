library(shiny)
library(wordcloud)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(datasets)
library(rAmCharts)
library(radarchart)
INgameDB <- read.csv("Video_Game_Sales.csv",header = T)
gameDB <- read.csv("Video_Game_Sales.csv",header = T)
gameDB[gameDB == 0] <-NA
noNAgameDBSale <- gameDB[complete.cases(gameDB[,6:10]),]
CrnoNAgameDB <- gameDB[complete.cases(gameDB[,11:15]),]
noAllNaDB <- gameDB[complete.cases(gameDB[,1:15]),]
labs <- c("North America","Europe","Japan","Other")
header <- dashboardHeader(title = span(tagList(icon("gamepad"), "Electronic Game!")))

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
                radioGroupButtons(
                  inputId = "selection",
                  label = "Choose a type:", 
                  choices = c("Publisher", "Platform", "Genre"),
                  status = "warning"
                ),
              
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
              absolutePanel(top = 530,right = 650,fixed = FALSE,materialSwitch(inputId = "switchinput", label = "Show all:", status = "warning")),
              
              absolutePanel(top = 45,right = 410, fixed = FALSE,
                            selectInput("selectGenre", "Choose a Genre:", choices = levels(gameDB$Genre),width = 200) 
              ),             
              absolutePanel(top = 45,right = 210, fixed = FALSE,
                            selectInput("selectArea", "Choose a Area:",c( "North America",
                                                                          "Europe",
                                                                          "Japan",
                                                                          "Other Area",
                                                                          "Global Amount"),width = 200)),
              hr(),
              amChartsOutput("rank"),
              hr(),
              hr(),
              hr(),
              amChartsOutput("allRank"),
              absolutePanel(top = 500,right = 210, fixed = FALSE,
                            selectInput("selectArea2", "Choose a Area:",c( "North America" ,
                                                                           "Europe",
                                                                           "Japan",
                                                                           "Other Area",
                                                                           "Global Amount"),width = 200)),
              
              absolutePanel(top = 480, right = 20, fixed = FALSE,
                            knobInput("rankyear", "", min = 1983, max = 2016, value = 2016,
                                      displayPrevious = FALSE, 
                                      lineCap = "round",
                                      fgColor = "#ffa31a",
                                      bgColor = "FFFFFF",
                                      inputColor = "#ffa31a",
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
            absolutePanel(top=300,right = 240, chartJSRadarOutput("radarPlot", width = "350", height = "350") )
    ),
    tabItem(tabName = "number",
            fluidRow(
              sliderInput("PieYear",
                          "Select Year for PIE:",
                          min = 1970,  max = 2020, value = 2015),
              radioGroupButtons(
                inputId = "selectGroup",
                label = "select Group:", 
                choices = c("Player", "Critic"),
                status = "warning"
              ),
              selectInput("selectplatFormP", "Choose a platform:", choices = levels(noAllNaDB$Platform),width = 200), 
              amChartsOutput("PiePlot",height = 520)
            ))
  )
  
)

ui <- dashboardPage(header, sidebar, body, skin = "yellow")
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
    if(input$switchinput == TRUE){
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
    selectInput("selectName", "Select Game:", choices = as.character(INgameDB[which(INgameDB$Genre %in% input$selectGenre1 & INgameDB$Platform %in% input$selectplatForm),"Name"]))
  }) 
  
  output$radarPlot<-renderChartJSRadar ({
    radarSaleDB <- INgameDB %>% filter(Name %in% input$selectName,Platform %in% input$selectplatForm) %>% select(NA_Sales,EU_Sales,JP_Sales,Other_Sales)
    radarNum <- as.numeric(radarSaleDB)
    tmpradarList <- as.list(radarNum)
    radarlist <- list(Sale = c(tmpradarList))
    
    chartJSRadar(scores = radarlist, labs = labs, maxScale = 20)
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
    switch(input$selectGroup,
           "Player"= amPie(data = newCriticPieDB,inner_radius = 50),
           "Critic"= amPie(data = newUserPieDB,inner_radius = 50)
    )
  })
  
}
shinyApp(ui = ui, server = server)
