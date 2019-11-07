library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(date)
library(dplyr)
library(ggplot2)
library(lubridate)
library(datasets)
library(rAmCharts)
fluidPage(
header <- dashboardHeader(title = span(tagList(icon("calendar"), "Press Freedom Index"))),
  
sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("wordcloud", tabName = "word")
    )
  ),
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "word",
            fluidRow(
      selectInput("selection", "Choose a type:",
                 list("Publisher",
                      "Platform",
                      "Genre")),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 10, value = 15),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 600,  value = 200)
    
            ),
  mainPanel(plotOutput("aDataPlot"))
  )
  )
  
    )
  )
