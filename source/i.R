library(shiny)

source("./charts.R")

shinyUI(
  fluidPage(
    titlePanel("Automoviles"),
    
    fluidRow(
      column(
        
      ) #column
    ), # row
    
    fluidRow( # two buttons
      column(6),# column
      column(6),# column
    ), # row
    
    fluidRow( # three buttons
      column(4),# column
      column(4),# column
      column(4),# column
    ), # row
  ) # fluidPage
)
