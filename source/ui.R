library(shiny)

source("./charts.R")

shinyUI(
  fluidPage(
    titlePanel("Automoviles"),
    
    fluidRow( # two buttons
      column(
        6,
        plotOutput("pie_chart"),
        plotOutput("summ_factor"),
        plotOutput("summ_numeric"),
        plotOutput ("cat_table"), 
        selectInput("table_cat", "",
                    choices = c("origen", "modelo", "cilindros"),
                    selected = "origen"),
        selectInput("table_color", "",
                    choices = c("origen", "modelo", "cilindros"),
                    selected = "origen")
      ),# column
      column(
        6,
        plotOutput("scatter_plot"),
        selectInput("scatter_x", "",
                    choices = c("masa", "aceleracion", "mpg", "HP", "distancia"),
                    selected = "masa"),
        selectInput("scatter_y", "",
                    choices = c("masa", "aceleracion", "mpg", "HP", "distancia"),
                    selected = "masa"),
        selectInput("scatter_color", "",
                    choices = c("origen", "modelo", "cilindros"),
                    selected = "origen"),
        selectInput("scatter_size", "",
                    choices = c("masa", "aceleracion", "mpg", "HP", "distancia"),
                    selected = "masa"),
        selectInput("scatter_alpha", "",
                    choices = c("masa", "aceleracion", "mpg", "HP", "distancia"),
                    selected = "masa"),
        plotOutput("box_plot"),
        selectInput("box_x","",
                    choices = c("origen", "modelo", "cilindros"),
                    selected = "origen"),
        selectInput("box_y", "",
                    choices = c("masa", "aceleracion", "mpg", "HP", "distancia"),
                    selected = "masa"),
        selectInput("box_fill", "",
                    choices = c("origen", "modelo", "cilindros"),
                    selected = "origen")
      ),# column
    ), # row
  ) # fluidPage
)
