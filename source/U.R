library(shiny)
library(ggplot2)


# Define the path to the data file
data_path <- file.path("..", "data", "automobile.csv")

# Load data
raw.data <- read.csv(data_path, stringsAsFactors = TRUE)

shinyUI(
  fluidPage(
    titlePanel("Análisis de datos sobre automóviles"),
    fluidRow(
      column(
        plotOutput("pie_chart"),
        plotOutput("cat_hist"),
        selectInput("hist_cat", "",
                    choices = c("modelo", "origen", "cilindros"),
                    selected = "modelo")
        selectInput("hist_color", "", 
                    choices = c("aceleracion", "masa"),
                    selected = "aceleracion")
        plotOutput ("cat_table"), 
        selectInput("cat_table", "",
                    choices = c("cilindros, origen"),
                    selected = "cilindros")
        selectInput("color_table","",
                    choices = c("modelo, mpg"),
                    selected = "modelo")
        plotOutput("scatter_plot")
        selectInput("scatter_x", "",
                    choices = c("modelo"),
                    selected = "modelo")
        selectInput("scatter_y", "", 
                    choices = c("aceleracion, HP"),
                    selected = "aceleracion")
        selectInput("scatter_color","",
                    choices = c("mpg", "origen"),
                    selected = "mpg")
        selectInput("scatter_size","",
                    choices = c("origen","aceleracion"),
                    selected = "origen")
        selectInput("scatter_alpha", "",
                    choices = c("mpg, modelo"),
                    selected = "mpg")
        plotOutput("box_plot")
        selectInput("box_x","",
                    choices = c("modelo, cilindros")
                    selected = "modelo")
        selectInput("box_y","",
                    choices = c("aceleracion, weight")
                    selected = "weight")
        selectInput("box_fill","",
                    choices = c("mpg", "origen")
                    selected = "mpg")
        )
      )
    )
  ) # fluidpage
) # shinyUI