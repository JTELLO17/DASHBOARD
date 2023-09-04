# ui.R

library(shiny)
library(ggplot2)


# Define the path to the data file
data_path <- file.path("..", "data", "automobile.csv")

# Load data
raw.data <- read.csv(data_path, stringsAsFactors = TRUE)

shinyUI(
  fluidPage(
    titlePanel("Automobile Data Analysis"),
    
    # First Row
    fluidRow(
      column(
        4, 
        plotOutput("pie_chart"), 
        textOutput("pie_text"),
        br(),
        selectInput("hist_category", "Frecuencia de:",
                    choices = c("model_year", "cylinders", "origin"), 
                    selected = "origin"
        ),
        selectInput("hist_color", "color:",
                    choices = c("model_year", "cylinders", "origin"), 
                    selected = "origin"
        ),
        plotOutput("histogram_plot")
        # selectInput("bar_category", "Frecuencia de:",
        #             choices = c("model_year", "cylinders", "origin"), selected = "origin"),
        # plotOutput("bar_chart")
      ),
      column(
        2, 
      ),
      column(
        6, 
        fluidRow(plotOutput("dispersion_plot")),
        fluidRow(
          selectInput("dispersion_x", "",
                      choices = c("mpg", "horsepower", "weight", "displacement", "acceleration"),
                      selected = "weight"),
          selectInput("dispersion_y", "",
                      choices = c("mpg", "horsepower", "weight", "displacement", "acceleration"),
                      selected = "acceleration"),
          selectInput("dispersion_color", "",
                      choices = c("origin", "cylinder", "weight", "displacement", "acceleration"),
                      selected = "weight")
        )
      ),      
    ),
  )
)

