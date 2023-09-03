library(dplyr)
library(ggplot2)
library(ggpubr)

# Define the path to the data file
data_path <- file.path("..", "data", "automobile.csv")

# Load data
raw.data <- read.csv(data_path, stringsAsFactors = TRUE) %>%
  mutate(
    # Categorical variables
    nombre = as.factor(name),
    origen = as.factor(origin),
    modelo =  as.factor(model_year, levels = order(raw.data$model_year)),
    cilindros = as.factor(cylinder, levels = order(raw.data$cylinders, decreasing = TRUE)),
    
    # Numerical variables
    masa = as.numeric(weight),
    aceleracion = as.numeric(acceleration),
    HP = as.integer(horsepower),
    mpg = as.numeric(mpg),
    distancia = as.numeric(displacement),
  )

shinyServer(function(input, output) {
  
  # Function to create a pie chart based on origin
  output$pie_chart <- renderPlot({
    data <- raw.data %>%
      group_by(origin) %>%
      count() %>%
      ungroup() %>%
      mutate(freq = n/sum(n),
             ymax = cumsum(freq),
             ymin = c(0, head(ymax, n=-1))) %>%
      arrange(desc(freq))
    
    data$procedencia <- factor(data %>% pull(origin),
                               levels = data %>% pull(origin))
    
    pie_chart <- ggplot(data = data, 
                        mapping = aes(x = 0.7, y = freq, fill = procedencia)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      geom_text(mapping = aes(x = 0.8, 
                              y = ymax - freq / 2, 
                              label = paste0(round(freq * 100, 1), "%")), 
                color = "black") +
      coord_polar(theta = "y", start = 0) +
      theme_void() +
      labs(title = "Distribución según procedéncia")
    
    print(pie_chart) # You can also return the plot directly instead of using print
  })
  
  output$pie_text <- renderText({
    text <- "
    Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
    Morbi eget scelerisque tortor, vel scelerisque arcu. 
    Phasellus quis dui semper, imperdiet nulla pulvinar, lacinia sem. 
    Vivamus vitae sollicitudin dolor. 
    Suspendisse condimentum, lacus at efficitur faucibus, tortor augue dictum lorem, in lobortis sapien nibh id tellus. 
    Donec fringilla, tellus sed rhoncus fringilla. 
    "
    text
  })
  
  output$histogram_plot <- renderPlot({
    cat <- as.name(input$hist_category)
    color <- as.name(input$hist_color)
    
    data <- raw.data
    
    gg_hist <- ggplot(data, aes(x = !!cat, fill = !!color)) +
      geom_histogram(stat = "count", position = "dodge") +
      labs(title = paste("Histogram of", cat),
           x = input$hist_category,
           y = "Count") +
      theme_pubr()
    
    gg_hist
  })
  
  # Function to create a single bar based on categorical column
  # output$bar_chart <- renderPlot({
  #   cat <- as.name(input$bar_category)
  #   data <- raw.data %>%
  #     group_by(!!cat) %>%
  #     count() %>%
  #     ungroup() %>%
  #     mutate(freq = n/sum(n),
  #            ymax = cumsum(freq),
  #            ymin = c(0, head(ymax, n=-1))) %>%
  #     arrange(desc(freq))
  #   
  #   
    # data$category <- factor(data %>% pull(!!cat),
    #                         levels = data %>% pull(!!cat))
    # bar_chart <- ggplot(data = data,
    #                     mapping = aes(y = freq, fill = category)) +
    #   geom_bar(stat = "identity", width = 1, color = "white") +
    #   theme_bw() +
    #   theme(panel.grid = element_blank()) +
    #   scale_y_continuous(labels = scales::percent, breaks = seq(0,1,0.1))
    # print(bar_chart) # You can also return the plot directly instead of using print
  # })
  
  # Function to create a dispersion plot chart based on origin
  output$dispersion_plot <- renderPlot({
    col1 <- as.name(input$dispersion_x)
    col2 <- as.name(input$dispersion_y)
    color <- as.name(input$dispersion_color)
    data <- raw.data %>%
      mutate(acc = as.numeric(acceleration))
    plot <- ggplot(data = data, mapping = aes(x = !!col1, y = !!col2)) +
      geom_point(mapping = aes(color = !!color, size = cylinders, alpha = acceleration)) +
      scale_alpha_continuous() +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      labs(title = paste0(substitute(col2), " vs. ", substitute(col1)))
    
    print(plot) # You can also return the plot directly instead of using print
  })
  
})
