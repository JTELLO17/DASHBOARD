library(dplyr)
library(ggplot2)

# Load data

raw.data <- read.csv('./data/automobile.csv', stringsAsFactors = TRUE)

# Function to create a pie chart based on origin
origin_pie_chart <- function() {
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
    # scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
    labs(title = "Distribución según procedéncia")
  
  return(pie_chart)
}


# Function to create a single bar based on categorical column
category_bar_chart <- function(cat) {
  data <- raw.data %>%
    group_by({{ cat }}) %>%
    count() %>%
    ungroup() %>%
    mutate(freq = n/sum(n),
           ymax = cumsum(freq),
           ymin = c(0, head(ymax, n=-1))) %>%
    arrange(desc(freq))
  
  data$category <- factor(data %>% pull({{ cat }}),
                          levels = data %>% pull({{ cat }}))
  
  bar_chart <- ggplot(data = data, 
                      mapping = aes(x = 'year', y = freq, fill = category)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    scale_y_continuous(labels = scales::percent, breaks = seq(0,1,0.1)) +
    labs(title = paste("Distribución según", substitute(cat)))
  
  return(bar_chart)
}


# Function to create a dispersion plot chart based on origin
dispersion_plot <- function(col1, col2, c) {
  data <- raw.data %>%
    mutate(acc = as.numeric(acceleration))
  plot <- ggplot(data = data, mapping = aes(x = {{ col1 }}, y = {{ col2 }})) +
    geom_point(mapping = aes(color = origin, size = cylinders, alpha = acceleration)) +
    scale_alpha_continuous() +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    labs(title = paste0(substitute(col2), " vs. ", substitute(col1)))
  
  return(plot)
}



# Make plots
dispersion_plot(weight, acceleration, origin)

dispersion_plot(c = model_year, col2 = displacement, col1 = horsepower)

origin_pie_chart()

category_bar_chart(model_year)

