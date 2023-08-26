library(dplyr)
library(ggplot2)

# Function to create a pie chart
create_pie_chart <- function(data, category_column) {
  data <- data %>%
    group_by({{ category_column }}) %>%
    count() %>%
    ungroup() %>%
    mutate(freq = n/sum(n),
           ymax = cumsum(freq),
           ymin = c(0, head(ymax, n=-1))) %>%
    arrange(desc(freq))
  
  data$category <- factor(data %>% pull({{ category_column }}),
                          levels = data %>% pull({{ category_column }}))
  
  pie_chart <- ggplot(data = data, 
                      mapping = aes(x = 0.7, y = freq, fill = category)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    geom_text(mapping = aes(x = 0.8, 
                            y = ymax - freq / 2, 
                            label = paste0(round(freq * 100, 1), "%")), 
              color = "black") +
    coord_polar(theta = "y", start = 0) +
    theme_void() +
    # scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
    labs(title = paste("Distribution of", substitute(category_column)))
  
  return(pie_chart)
}

create_bar_chart <- function(data, category_column) {
  data <- data %>%
    group_by({{ category_column }}) %>%
    count() %>%
    ungroup() %>%
    mutate(freq = n/sum(n),
           ymax = cumsum(freq),
           ymin = c(0, head(ymax, n=-1))) %>%
    arrange(desc(freq))
  
  data$category <- factor(data %>% pull({{ category_column }}),
                          levels = data %>% pull({{ category_column }}))
  
  bar_chart <- ggplot(data = data, 
                      mapping = aes(x = 0.7, y = freq, fill = category)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    geom_text(mapping = aes(x = 0.8, 
                            y = ymax - freq / 2, 
                            label = paste0(round(freq * 100, 1), "%")), 
              color = "black") +
    theme_void() +
    labs(title = paste("Distribution of", substitute(category_column)))
  
  return(bar_chart)
}

# Load dataset
dataset <- read.csv('./data/automobile.csv', stringsAsFactors = TRUE)

# Create pie chart using the function
pie_chart <- create_pie_chart(dataset, origin)

# Print the plot
print(pie_chart)

create_bar_chart(dataset, model_year)

create_pie_chart(dataset, model_year)
