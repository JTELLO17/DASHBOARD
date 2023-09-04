library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)

raw_data <- function() {
  # Define the path to the data file
  data_path <- file.path("..", "data", "automobile.csv")
  
  # Load data
  raw.data <- read.csv(data_path, stringsAsFactors = TRUE) %>%
    mutate(
      # Categorical variables
      nombre = factor(name),
      origen = factor(origin),
      modelo =  factor(model_year),
      cilindros = factor(cylinders),
      
      # Numerical variables
      masa = as.numeric(weight),
      aceleracion = as.numeric(acceleration),
      HP = as.integer(horsepower),
      mpg = as.numeric(mpg),
      distancia = as.numeric(displacement),
    ) %>%
    select(
      nombre,
      origen,
      modelo,
      cilindros,
      masa,
      aceleracion,
      HP,
      mpg,
      distancia
    ) %>%
    arrange(modelo)
  
  return(raw.data)
}
raw.data <- raw_data()


cat_hist <- function(cat, color, binsize = 30) {
  data <- raw.data %>%
    select(!!cat, !!color)

  pl <- gghistogram(
    data = data,
    x = cat,
    color = color,
    fill = color,
    bins = binsize,
    stat = "count",
    alpha = 0.7,
    position = "stack"
  )
  
  return(pl)
}

scatter_plot <- function(x, y, color = 'origen', size = 'mpg', alpha = "HP") {
  col1 <- as.name(x)
  col2 <- as.name(y)
  col3 <- as.name(color)
  col4 <- as.name(size)
  col5 <- as.name(alpha)
  
  data <- raw.data %>%
    dplyr::select(!!col1, !!col2, !!col3, !!col4, !!col5)
  
  pl <- ggplot(data = data,
               mapping = aes(x = !!col1, y = !!col2)) +
    geom_point(mapping = aes(color = !!col3,
                             size = !!col4,
                             alpha = !!col5)) +
    scale_alpha_continuous() +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  return(pl)
}

box_plot <- function(x, y, color = 'origen', size = 'mpg', alpha = "HP") {
  col1 <- as.name(x)
  col2 <- as.name(y)
  col3 <- as.name(color)
  col4 <- as.name(size)
  col5 <- as.name(alpha)
  
  data <- raw.data %>%
    dplyr::select(!!col1, !!col2, !!col3, !!col4, !!col5)
  
  pl <- ggplot(data = data,
               mapping = aes(x = !!col1, y = !!col2)) +
    geom_boxplot(mapping = aes(fill = !!col3), color = "black") +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  return(pl)
}

cat_table <- function(cat, color) {
  col1 <- as.name(cat)
  col2 <- as.name(color)
  
  tb <- raw.data %>%
    dplyr::select(!!col1, !!col2) %>% 
    dplyr::group_by(!!col1, !!col2) %>% 
    count() %>%
    pivot_wider(id_cols = !!color, names_from = !!cat, values_from = n) %>%
    as.data.frame()
  tb <- replace(tb, is.na(tb), 0)
  
  return(tb)
}

cat_table_render <- function(cat, color) {
  tb <- cat_table(cat, color)
  rownames(tb) <- tb[[color]]
  tb[[color]] <- NULL
  
  pl <- ggtexttable(x = tb, 
                    rows = rownames(tb), 
                    cols = colnames(tb),
                    theme = ttheme('mOrange')) %>%
    tbody_add_border()
  
  return(pl)
}

summary_table_render <- function(sel = is.numeric) {
  tb <- raw.data %>% 
    select_if(sel) %>% 
    summary()
  
  pl <- ggtexttable(x = tb,
                    rows = rownames(tb),
                    cols = colnames(tb),
                    theme = ttheme('mOrange')) %>%
    tbody_add_border() 
  
  return(pl)
}


cat_hist("modelo", "cilindros")
cat_table("cilindros", "origen")
cat_table_render("modelo", "cilindros")
scatter_plot("modelo", "aceleracion", "origen")
box_plot("modelo", "masa", "origen")
summary_table_render(is.numeric)
