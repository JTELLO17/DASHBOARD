source("./charts.R")

shinyServer(function(input, output) {
  
  # Function to create a pie chart based on origin
  output$pie_chart <- renderPlot({
    pie_chart()
  })
  
  output$foo_text <- renderText({
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
  
  output$cat_hist <- renderPlot({
    cat_hist(cat = input$hist_cat, color = input$hist_color)
  })
  
  output$cat_table <- renderPlot({
    cat_table_render(cat = input$table_cat, color = input$table_color)
  })

  output$scatter_plot <- renderPlot({
    scatter_plot(x = input$scatter_x, 
                 y = input$scatter_y, 
                 color = input$scatter_color, 
                 size = input$scatter_size, 
                 alpha = input$scatter_alpha)
  })

  output$box_plot <- renderPlot({
    box_plot(x = input$box_x, y = input$box_y, fill = input$box_fill)
  })

  output$summ_factor <- renderPlot({
    summary_table_render(is.factor)    
  })
  
  output$summ_numeric <- renderPlot({
    summary_table_render(is.numeric)    
  })
  
})
