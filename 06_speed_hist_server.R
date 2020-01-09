output$select_pitch_name1 <- renderUI({
  selectInput(inputId = "pitch_name1", 
              label = "球種選択1: ", 
              choices = database() %>% 
                dplyr::select(pitch_name) %>% 
                dplyr::distinct(pitch_name))
})

output$select_pitch_name2 <- renderUI({
  selectInput(inputId = "pitch_name2", 
              label = "球種選択2: ", 
              choices = database() %>% 
                dplyr::select(pitch_name) %>% 
                dplyr::distinct(pitch_name))
})

speed_hist_xlim1 <- reactive({
  if (input$speed_hist_select==0) {
    database() %>% 
      dplyr::filter(pitch_name%in%c(input$pitch_name1)) %>% 
      dplyr::select(effective_speed)
  } else {
    database() %>% 
      dplyr::filter(pitch_name%in%c(input$pitch_name1, input$pitch_name2)) %>% 
      dplyr::select(effective_speed)
  }
})

speed_hist_xlim2 <- reactive({
  if (input$speed_hist_select==0) {
    database() %>% 
      dplyr::filter(pitch_name%in%c(input$pitch_name2)) %>% 
      dplyr::select(effective_speed)
  } else {
    database() %>% 
      dplyr::filter(pitch_name%in%c(input$pitch_name1, input$pitch_name2)) %>% 
      dplyr::select(effective_speed)
  }
})

output$speed_hist_plot1 <- renderPlot({
  g <- database() %>%
    dplyr::filter(pitch_name==input$pitch_name1) %>% 
    ggplot(aes(x=effective_speed)) +
    geom_histogram(binwidth=.25) + 
    labs(title=paste0(input$Name, " ", input$year, "年度 ", input$pitch_name1, " 球速帯"), x="球速 (mph)", y="投球数") + 
    xlim(min(speed_hist_xlim1()), max(speed_hist_xlim1())) + 
    theme_cowplot(16)
  print(g)
})

output$speed_hist_plot2 <- renderPlot({
  g <- database() %>%
    dplyr::filter(pitch_name==input$pitch_name2) %>% 
    ggplot(aes(x=effective_speed)) +
    geom_histogram(binwidth=.25) + 
    labs(title=paste0(input$Name, " ", input$year, "年度 ", input$pitch_name2, " 球速帯"), x="球速 (mph)", y="投球数") + 
    xlim(min(speed_hist_xlim2()), max(speed_hist_xlim2())) + 
    theme_cowplot(16)
  print(g)
})