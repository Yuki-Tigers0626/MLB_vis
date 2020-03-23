output$select_pitch_name5 <- renderUI({
  radioButtons(inputId = "pitch_name5", 
               label = "球種選択: ", 
               choices = Database() %>% 
                 dplyr::select(pitch_name) %>% 
                 dplyr::distinct(pitch_name) %>% 
                 as.matrix() %>% 
                 purrr::set_names())
})

output$year_by_year_range <- renderUI({
  sliderInput(inputId = "year_range",
              label = "年度範囲選択: ",
              min = min(Database()$game_year, na.rm=T),
              max = max(Database()$game_year, na.rm=T), 
              value = c(min(Database()$game_year, na.rm=T), max(Database()$game_year, na.rm=T)))
})

tmp <- reactive({
  Database() %>% 
    dplyr::filter(pitch_name==as.character(input$pitch_name5), 
                  game_year>=min(input$year_range), game_year<=max(input$year_range)
                  )
})

output$pfx_year_by_year_Both_plot <- renderPlot({
  g <- ggplot() + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
    labs(title=paste0(as.character(player_Name()), " ", as.character(input$pitch_name5), "の変化量推移"), 
         x=paste0("横変化量 (", input$unit, ")"), y=paste0("縦変化量 (", input$unit, ")")) + 
    xlim(-75, 75) + 
    ylim(-75, 75) + 
    theme_cowplot(16)
  
  if (input$plot_add_pfx2==1) {
    g <- g + 
      geom_point(data = tmp(), 
                 mapping = aes(x=pfx_x, y=pfx_z, color=factor(game_year)), size=2) + 
      scale_color_discrete(name="年度")
  }
  
  if (input$plot_add_pfx2==2) {
    g <- g + 
      stat_ellipse(data = tmp(), 
                   mapping = aes(x=pfx_x, y=pfx_z, color=factor(game_year), fill=factor(game_year)), 
                   geom="polygon", level=.8, alpha=.2) + 
      scale_color_discrete(name="年度") + 
      guides(fill=FALSE)
  }
  
  # if (input$plot_add_pfx2==3) {
  #   g <- g + 
  #     geom_text(data = tmp() %>%
  #                 dplyr::mutate(n = n()) %>%
  #                 dplyr::group_by(pitch_name, game_year) %>%
  #                 dplyr::summarise(pfx_x = mean(pfx_x, na.rm=T),
  #                                  pfx_z = mean(pfx_z, na.rm=T),
  #                                  `pitch%` = n()/mean(n)*100, 
  #                                  spin = mean(spin, na.rm=T)),
  #               mapping = aes(x=pfx_x, y=pfx_z, color=factor(game_year), size=`pitch%`, angle=(540-spin-45)%%360),
  #               label=emoji("baseball"), family="EmojiOne") + 
  #     scale_size(range = c(3, 10))
  # }
  
  print(g)
})

output$pfx_year_by_year_L_plot <- renderPlot({
  g <- ggplot() + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
    labs(title=paste0(as.character(player_Name()), " ", as.character(input$pitch_name5), "の対左変化量推移"), 
         x=paste0("横変化量 (", input$unit, ")"), y=paste0("縦変化量 (", input$unit, ")")) + 
    xlim(-75, 75) + 
    ylim(-75, 75) + 
    theme_cowplot(16)
  
  if (input$plot_add_pfx2==1) {
    g <- g + 
      geom_point(data = tmp() %>% 
                   dplyr::filter(stand=="L"), 
                 mapping = aes(x=pfx_x, y=pfx_z, color=factor(game_year)), size=2) + 
      scale_color_discrete(name="年度")
  }
  
  if (input$plot_add_pfx2==2) {
    g <- g + 
      stat_ellipse(data = tmp() %>% 
                     dplyr::filter(stand=="L"), 
                   mapping = aes(x=pfx_x, y=pfx_z, color=factor(game_year), fill=factor(game_year)), 
                   geom="polygon", level=.8, alpha=.2) + 
      scale_color_discrete(name="年度") + 
      guides(fill=FALSE)
  }
  
  print(g)
})

output$pfx_year_by_year_R_plot <- renderPlot({
  g <- ggplot() + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
    labs(title=paste0(as.character(player_Name()), " ", as.character(input$pitch_name5), "の対右変化量推移"), 
         x=paste0("横変化量 (", input$unit, ")"), y=paste0("縦変化量 (", input$unit, ")")) + 
    xlim(-75, 75) + 
    ylim(-75, 75) + 
    theme_cowplot(16)
  
  if (input$plot_add_pfx2==1) {
    g <- g + 
      geom_point(data = tmp() %>% 
                   dplyr::filter(stand=="R"), 
                 mapping = aes(x=pfx_x, y=pfx_z, color=factor(game_year)), size=2) + 
      scale_color_discrete(name="年度")
  }
  
  if (input$plot_add_pfx2==2) {
    g <- g + 
      stat_ellipse(data = tmp() %>% 
                     dplyr::filter(stand=="R"), 
                   mapping = aes(x=pfx_x, y=pfx_z, color=factor(game_year), fill=factor(game_year)), 
                   geom="polygon", level=.8, alpha=.2) + 
      scale_color_discrete(name="年度") + 
      guides(fill=FALSE)
  }
  
  print(g)
})