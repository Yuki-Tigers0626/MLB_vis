output$pfx_Both_plot <- renderPlot({
  g <- ggplot() + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
    labs(title=paste0(as.character(player_Name()), " ", input$year, "年度 通算変化量"), 
         x=paste0("横変化量 (", input$unit, ")"), y=paste0("縦変化量 (", input$unit, ")")) + 
    xlim(min(Database2()$pfx_x)*1.1, max(Database2()$pfx_x)*1.1) + 
    ylim(min(Database2()$pfx_z)*1.1, max(Database2()$pfx_z)*1.1) + 
    theme_cowplot(16)
  
  if (input$plot_add_pfx==1) {
    g <- g + 
      geom_point(data = database(), 
                 mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name), size=2)
  }
  
  if (input$plot_add_pfx==2) {
    g <- g + 
      stat_ellipse(data = database(), 
                   mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name, fill = pitch_name), 
                   geom="polygon", level=.8, alpha=.2)
  }
  
  if (input$plot_add_pfx==3) {
    g <- g + 
      geom_point(data = database() %>% 
                   dplyr::mutate(n = n()) %>% 
                   dplyr::group_by(pitch_name) %>% 
                   dplyr::summarise(pfx_x = mean(pfx_x, na.rm=T), 
                                    pfx_z = mean(pfx_z, na.rm=T),  
                                    `pitch%` = n()/mean(n)*100), 
                 mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name, size=`pitch%`), 
                 alpha=.5) + 
      scale_size(range = c(3, 10))
  }
  print(g)
})

output$pfx_L_plot <- renderPlot({
  g <- ggplot() + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
    labs(title=paste0(as.character(player_Name()), " ", input$year, "年度 対左変化量"), 
         x=paste0("横変化量 (", input$unit, ")"), y=paste0("縦変化量 (", input$unit, ")")) + 
    xlim(min(Database2()$pfx_x)*1.1, max(Database2()$pfx_x)*1.1) + 
    ylim(min(Database2()$pfx_z)*1.1, max(Database2()$pfx_z)*1.1) + 
    theme_cowplot(16)
  
  if (input$plot_add_pfx==1) {
    g <- g + 
      geom_point(data = database() %>% 
                   dplyr::filter(stand=="L"), 
                 mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name), size=2)
  }
  
  if (input$plot_add_pfx==2) {
    g <- g + 
      stat_ellipse(data = database() %>% 
                     dplyr::filter(stand=="L"), 
                   mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name, fill = pitch_name), 
                   geom="polygon", level=.8, alpha=.2)
  }
  
  if (input$plot_add_pfx==3) {
    g <- g + 
      geom_point(data = database() %>% 
                   dplyr::filter(stand=="L") %>% 
                   dplyr::mutate(n = n()) %>% 
                   dplyr::group_by(pitch_name) %>% 
                   dplyr::summarise(pfx_x = mean(pfx_x, na.rm=T), 
                                    pfx_z = mean(pfx_z, na.rm=T), 
                                    `pitch%` = n()/mean(n)*100), 
                 mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name, size=`pitch%`), 
                 alpha=.5) + 
      scale_size(range = c(3, 10))
  }
  print(g)
})

output$pfx_R_plot <- renderPlot({
  g <- ggplot() + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
    labs(title=paste0(as.character(player_Name()), " ", input$year, "年度 対右変化量"), 
         x=paste0("横変化量 (", input$unit, ")"), y=paste0("縦変化量 (", input$unit, ")")) + 
    xlim(min(Database2()$pfx_x)*1.1, max(Database2()$pfx_x)*1.1) + 
    ylim(min(Database2()$pfx_z)*1.1, max(Database2()$pfx_z)*1.1) + 
    theme_cowplot(16)
  
  if (input$plot_add_pfx==1) {
    g <- g + 
      geom_point(data = database() %>% 
                   dplyr::filter(stand=="R"), 
                 mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name), size=2)
  }
  
  if (input$plot_add_pfx==2) {
    g <- g + 
      stat_ellipse(data = database() %>% 
                     dplyr::filter(stand=="R"), 
                   mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name, fill = pitch_name), 
                   geom="polygon", level=.8, alpha=.2)
  }
  
  if (input$plot_add_pfx==3) {
    g <- g + 
      geom_point(data = database() %>% 
                   dplyr::filter(stand=="R") %>% 
                   dplyr::mutate(n = n()) %>% 
                   dplyr::group_by(pitch_name) %>% 
                   dplyr::summarise(pfx_x = mean(pfx_x, na.rm=T), 
                                    pfx_z = mean(pfx_z, na.rm=T), 
                                    `pitch%` = n()/mean(n)*100), 
                 mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name, size=`pitch%`), 
                 alpha=.5) + 
      scale_size(range = c(3, 10))
  }
  print(g)
})