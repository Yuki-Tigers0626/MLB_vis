output$pfx_Both_plot <- renderPlot({
  g <- ggplot() + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
    labs(title=paste0(as.character(player_Name()), " ", input$Year, "年度 通算変化量"), 
         x=paste0("横変化量 (", input$unit, ")"), y=paste0("縦変化量 (", input$unit, ")")) + 
    xlim(-75, 75) + 
    ylim(-75, 75) + 
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
                                   `pitch%` = n()/mean(n)*100, 
                                   spin = mean(spin, na.rm=T)),
                mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name, size=`pitch%`, angle=(540-spin-45)%%360), shapes=1, alpha=.5, 
                # label=emoji("baseball"), family="EmojiOne"
                ) +
      scale_size(range = c(3, 10))
  }
  print(g)
})

output$pfx_L_plot <- renderPlot({
  g <- ggplot() + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
    labs(title=paste0(as.character(player_Name()), " ", input$Year, "年度 対左変化量"), 
         x=paste0("横変化量 (", input$unit, ")"), y=paste0("縦変化量 (", input$unit, ")")) + 
    xlim(-75, 75) + 
    ylim(-75, 75) + 
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
                                   `pitch%` = n()/mean(n)*100, 
                                   spin = mean(spin, na.rm=T)),
                mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name, size=`pitch%`, angle=(540-spin-45)%%360), shapes=1, alpha=.5, 
                # label=emoji("baseball"), family="EmojiOne"
                ) + 
      scale_size(range = c(3, 10))
  }
  print(g)
})

output$pfx_R_plot <- renderPlot({
  g <- ggplot() + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
    labs(title=paste0(as.character(player_Name()), " ", input$Year, "年度 対右変化量"), 
         x=paste0("横変化量 (", input$unit, ")"), y=paste0("縦変化量 (", input$unit, ")")) + 
    xlim(-75, 75) + 
    ylim(-75, 75) + 
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
                                   `pitch%` = n()/mean(n)*100, 
                                   spin = mean(spin, na.rm=T)),
                mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name, size=`pitch%`, angle=(540-spin-45)%%360), shapes=1, alpha=.5, 
                # label=emoji("baseball"), family="EmojiOne"
                ) + 
      scale_size(range = c(3, 10))
  }
  print(g)
})

output$pfx_Both_plot_top <- renderPlot({
  g <- ggplot() + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
    labs(title=paste0(as.character(player_Name()), " ", input$Year, "年度 通算変化量"), 
         x=paste0("横変化量 (", input$unit, ")"), y=paste0("縦変化量 (", input$unit, ")")) + 
    xlim(-75, 75) + 
    ylim(-75, 75) + 
    theme_cowplot(16)
  
  if (input$plot_add_pfx==1) {
    g <- g + 
      geom_point(data = database() %>% 
                   dplyr::filter(inning_topbot=="Top"), 
                 mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name), size=2)
  }
  
  if (input$plot_add_pfx==2) {
    g <- g + 
      stat_ellipse(data = database() %>% 
                     dplyr::filter(inning_topbot=="Top"), 
                   mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name, fill = pitch_name), 
                   geom="polygon", level=.8, alpha=.2)
  }
  
  if (input$plot_add_pfx==3) {
    g <- g + 
      geom_point(data = database() %>% 
                  dplyr::filter(inning_topbot=="Top") %>%
                  dplyr::mutate(n = n()) %>%
                  dplyr::group_by(pitch_name) %>%
                  dplyr::summarise(pfx_x = mean(pfx_x, na.rm=T),
                                   pfx_z = mean(pfx_z, na.rm=T),
                                   `pitch%` = n()/mean(n)*100, 
                                   spin = mean(spin, na.rm=T)),
                mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name, size=`pitch%`, angle=(540-spin-45)%%360), shapes=1, alpha=.5, 
                # label=emoji("baseball"), family="EmojiOne"
                ) + 
      scale_size(range = c(3, 10))
  }
  print(g)
})

output$pfx_L_plot_top <- renderPlot({
  g <- ggplot() + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
    labs(title=paste0(as.character(player_Name()), " ", input$Year, "年度 対左変化量"), 
         x=paste0("横変化量 (", input$unit, ")"), y=paste0("縦変化量 (", input$unit, ")")) + 
    xlim(-75, 75) + 
    ylim(-75, 75) + 
    theme_cowplot(16)
  
  if (input$plot_add_pfx==1) {
    g <- g + 
      geom_point(data = database() %>% 
                   dplyr::filter(inning_topbot=="Top", stand=="L"), 
                 mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name), size=2)
  }
  
  if (input$plot_add_pfx==2) {
    g <- g + 
      stat_ellipse(data = database() %>% 
                     dplyr::filter(inning_topbot=="Top", stand=="L"), 
                   mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name, fill = pitch_name), 
                   geom="polygon", level=.8, alpha=.2)
  }
  
  if (input$plot_add_pfx==3) {
    g <- g + 
      geom_point(data = database() %>% 
                  dplyr::filter(inning_topbot=="Top", stand=="L") %>% 
                  dplyr::mutate(n = n()) %>% 
                  dplyr::group_by(pitch_name) %>% 
                  dplyr::summarise(pfx_x = mean(pfx_x, na.rm=T),
                                   pfx_z = mean(pfx_z, na.rm=T),
                                   `pitch%` = n()/mean(n)*100, 
                                   spin = mean(spin, na.rm=T)),
                mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name, size=`pitch%`, angle=(540-spin-45)%%360), shapes=1, alpha=.5, 
                # label=emoji("baseball"), family="EmojiOne"
                ) + 
      scale_size(range = c(3, 10))
  }
  print(g)
})

output$pfx_R_plot_top <- renderPlot({
  g <- ggplot() + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
    labs(title=paste0(as.character(player_Name()), " ", input$Year, "年度 対右変化量"), 
         x=paste0("横変化量 (", input$unit, ")"), y=paste0("縦変化量 (", input$unit, ")")) + 
    xlim(-75, 75) + 
    ylim(-75, 75) + 
    theme_cowplot(16)
  
  if (input$plot_add_pfx==1) {
    g <- g + 
      geom_point(data = database() %>% 
                   dplyr::filter(inning_topbot=="Top", stand=="R"), 
                 mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name), size=2)
  }
  
  if (input$plot_add_pfx==2) {
    g <- g + 
      stat_ellipse(data = database() %>% 
                     dplyr::filter(inning_topbot=="Top", stand=="R"), 
                   mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name, fill = pitch_name), 
                   geom="polygon", level=.8, alpha=.2)
  }
  
  if (input$plot_add_pfx==3) {
    g <- g + 
      geom_point(data = database() %>% 
                  dplyr::filter(inning_topbot=="Top", stand=="R") %>% 
                  dplyr::mutate(n = n()) %>% 
                  dplyr::group_by(pitch_name) %>% 
                  dplyr::summarise(pfx_x = mean(pfx_x, na.rm=T),
                                   pfx_z = mean(pfx_z, na.rm=T),
                                   `pitch%` = n()/mean(n)*100, 
                                   spin = mean(spin, na.rm=T)),
                mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name, size=`pitch%`, angle=(540-spin-45)%%360), shapes=1, alpha=.5, 
                # label=emoji("baseball"), family="EmojiOne"
                ) + 
      scale_size(range = c(3, 10))
  }
  print(g)
})

output$pfx_Both_plot_bot <- renderPlot({
  g <- ggplot() + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
    labs(title=paste0(as.character(player_Name()), " ", input$Year, "年度 通算変化量"), 
         x=paste0("横変化量 (", input$unit, ")"), y=paste0("縦変化量 (", input$unit, ")")) + 
    xlim(-75, 75) + 
    ylim(-75, 75) + 
    theme_cowplot(16)
  
  if (input$plot_add_pfx==1) {
    g <- g + 
      geom_point(data = database() %>% 
                   dplyr::filter(inning_topbot=="Bot"), 
                 mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name), size=2)
  }
  
  if (input$plot_add_pfx==2) {
    g <- g + 
      stat_ellipse(data = database() %>% 
                     dplyr::filter(inning_topbot=="Bot"), 
                   mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name, fill = pitch_name), 
                   geom="polygon", level=.8, alpha=.2)
  }
  
  if (input$plot_add_pfx==3) {
    g <- g + 
      geom_point(data = database() %>% 
                  dplyr::filter(inning_topbot=="Bot") %>%
                  dplyr::mutate(n = n()) %>%
                  dplyr::group_by(pitch_name) %>%
                  dplyr::summarise(pfx_x = mean(pfx_x, na.rm=T),
                                   pfx_z = mean(pfx_z, na.rm=T),
                                   `pitch%` = n()/mean(n)*100, 
                                   spin = mean(spin, na.rm=T)),
                mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name, size=`pitch%`, angle=(540-spin-45)%%360), shapes=1, alpha=.5, 
                # label=emoji("baseball"), family="EmojiOne"
                ) + 
      scale_size(range = c(3, 10))
  }
  print(g)
})

output$pfx_L_plot_bot <- renderPlot({
  g <- ggplot() + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
    labs(title=paste0(as.character(player_Name()), " ", input$Year, "年度 対左変化量"), 
         x=paste0("横変化量 (", input$unit, ")"), y=paste0("縦変化量 (", input$unit, ")")) + 
    xlim(-75, 75) + 
    ylim(-75, 75) + 
    theme_cowplot(16)
  
  if (input$plot_add_pfx==1) {
    g <- g + 
      geom_point(data = database() %>% 
                   dplyr::filter(inning_topbot=="Bot", stand=="L"), 
                 mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name), size=2)
  }
  
  if (input$plot_add_pfx==2) {
    g <- g + 
      stat_ellipse(data = database() %>% 
                     dplyr::filter(inning_topbot=="Bot", stand=="L"), 
                   mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name, fill = pitch_name), 
                   geom="polygon", level=.8, alpha=.2)
  }
  
  if (input$plot_add_pfx==3) {
    g <- g + 
      geom_point(data = database() %>% 
                  dplyr::filter(inning_topbot=="Bot", stand=="L") %>% 
                  dplyr::mutate(n = n()) %>% 
                  dplyr::group_by(pitch_name) %>% 
                  dplyr::summarise(pfx_x = mean(pfx_x, na.rm=T),
                                   pfx_z = mean(pfx_z, na.rm=T),
                                   `pitch%` = n()/mean(n)*100, 
                                   spin = mean(spin, na.rm=T)),
                mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name, size=`pitch%`, angle=(540-spin-45)%%360), shapes=1, alpha=.5, 
                # label=emoji("baseball"), family="EmojiOne"
                ) + 
      scale_size(range = c(3, 10))
  }
  print(g)
})

output$pfx_R_plot_bot <- renderPlot({
  g <- ggplot() + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
    labs(title=paste0(as.character(player_Name()), " ", input$Year, "年度 対右変化量"), 
         x=paste0("横変化量 (", input$unit, ")"), y=paste0("縦変化量 (", input$unit, ")")) + 
    xlim(-75, 75) + 
    ylim(-75, 75) + 
    theme_cowplot(16)
  
  if (input$plot_add_pfx==1) {
    g <- g + 
      geom_point(data = database() %>% 
                   dplyr::filter(inning_topbot=="Bot", stand=="R"), 
                 mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name), size=2)
  }
  
  if (input$plot_add_pfx==2) {
    g <- g + 
      stat_ellipse(data = database() %>% 
                     dplyr::filter(inning_topbot=="Bot", stand=="R"), 
                   mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name, fill = pitch_name), 
                   geom="polygon", level=.8, alpha=.2)
  }
  
  if (input$plot_add_pfx==3) {
    g <- g + 
      geom_point(data = database() %>% 
                  dplyr::filter(inning_topbot=="Bot", stand=="R") %>% 
                  dplyr::mutate(n = n()) %>% 
                  dplyr::group_by(pitch_name) %>% 
                  dplyr::summarise(pfx_x = mean(pfx_x, na.rm=T),
                                   pfx_z = mean(pfx_z, na.rm=T),
                                   `pitch%` = n()/mean(n)*100, 
                                   spin = mean(spin, na.rm=T)),
                mapping = aes(x=pfx_x, y=pfx_z, color=pitch_name, size=`pitch%`, angle=(540-spin-45)%%360), shapes=1, alpha=.5, 
                # label=emoji("baseball"), family="EmojiOne"
                ) + 
      scale_size(range = c(3, 10))
  }
  print(g)
})