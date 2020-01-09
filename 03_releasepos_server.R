output$release_pos_xz_plot <- renderPlot({
  g <- ggplot() + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
    geom_text(aes(x=0, y=0, label="Pitcher's\nPlate")) + 
    labs(title=as.character(input$Name), subtitle="リリースポイント(x-z：正面から見た図)") + 
    xlim(-max(abs(Database2()$release_pos_x))*1.1, max(abs(Database2()$release_pos_x))*1.1) + 
    ylim(0, max(abs(Database2()$release_pos_z))*1.1) + 
    theme_cowplot(16)
  
  if (input$plot_add_release_pos==1) {
    g <- g + 
      geom_point(data = database(), 
                 mapping = aes(x=release_pos_x, y=release_pos_z, color=pitch_name), 
                 size=.5)
  }
  
  if (input$plot_add_release_pos==2) {
    g <- g + 
      stat_ellipse(data = database(), 
                   mapping = aes(x=release_pos_x, y=release_pos_z, color=pitch_name, fill = pitch_name), 
                   geom="polygon", level=.8, alpha=.2)
  }
  
  if (input$plot_add_release_pos==3) {
    g <- g + 
      geom_point(data = database() %>% 
                   dplyr::mutate(n = n()) %>% 
                   dplyr::group_by(pitch_name) %>% 
                   dplyr::summarise(release_pos_x = mean(release_pos_x, na.rm=T), 
                                    release_pos_z = mean(release_pos_z, na.rm=T), 
                                    `pitch%` = n()/mean(n)*100), 
                 mapping = aes(x=release_pos_x, y=release_pos_z, color=pitch_name, size=`pitch%`), 
                 alpha=.5) + 
      scale_size(range = c(3, 10))
  }
  print(g)
})

output$release_pos_xy_plot <- renderPlot({
  g <- ggplot() + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
    geom_text(aes(x=0, y=0, label="Pitcher's\nPlate", angle=270)) + 
    labs(title=as.character(input$Name), subtitle="リリースポイント(y-x：真上から見た図)") + 
    xlim(0, max(abs(Database2()$release_extension))*1.1) + 
    ylim(-max(abs(Database2()$release_pos_x))*1.1, max(abs(Database2()$release_pos_x))*1.1) + 
    theme_cowplot(16)
  
  if (input$plot_add_release_pos==1) {
    g <- g + 
      geom_point(data = database(), 
                 mapping = aes(x=release_extension, y=release_pos_x, color=pitch_name), 
                 size=.5)
  }
  
  if (input$plot_add_release_pos==2) {
    g <- g + 
      stat_ellipse(data = database(), 
                   mapping = aes(x=release_extension, y=release_pos_x, color=pitch_name, fill = pitch_name), 
                   geom="polygon", level=.8, alpha=.2)
  }
  
  if (input$plot_add_release_pos==3) {
    g <- g + 
      geom_point(data = database() %>% 
                   dplyr::mutate(n = n()) %>% 
                   dplyr::group_by(pitch_name) %>% 
                   dplyr::summarise(release_extension = mean(release_extension, na.rm=T), 
                                    release_pos_x = mean(release_pos_x, na.rm=T), 
                                    `pitch%` = n()/mean(n)*100), 
                 mapping = aes(x=release_extension, y=release_pos_x, color=pitch_name, size=`pitch%`), 
                 alpha=.5) + 
      scale_size(range = c(3, 10))
  }
  print(g)
})
  
output$release_pos_yz_plot <- renderPlot({
  g <- ggplot() + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
    geom_text(aes(x=0, y=0, label="Pitcher's\nPlate")) + 
    labs(title=as.character(input$Name), subtitle="リリースポイント(y-z：真横から見た図)") + 
    xlim(0, max(abs(Database2()$release_extension))*1.1) + 
    ylim(0, max(abs(Database2()$release_pos_z))*1.1) + 
    theme_cowplot(16)
  
  if (input$plot_add_release_pos==1) {
    g <- g + 
      geom_point(data = database(), 
                 mapping = aes(x=release_extension, y=release_pos_z, color=pitch_name), 
                 size=.5)
  }
  
  if (input$plot_add_release_pos==2) {
    g <- g + 
      stat_ellipse(data = database(), 
                   mapping = aes(x=release_extension, y=release_pos_z, color=pitch_name, fill = pitch_name), 
                   geom="polygon", level=.8, alpha=.2)
  }
  
  if (input$plot_add_release_pos==3) {
    g <- g + 
      geom_point(data = database() %>% 
                   dplyr::mutate(n = n()) %>% 
                   dplyr::group_by(pitch_name) %>% 
                   dplyr::summarise(release_extension = mean(release_extension, na.rm=T), 
                                    release_pos_z = mean(release_pos_z, na.rm=T), 
                                    `pitch%` = n()/mean(n)*100), 
                 mapping = aes(x=release_extension, y=release_pos_z, color=pitch_name, size=`pitch%`), 
                 alpha=.5) + 
      scale_size(range = c(3, 10))
  }
  print(g)
})