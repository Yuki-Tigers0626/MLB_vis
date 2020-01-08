output$pfx_L_plot <- renderPlot({
  g <- database() %>% 
    dplyr::filter(stand=="L") %>% 
    ggplot(aes(x=pfx_x, y=pfx_z, color=pitch_name)) + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
    geom_point(size=.5) + 
    stat_ellipse(aes(fill = pitch_name), geom="polygon", level=.95, alpha=.2) + 
    labs(title=as.character(player_Name()), subtitle="対左変化量プロット") + 
    xlim(min(database()$pfx_x), max(database()$pfx_x)) + 
    ylim(min(database()$pfx_z), max(database()$pfx_z))
  print(g)
})

output$pfx_R_plot <- renderPlot({
  g <- database() %>% 
    dplyr::filter(stand=="R") %>% 
    ggplot(aes(x=pfx_x, y=pfx_z, color=pitch_name)) + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
    geom_point(size=.5) + 
    stat_ellipse(aes(fill = pitch_name), geom="polygon", level=.95, alpha=.2) + 
    labs(title=as.character(player_Name()), subtitle="対右変化量プロット") + 
    xlim(min(database()$pfx_x), max(database()$pfx_x)) + 
    ylim(min(database()$pfx_z), max(database()$pfx_z))
  print(g)
})