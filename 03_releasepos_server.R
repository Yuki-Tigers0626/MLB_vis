output$release_pos_xz_plot <- renderPlot({
  g <- database() %>% 
    ggplot(aes(x=release_pos_x, y=release_pos_z, color=pitch_name)) + 
    stat_ellipse(aes(fill = pitch_name), geom="polygon", level=.95, alpha=.2) + 
    geom_point() + 
    labs(title=player_Name(), subtitle="リリースポイント(x-z)") 
  ggMarginal(g, type="density", margins="both", size=5, groupFill=T)
})

output$release_pos_xy_plot <- renderPlot({
  g <- database() %>% 
    ggplot(aes(x=release_pos_x, y=release_pos_y, color=pitch_name)) + 
    stat_ellipse(aes(fill = pitch_name), geom="polygon", level=.95, alpha=.2) + 
    geom_point() + 
    labs(title=player_Name(), subtitle="リリースポイント(x_y)") 
  ggMarginal(g, type="density", margins="both", size=5, groupFill=T)
})
  
output$release_pos_yz_plot <- renderPlot({
  g <- database() %>% 
    ggplot(aes(x=release_pos_z, y=release_pos_y, color=pitch_name)) + 
    stat_ellipse(aes(fill = pitch_name), geom="polygon", level=.95, alpha=.2) + 
    geom_point() + 
    labs(title=player_Name(), subtitle="リリースポイント(y-z)") 
  ggMarginal(g, type="density", margins="both", size=5, groupFill=T)
})