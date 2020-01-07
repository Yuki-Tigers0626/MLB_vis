output$speed_spin_plot <- renderPlot({
  g <- database() %>% 
    ggplot(aes(x=release_speed, y=release_spin_rate, color=pitch_name)) + 
    geom_point(size=.5) + 
    stat_ellipse(aes(fill = pitch_name), geom="polygon", level=.95, alpha=.2) + 
    labs(title=player_Name(), subtitle="球速と回転数のプロット") 
  ggMarginal(g, type="density", margins="both", size=5, groupFill=T)
})