output$speed_spin_plot <- renderPlot({
  g <- database() %>% 
    ggplot(aes(x=release_speed, y=release_spin_rate, color=pitch_name)) + 
    geom_point(size=.5) + 
    stat_ellipse(aes(fill = pitch_name), geom="polygon", level=.95, alpha=.2) + 
    labs(title=player_Name(), subtitle="球速と回転数のプロット") + 
    scale_color_discrete(guide=FALSE) + 
    xlim(min(Database2()$release_speed)*1.1, max(Database2()$release_speed)*1.1) + 
    ylim(min(Database2()$release_spin_rate)*1.1, max(Database2()$release_spin_rate)*1.1)
  ggMarginal(g, type="density", margins="both", size=5, groupFill=T)
})