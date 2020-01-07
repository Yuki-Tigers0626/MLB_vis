output$type_speed_plot <- renderPlot({
  g <- database() %>%
    dplyr::select(pitch_name, release_speed, effective_speed) %>%
    tidyr::gather(key="type_speed", value="speed(mph)", release_speed, effective_speed) %>%
    ggplot(aes(x=pitch_name, y=`speed(mph)`, fill=type_speed)) +
    geom_violin(draw_quantiles=c(.25, .5, .75)) +
    labs(title=player_Name(), subtitle="リリース速度と体感速度の箱ひげ")
  print(g)
})