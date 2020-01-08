output$release_pos_3d_plot <- renderRglwidget({
  rgl.open(useNULL=T)
  rgl.bg(color=c("white", "black"))
  plot3d(x = database()$release_pos_x, 
         y = database()$release_pos_z, 
         z = database()$release_extension, 
         col = rainbow(length(unique(database()$pitch_name)))[factor(database()$pitch_name)], 
         xlab = "release_pos_x", ylab = "release_pos_z", zlab = "release_extension")
  rglwidget()
})