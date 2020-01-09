output$release_pos_3d_plot <- renderRglwidget({
  pitch_name <- unique(Database2()$pitch_name)
  rgl.open(useNULL=T)
  rgl.bg(color=c("white", "black"))
  par3d(windowRect = c(100, 100, 612, 612))
  plot3d(x = database()$release_pos_x, 
         y = database()$release_pos_z, 
         z = database()$release_extension, 
         col = rainbow(length(unique(Database2()$pitch_name)))[factor(Database2()$pitch_name)], 
         xlab = "マウンド", ylab = "高さ", zlab = "奥行き", 
         xlim = c(-max(abs(Database2()$release_pos_x))*1.1, max(abs(Database2()$release_pos_x))*1.1), 
         ylim = c(0, max(abs(Database2()$release_pos_z))*1.1), 
         zlim = c(0, 300))
  legend3d("topright", 
           col = rainbow(length(unique(Database2()$pitch_name))), 
           legend=pitch_name, pch=1, cex=1)
  rglwidget()
})