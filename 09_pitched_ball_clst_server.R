output$select_pitch_name4 <- renderUI({
  checkboxGroupInput(inputId = "pitch_name4", 
                     label = "球種選択: ", 
                     choices = database() %>% 
                       dplyr::select(pitch_name) %>% 
                       dplyr::distinct(pitch_name) %>% 
                       as.matrix() %>% 
                       purrr::set_names(), 
                     inline=T)
})

database2 <- reactive({
  data.frame(fav = database() %>% 
                     dplyr::filter(pitch_name=="4-Seam Fastball") %>% 
                     dplyr::group_by() %>% 
                     dplyr::summarise(fav = max(release_speed, na.rm=T)) %>% 
                     dplyr::select(fav) %>% as.numeric(), 
                   database()) %>% 
    dplyr::filter(pitch_name%in%input$pitch_name4) %>% 
    dplyr::mutate(`v/fav` = release_speed/fav) %>% 
    dplyr::group_by(game_date, inning) %>% 
    dplyr::mutate(rownum = row_number()) %>% ungroup() %>%  
    dplyr::mutate(rowname = paste0(game_date, "_", inning, "_", rownum))
})

database2_clst <- reactive({
  hclust(database2() %>% 
           dplyr::select(`v/fav`, release_spin_rate, release_pos_x, release_pos_z, spin, pfx_x, pfx_z, release_extension) %>% 
           dplyr::mutate_all(scale) %>% 
           dist(), method="ward.D2")
})

output$pitched_ball_clst <- renderPlot({
  plot(database2_clst(), hang=-1, labels=database2()$rowname, xlab="")
  if (input$number_clst!=1) {
    rect.hclust(database2_clst(), k=input$number_clst, border="red")
  }
})

database2_clsted <- reactive({
  data.frame(clst = cutree(database2_clst(), k=input$number_clst), 
             database2())
})

output$pitched_ball_clsted <- renderPlot({
  g1 <- database2_clsted() %>% 
    ggplot() + 
    geom_point(aes(x=release_spin_rate, y=spin, color=factor(clst))) + 
    labs(title="回転数と回転軸", color="clst")
  
  g2 <- database2_clsted() %>% 
    ggplot() + 
    geom_histogram(aes(x=effective_speed, fill=factor(clst)), binwidth=.2, position="stack") + 
    labs(title="球速", fill="clst")
  
  g3 <- database2_clsted() %>% 
    ggplot() + 
    geom_point(aes(x=plate_x, y=plate_z, color=factor(clst))) + 
    geom_hline(yintercept=0) + 
    labs(title="投球座標", color="clst")
  
  g4 <- database2_clsted() %>% 
    ggplot() + 
    geom_point(aes(x=pfx_x, y=pfx_z, color=factor(clst))) + 
    geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
    labs(title="変化量", color="clst")
  
  g5 <- database2_clsted() %>% 
    ggplot() + 
    geom_point(aes(x=release_pos_x, y=release_pos_z, color=factor(clst))) + 
    labs(title="リリースポイント", color="clst")
  
  print(gridExtra::grid.arrange(g1, g2, g3, g4, g5, nrow=1))
})

output$pitched_ball_clsted_boxplot <- renderPlot({
  g1 <- database2_clsted() %>% 
    ggplot() + 
    geom_boxplot(aes(x=clst, y=effective_speed, group=factor(clst), fill=factor(clst))) + 
    labs(title="球速", fill="clst", group="clst")
  
  g2 <- database2_clsted() %>% 
    ggplot() + 
    geom_boxplot(aes(x=clst, y=release_spin_rate, group=factor(clst), fill=factor(clst))) + 
    labs(title="回転数", fill="clst", group="clst")
  
  g3 <- database2_clsted() %>% 
    ggplot() + 
    geom_boxplot(aes(x=clst, y=spin, group=factor(clst), fill=factor(clst))) + 
    labs(title="回転軸", fill="clst", group="clst")
  
  g4 <- database2_clsted() %>% 
    ggplot() + 
    geom_boxplot(aes(x=clst, y=release_pos_x, group=factor(clst), fill=factor(clst))) + 
    labs(title="リリース横", fill="clst", group="clst")
  
  g5 <- database2_clsted() %>% 
    ggplot() + 
    geom_boxplot(aes(x=clst, y=release_pos_z, group=factor(clst), fill=factor(clst))) + 
    labs(title="リリース縦", fill="clst", group="clst")
  
  g6 <- database2_clsted() %>% 
    ggplot() + 
    geom_boxplot(aes(x=clst, y=release_extension, group=factor(clst), fill=factor(clst))) + 
    labs(title="リリース前", fill="clst", group="clst")
  
  g7 <- database2_clsted() %>% 
    ggplot() + 
    geom_boxplot(aes(x=clst, y=pfx_x, group=factor(clst), fill=factor(clst))) + 
    labs(title="横変化量", fill="clst", group="clst")
  
  g8 <- database2_clsted() %>% 
    ggplot() + 
    geom_boxplot(aes(x=clst, y=pfx_z, group=factor(clst), fill=factor(clst))) + 
    labs(title="縦変化量", fill="clst", group="clst")
  
  g9 <- database2_clsted() %>% 
    ggplot() + 
    geom_boxplot(aes(x=clst, y=game_date, group=factor(clst), fill=factor(clst))) + 
    coord_flip() + 
    labs(title="投球時期", fill="clst", group="clst")
  
  print(gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, ncol=3))
})