output$launch_angle_plot <- renderPlot({
  launch_angle_group_df <- data.frame(launch_angle_group=c(9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2, -3, -4, -5, -6, -7, -8, -9), 
                                      打球角度=c("[90 ,80)", "[80, 70)", "[70, 60)", "[60, 50)", "[50, 40)", 
                                             "[40, 30)", "[30, 20)", "[20, 10)", "[10, 1]", "(1, -1)", 
                                             "[-1, -10]", "(-10, -20]", "(-20, -30]", "(-30, -40]", "(-40, -50]", 
                                             "(-50, -60]", "(-60, -70]", "(-70, -80]", "(-80, -90]"), 
                                      color=c("white","white","white","white","white","orange","red","orange","white","white",
                                              "white","white","white","white","white","white","white","white","white"))
  
  
  
  db <- database() %>% 
    dplyr::filter(!is.na(launch_angle)) %>% 
    dplyr::mutate(launch_angle_group = as.integer(ifelse(abs(launch_angle)==90, 8*launch_angle/abs(launch_angle), launch_angle%/%10+1)), 
                  launch_angle_group = as.integer(ifelse(abs(launch_angle)<1, 0, launch_angle_group))) %>% 
    dplyr::left_join(launch_angle_group_df, by="launch_angle_group") %>% 
    dplyr::group_by(pitch_name) %>% 
    dplyr::mutate(n = n()) %>% ungroup() %>% 
    dplyr::group_by(pitch_name, 打球角度) %>% 
    dplyr::summarise(prob = n()/mean(n)*100)
  
  df <- launch_angle_group_df %>% 
    dplyr::filter(打球角度%in%unique(db$打球角度))
  
  db %>% 
    ggplot(aes(x=pitch_name, y=prob)) + 
    geom_bar(aes(fill=打球角度), 
             color="black", position="stack", stat="identity") +
    scale_y_reverse() + 
    scale_fill_manual(breaks=df$打球角度, labels=df$打球角度, 
                      values=as.vector(rev(df$color))) + 
    labs(title=paste0(as.character(player_Name()), " ", input$year, "年度 球種別打球角度")) + 
    theme_cowplot(16)
})

output$launch_speed_plot <- renderPlot({
  launch_speed_group_df <- data.frame(launch_speed_group=c(13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0), 
                                      打球初速度=c("(140, 130]", "(130, 120]", "(120, 110]", "(110, 100]", "(100, 90]", 
                                              "(90 ,80]", "(80, 70]", "(70, 60]", "(60, 50]", "(50, 40]", 
                                              "(40, 30]", "(30, 20]", "(20, 10]", "(10, 0]"))
  
  db <- database() %>% 
    dplyr::filter(!is.na(launch_speed)) %>% 
    dplyr::mutate(launch_speed_group = launch_speed%/%10) %>% 
    dplyr::left_join(launch_speed_group_df, by="launch_speed_group") %>% 
    dplyr::group_by(pitch_name) %>% 
    dplyr::mutate(n = n()) %>% ungroup() %>% 
    dplyr::group_by(pitch_name, launch_speed_group) %>% 
    dplyr::summarise(prob = n()/mean(n)*100)
  
  df <- launch_speed_group_df %>% 
    dplyr::filter(launch_speed_group%in%unique(db$launch_speed_group))
  
  db %>% 
    ggplot(aes(x=pitch_name, y=prob)) + 
    geom_bar(aes(fill=factor(launch_speed_group)), 
             color="black", position="stack", stat="identity") + 
    scale_y_reverse() + 
    scale_fill_discrete(name="打球初速度", breaks=df$launch_speed_group, labels=df$打球初速度) + 
    labs(title=paste0(as.character(player_Name()), " ", input$year, "年度 球種別打球初速度")) + 
    theme_cowplot(16)
})

output$launch_angle_speed_plot <- renderPlot({
  database() %>% 
    dplyr::filter(!is.na(launch_angle)&!is.na(launch_speed)) %>% 
    ggplot(aes(x=launch_speed, y=launch_angle, color=pitch_name)) + 
    geom_point(size=2, alpha=.8) + 
    geom_hline(yintercept=0) + geom_vline(xintercept=mean(Database()$launch_speed, na.rm=T)) + 
    xlim(min(Database()$launch_speed, na.rm=T), max(Database()$launch_speed, na.rm=T)) + 
    ylim(min(Database()$launch_angle, na.rm=T), max(Database()$launch_angle, na.rm=T)) + 
    labs(title=paste0(as.character(player_Name()), " ", input$year, "年度 球種別打球角度・初速度")) + 
    theme_cowplot(16)
})