output$table <- DT::renderDataTable({
  DT <- data.frame(
    dplyr::bind_rows(
      data.frame(
        stand = "both", 
        database() %>% 
          dplyr::group_by(pitch_name) %>% 
          dplyr::summarise(Frequency = n(), 
                           `pitch%` = as.numeric(sprintf("%.2f", Frequency/nrow(database())*100)), 
                           Velocity = as.numeric(sprintf("%.2f", mean(effective_speed, na.rm=T))), 
                           Max = as.numeric(sprintf("%.2f", max(effective_speed, na.rm=T))), 
                           Min = as.numeric(sprintf("%.2f", min(effective_speed, na.rm=T))), 
                           `Spin-Rate` = as.numeric(sprintf("%.2f", mean(release_spin_rate, na.rm=T))), 
                           `Spin-Direction/Tilt` = NA, 
                           `Gyro-Degree` = NA, 
                           `Release-Extension` = as.numeric(sprintf("%.2f", mean(release_pos_y, na.rm=T))), 
                           `Release-Side` = as.numeric(sprintf("%.2f", mean(release_pos_x, na.rm=T))), 
                           `Release-Height` = as.numeric(sprintf("%.2f", mean(release_pos_z, na.rm=T)))) %>% 
          dplyr::arrange(desc(Frequency))
      ), 
      database() %>% 
        dplyr::filter(stand=="L") %>% 
        dplyr::group_by(stand, pitch_name) %>% 
        dplyr::summarise(Frequency = n(), 
                         `pitch%` = as.numeric(sprintf("%.2f", Frequency/nrow(database() %>% 
                                                                                dplyr::filter(stand=="L"))*100)), 
                         Velocity = as.numeric(sprintf("%.2f", mean(effective_speed, na.rm=T))), 
                         Max = as.numeric(sprintf("%.2f", max(effective_speed, na.rm=T))), 
                         Min = as.numeric(sprintf("%.2f", min(effective_speed, na.rm=T))), 
                         `Spin-Rate` = as.numeric(sprintf("%.2f", mean(release_spin_rate, na.rm=T))), 
                         `Spin-Direction/Tilt` = NA, 
                         `Gyro-Degree` = NA, 
                         `Release-Extension` = as.numeric(sprintf("%.2f", mean(release_pos_y, na.rm=T))), 
                         `Release-Side` = as.numeric(sprintf("%.2f", mean(release_pos_x, na.rm=T))), 
                         `Release-Height` = as.numeric(sprintf("%.2f", mean(release_pos_z, na.rm=T)))) %>% 
        dplyr::arrange(desc(Frequency)) %>% 
        data.frame(), 
      database() %>% 
        dplyr::filter(stand=="R") %>% 
        dplyr::group_by(stand, pitch_name) %>% 
        dplyr::summarise(Frequency = n(), 
                         `pitch%` = as.numeric(sprintf("%.2f", Frequency/nrow(database() %>% 
                                                                                dplyr::filter(stand=="R"))*100)), 
                         Velocity = as.numeric(sprintf("%.2f", mean(effective_speed, na.rm=T))), 
                         Max = as.numeric(sprintf("%.2f", max(effective_speed, na.rm=T))), 
                         Min = as.numeric(sprintf("%.2f", min(effective_speed, na.rm=T))), 
                         `Spin-Rate` = as.numeric(sprintf("%.2f", mean(release_spin_rate, na.rm=T))), 
                         `Spin-Direction/Tilt` = NA, 
                         `Gyro-Degree` = NA, 
                         `Release-Extension` = as.numeric(sprintf("%.2f", mean(release_pos_y, na.rm=T))), 
                         `Release-Side` = as.numeric(sprintf("%.2f", mean(release_pos_x, na.rm=T))), 
                         `Release-Height` = as.numeric(sprintf("%.2f", mean(release_pos_z, na.rm=T)))) %>% 
        dplyr::arrange(desc(Frequency)) %>% 
        data.frame()
    )
  ) %>% 
  dplyr::rename(batter_stand = stand)
  colnames(DT) <- c("batter_stand", "pitch_name", "Frequency", "Pitch%", "Velocity", "Max", "Min", "Spin-Rate", 
                    "Spin-Direction/Tilt", "Gyro-Degree", "Release-Extension", "Release-Side", "Release-Height")
  return(DT)
})