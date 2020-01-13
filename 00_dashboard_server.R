datatable <- reactive({
  DT <- data.frame(
    dplyr::bind_rows(
      data.frame(
        stand = "Both", 
        database() %>% 
          dplyr::group_by(pitch_name) %>% 
          dplyr::summarise(Frequency = n(), 
                           `pitch%` = as.numeric(sprintf("%.2f", Frequency/nrow(database())*100)), 
                           Velocity = as.numeric(sprintf("%.2f", mean(effective_speed, na.rm=T))), 
                           Max = as.numeric(sprintf("%.2f", max(effective_speed, na.rm=T))), 
                           Min = as.numeric(sprintf("%.2f", min(effective_speed, na.rm=T))), 
                           `Spin-Rate` = as.numeric(sprintf("%.2f", mean(release_spin_rate, na.rm=T))), 
                           `Spin-Axis` = as.numeric(sprintf("%.2f", mean(spin, na.rm=T))), 
                           `Spin-Direction/Tilt` = paste0(((540-`Spin-Axis`)%/%30)%%12, ":", formatC(round((((540-`Spin-Axis`)/30)%%1)*60, digits=0), width=2, flag="0")), 
                           `Gyro-Degree` = NA, 
                           `Release-Extension` = as.numeric(sprintf("%.2f", mean(release_extension, na.rm=T))), 
                           `Release-Side` = as.numeric(sprintf("%.2f", mean(release_pos_x, na.rm=T))), 
                           `Release-Height` = as.numeric(sprintf("%.2f", mean(release_pos_z, na.rm=T))), 
                           `Horizontal-Mov` = as.numeric(sprintf("%.2f", mean(pfx_x, na.rm=T))), 
                           `Vertical-Mov` = as.numeric(sprintf("%.2f", mean(pfx_z, na.rm=T)))) %>% 
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
                         `Spin-Axis` = as.numeric(sprintf("%.2f", mean(spin, na.rm=T))), 
                         `Spin-Direction/Tilt` = paste0(((540-`Spin-Axis`)%/%30)%%12, ":", formatC(round((((540-`Spin-Axis`)/30)%%1)*60, digits=0), width=2, flag="0")), 
                         `Gyro-Degree` = NA, 
                         `Release-Extension` = as.numeric(sprintf("%.2f", mean(release_extension, na.rm=T))), 
                         `Release-Side` = as.numeric(sprintf("%.2f", mean(release_pos_x, na.rm=T))), 
                         `Release-Height` = as.numeric(sprintf("%.2f", mean(release_pos_z, na.rm=T))), 
                         `Horizontal-Mov` = as.numeric(sprintf("%.2f", mean(pfx_x, na.rm=T))), 
                         `Vertical-Mov` = as.numeric(sprintf("%.2f", mean(pfx_z, na.rm=T)))) %>% 
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
                         `Spin-Axis` = as.numeric(sprintf("%.2f", mean(spin, na.rm=T))), 
                         `Spin-Direction/Tilt` = paste0(((540-`Spin-Axis`)%/%30)%%12, ":", formatC(round((((540-`Spin-Axis`)/30)%%1)*60, digits=0), width=2, flag="0")), 
                         `Gyro-Degree` = NA, 
                         `Release-Extension` = as.numeric(sprintf("%.2f", mean(release_extension, na.rm=T))), 
                         `Release-Side` = as.numeric(sprintf("%.2f", mean(release_pos_x, na.rm=T))), 
                         `Release-Height` = as.numeric(sprintf("%.2f", mean(release_pos_z, na.rm=T))), 
                         `Horizontal-Mov` = as.numeric(sprintf("%.2f", mean(pfx_x, na.rm=T))), 
                         `Vertical-Mov` = as.numeric(sprintf("%.2f", mean(pfx_z, na.rm=T)))) %>% 
        dplyr::arrange(desc(Frequency)) %>% 
        data.frame()
    )
  ) %>% 
    dplyr::rename(batter_stand = stand)
  colnames(DT) <- c("batter_stand", "pitch_name", "Frequency", "Pitch%", "Velocity", "Max", "Min", "Spin-Rate", 
                    "Spin-Axis", "Spin-Direction/Tilt", "Gyro-Degree", "Release-Extension", "Release-Side", 
                    "Release-Height", "Horizontal-Mov", "Vertical-Mov")
  return(DT)
})

datatable_difference_LR <- reactive({
  DT_Both <- datatable() %>% 
    dplyr::filter(batter_stand=="Both") %>% 
    dplyr::select(-batter_stand)
  DT_Left <- datatable() %>% 
    dplyr::filter(batter_stand=="L") %>% 
    dplyr::select(-batter_stand)
  DT_Right <- datatable() %>% 
    dplyr::filter(batter_stand=="R") %>% 
    dplyr::select(-batter_stand)
  
  if (input$difference_LR==0) {
    DT <- dplyr::left_join(DT_Both, DT_Left, by="pitch_name")
  } else if (input$difference_LR==1) {
    DT <- dplyr::left_join(DT_Both, DT_Right, by="pitch_name")
  } else if (input$difference_LR==2) {
    DT <- dplyr::left_join(DT_Left, DT_Right, by="pitch_name")
  }
})

output$table <- DT::renderDataTable({
  return(datatable())
})

output$difference_LR <- DT::renderDataTable({
  DT <- datatable_difference_LR() %>%
    dplyr::mutate(Frequency = as.numeric(sprintf("%.2f", Frequency.x-Frequency.y)),
                  `Pitch%` = as.numeric(sprintf("%.2f", `Pitch%.x`-`Pitch%.y`)),
                  Velocity = as.numeric(sprintf("%.2f", Velocity.x-Velocity.y)),
                  Max = as.numeric(sprintf("%.2f", Max.x-Max.y)), 
                  Min = as.numeric(sprintf("%.2f", Min.x-Min.y)), 
                  `Spin-Rate` = as.numeric(sprintf("%.2f", `Spin-Rate.x`-`Spin-Rate.y`)), 
                  `Spin-Axis` = as.numeric(sprintf("%.2f", `Spin-Axis.x`-`Spin-Axis.y`)), 
                  `Spin-Direction/Tilt` = NA, 
                  `Gyro-Degree` = NA, 
                  `Release-Extension` = as.numeric(sprintf("%.2f", `Release-Extension.x`-`Release-Extension.y`)), 
                  `Release-Side` = as.numeric(sprintf("%.2f", `Release-Side.x`-`Release-Side.y`)), 
                  `Release-Height` = as.numeric(sprintf("%.2f", `Release-Height.x`-`Release-Height.y`)), 
                  `Horizontal-Mov` = as.numeric(sprintf("%.2f", `Horizontal-Mov.x`-`Horizontal-Mov.y`)), 
                  `Vertical-Mov` = as.numeric(sprintf("%.2f", `Vertical-Mov.x`-`Vertical-Mov.y`))) %>%
    dplyr::select(pitch_name, Frequency, `Pitch%`, Velocity, Max, Min, `Spin-Rate`, `Spin-Axis`, 
                  `Spin-Direction/Tilt`, `Gyro-Degree`, `Release-Extension`, `Release-Side`, `Release-Height`,
                  `Horizontal-Mov`, `Vertical-Mov`)
  colnames(DT) <- c("pitch_name", "Frequency", "Pitch%", "Velocity", "Max", "Min", "Spin-Rate", 
                    "Spin-Axis", "Spin-Direction/Tilt", "Gyro-Degree", "Release-Extension", "Release-Side", 
                    "Release-Height", "Horizontal-Mov", "Vertical-Mov")
  
  return(DT)
})