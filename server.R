# サーバ
function(input, output, session) {
    # 作業用関数
    source("unit_convert.R", local=T, encoding="UTF-8")
    
    # データベース
    DataBase <- reactive({
        read_rds("Data/Statcast2018.rds")
    })
    
    Database <- reactive({
        DataBase() %>% 
            dplyr::filter(game_year==input$year) %>%
            dplyr::mutate(release_pos_x = release_pos_x*unit_convert(input$unit), 
                          release_pos_z = release_pos_z*unit_convert(input$unit), 
                          pfx_x = pfx_x*unit_convert(input$unit), 
                          pfx_z = pfx_z*unit_convert(input$unit), 
                          plate_x = plate_x*unit_convert(input$unit), 
                          plate_z = plate_z*unit_convert(input$unit), 
                          hc_x = hc_x*unit_convert(input$unit), 
                          hc_y = hc_y*unit_convert(input$unit), 
                          vx0 = vx0*unit_convert(input$unit), 
                          vy0 = vy0*unit_convert(input$unit), 
                          vz0 = vz0*unit_convert(input$unit), 
                          ax = ax*unit_convert(input$unit), 
                          ay = ay*unit_convert(input$unit), 
                          az = az*unit_convert(input$unit), 
                          sz_top = sz_top*unit_convert(input$unit), 
                          sz_bot = sz_bot*unit_convert(input$unit), 
                          hit_distance_sc = hit_distance_sc*unit_convert(input$unit), 
                          release_extension = release_extension*unit_convert(input$unit), 
                          release_pos_y = release_pos_y*unit_convert(input$unit))
    })
    
    PlayerNames <- reactive({
        DataBase() %>% 
            dplyr::select(pitcher) %>% 
            dplyr::rename(key_mlbam = pitcher) %>% 
            dplyr::distinct(key_mlbam) %>% 
            dplyr::left_join(read_rds("Data/PlayerNames.rds"), 
                             by = "key_mlbam") %>% 
            dplyr::arrange(Name)
    })
    
    output$selectPlayer <- renderUI({
        selectInput(inputId = "Name", 
                    label = "Choose: ", 
                    choices = PlayerNames() %>% 
                        # dplyr::filter(key_mlbam%in%c(506433, 547888)) %>%
                        dplyr::select(Name))
    })
    
    player_Name <- reactive({
        PlayerNames() %>% 
            dplyr::filter(Name==input$Name) %>% 
            dplyr::select(Name) %>% as.character()
    })
    
    player_Id <- reactive({
        PlayerNames() %>% 
            dplyr::filter(Name==input$Name) %>% 
            dplyr::select(key_mlbam) %>% as.integer()
    })
    
    database <- reactive({
        Database() %>% 
            dplyr::filter(pitcher==player_Id(), pitch_name!="")
    })
    
    source("00_dashboard_server.R", local=T, encoding="UTF-8") 
    source("01_typespeed_server.R", local=T, encoding="UTF-8") 
    source("02_speedspin_server.R", local=T, encoding="UTF-8") 
    source("03_releasepos_server.R", local=T, encoding="UTF-8") 
    source("04_releasepos3d_server.R", local=T, encoding="UTF-8") 
    source("05_pfx_server.R", local=T, encoding="UTF-8") 
}
