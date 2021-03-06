

# サーバ
function(input, output, session) {
    # 作業用関数
    source("unit_convert.R", local=T, encoding="UTF-8")
    
    # データベース
    DataBase <- reactive({
        read_rds("Data/StatcastData.rds") %>% 
            dplyr::rename(pitch_name1=pitch_name)
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
                    label = "投手選択: ", 
                    choices = PlayerNames() %>% 
                        dplyr::select(Name), 
                    selected = "Yu Darvish")
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
    
    Database <- reactive({
        db <- DataBase() %>% 
            dplyr::filter(pitcher==player_Id()) %>%
            dplyr::mutate(description = as.character(description), 
                          release_pos_x = release_pos_x*unit_convert(input$unit)*as.integer(input$view), 
                          release_pos_z = release_pos_z*unit_convert(input$unit), 
                          pfx_x = pfx_x*unit_convert(input$unit)*as.integer(input$view), 
                          pfx_z = pfx_z*unit_convert(input$unit), 
                          plate_x = plate_x*unit_convert(input$unit)*as.integer(input$view), 
                          plate_z = plate_z*unit_convert(input$unit), 
                          hc_x = hc_x*unit_convert(input$unit), 
                          hc_y = hc_y*unit_convert(input$unit), 
                          vx0 = vx0*unit_convert(input$unit)*as.integer(input$view), 
                          vy0 = vy0*unit_convert(input$unit), 
                          vz0 = vz0*unit_convert(input$unit), 
                          ax = ax*unit_convert(input$unit)*as.integer(input$view), 
                          ay = ay*unit_convert(input$unit), 
                          az = az*unit_convert(input$unit), 
                          sz_top = sz_top*unit_convert(input$unit)*as.integer(input$view), 
                          sz_bot = sz_bot*unit_convert(input$unit), 
                          hit_distance_sc = hit_distance_sc*unit_convert(input$unit), 
                          release_extension = release_extension*unit_convert(input$unit), 
                          release_pos_y = release_pos_y*unit_convert(input$unit), 
                          spin = spin*as.integer(input$view), 
                          `Active-Spin` = as.numeric(`Active-Spin`))
        if (input$pitch_name_select==2) {
            db <- db %>% 
                dplyr::mutate(pitch_name = pitch_name2)
        } else if (input$pitch_name_select==1) {
            db <- db %>% 
                dplyr::mutate(pitch_name = pitch_name1)
        }
    })
    
    output$selectYear <- renderUI({
        selectInput(inputId = "Year", 
                    label = "年度選択: ", 
                    choices = Database() %>% 
                        dplyr::select(game_year) %>% 
                        dplyr::distinct() %>% 
                        dplyr::arrange(desc(game_year)))
    })
    
    Database2 <- reactive({
        Database() %>% 
            dplyr::filter(game_year==as.integer(input$Year), 
                          pitch_name!="")
    })
    
    Date_Range_max <- reactive({
        Database2() %>% 
            dplyr::select(game_date) %>% 
            dplyr::group_by() %>% 
            dplyr::summarise(max(game_date)) %>% 
            as.integer()
    })
    
    Date_Range_min <- reactive({
        Database2() %>% 
            dplyr::select(game_date) %>% 
            dplyr::group_by() %>% 
            dplyr::summarise(min(game_date)) %>% 
            as.integer()
    })
    
    output$DateRange <- renderUI({
        dateRangeInput(inputId = "date_range", 
                       label = "期間選択: ", 
                       start =  as.Date(Date_Range_min(), origin="1970-01-01"), 
                       end = as.Date(Date_Range_max(), origin="1970-01-01"), 
                       min = as.Date(Date_Range_min(), origin="1970-01-01"), 
                       max = as.Date(Date_Range_max(), origin="1970-01-01"))
    })
    
    database <- reactive({
        db <- Database2()
        
        if (input$Year>=2015) {
            db <- db %>% 
                dplyr::filter(outs_when_up%in%input$outcount)
        }
        
        db <- db %>% 
            dplyr::mutate(tmp = as.integer(game_date)) %>% 
            dplyr::filter(tmp>=as.integer(input$date_range[1]), 
                          tmp<=as.integer(input$date_range[2])) %>% 
            dplyr::select(-tmp)
        
        if (input$runner==0) {
            db <- db
        } else if (input$runner==1) {
            db <- db %>% 
                dplyr::filter(is.na(on_1b), is.na(on_2b), is.na(on_3b))
        } else if (input$runner==2) {
            db <- db %>% 
                dplyr::filter(!is.na(on_1b), is.na(on_2b), is.na(on_3b))
        } else if (input$runner==3) {
            db <- db %>% 
                dplyr::filter(!is.na(on_2b)|!is.na(on_3b))
        } else if (input$runner==4) {
            db <- db %>% 
                dplyr::filter(!is.na(on_1b))
        } else if (input$runner==5) {
            db <- db %>% 
                dplyr::filter(!is.na(on_2b))
        } else if (input$runner==6) {
            db <- db %>% 
                dplyr::filter(!is.na(on_3b))
        } else if (input$runner==7) {
            db <- db %>% 
                dplyr::filter(is.na(on_1b), !is.na(on_2b), is.na(on_3b))
        } else if (input$runner==8) {
            db <- db %>% 
                dplyr::filter(is.na(on_1b), is.na(on_2b), !is.na(on_3b))
        } else if (input$runner==9) {
            db <- db %>% 
                dplyr::filter(!is.na(on_1b), !is.na(on_2b), is.na(on_3b))
        } else if (input$runner==10) {
            db <- db %>% 
                dplyr::filter(!is.na(on_1b), is.na(on_2b), !is.na(on_3b))
        } else if (input$runner==11) {
            db <- db %>% 
                dplyr::filter(is.na(on_1b), !is.na(on_2b), !is.na(on_3b))
        } else if (input$runner==12) {
            db <- db %>% 
                dplyr::filter(!is.na(on_1b), !is.na(on_2b), !is.na(on_3b))
        }
        
        return(db)
    })
    
    source("00_dashboard_server.R", local=T, encoding="UTF-8")
    source("01_typespeed_server.R", local=T, encoding="UTF-8")
    source("02_speedspin_server.R", local=T, encoding="UTF-8")
    source("03_releasepos_server.R", local=T, encoding="UTF-8")
    source("04_releasepos3d_server.R", local=T, encoding="UTF-8")
    source("05_pfx_server.R", local=T, encoding="UTF-8")
    source("06_speed_hist_server.R", local=T, encoding="UTF-8")
    source("07_command_server.R", local=T, encoding="UTF-8")
    source("08_launch_angle_server.R", local=T, encoding="UTF-8")
    source("09_pitched_ball_clst_server.R", local=T, encoding="UTF-8")
    source("10_pfx_year_by_year_server.R", local=T, encoding="UTF-8")
}