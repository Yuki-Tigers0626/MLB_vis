# サーバ
function(input, output, session) {
    # データベース
    Database <- reactive({
        read_rds("../../BaseballR/Statcast2018.rds")
    })
    
    PlayerNames <- reactive({
        read_rds("../../BaseballR/PlayerNames.rds") %>% 
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
