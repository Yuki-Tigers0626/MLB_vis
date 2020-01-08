require("data.table")
require("DT")
require("ggExtra")
require("rgl")
require("shiny")
require("shinydashboard")
require("tidyverse")

# https://github.com/Tychobra/shiny-insurance-examples/tree/master/basic-insurer-dashboard

# ヘッダ
header <- dashboardHeader(
    title = "Pitcher Visualize"
)

# サイドバー
# https://fontawesome.com/icons?d=gallery
sidebar <- dashboardSidebar(
    uiOutput("selectPlayer"), 
    radioButtons("unit", "長さ単位：", 
                 choices =  c("cm", "ft"), selected = "cm"), 
    radioButtons("year", "年度：", 
                 choices =  c(2018, 2019), selected = 2018), 
    sidebarMenu(
        menuItem("Dashboard", tabName="dashboard", icon=icon("baseball-ball")), 
        menuItem("リリース速度と体感速度", tabName="type_speed", icon=icon("dashboard")), 
        menuItem("球速と回転数", tabName="speed_spin", icon=icon("bullseye")), 
        menuItem("リリースポイント", tabName="release_pos", icon=icon("hand-peace")), 
        menuItem("リリースポイント(3D)", tabName="release_pos_3d", icon=icon("hand-lizard")), 
        menuItem("変化量", tabName="pfx", icon=icon("arrows-alt"))
    )
)

# ボディ
body <- dashboardBody(
    tabItems(
        source("00_dashboard_ui.R", local=T, encoding="UTF-8")$value, 
        source("01_typespeed_ui.R", local=T, encoding="UTF-8")$value, 
        source("02_speedspin_ui.R", local=T, encoding="UTF-8")$value, 
        source("03_releasepos_ui.R", local=T, encoding="UTF-8")$value, 
        source("04_releasepos3d_ui.R", local=T, encoding="UTF-8")$value, 
        source("05_pfx_ui.R", local=T, encoding="UTF-8")$value
    )
)

dashboardPage(
    header, 
    sidebar, 
    body, 
    skin="black" 
)