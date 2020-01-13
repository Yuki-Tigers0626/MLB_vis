# 必要なパッケージの確認・インストール
RequiredPackages <- c("cowplot", "data.table", "DT", "ggExtra", 
                      "mgcv", "rgl", "shiny", "shinydashboard", "tidyverse") 
newPackages <- RequiredPackages[!(RequiredPackages%in%installed.packages()[,"Package"])]
if (length(newPackages)) {
    install.packages(newPackages, repos = "http://cran.us.r-project.org")
    for (package in targetPackages) {
        library(package, character.only = T)
    }
}

require("cowplot")
require("data.table")
require("DT")
require("ggExtra")
require("mgcv")
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
    selectInput("unit", "長さ単位：", 
                choices = c("cm", "ft"), selected = "cm"), 
    selectInput("year", "年度：", 
                choices = c("2018"=2018, "2019"=2019), selected = 2019), 
    uiOutput("DateRange"), 
    checkboxGroupInput("outcount", "アウトカウント：", 
                       choices = c("0out"=0, "1out"=1, "2out"=2), 
                       selected = c(0, 1, 2), 
                       inline=T), 
    selectInput("runner", "塁状況：", 
                choices = c("全状況"=0, 
                            "走者なし"=1, "一塁"=2, "得点圏"=3, 
                            "一塁存在"=4, "二塁存在"=5, "三塁存在"=6, 
                            "二塁"=7, "三塁"=8, "一二塁"=9, "一三塁"=10, "二三塁"=11, "満塁"=12), 
                selected = 0), 
    sidebarMenu(
        menuItem("Dashboard", tabName="dashboard", icon=icon("baseball-ball")), 
        menuItem("リリース速度と体感速度", tabName="type_speed", icon=icon("dashboard")), 
        menuItem("球速と回転数", tabName="speed_spin", icon=icon("arrow-circle-down")), 
        menuItem("リリースポイント", tabName="release_pos", icon=icon("hand-peace")), 
        menuItem("リリースポイント(3D)", tabName="release_pos_3d", icon=icon("hand-lizard")), 
        menuItem("変化量", tabName="pfx", icon=icon("arrows-alt")), 
        menuItem("球速ヒストグラム", tabName="speed_hist", icon=icon("chart-bar")), 
        menuItem("コマンド可視化", tabName="command_plot", icon=icon("bullseye")), 
        menuItem("球種別打球角度・初速度", tabName="launch_angle_plot", icon=icon("chart-line"))
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
        source("05_pfx_ui.R", local=T, encoding="UTF-8")$value, 
        source("06_speed_hist_ui.R", local=T, encoding="UTF-8")$value, 
        source("07_command_ui.R", local=T, encoding="UTF-8")$value, 
        source("08_launch_angle_ui.R", local=T, encoding="UTF-8")$value
    )
)

dashboardPage(
    header, 
    sidebar, 
    body, 
    skin="black" 
)