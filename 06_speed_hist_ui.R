tabItem(
  tabName = "speed_hist", 
  selectInput("speed_hist_select", "x軸範囲：", 
              choices = c("単球種"=0, "比較"=1), selected=0), 
  uiOutput("select_pitch_name1"), 
  fluidRow(
    column(width = 6,
           plotOutput("speed_hist_plot1")
    )
  ), 
  uiOutput("select_pitch_name2"), 
  fluidRow(
    column(width = 6,
           plotOutput("speed_hist_plot2")
    )
  )
)
