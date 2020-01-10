tabItem(
  tabName = "command_plot", 
  fluidRow(
    h5("計算に時間がかかります"), 
    uiOutput("select_pitch_name"), 
    column(width = 4, 
           plotOutput("cs_prob_Both")
    ), 
    column(width = 4, 
           plotOutput("cs_prob_Left")
    ), 
    column(width = 4, 
           plotOutput("cs_prob_Right")
    )
  ), 
  fluidRow(
    column(width = 4, 
           plotOutput("cs_plot_Both")
    ), 
    column(width = 4, 
           plotOutput("cs_plot_Left")
    ), 
    column(width = 4, 
           plotOutput("cs_plot_Right")
    )
  )
)
