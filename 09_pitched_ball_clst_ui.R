tabItem(
  tabName = "pitched_ball_clst", 
  fluidRow(
    uiOutput("select_pitch_name4"), 
    column(width = 12, 
           plotOutput("pitched_ball_clst")
    )
  ), 
  fluidRow(
    sliderInput("number_clst", "分類数",
                min=1, max=10,
                value=1),
    column(width = 12, 
           plotOutput("pitched_ball_clsted", height=250)
    )
  ), 
  fluidRow(
    column(width = 12, 
           plotOutput("pitched_ball_clsted_boxplot", height=600)
    )
  )
)
