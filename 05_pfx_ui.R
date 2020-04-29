tabItem(
  tabName = "pfx", 
  fluidRow(
    column(width = 3,
           radioButtons("plot_add_pfx", 
                        label = h5("プロット内容"), 
                        choices = list("全変化量"=1, "変化量の80%確率楕円"=2, "変化量の平均値"=3), 
                        selected = 1))
    ), 
  fluidRow(
    h3("all"), 
    column(width = 4, 
           plotOutput("pfx_Both_plot", height=400)
    ),
    column(width = 4, 
           plotOutput("pfx_L_plot", height=400)
    ),
    column(width = 4, 
           plotOutput("pfx_R_plot", height=400)
    )
  ), 
  fluidRow(
    h3("inning_top"), 
    column(width = 4, 
           plotOutput("pfx_Both_plot_top", height=400)
    ),
    column(width = 4, 
           plotOutput("pfx_L_plot_top", height=400)
    ),
    column(width = 4, 
           plotOutput("pfx_R_plot_top", height=400)
    )
  ), 
  fluidRow(
    h3("inning_bot"), 
    column(width = 4, 
           plotOutput("pfx_Both_plot_bot", height=400)
    ),
    column(width = 4, 
           plotOutput("pfx_L_plot_bot", height=400)
    ),
    column(width = 4, 
           plotOutput("pfx_R_plot_bot", height=400)
    )
  )
)
