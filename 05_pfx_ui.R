tabItem(
  tabName = "pfx", 
  fluidRow(
    column(width = 3,
           radioButtons("plot_add_pfx", 
                        label = h5("プロット内容"), 
                        choices = list("全変化量"=1, "変化量の80%確率楕円"=2, "変化量の平均値・投球率"=3), 
                        selected = 1)), 
    column(width = 6, 
           plotOutput("pfx_Both_plot", height=600)
    )
  ),
  fluidRow(
    column(width = 6, 
           plotOutput("pfx_L_plot", height=600)
    ),
    column(width = 6, 
           plotOutput("pfx_R_plot", height=600)
    )
  )
)
