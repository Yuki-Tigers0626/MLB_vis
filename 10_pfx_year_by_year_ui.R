tabItem(
  tabName = "year_by_year_pfx", 
  fluidRow(
    column(width = 3,
           fluidRow(
             radioButtons("plot_add_pfx2", 
                          label = h5("プロット内容"), 
                          choices = list("全変化量"=1, "変化量の80%確率楕円"=2#, "変化量の平均値"=3
                                         ),
                          selected = 1), 
             uiOutput("select_pitch_name5"), 
             uiOutput("year_by_year_range")
             )
           ), 
    column(width = 6, 
           plotOutput("pfx_year_by_year_Both_plot", height=600)
    )
  ),
  fluidRow(
    column(width = 6, 
           plotOutput("pfx_year_by_year_L_plot", height=600)
    ),
    column(width = 6, 
           plotOutput("pfx_year_by_year_R_plot", height=600)
    )
  )
)
