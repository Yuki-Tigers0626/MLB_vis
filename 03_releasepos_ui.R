tabItem(
  tabName = "release_pos", 
  fluidRow(
    column(width = 2,
           radioButtons("plot_add_release_pos", 
                       label = h5("プロット内容"), 
                       choices = list("全投球"=1, "80%確率楕円"=2, "平均値"=3), 
                       selected = 3)), 
    column(
      width = 5, 
      plotOutput(
        "release_pos_xz_plot"
      )
    )
  ), 
  fluidRow(
    column(
      width = 6, 
      plotOutput(
        "release_pos_xy_plot"
      )
    ), 
    column(
      width = 6, 
      plotOutput(
        "release_pos_yz_plot"
      )
    )
  )
)
