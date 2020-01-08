tabItem(
  tabName = "release_pos", 
  fluidRow(
    column(width = 3,
           radioButtons("plot_add_release_pos", 
                       label = h5("プロット内容"), 
                       choices = list("全リリース位置"=1, "リリース位置の80%確率楕円"=2, "リリース位置の平均値・投球率"=3), 
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
