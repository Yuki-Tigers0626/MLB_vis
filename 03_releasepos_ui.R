tabItem(
  tabName = "release_pos",
  fluidRow(
    column(
      width = 12, 
      plotOutput(
        "release_pos_xz_plot"
      )
    ), 
    column(
      width = 12, 
      plotOutput(
        "release_pos_xy_plot"
      )
    ), 
    column(
      width = 12, 
      plotOutput(
        "release_pos_yz_plot"
      )
    )
  )
)
