tabItem(
  tabName = "launch_angle_plot", 
  fluidRow(
    column(width = 12, 
           plotOutput("launch_angle_plot")
    )
  ), 
  fluidRow(
    column(width = 12, 
           plotOutput("launch_speed_plot")
    )
  ), 
  fluidRow(
    column(width = 5, 
           plotOutput("launch_angle_speed_plot")
    )
  )
)
