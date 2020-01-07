tabItem(
  tabName = "pfx", 
  fluidRow(
    column(width = 6, 
           plotOutput("pfx_L_plot", height=800)
    ),
    column(width = 6, 
           plotOutput("pfx_R_plot", height=800)
    )
  )
)
