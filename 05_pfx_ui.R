tabItem(
  tabName = "pfx", 
  fluidRow(
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
