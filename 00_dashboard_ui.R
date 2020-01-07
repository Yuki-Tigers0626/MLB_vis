tabItem(
  tabName = "dashboard", 
  fluidRow(
    column(width = 12, 
           DT::dataTableOutput("table")
    )
  )
)
