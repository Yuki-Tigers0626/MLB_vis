tabItem(
  tabName = "dashboard", 
  fluidRow(
    h3("投球内容"),
    column(width = 12, 
           DT::dataTableOutput("table"), 
    ), 
    h3("条件差"),
    selectInput("difference_LR", "比較内容：", 
                choices =  c("通算－対左"=0, "通算－対右"=1, "対左－対右"=2), selected = 0), 
    column(width = 12, 
           DT::dataTableOutput("difference_LR"), 
    )
  )
)
