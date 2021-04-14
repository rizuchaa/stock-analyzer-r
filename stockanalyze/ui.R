head<-dashboardHeader(title="Stock Analyzer")

side<-dashboardSidebar(disable = TRUE)

bod<-dashboardBody(
  fluidRow(
    valueBoxOutput("vbox"),
    valueBoxOutput("vbox2"),
    valueBoxOutput("vbox3")
  ),
  fluidRow(
    column(width = 12,
           plotlyOutput("candle"),
           style='padding:20px;'
           )
  ),
  fluidRow(
    column(width=12,
    dataTableOutput("table"))
  )
)
  
dashboardPage(
  header=head,
  sidebar=side,
  body=bod,
  skin = "black"
)