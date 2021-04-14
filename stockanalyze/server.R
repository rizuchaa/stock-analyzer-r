function(input, output) {
  
  output$vbox <- renderValueBox(vb)
  output$vbox2 <- renderValueBox(vb2)
  output$vbox3 <- renderValueBox(vb3)
  output$candle <- renderPlotly(plot4)
  #output$table <- renderDataTable(BBCA.JK[,c("BBCA.JK.Open","BBCA.JK.Close","BBCA.JK.High","BBCA.JK.Low","BBCA.JK.Adjusted","BBCA.JK.Volume")])
  output$table <- renderDataTable(BBCA.JK.tbl)
}