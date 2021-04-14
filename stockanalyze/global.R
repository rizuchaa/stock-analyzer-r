library(shiny)
library(shinydashboard)
library(quantmod)
library(highcharter)
library(plotly)
library(tidyverse)
library(tidyr)

# Box Card ---------------------------------------------------------------------

valueBox <- function(value, title, sparkobj = NULL, subtitle, icon = NULL,
                      color = "aqua", width = 4, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      h3(value),
      if (!is.null(sparkobj)) sparkobj,
      p(subtitle)
    ),
    if (!is.null(icon)) div(class = "icon-large", icon, style = "z-index; 0")
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}

# Graph Box Card Theme ---------------------------------------------------------

hc_theme_sparkline_vb <- function(...) {
  
  theme <- list(
    chart = list(
      backgroundColor = NULL,
      margins = c(0, 0, 0, 0),
      spacingTop = 0,
      spacingRight = 0,
      spacingBottom = 0,
      spacingLeft = 0,
      plotBorderWidth = 0,
      borderWidth = 0,
      style = list(overflow = "visible")
    ),
    xAxis = list(
      visible = FALSE, 
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    yAxis = list(
      visible = FALSE,
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    tooltip = list(
      outside = FALSE,
      shadow = FALSE,
      borderColor = "transparent",
      botderWidth = 0,
      backgroundColor = "transparent"
    ),
    plotOptions = list(
      series = list(
        marker = list(enabled = FALSE),
        lineWidth = 2,
        shadow = FALSE,
        fillOpacity = 0.25,
        color = "#FFFFFFBF",
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
          stops = list(
            list(0.00, "#FFFFFF00"),
            list(0.50, "#FFFFFF7F"),
            list(1.00, "#FFFFFFFF")
          )
        )
      )
    ),
    credits = list(
      enabled = FALSE,
      text = ""
    )
  )
  
  theme <- structure(theme, class = "hc_theme")
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }
  
  theme
}

# Data Scrap -------------------------------------------------------------------

## Get Datas
getSymbols(c("^JKSE","^IXIC","^DJI","BBCA.JK"),src='yahoo')

## Fetch Datas

### Card Data
JKSE<-as.data.frame(JKSE)
JKSE<-tail(JKSE,5)
JKSE%>%
  fill(everything())
JKSE$dif<-((JKSE$JKSE.Close-lag(JKSE$JKSE.Close,1))/lag(JKSE$JKSE.Close,1))*100
JKSE$date<-rownames(JKSE)
JKSE

IXIC<-as.data.frame(IXIC)
IXIC<-tail(IXIC,5)
IXIC%>%
  fill(everything())
IXIC$dif<-((IXIC$IXIC.Close-lag(IXIC$IXIC.Close,1))/lag(IXIC$IXIC.Close,1))*100
IXIC$date<-rownames(IXIC)
IXIC

DJI<-as.data.frame(DJI)
DJI<-tail(DJI,5)
DJI%>%
  fill(everything())
DJI$dif<-((DJI$DJI.Close-lag(DJI$DJI.Close,1))/lag(DJI$DJI.Close,1))*100
DJI$date<-rownames(DJI)
DJI


### Graph Data 
BBCA.JK<-data.frame(Date=index(BBCA.JK),coredata(BBCA.JK))
# BBCA.JK<-tail(BBCA.JK,86)
BBCA.JK<-BBCA.JK%>%
  fill(everything())
BBCA.JK$dif<-((BBCA.JK$BBCA.JK.Close-lag(BBCA.JK$BBCA.JK.Close,1))/lag(BBCA.JK$BBCA.JK.Close,1))*100

bbands <- BBands(BBCA.JK[,c("BBCA.JK.High","BBCA.JK.Low","BBCA.JK.Close")])
BBCA.JK <- subset(cbind(BBCA.JK, data.frame(bbands[,1:3])), Date >= Date[61])

BBCA.JK.tbl<-BBCA.JK%>% arrange(desc(BBCA.JK$Date))

# Plot Graph -------------------------------------------------------------------

plot1 <- hchart(JKSE, "line", hcaes(date,JKSE.Close), name = "value: ")  %>%
  hc_size(height = 110)%>% 
  hc_add_theme(hc_theme_sparkline_vb()) %>% 
  hc_credits(enabled = FALSE)

plot2 <- hchart(IXIC, "line", hcaes(date,IXIC.Close), name = "value: ")  %>%
  hc_size(height = 110)%>%
  hc_add_theme(hc_theme_sparkline_vb()) %>% 
  hc_credits(enabled = FALSE)

plot3 <- hchart(DJI, "line", hcaes(date,DJI.Close), name = "value: ")  %>%
  hc_size(height = 110)%>%
  hc_add_theme(hc_theme_sparkline_vb()) %>% 
  hc_credits(enabled = FALSE)

plot4 <- BBCA.JK %>% 
  plot_ly(x = ~Date, type="candlestick", name="BBCA.JK",
                           open = ~BBCA.JK.Open, close = ~BBCA.JK.Close,
                           high = ~BBCA.JK.High, low = ~BBCA.JK.Low) %>%
  layout(title = "BBCA Chart", yaxis = list(title = ""))%>% 
  add_lines(x = ~Date, y = ~up , name = "B Bands",
                         line = list(color = '#ccc', width = 0.6),
                         legendgroup = "Bollinger Bands",
                         hoverinfo = "none", inherit = F) %>% 
  add_lines(x = ~Date, y = ~dn, name = "B Bands",
                         line = list(color = '#ccc', width = 0.6),
                         legendgroup = "Bollinger Bands", inherit = F,
                         showlegend = F, hoverinfo = "none") %>% 
  add_lines(x = ~Date, y = ~mavg, name = "Moving Avg.",
                         line = list(color = '#E377C2', width = 0.6),
                         hoverinfo = "none", inherit = F)

# Attach Graph Highcharter to Card ---------------------------------------------

vb <-  valueBox(
  value = round(tail(DJI$DJI.Close, 1),2),
  title = "Dow Jones 5 Days",
  sparkobj = plot3,
  subtitle = print(paste0(round(tail(DJI$dif,1),2),"% From Yesterday")),
  icon = NULL,
  width = 4,
  color = "red",
  href = NULL)

vb2 <-  valueBox(
  value = round(tail(IXIC$IXIC.Close, 1),2),
  title = "NASDAQ 5 Days",
  sparkobj = plot2,
  subtitle = print(paste0(round(tail(IXIC$dif,1),2),"% From Yesterday")),
  icon = NULL,
  width = 4,
  color = "blue",
  href = NULL)

vb3 <- valueBox(
  value = round(tail(JKSE$JKSE.Close, 1),2),
  title = "IHSG 5 Days",
  sparkobj = plot1,
  subtitle = print(paste0(round(tail(JKSE$dif,1),2),"% From Yesterday")),
  icon = NULL,
  width = 4,
  color = "orange",
  href = NULL)



