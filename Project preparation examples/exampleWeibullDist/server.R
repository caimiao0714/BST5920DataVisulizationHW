## server.R
## Weibull distribution plot in Shiny
## Author: Pawel Cwiek <pablorcw@users.noreply.github.com> [aut,cre]
## License: GPL-3
## DisplayMode: Showcase
## Tags: weibull
## Type: Shiny


library(ggplot2)
source("windfun.R")

df.wd <- data.frame(ws=seq(0,30,0.01),p=seq(0,30,0.01))
df.wd[2:3000,2] <- weibull.dist(ws=df.wd[2:3000,1])
k.value <- 2.0
c.value <- 7.0
v.ave <- round(c.value*gamma(1+1/k.value),2)

shinyServer(function(input, output){

  
  prepare.data <- reactive({
    v.min <- input$range[1]
    v.max <- input$range[2]
    s.range <- v.min/0.01
    e.range <- v.max/0.01
    k.value <- input$k
    c.value <- input$c
    v.ave <- round(c.value*gamma(1+1/k.value),2)
    df.wd[2:3000,2] <- weibull.dist(ws=df.wd[2:3000,1], k = k.value, c = c.value)
    df.wdplot <- as.data.frame(df.wd[s.range:e.range,])
    df.wdplot
  })
  
  mean.ws <- reactive({
    k.value <- input$k
    c.value <- input$c
    v.ave <- round(c.value*gamma(1+1/k.value),2)
    v.ave
  })
  
  output$wbplot <- renderPlot({
    df.wdplot <- prepare.data()
    ggplot(df.wdplot, aes(x = df.wdplot$ws, y = df.wdplot$p)) +
      xlab("Wind speed (m/s)") +
      ylab("Probability") +
      geom_line(color = "blue", size=1)
    
  })
  
  output$text1 <- renderText({
    paste("Mean wind speed:", mean.ws(), "m/s")
    
  }) 
  

  
})