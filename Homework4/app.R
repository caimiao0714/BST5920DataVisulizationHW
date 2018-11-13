library(shiny)
library(ggplot2)
library(shinydashboard)
library(plotly)

dat = read.csv("PEEP1.csv")
dat$PEEP = factor(dat$PEEP, levels = c(0, 1), labels = c("No PEEP", "PEEP Tr"))

zzz = dropdownMenu(type = "messages",
             messageItem(
               from = "Author",
               message = "Miao Cai"
             ),
             messageItem(
               from = "E-mail",
               message = "miao.cai@slu.edu",
               icon = icon("table"),
               time = "Nov 12, 2018"
             )
)


body <- dashboardBody(
  fluidRow(
    valueBoxOutput("y"),
    valueBoxOutput("x"),
    valueBoxOutput("algorithm")
  ),
  h3("Exploratory data analysis for PEEP1.csv", align = "center"),
  fluidRow(
    box(solidHeader = TRUE, width = NULL, height = NULL, 
        plotlyOutput("xyplot"))
  ))


sidebar <- dashboardSidebar(
  sidebarMenu(
    selectInput(inputId = "x", label = "x axis", 
                choices = c("OpTime", "PackYear", "r.FRC", "a.PO"), 
                selected = "a.PO"),
    radioButtons(inputId = "algo", label = "Algorithm", 
                choices = c("None", "Linear", "Quadratic", "LOESS"), 
                selected = "Linear")
  ),
  br(),
  img(src='SLUlogo.png', style="display: block; margin-left: auto; margin-right: auto;", width = 180)
)


ui <- dashboardPage(
  dashboardHeader(title = "BST 5920 Data Visualization - Homework 5",
                  titleWidth = 420,
                  zzz),
  sidebar,
  body
)
  

server <- function(input, output){
  output$xyplot = renderPlotly({
    p = ggplot(dat, aes_string(input$x, "a.PODay2", color = "PEEP")) + 
      scale_color_manual("Treatment", values = c("red", "green")) +
      geom_point() + coord_cartesian(ylim = c(0, 12))
      ggtitle(paste("Scatter plot of", "aPODay2", "over", input$x)) + 
        theme(plot.title = element_text(hjust = 0.5)) + coord_fixed(0.618)
     p1 = switch(input$algo, 
             "None" = p ,
             "Linear" = p + geom_smooth(method = "lm", fill = NA) ,
             "Quadratic" = p + 
               geom_smooth(method = "lm", formula = y ~ x + I(x^2), fill = NA),
             "LOESS" = p + geom_smooth(method = "loess", fill = NA))
    ggplotly(p1 + theme_bw())
  })
  
  output$y <- renderValueBox({
    valueBox(
      value = input$x,
      subtitle = "Explantory variable",
      icon = icon("arrow-alt-circle-right"),
      color = "light-blue"
    )
  })
  
  output$x <- renderValueBox({
    valueBox(
      value = "a.PODay2",
      subtitle = "Response variable",
      icon = icon("arrow-alt-circle-up"),
      color = "light-blue"
    )
  })
  
  output$algorithm <- renderValueBox({
    valueBox(
      value = input$algo,
      "Algorithm",
      icon = icon("calculator"),
      color = "maroon"
    )
  })
}

shiny::shinyApp(ui = ui, server = server)
