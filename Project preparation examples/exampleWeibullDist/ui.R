## ui.R
## Weibull distribution plot in Shiny
## Author: Pawel Cwiek <pablorcw@users.noreply.github.com> [aut,cre]
## License: GPL-3
## DisplayMode: Showcase
## Tags: weibull
## Type: Shiny

shinyUI(fluidPage(
  
  includeCSS("styles.css"),
  
  fluidRow(
  column(6, offset = 4,
    h1("Weibull Distribution"),
    h3("Create Weibull distribution graph"),
    numericInput("k", label = h5("Shape factor k"), value = 2.0, step = 0.1, min = 0),
    numericInput("c", label = h5("Scale factor c"), value = 7.0, step = 0.1, min = 0),
    sliderInput("range", label = h5("Range of wind speeds"),
              min = 0, max = 30, value = c(0,30))
  )),
  fluidRow(
  column(8, offset = 2,
    plotOutput("wbplot"),
    textOutput("text1")
  ))
))
