## Weibull distribution plot in Shiny
## Author: Pawel Cwiek <pablorcw@users.noreply.github.com> [aut,cre]
## License: GPL-3
## DisplayMode: Showcase
## Tags: weibull
## Type: function

weibull.dist <- function(c = 7.0, k = 2.0, ws = 5.0){
  out <- round((k/c)*((ws/c)^(k-1))*exp(-(ws/c)^k),6)
  out
}
