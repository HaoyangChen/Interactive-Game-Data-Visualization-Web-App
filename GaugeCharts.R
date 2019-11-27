library(dplyr)
library(plotly)

draw_gauge <- function(pop, max) {
  section <- max / 5
  rad <- (1 - (pop / max)) * pi
  
  base_plot <- plot_ly(
    type = "pie",
    values = c(40, 10, 10, 10, 10, 10, 10),
    labels = c(" ", "0", as.character(section), as.character(section * 2), as.character(section * 3), as.character(section * 4), as.character(section * 5)),
    rotation = 108,
    direction = "clockwise",
    hole = 0.6,
    textinfo = "label",
    textposition = "outside",
    hoverinfo = "none",
    domain = list(x = c(0, 1), y = c(0, 1)),
    marker = list(colors = c('rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)','rgb(255, 255, 255)')),
    showlegend = FALSE, 
    width = 600, 
    height = 600
  ) 
  
  base_plot <- add_trace(
    base_plot,
    type = "pie",
    values = c(50, 10, 10, 10, 10, 10),
    labels = c(" ", "Dead", "Fading", "Alive", "Hot", "Red Hot!"),
    rotation = 90,
    direction = "clockwise",
    hole = 0.5,
    textinfo = "label",
    textposition = "inside",
    hoverinfo = "game name",
    domain = list(x = c(0, 1), y = c(0, 1)),
    marker = list(colors = c('rgb(255, 255, 255)', 'rgb(232,226,202)', 'rgb(244,220,66)', 'rgb(244,166,66)', 'rgb(244,100,66)', 'rgb(244,66,66)')),
    showlegend= FALSE
  )
  
  a <- list(
    showticklabels = FALSE,
    autotick = FALSE,
    showgrid = FALSE,
    zeroline = FALSE)
  
  b <- list(
    xref = 'paper',
    yref = 'paper',
    x = 0.5,
    y = 0.4,
    showarrow = F,
    text = pop)
  
  base_chart <- layout(
    base_plot,
    shapes = list(
      list(
        type = 'path',
        path = paste('M', as.character(0.3 * cos(rad) + 0.5), as.character(0.3 * sin(rad) + 0.5), 'L 0.5 0.5 Z'),
        xref = '0.5 0.5',
        yref = '0.5 0.5',
        fillcolor = 'rgba(44, 160, 101, 0.5)'
      )
    ),
    xaxis = a,
    yaxis = a,
    annotations = b
  ) 
  print(base_chart)
}


