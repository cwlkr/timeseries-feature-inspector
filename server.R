library(d3heatmap)
library(shinydashboard)
library(shiny)
library(heatmaply)
library(plotly)
library(data.table)

shinyServer(function(input, output, session) {
  useShinyjs()
  output$vreT = renderText({
    file = input$inFileLoadNuc
    if (is.null(file))
      return(NULL)
    file$datapath
    })
  }
)
