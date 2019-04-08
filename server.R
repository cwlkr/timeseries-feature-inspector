library(d3heatmap)
library(shinydashboard)
library(shiny)
library(heatmaply)
library(plotly)
library(data.table)
options(shiny.maxRequestSize = 200 * 1024 ^ 2)

shinyServer(function(input, output, session) {
  useShinyjs()
  # loader = reactiveValues(dataLoadNuc  = isolate(input$loadAll))
  
  ## TODO load and merge and do all the stuff!!
  dataLoadNuc <- eventReactive(input$loadAll, {
    
    #----------
    cat("dataLoadNuc\n")
    locFilePath = input$seriesSelectedFileIn$datapath
    locmeta = input$metadata$datapath
    #locrec = input$receptordata$datapath
    #counter$dataLoadNuc <- input$loadAll - 1
    # validate(
    #   need(input$locFilePath != "" || !is.null(locFilePath)  &&
    #           input$locmeta != "" || !is.null(locmeta) #&&
    #          # input$locrec != "" || !is.null(locrec)
    #          , "Please input all required Files")
    # )
    if (is.null(locFilePath) || locFilePath == '')
      return(NULL)
    else {
      dt = fread(locFilePath)
      #meta = 
      #rec = 

      return(dt)
    }
  })
  
    output$cond = renderUI({
      #calc stuff and give to inputs
      tagList(
        sliderInput("n", "N", 1, 1000, 500),
        textInput("label", colnames(dataLoadNuc())),
        actionButton("mergeandplot", "OK")
      )
    })
    
    
  
  #output$table =  DT::renderDataTable(dataLoadNuc())
  #callModule(meanPlots, "meanPlots", data = reactive(dataLoadNuc()))
  
  }
)
