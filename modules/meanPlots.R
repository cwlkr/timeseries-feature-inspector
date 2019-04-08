meanPlotsUI = function(id){
  ns = NS(id)
  tagList(
  fluidRow(
  column(6,textOutput("names"))
  #column(DT::dataTableOutput(ns("table"))),
  ),
  uiOutput(ns("mytabs")
    )
  )
}

meanPlots = function(input, output, session, data){
  output$table = DT::renderDataTable(data())
  output$names = renderText(colnames(data()))
  df = reactiveValues(data = isolate(reactive(data())))
  
  output$mytabs <- renderUI({
    data = data()
    groups = length(unique(df$data$Image_Metadata_Site))
    # create tabPanel with datatable in it
    myTabs = lapply(seq_len(groups), function(i) {
      tabPanel(paste0("group_",i),
               DT::dataTableOutput(paste0("group_",i))
      )
    })

    do.call(tabsetPanel, myTabs)
  })

  # create datatables
  observe(
    lapply(seq_len(length(df$data$Image_Metadata_Site)), function(i) {
      output[[paste0("group_",i)]] <- DT::renderDataTable({
        as.data.frame(df$data[Image_Metadata_Site==i])
      })
    })
  )
    
}