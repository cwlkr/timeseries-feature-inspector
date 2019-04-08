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

meanPlots = function(input, output, session, data, stim.time, ngroups){
  
  ns <- session$ns

  output$mytabs <- renderUI({
    # create tabPanel with datatable in it
    myTabs = lapply(seq_len(ngroups()), function(i) {
      tabPanel(paste0("group_",i),
               plotlyOutput(ns(paste0("group_",i)))
      )
    })

    do.call(tabsetPanel, myTabs)
  })
  #create datatables #TODO slider for CI
  observe({
    lapply(seq_len(ngroups()), function(i) {
      output[[paste0("group_",i)]] <- renderPlotly({
        dt = setDT(data())[get(input$meta.grouping) == i]
        stims = stim.times()
        ggplotly(
        create_plot(dt, ci.lvl = 0.05,
        stimulus.rug = stims,
        nuc.erk = input$nuc.erk,
        cyto.erk = input$cyto.erk,
        time.var = input$time.var,
        stim.var = input$stim.var,
        erk.ratio.var = erk.ratio,
        vlines = False)
        )
      })
    })
  }
  )
    
}