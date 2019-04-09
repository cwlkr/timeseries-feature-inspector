#TODO Aquisition Freq time axis
meanPlotsUI = function(id){
  ns = NS(id)
  tagList(
  uiOutput(ns("mytabs")),
  uiOutput(ns("ribbonbutton")),
  uiOutput(ns("dnr"))
  )
}

meanPlots = function(input, output, session, data, stim.times, ngroups,
                     meta.grouping,
                     nuc.erk, cyto.erk, time.var, stim.var){
  
  ns <- session$ns
  
  observeEvent(data(),{
    output$dnr = renderUI(downloadButton(ns("pdfr"), "Download Average Plots"))}
    )
  observeEvent(data(),{
    output$ribbonbutton = renderUI(checkboxInput(ns("checkrug"), "Display CI ribbon", value = TRUE))
    }
  )
  

  
  output$pdfr <- downloadHandler(
    filename = function() {"averagePlots.pdf"},
    content = function(file) {
      create_pdf(setDT(isolate(data())), ci.lvl = 0.05,
                 stimulus.rug = stim.times(),
                 nuc.erk = nuc.erk(),
                 cyto.erk = cyto.erk(),
                 time.var = time.var(),
                 stim.var = stim.var(),
                 erk.ratio.var = "erk.ratio",
                 vlines = FALSE, ngroups = ngroups(), pdf.filename = file, meta.grouping  = meta.grouping())
    },contentType = "pdf"
    )
  
  
  output$mytabs <- renderUI({
    # create tabPanel with datatable in it
    myTabs = lapply(seq_len(ngroups()+1), function(i) {
      tabPanel(paste0("group_",(i-1)),
               plotlyOutput(ns(paste0("group_",(i-1))), width = "1000px", height = "800px")
      )
    })

    do.call(tabsetPanel, myTabs)
  })
  #create datatables #TODO slider for CI
  observe({
    lapply(seq_len(ngroups()+1), function(i) {
      output[[paste0("group_",(i-1))]] <- renderPlotly({
       
     
        if(i == (ngroups()+1)){
          gg = create_plot(data = setDT(data())[get(stim.var()) %like% "CTRL"], ci.lvl = 0.05,
                           stimulus.rug = stim.times(),
                           nuc.erk = nuc.erk(),
                           cyto.erk = cyto.erk(),
                           time.var = time.var(),
                           stim.var = stim.var(),
                           erk.ratio.var = "erk.ratio",
                           vlines = FALSE, ribbon = input$checkrug)
        }else{
          gg = create_plot(data = setDT(data())[get(meta.grouping()) == (i-1)], ci.lvl = 0.05,
                           stimulus.rug = stim.times(),
                           nuc.erk = nuc.erk(),
                           cyto.erk = cyto.erk(),
                           time.var = time.var(),
                           stim.var = stim.var(),
                           erk.ratio.var = "erk.ratio",
                           vlines = FALSE, ribbon = input$checkrug)
        }
        ggplotly(print(gg))
      })
    })
  }
  )
    
}