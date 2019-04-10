
pacman::p_load(d3heatmap, shinydashboard, tidyverse, heatmaply, plotly, data.table, R.utils, readxl, shinyBS)

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
    locrec = input$receptordata$datapath
    #counter$dataLoadNuc <- input$loadAll - 1
    
    shiny::validate(
      need(locFilePath != "" || !is.null(locFilePath)  &&
             locmeta != "" || !is.null(locmeta), #&&
           # input$locrec != "" || !is.null(locrec)
           "Please select at least a data file")
    )
    if (is.null(locFilePath) || locFilePath == '')
      return(NULL)
    else {
      dt = fread(locFilePath)
      #meta = 
      #rec = 

      return(dt)
    }
  })
  loadMetaData <- eventReactive(input$loadAll, {
    locFilePath = input$seriesSelectedFileIn$datapath
    locrec = input$receptordata$datapath
    #----------
    cat("loadmetadata\n")
    locmeta = input$metadata$datapath
    #counter$dataLoadNuc <- input$loadAll - 1
    shiny::validate(
      need(locFilePath != "" || !is.null(locFilePath)  &&
             locmeta != "" || !is.null(locmeta), #&&
           # input$locrec != "" || !is.null(locrec)
           "Please select at least a data file")
    )
    if (is.null(locmeta) || locmeta == '')
      return(NULL)
    else {
      #meta = 
      #rec = 
      metadata = as.data.table(read_xlsx(locmeta))[-(1:2)]
      metanamecol = paste0(as.vector(metadata[1,]), "") # dirty hack to get "real" vector
      metadata = `colnames<-`(metadata, metanamecol)[-1,]
      return(metadata)
    }
  })
  loadRecData <- eventReactive(input$loadAll, {
    
    #----------
    cat("loadmetadata\n")
    locrec = input$receptordata$datapath
    #counter$dataLoadNuc <- input$loadAll - 1
    
    if (is.null(locrec) || locrec == '')
      return(NULL)
    else {
      dt = fread(locrec)
      #meta = 
      #rec = 
      
      return(dt)
    }
  })
  
    output$cond = renderUI({
      #calc stuff and give to inputs // select clean features with receptor
      locCols = colnames(dataLoadNuc())
      locMet = colnames(loadMetaData())
      if (!is.null(locCols) && !is.null(locMet)) {
        locColSelnuc = locCols[grep('objNuc_Intensity_MeanIntensity_imErk', locCols)[1]] # index 1 at the end in case more matches; select 1st
        locColSelcyto = locCols[grep('objCyto_ring_Intensity_MeanIntensity_imErk', locCols)[1]]
        locColTime = locCols[grep('(T|t)ime|Metadata_T', locCols)[1]]
        locColStim = locMet[grep('Stimulation_treatment', locMet)[1]]
        locColStimT = locMet[grep('Stimulation_time', locMet)[1]]
        locColMetGroup = locMet[grep('Grouping', locMet)[1]]
        locColTrackID = locCols[grep('track_id', locCols)[1]]
        locColSite = locCols[grep('Image_Metadata_Site', locCols)[1]]
        locColGroup = c(locColSite, locColTrackID)
        locMetSite =locMet[grep('Image_Metadata_Site|Position', locMet)[1]]
        locMetWell = locMet[grep('Well', locMet)[1]]
          }
      tagList(
        checkboxInput("check.receptor", "Filter by Receptor Levels", TRUE),
        uiOutput("rec.selection"),
        bsCollapse(
          bsCollapsePanel(title = "Column Selector:",
        selectInput(
          'nuc.erk',
          'Select Nuclear Erk Label (e.g. objNuc_Intensity_MeanIntensity_imErk):',
          locCols,
          width = '100%',
          selected = locColSelnuc
        ),
        selectInput(
          'cyto.erk',
          'Select Cytosolic Erk Label (e.g. objCyto_ring_Intensity_MeanIntensity_imErk):',
          locCols,
          width = '100%',
          selected = locColSelcyto
        ),
        selectInput(
          'time.var',
          'Select Time Label (e.g. RealTime or Image_Metadata_T):',
          locCols,
          width = '100%',
          selected = locColTime
        ),
        selectInput(
          'stim.time.var',
          'Select Stimulation Time in Metafile:',
          locMet,
          width = '100%',
          selected = locColStimT
        ),       
        selectInput(
          'stim.var',
          'Select Variable with Stimulation treatment:',
          locMet,
          width = '100%',
          selected = locColStim
        ),        
        selectInput(
          'meta.grouping',
          'Grouping Variable for average Plots:',
          locMet,
          width = '100%',
          selected = locColMetGroup
        ),        
        selectInput(
          'group.var',
          'Additional Grouping variables for unique Grouping:',
          locCols,
          width = '100%',
          selected = locColGroup, multiple = TRUE
        ),       
        selectInput(
          'track.var',
          'unique Identifier',
          locCols,
          width = '100%',
          selected = locColTrackID
        ),       
        selectInput(
          'site.var',
          'Name of Site variable (eg. Image_Metadata_Site):',
          locCols,
          width = '100%',
          selected = locColSite
        ),
        selectInput(
          'pos.met',
          'Position in Metadata:',
          locMet,
          width = '100%',
          selected = locMetSite
        ),
        selectInput(
          'well.met',
          'Well in Metadata:',
          locMet,
          width = '100%',
          selected = locMetWell
        ))),
        actionButton("mergeandplot", "Plot")
        # downloadButton("downloadres", "Download Merged File")
      )
    })
    get.stim.times = eventReactive(input$mergeandplot, {
      stim.times <- loadMetaData() %>% select(contains(input$stim.time.var), input$stim.var) %>% 
        select(contains(input$stim.time.var)) %>% slice(1)  %>% c(., recursive=TRUE) %>% as.numeric()
      return(stim.times)
    })
    
    # output$downloadres <- downloadHandler("mergedFile.csv",
    #                       content = function(file){fwrite(x = data.for.d(),file = file)},
    #                       contentType = "csv"
    #   
    # )
    # 
    output$rec.selection = renderUI({
      tagList(  
      plotOutput("rec.dist"),
      sliderInput("rec.slider", label="select quantiles", min = 0, max = 1, value = c(0.1,0.95))
      )
    })
    observe({
      receptordata = loadRecData()
      cut = (receptordata$obj_Rec_Intensity_MeanIntensity_imRecCorrOrig)
      breaks =  seq(0,1000,1) / (1000 / max(receptordata$obj_Rec_Intensity_MeanIntensity_imRecCorrOrig))
      if(!is.null(input$check.receptor)){
      if(input$check.receptor){
      output$rec.dist = renderPlot({ hist(cut, freq = F, breaks = breaks)
                     
                     abline(v=quantile(receptordata$obj_Rec_Intensity_MeanIntensity_imRecCorrOrig, input$rec.slider[1], na.rm = T), col = "red")
                     abline(v = quantile( receptordata$obj_Rec_Intensity_MeanIntensity_imRecCorrOrig,input$rec.slider[2], na.rm = T) , col = "red")})
    }}}
    )
    get.dt.data = eventReactive({input$mergeandplot}, {
      #actual data merging testing and stuff.
      dt.data = dataLoadNuc()
      metadata = loadMetaData()
      receptordata = NULL
      if(input$check.receptor == TRUE){
        shiny::validate(need(input$receptordata$datapath != "" | is.null(input$receptordata$datapath), "receptorFile needed!"))
        receptordata = loadRecData()
      }
      dt.data = dt.data[, erk.ratio := get(input$cyto.erk)/get(input$nuc.erk)]
      
      #
      groups <- metadata %>% # group_by(.dots = meta.grouping) %>% 
        select(input$stim.var, input$meta.grouping, input$pos.met, input$well.met) %>% rename("Image_Metadata_Site" = "Position", "Metadata_Well" = "Well") %>%
        mutate("Grouping" = as.numeric(get(input$meta.grouping)), Image_Metadata_Site = as.numeric(Image_Metadata_Site), Metadata_Well = as.numeric(Metadata_Well)) 
      
      dt.data =   left_join(dt.data, groups)
      
      if(!is.null(receptordata) & input$check.receptor == TRUE){
        # change to selector input stuff.
        ci_l = quantile( receptordata$obj_Rec_Intensity_MeanIntensity_imRecCorrOrig,input$rec.slider[1], na.rm = T)
        ci_u = quantile( receptordata$obj_Rec_Intensity_MeanIntensity_imRecCorrOrig,input$rec.slider[2], na.rm = T)
        receptordata %>% filter(obj_Rec_Intensity_MeanIntensity_imRecCorrOrig > ci_l &
                                  obj_Rec_Intensity_MeanIntensity_imRecCorrOrig < ci_u) %>% select(Image_Metadata_Site, track_id) -> reduce
        dt.data = left_join(reduce, dt.data)
      }
      # if(!is.null(loadRecData())){
      #   # TODO merge rec data
      # }
      return(dt.data)
    })
    get.dt.ngroups = reactive(
      get.dt.data() %>% select(input$meta.grouping)  %>% summarise(n = length(unique(get(input$meta.grouping)))) %>% pull()
    )
  data.container = reactiveValues(dt.data = reactive(get.dt.data()))
  #output$table =  DT::renderDataTable(dataLoadNuc())
  callModule(meanPlots, "meanPlots",
             data = reactive(get.dt.data()),
             stim.times = reactive(get.stim.times()),
             ngroups = reactive(get.dt.ngroups()),
             meta.grouping = reactive(input$meta.grouping),
             nuc.erk = reactive(input$nuc.erk),
             cyto.erk = reactive(input$cyto.erk),
             time.var = reactive(input$time.var),
             stim.var = reactive(input$stim.var)
             )
  
  }
)
