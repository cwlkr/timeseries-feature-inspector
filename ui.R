
library(shiny)
library(shinyjs) #http://deanattali.com/shinyjs/

shinyUI(fluidPage(
  useShinyjs(),
  # Include shinyjs
  
  # Application title
  title = "Timecourse Inspector",
  sidebarLayout(
    sidebarPanel(
      #Selector for file upload
      fileInput(
        'seriesSelectedFileIn',
        'Select data file (e.g. tCoursesSelected.csv) and press "Load Data"',
        accept = c('text/csv', 'text/comma-separated-values,text/plain')
      ),
      fileInput(
        'metadata',
        'Select data file (e.g. metadata) and press "Load Data"',
        accept = c('text/csv', 'text/comma-separated-values,text/plain')
      ),
      fileInput(
        'receptordata',
        'Select data file (e.g. receptor) and press "Load Data"',
        accept = c('text/csv', 'text/comma-separated-values,text/plain')
      ),
      actionButton("loadAll", "Load Files"),
      uiOutput("cond")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("meanPlots"
                 #DT::dataTableOutput("table")
                 #meanPlotsUI("meanPlots")
                 ))
      )
    )
  )
)


# library(d3heatmap)
# library(shinydashboard)
# library(shiny)
# library(heatmaply)
# library(plotly)
# 
# ui <- fluidPage(
#   h1("A heatmap demo"),
#   sliderInput("k", "Number of clusters",1,500, 5),
#   plotlyOutput("heatmap"),
#   verbatimTextOutput("hevent")
# )
# 
# server <- function(input, output, session) {
#   output$heatmap <- renderPlotly({
#     heatmaply(
#       na.omit(m_clipped),
#       dendrogram = "both", k_row = input$k,
#       scale="none", show_grid = 0,
#       anim_duration = 0
#     )
#     })
#     output$hevent <- renderPrint({
#       d <- event_data("plotly_hover")
#       if (is.null(d)) "Hover events appear here (unhover to clear)" else d
#   
#   })
# }
# 
# shinyApp(ui, server)
