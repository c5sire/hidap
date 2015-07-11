#######################################
# Shiny interface for data functions
#######################################

output$data_filter <- renderUI({
  
})

output$show_filter <- renderUI({
  
})


# output$tabed_doe <- renderRHandsontable({
#   DF <- .getdata()#.fieldbook_doe()
#   #if (!is.null(input$tabed_doe)) {
#   #setHot(DF)
#   rhandsontable(DF) %>%
#     hot_table(highlightCol = TRUE, highlightRow = TRUE ) %>%
#     hot_cols( fixedColumnsLeft = 3)
#   #}
#   ######################
#   DF = data.frame(val = 1:10, bool = TRUE, big = LETTERS[1:10],
#                   small = letters[1:10],
#                   dt = seq(from = Sys.Date(), by = "days", length.out = 10),
#                   stringsAsFactors = F)
#   
#   rhandsontable(DF, rowHeaders = NULL)
#   
#   
#   
#   
#   ###################
# })

output$ui_filter_error <- renderUI({
  if(is_empty(r_data$filter_error)) return()
  helpText(r_data$filter_error)
})

# data ui and tabs
output$ui_data <- renderUI({
  list(
    includeCSS("../hidap/www/style.css"),
    includeScript("../hidap/www/js/jquery-ui.custom.min.js"),
    includeScript("../hidap/www/js/returnTextAreaBinding.js"),
    sidebarLayout(
      sidebarPanel(
        # based on https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/PzlSAmAxxwo
        wellPanel(
          uiOutput("ui_datasets"),
         conditionalPanel("input.datatabs == 'Manage1'",
            checkboxInput('show_filter', 'Filter (e.g., price > 5000)', 
                          value = state_init("show_filter",FALSE)),
            conditionalPanel("input.show_filter == true",
              returnTextAreaInput("data_filter", label = "", value = state_init("data_filter")),
              uiOutput("ui_filter_error")))
        ),
        #conditionalPanel("input.datatabs == 'Manage'", uiOutput("ui_Manage")),
        conditionalPanel("input.datatabs == 'View'",uiOutput("ui_View")),
        conditionalPanel("input.datatabs == 'Summary'",uiOutput("ui_Summary")),
        #conditionalPanel("input.datatabs == 'Edit'",uiOutput("ui_Edit")),
        conditionalPanel("input.datatabs == 'Visualize'", uiOutput("ui_Visualize")),
        conditionalPanel("input.datatabs == 'Pivot'",uiOutput("ui_Pivot")),
        conditionalPanel("input.datatabs == 'Explore'", uiOutput("ui_Explore"))
      ),
        #conditionalPanel("input.datatabs == 'Transform'", uiOutput("ui_Transform"))),
        #conditionalPanel("input.datatabs == 'Merge'", uiOutput("ui_Merge"))),
      mainPanel(
        uiOutput("datatabs")
      )
    )
  )
})

# data tabs
output$datatabs <- renderUI({
  tabsetPanel(id = "datatabs",
    tabPanel("Manage", htmlOutput("htmlDataExample"),
      conditionalPanel("input.man_add_descr == false", uiOutput("dataDescriptionHTML")),
      conditionalPanel("input.man_add_descr == true", uiOutput("dataDescriptionMD"))),
    tabPanel("View", dataTableOutput("dataviewer")),
    tabPanel("Summary", dataTableOutput("datasummary")),
    #tabPanel("View", tableOutput("dataviewer")),
    #tabPanel("Edit", rHandsontableOutput("tabed_doe")),
    tabPanel("Visualize",plotOutput("visualize", width = "100%", height = "100%")),
    tabPanel("Pivot", rpivotTable::rpivotTableOutput("pivotData")),
    tabPanel("Explore", verbatimTextOutput("expl_summary"), plotOutput("expl_plots", width = "100%", height = "100%"))#,
    #tabPanel("Transform", htmlOutput("transform_data"), verbatimTextOutput("transform_summary"))#,
    #tabPanel("Merge", htmlOutput("merge_possible"), htmlOutput("mergedata1"), htmlOutput("mergedata2"))
  )
})

# to add
# tabPanel("View", DT::dataTableOutput("dataviewer")),
# tabPanel("Generate", HTML("<h3>Generate input data for simulation and prediction</h3>")),
