#source("../hidap/tools/design/doe.R")

doe_args <- as.list(formals(doe))
#cat("hi")
#print(doe_args)

doe_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(doe_args)) {
    doe_args[[i]] <- input[[i]]
  }
  #print(doe_args)
  #if(!input$show_filter) doe_args$data_filter = ""
  #print(doe_args)
  doe_args
})

.summary_doe <- reactive({
#   if(not_available(input$doe_var))
#     return("This analysis requires a variable of type numeric or interval.\nIf none are available please select another dataset")
  #   if(is.na(input$ma_comp_value))
  #     return("Please choose a comparison value")
  out <- summary(.doe())
  out
})

.fieldbook_doe <- reactive({
  out <- fieldbook.doe(.doe())
  out
})

.doe <- reactive({
  do.call(doe, doe_inputs())
  
})

observe({
  if(not_pressed(input$doe_report)) return()
  isolate({
    outputs <- c("summary", "fb_draft", "plot")
    inp_out <- list(doe_plots = input$doe_plots) %>% list("",.)
    figs <- TRUE
    if(length(input$doe_plots) == 0) {
      figs <- FALSE
      outputs <- c("summary")
      inp_out <- list("","")
    }
    update_report(inp_main = clean_args(doe_inputs(), doe_args),
                  fun_name = "doe",
                  inp_out = inp_out,
                  outputs = outputs,
                  figs = figs,
                  fig.width = round(7 * doe_plot_width()/650,2),
                  fig.height = round(7 * doe_plot_height()/650,2))
   })
})



output$ui_doe_par <- renderUI({
  choices <- c("Randomized Complete Block Design" = "RCBD",
               "Completely Randomized Design" = "CRD",
               "Latin Square Design" = "LSD",
               "Graeco-Latin Design" = "GLD",
               "Youden Design" = "YD",
               "Balanced Incomplete Block Design" = "BIB",
               "Cyclic Design" = "CD",
               "Lattice Design" = "LD",
               "Alpha Design" = "AD",
               "Augmented Block Design" = "ABD",
               "Augmented Partically Replicated Design" = "APRD",
               "Split-plot Design" = "SPPD",
               "Strip-plot Design" = "STPD",
               "Factorial Design" = "F2SPPD"
               )
  selectInput("design", "Design method:", choices, multiple = FALSE)
})

# .doe_r_low <- reactive({
#   rl = 1
#   #if(input$design == "RCBD") rl = 2
#   rl
# })

# get_r_range <- function(doe) {
#   des <-doe$design
#   if(is.null(des)) des = "RCBD"
#   print(des)
#   if(des == "CRD") return(2:4)
#   if(des == "RCBD") return(2:10)
# }


output$fieldbook_doe <- renderDataTable(.fieldbook_doe())

output$ui_doe <- renderUI({
  tagList(
#     conditionalPanel(condition = "input.tabs_doe == 'Plot'",
#                      wellPanel(
#                        selectizeInput(inputId = "doe_plots", label = "Select plots:",
#                                       choices = ma_plots,
#                                       selected = state_single("ma_plots", ma_plots, "hist"),
#                                       multiple = TRUE,
#                                       options = list(plugins = list('remove_button', 'drag_drop')))
#                      )
#     ),
    wellPanel(
      uiOutput("ui_doe_par"),
      checkboxInput("zigzag", "Zigzag:", TRUE),
      radioButtons("serie", "Label series:", 
                   #get_series_labels(), "101, 102, ...", #get_series_labels()[[2]], 
                   1:3, 2, 
                   inline = TRUE),
      selectInput("trt", "Treatment (Germplasm)", get_germplasm_lists() , 
                    multiple = FALSE),
      conditionalPanel(condition = 
        "input.design == 'CRD'",
        radioButtons("r", "r:", 2:4, 2, inline = TRUE)
      ),
      conditionalPanel(condition =  "input.design == 'RCBD' ", 
        wellPanel(
        selectInput("rcbd_r", "r:", 2:10, 2),
        checkboxInput("rcbd_first", "Randomize first block", FALSE), 
        checkboxInput("rcbd_continue", "Use continuous numeration", FALSE) 
       )
      ),
      conditionalPanel(condition =  "input.design == 'LSD' ", 
       wellPanel(
         selectInput("lsd_r", "r:", 2:5, 2)
       )
      ),
      conditionalPanel(condition =  "input.design == 'GLD' ", 
         selectInput("trt2", "Treatment 2 (Germplasm)", get_germplasm_lists() , 
                                           multiple = FALSE)
      )
      
      
#       conditionalPanel(condition = 
#       " (input.design == 'CRD' |
#          input.design == 'RCBD' |
#          input.design == 'YD' )",
#         
#           radioButtons("r", "r:", 2:7, 2, inline = TRUE)
#         
#       ),
#       conditionalPanel(condition = 
#          "input.design == 'AD'",
#           radioButtons("r", "r:", 2:4, 2, inline = TRUE)
#       ),
#       conditionalPanel(condition = 
#          "input.design == 'LD'",
#           radioButtons("r", "r:", 2:3, 2, inline = TRUE)
#       ),
#       conditionalPanel(condition = 
#          "input.design == 'CD'",
#          wellPanel(
#           radioButtons("r", "r:", c(2,4,6,8), 2, inline = TRUE),
#           selectInput("k", "k:", 2:10, 2)
#          )
#          
#       ),
#       
#       conditionalPanel(condition =
#         "input.design == 'RCBD' |
#          input.design == 'LSD' |
#          input.design == 'YD' ",
#          checkboxInput("first", "Randomize first row:", FALSE)
#       ),
#       conditionalPanel(condition =
#         "input.design == 'RCBD' ",
#          checkboxInput("continue", "Continued labeling:", FALSE)
#       ),
#       conditionalPanel(condition =
#         "input.design == 'CD'",
#         checkboxInput("rowcol", "Row or column:", FALSE)
#       ),
#       conditionalPanel(condition =
#         "input.design == 'BIB' |
#          input.design == 'AD'
#         ",
#          selectInput("k", "k:", 2:10, 3)
#       ),
# #       conditionalPanel(condition =
# #          "input.design == 'AD'",
# #          #selectInput("k", "k:", guess_k(length(doe_inputs()$trt)))
# #          selectInput("k", "k:", 2:10, 2)
# #       ),
#       
#       conditionalPanel(condition =
#          "input.design == 'GLD'",
#          selectInput("trt2", "Treatment (Germplasm)", get_germplasm_lists() , 
#                      multiple = FALSE)
#       )
    
      
      
    )
    
      #     ,
#     help_and_report(modal_title = 'Experimental design', fun_name = 'doe',
#                     help_file = inclMD(file.path("..",app_dir,"tools","help",
#                                                  "my_analysis.md"))
#     )
  )
})



output$doe <- renderUI({
 
  register_print_output("summary_doe", ".summary_doe")
  #register_print_output("fieldbook_doe", ".fieldbook_doe")

#   register_plot_output("plot_my_analysis", ".plot_my_analysis",
#                        height_fun = "ma_plot_height")
  # two separate tabs
  
  doe_output_panels <- tabsetPanel(
    id = "tabs_doe",
    withProgress(message = 'Creating fieldbook', value = 0.1, {
      tabPanel("Summary", verbatimTextOutput("summary_doe"))
    })  ,
    withProgress(message = 'Creating fieldbook', value = 0.1, {  
      tabPanel("Fielbook draft", dataTableOutput("fieldbook_doe"))
    })
    #,
    #tabPanel("Plot", plotOutput("plot_my_analysis", height = "100%"))
  )

  # one output with components stacked
  # sm_output_panels <- tagList(
  # tabPanel("Summary", verbatimTextOutput("summary_single_mean")),
  # tabPanel("Plot", plotOutput("plot_single_mean", height = "100%"))
  # )
  stat_tab_panel(menu = "Design",
                 tool = "Design of experiments",
                 tool_ui = "ui_doe",
                 output_panels = doe_output_panels,
                 data = NULL)
  # add "data = NULL" if the app doesn't doesn't use data
 
})


