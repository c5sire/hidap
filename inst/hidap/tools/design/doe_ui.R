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

.doe <- reactive({
  do.call(doe, doe_inputs())
  
})

observe({
  if(not_pressed(input$doe_report)) return()
  isolate({
    outputs <- c("summary", "plot")
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
               "Split-plot Design" = "SPPD",
               "Strip-plot Design" = "STPD",
               "Factorial Design" = "F2SPPD"
               )
  selectInput("design", "Design method:", choices, multiple = FALSE)
})

.doe_r_low <- reactive({
  rl = 1
  #if(input$design == "RCBD") rl = 2
  rl
})

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
      selectInput("trt", "Treatment (Germplasm)", get_germplasm_lists() , 
                    multiple = FALSE),
      checkboxInput("zigzag", "Zigzag:", TRUE),
      radioButtons("serie", "Label series:", 
                   c("11, 12, ...", "101, 102, ...", "1001, 1002, ..." ), 
                   inline = TRUE),
      
      conditionalPanel(condition = "input.design == 'RCBD'",
        wellPanel(
          radioButtons("r", "r:", 2:5, 2, inline = TRUE)
        )
      ),
      conditionalPanel(condition = "input.design == 'YD'",
                       wellPanel(
                         radioButtons("r", "r:", 1:5, 2, inline = TRUE)
                       )
      )
      
      ,
      conditionalPanel(condition = "input.design == 'CRD'",
                       wellPanel(
                         selectInput("r", "r:", 
                                     choices=c(1:5), selected = 2, multiple = TRUE,
                                     selectize = TRUE)
                       )
      )
      
      
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
#   register_plot_output("plot_my_analysis", ".plot_my_analysis",
#                        height_fun = "ma_plot_height")
  # two separate tabs
  doe_output_panels <- tabsetPanel(
    id = "tabs_doe",
    tabPanel("Summary", verbatimTextOutput("summary_doe"))#,
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



