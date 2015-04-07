my_analysis <- function(dataset, ma_var ,
                        data_filter = ""){
  dat <- getdata(dataset, ma_var, filt = data_filter)
  mean(dat[, ma_var]) -> res

  environment() %>% as.list %>% set_class(c("my_analysis",class(.)))
}

summary.my_analysis <- function(object, 
                                ...){
  cat("My analysis (mean) \n")
  cat("Mean:", object$res, "\n")
}

plot.my_analysis <- function(x,
                             ma_plots = "hist",
                             shiny = FALSE,
                             ...){
  object <- x; rm(x)
  plots <- list()
  if("hist" %in% ma_plots) {
    bw <- object$dat %>% range(na.rm = TRUE) %>% diff %>% divide_by(10)
    plots[[which("hist" == ma_plots)]] <-
      ggplot(object$dat, aes_string(x=object$ma_var)) +
      geom_histogram(colour = 'black', fill = 'blue', binwidth = bw, alpha = .1) +
#       geom_vline(xintercept = object$ma_comp_value, color = 'red',
#                  linetype = 'solid', size = 1) +
      geom_vline(xintercept = object$res, color = 'black',
                 linetype = 'solid', size = 1) 
#       geom_vline(xintercept = c(object$res$conf.low, object$res$conf.high),
#                  color = 'black', linetype = 'longdash', size = .5)
  }
}


##########################
ma_plots <- c("Histogram" = "hist")
ma_args <- as.list(formals(my_analysis))


ma_plot <- reactive({
  list(plot_width = 650, plot_height = 400 * length(input$ma_plots))
})
ma_plot_width <- function()
  ma_plot() %>% { if (is.list(.)) .$plot_width else 650 }
ma_plot_height <- function()
  ma_plot() %>% { if (is.list(.)) .$plot_height else 400 }



.plot_my_analysis <- reactive({
  if(not_available(input$ma_var))
    return("This analysis requires a variable of type numeric or interval.\nIf none are available please select another dataset")
#   if(is.na(input$ma_comp_value))
#     return("Please choose a comparison value")
  plot(.my_analysis(), ma_plots = input$ma_plots, shiny = TRUE)
})



# list of function inputs selected by user
ma_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(ma_args))
    ma_args[[i]] <- input[[i]]
  if(!input$show_filter) ma_args$data_filter = ""
  ma_args
})

output$ui_ma_var <- renderUI({
  isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
  vars <- varnames()[isNum]
  selectInput(inputId = "ma_var", label = "Variable (select one):",
              choices = vars, selected = state_single("ma_var",vars), multiple = FALSE)
})

output$ui_my_analysis <- renderUI({
  tagList(
    conditionalPanel(condition = "input.tabs_my_analysis == 'Plot'",
                     wellPanel(
                       selectizeInput(inputId = "ma_plots", label = "Select plots:",
                                      choices = ma_plots,
                                      selected = state_single("ma_plots", ma_plots, "hist"),
                                      multiple = TRUE,
                                      options = list(plugins = list('remove_button', 'drag_drop')))
                     )
    ),
    wellPanel(
      uiOutput("ui_ma_var")
#       ,
#       selectInput(inputId = "sm_alternative", label = "Alternative hypothesis:",
#                   choices = sm_alt,
#                   selected = state_single("sm_alternative", sm_alt, sm_args$sm_alternative),
#                   multiple = FALSE),
#       sliderInput('sm_sig_level',"Significance level:", min = 0.85, max = 0.99,
#                   value = state_init('sm_sig_level',sm_args$sm_sig_level), step = 0.01),
#       numericInput("sm_comp_value", "Comparison value:",
#                    state_init('sm_comp_value',sm_args$sm_comp_value))
#     ),
#     help_and_report(modal_title = 'Single mean', fun_name = 'single_mean',
#                     help_file = inclMD("../quant/tools/help/single_mean.md")
    )
    ,
    help_and_report(modal_title = 'My analysis', fun_name = 'my_analysis',
                    help_file = inclMD(file.path("..",app_dir,"tools","help",
                                    "my_analysis.md"))
    )
  )
})



output$my_analysis <- renderUI({
  register_print_output("summary_my_analysis", ".summary_my_analysis")
  register_plot_output("plot_my_analysis", ".plot_my_analysis",
                        height_fun = "ma_plot_height")
  # two separate tabs
  ma_output_panels <- tabsetPanel(
    id = "tabs_my_analysis",
    tabPanel("Summary", verbatimTextOutput("summary_my_analysis")),
    tabPanel("Plot", plotOutput("plot_my_analysis", height = "100%"))
  )
  # one output with components stacked
  # sm_output_panels <- tagList(
  # tabPanel("Summary", verbatimTextOutput("summary_single_mean")),
  # tabPanel("Plot", plotOutput("plot_single_mean", height = "100%"))
  # )
  stat_tab_panel(menu = "Analysis",
                 tool = "My analysis",
                 tool_ui = "ui_my_analysis",
                 output_panels = ma_output_panels)
  # add "data = NULL" if the app doesn't doesn't use data
})

.my_analysis <- reactive({
  do.call(my_analysis, ma_inputs())
  
})

.summary_my_analysis <- reactive({
  if(not_available(input$ma_var))
    return("This analysis requires a variable of type numeric or interval.\nIf none are available please select another dataset")
#   if(is.na(input$ma_comp_value))
#     return("Please choose a comparison value")
  out <- summary(.my_analysis())
  out
})



observe({
  if(not_pressed(input$my_analysis_report)) return()
  isolate({
    outputs <- c("summary","plot")
    inp_out <- list(ma_plots = input$ma_plots) %>% list("",.)
    figs <- TRUE
    if(length(input$ma_plots) == 0) {
      figs <- FALSE
      outputs <- c("summary")
      inp_out <- list("","")
    }
    update_report(inp_main = clean_args(ma_inputs(), ma_args),
                  fun_name = "my_analysis",
                  inp_out = inp_out,
                  outputs = outputs,
                  figs = figs,
                  fig.width = round(7 * ma_plot_width()/650,2),
                  fig.height = round(7 * ma_plot_height()/650,2))
  })
})
