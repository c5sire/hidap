# # output$aov_ui_data <- renderUI({
# #   list(
# #     includeCSS("../hidap/www/style.css"),
# #     includeScript("../hidap/www/js/jquery-ui.custom.min.js"),
# #     includeScript("../hidap/www/js/returnTextAreaBinding.js"),
# #     sidebarLayout(
# #       sidebarPanel(
# #         # based on https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/PzlSAmAxxwo
# #         wellPanel(
# #           uiOutput("ui_aov_datasets")
# # #           conditionalPanel("input.aov_datatabs == 'Manage1'",
# # #                            checkboxInput('show_filter', 'Filter (e.g., price > 5000)', 
# # #                                          value = state_init("show_filter",FALSE)),
# # #                            conditionalPanel("input.show_filter == true",
# # #                                             returnTextAreaInput("data_filter", label = "", value = state_init("data_filter")),
# # #                                             uiOutput("ui_filter_error")))
# #         ),
# #         #conditionalPanel("input.datatabs == 'Manage'", uiOutput("ui_Manage")),
# #         #conditionalPanel("input.datatabs == 'View'",uiOutput("ui_View")),#'Fieldbook' == 'View'
# #         conditionalPanel("input.aov_datatabs == 'ANOVA'",uiOutput("ui_View"))#'Fieldbook' == 'View'
# #         #conditionalPanel("input.aov_datatabs == 'Summary'",uiOutput("ui_Summary")),
# #         #conditionalPanel("input.aov_datatabs == 'Check_Format'",uiOutput("ui_Checks"))
# #         #         conditionalPanel("input.datatabs == 'Edit'",uiOutput("ui_Edit")),
# #         #         conditionalPanel("input.datatabs == 'Visualize'", uiOutput("ui_Visualize")),
# #         #         conditionalPanel("input.datatabs == 'Pivot'",uiOutput("ui_Pivot")),
# #         #         conditionalPanel("input.datatabs == 'Explore'", uiOutput("ui_Explore"))
# #       ),
# #       #conditionalPanel("input.datatabs == 'Transform'", uiOutput("ui_Transform"))),
# #       #conditionalPanel("input.datatabs == 'Merge'", uiOutput("ui_Merge"))),
# #       mainPanel(
# #         uiOutput("aov_datatabs")
# #       )
# #     )
# #   )
# # })
# # 
# # output$aov_datatabs <- renderUI({
# #   tabsetPanel(id = "aov_datatabs",
# #               #     tabPanel("Manage", htmlOutput("htmlDataExample"),
# #               #       conditionalPanel("input.man_add_descr == false", uiOutput("dataDescriptionHTML")),
# #               #       conditionalPanel("input.man_add_descr == true", uiOutput("dataDescriptionMD"))),
# #               #tabPanel("View", dataTableOutput("dataviewer")),#'Fieldbook' == 'View'
# #               tabPanel("ANOVA", dataTableOutput("aov_data")), #'Fieldbook' == 'View'
# #               #tabPanel("Summary", dataTableOutput("datasummary")),
# #               #tabPanel("Check_Format",dataTableOutput("datacheckformat"))
# #               #tabPanel("View", tableOutput("dataviewer")),
# #               #     tabPanel("Edit", rHandsontableOutput("tabed_doe")),
# #               #     tabPanel("Visualize",plotOutput("visualize", width = "100%", height = "100%")),
# #               #     tabPanel("Pivot", rpivotTable::rpivotTableOutput("pivotData")),
# #               #     tabPanel("Explore", verbatimTextOutput("expl_summary"), plotOutput("expl_plots", width = "100%", height = "100%"))#,
# #               #tabPanel("Transform", htmlOutput("transform_data"), verbatimTextOutput("transform_summary"))#,
# #               #tabPanel("Merge", htmlOutput("merge_possible"), htmlOutput("mergedata1"), htmlOutput("mergedata2"))
# #   )
# # })
# 
# 
# 
# # output$ui_aov_View <- renderUI({
# #   list(
# #     wellPanel(
# #       #uiOutput("uiView_vars")
# #       # shiny::fileInput(inputId = "view_vars",label ="Select your fieldbook" ,accept = c(".xlsx",".xls"))  
# #     ),
# #     #uiOutput("dependent"),
# #     #uiOutput("independents"),
# #     tags$hr(),
# #     uiOutput('ui_aov_action'), 
# #     #uiOutput('ui.exportfb.action'),
# #     #print(output$ui.action),# instead of conditionalPanel
# #     tags$hr(),
# #     #uiOutput("independents"),
# #     #tags$hr(),
# #     #     uiOutput("ui_action_sum"), 
# #     #     tags$hr(),
# #     #help_modal('View','viewHelp',inclMD(file.path("..",app_dir,"tools/help/view.md"))) 'Fieldbook'=='View'
# #     help_modal('anova','viewHelp',inclMD(file.path("..",app_dir,"tools/help/view.md")))
# #   )})
# # 
# # output$ui_aov_action <- renderUI({
# #   if (is.null(fb_data())) return()
# #   actionButton("aov.action", "Analyse!")
# # })
# 
# ###############################
# # Single mean - ui
# ###############################
# # alternative hypothesis options
# sm_alt <- c("Two sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")
# sm_plots <- c("Histogram" = "hist", "Simulate" = "simulate")
# # list of function arguments
# #sm_args <- as.list(formals(single_anova))
# # list of function inputs selected by user
# sm_inputs <- reactive({
#   # loop needed because reactive values don't allow single bracket indexing
#   for(i in names(sm_args))
#     sm_args[[i]] <- input[[i]]
#   if(!input$show_filter) sm_args$data_filter = ""
#   sm_args
# })
# output$ui_sm_var <- renderUI({
#   isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
#   vars <- varnames()[isNum]
#   selectInput(inputId = "sm_var", label = "Variable (select one):",
#               choices = vars, selected = state_single("sm_var",vars), multiple = FALSE)
# })
# output$ui_single_anova <- renderUI({
#   tagList(
#     conditionalPanel(condition = "input.tabs_single_anova == 'Plot'",
#                      wellPanel(
#                        selectizeInput(inputId = "sm_plots", label = "Select plots:",
#                                       choices = sm_plots,
#                                       selected = state_single("sm_plots", sm_plots, "hist"),
#                                       multiple = TRUE,
#                                       options = list(plugins = list('remove_button', 'drag_drop')))
#                      )
#     ),
#     wellPanel(
#       uiOutput("ui_sm_var"),
#       selectInput(inputId = "sm_alternative", label = "Alternative hypothesis:",
#                   choices = sm_alt,
#                   selected = state_single("sm_alternative", sm_alt, sm_args$sm_alternative),
#                   multiple = FALSE),
#       sliderInput('sm_sig_level',"Significance level:", min = 0.85, max = 0.99,
#                   value = state_init('sm_sig_level',sm_args$sm_sig_level), step = 0.01),
#       numericInput("sm_comp_value", "Comparison value:",
#                    state_init('sm_comp_value',sm_args$sm_comp_value))
#     ),
#     help_and_report(modal_title = 'Single anova', fun_name = 'single_anova',
#                     help_file = inclMD(file.path("..",app_dir, "tools","help","single_anova.md"))
#     )
#   )
# })
# sm_plot <- reactive({
#   list(plot_width = 650, plot_height = 400 * length(input$sm_plots))
# })
# sm_plot_width <- function()
#   sm_plot() %>% { if (is.list(.)) .$plot_width else 650 }
# sm_plot_height <- function()
#   sm_plot() %>% { if (is.list(.)) .$plot_height else 400 }
# # output is called from the main radiant ui.R
# output$single_anova <- renderUI({
#   register_print_output("summary_single_anova", ".summary_single_anova")
#   register_plot_output("plot_single_anova", ".plot_single_anova",
#                        height_fun = "sm_plot_height")
#   # two separate tabs
#   sm_output_panels <- tabsetPanel(
#     id = "tabs_single_anova",
#     tabPanel("Summary", verbatimTextOutput("summary_single_anova")),
#     tabPanel("Plot", plotOutput("plot_single_anova", height = "100%"))
#   )
#   # one output with components stacked
#   # sm_output_panels <- tagList(
#   # tabPanel("Summary", verbatimTextOutput("summary_single_anova")),
#   # tabPanel("Plot", plotOutput("plot_single_anova", height = "100%"))
#   # )
#   stat_tab_panel(menu = "Analysis",
#                  tool = "Single anova",
#                  tool_ui = "ui_single_anova",
#                  output_panels = sm_output_panels)
#   # add "data = NULL" if the app doesn't doesn't use data
# })
# .single_anova <- reactive({
#   do.call(single_anova, sm_inputs())
# })
# .summary_single_anova <- reactive({
#   if(not_available(input$sm_var))
#     return("This analysis requires a variable of type numeric or interval.\nIf none are available please select another dataset")
#   if(is.na(input$sm_comp_value))
#     return("Please choose a comparison value")
#   out <- summary(.single_anova())
#   #cat(str(out))
#   #print(out)
#   #cat(out)
#   out
# })
# 
# .plot_single_anova <- reactive({
#   if(not_available(input$sm_var))
#     return("This analysis requires a variable of type numeric or interval.\nIf none are available please select another dataset")
#   if(is.na(input$sm_comp_value))
#     return("Please choose a comparison value")
#   plot(.single_anova(), sm_plots = input$sm_plots, shiny = TRUE)
# })
# observe({
#   if(not_pressed(input$single_anova_report)) return()
#   isolate({
#     outputs <- c("summary","plot")
#     inp_out <- list(sm_plots = input$sm_plots) %>% list("",.)
#     figs <- TRUE
#     if(length(input$sm_plots) == 0) {
#       figs <- FALSE
#       outputs <- c("summary")
#       inp_out <- list("","")
#     }
#     update_report(inp_main = clean_args(sm_inputs(), sm_args),
#                   fun_name = "single_anova",
#                   inp_out = inp_out,
#                   outputs = outputs,
#                   figs = figs,
#                   fig.width = round(7 * sm_plot_width()/650,2),
#                   fig.height = round(7 * sm_plot_height()/650,2))
#   })
# })
# 
# 
# 
