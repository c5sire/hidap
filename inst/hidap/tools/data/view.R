#############################################
# View table output of the selected dataset
#############################################
#BEGIN Old CODE

# output$uiView_vars <- renderUI({
#   vars <- varnames()
#   #vars <- vars()
#   print(vars)
#      selectInput("view_vars", "Select variables to show:", choices  = vars,
#        selected = state_multiple("view_vars",vars, vars), multiple = TRUE,
#        selectize = FALSE, size = min(15, length(vars)))
#   #shiny::fileInput(inputId = "fb_view_vars",label ="Select your fieldbook" ,accept = c(".xlsx",".xls"))  
#   
#   
#   #shiny::fileInput(inputId = "view_vars",label ="Select your fieldbook" ,accept = c(".xlsx",".xls"))  
# })
# 
# vars <- reactive({
#   validate(
#     need(!is.null(varnames()), "No dataset chosen!")
#   )
#   get(varnames())
# })
# 
# #observe(output$uiView_vars)({
# output$ui_View <- renderUI({
#   list(
#     wellPanel(
#       uiOutput("uiView_vars")
#     ),
#     help_modal('View','viewHelp',inclMD(file.path("..",app_dir,"tools/help/view.md")))
# )}
# )
# 
# output$dataviewer <- renderDataTable({
# 
#   if(input$view_vars %>% not_available) return()
#   select_(.getdata(), .dots = input$view_vars)
# 
# }, options = list(orderClasses = TRUE, caseInsensitive = TRUE,
#   lengthMenu = list(c(10, 25, 50, -1),c('10','25','50','All')),
#   pageLength = 10, search = list(regex = TRUE)))


# use DT to add dplyr - server side code
# output$dataviewer <- DT::renderDataTable({

#  if(input$view_vars %>% not_available) return()
#  select_(.getdata(), .dots = input$view_vars) %>%
#  DT::datatable(., server = TRUE)
#})

#options = list(orderClasses = TRUE, caseInsensitive = TRUE,
 # lengthMenu = list(c(10, 25, 50, -1),c('10','25','50','All')),
#  pageLength = 10, search = list(regex = TRUE)))
#END OLD CODE

###################################################################################  
#Begin NEW code

# output$uiView_vars <- renderUI({
#   #vars <- varnames()
#   #vars <- vars()
#   #print(vars)
# #   selectInput("view_vars", "Select variables to show:", choices  = vars,
# #     selected = state_multiple("view_vars",vars, vars), multiple = TRUE,
# #     selectize = FALSE, size = min(15, length(vars)))
#   shiny::fileInput(inputId = "view_vars",label ="Select your fieldbook" ,accept = c(".xlsx",".xls"))  
# })

#observe(output$uiView_vars)({
output$ui_View <- renderUI({
  list(
    wellPanel(
      #uiOutput("uiView_vars")
     # shiny::fileInput(inputId = "view_vars",label ="Select your fieldbook" ,accept = c(".xlsx",".xls"))  
    ),
     #uiOutput("dependent"),
    #uiOutput("independents"),
    tags$hr(),
    uiOutput('ui.action'), # instead of conditionalPanel
    tags$hr(),
    uiOutput("independents"),
    tags$hr(),
#     uiOutput("ui_action_sum"), 
#     tags$hr(),
    help_modal('View','viewHelp',inclMD(file.path("..",app_dir,"tools/help/view.md")))
)})

fb_data <- reactive({
#vars <- fb_data
  #fb_file <- input$view_vars
#   print("===")
#   print(input$view_vars)
  #fb_file <- input$view_vars
  fb_file <- input$view_vars_input
  #   print("---2---")
#   print(input$view_vars)
#   print("---3---")
#   print(fb_file)
  #if(is.null(input$view_vars)){return()}
  #if(!is.null(fb_file)){
  if(is.null(input$view_vars_input)){return()}
  if(!is.null(fb_file)){
    file.copy(fb_file$datapath,paste(fb_file$datapath, ".xlsx", sep=""))
    fb_data <- read_excel(paste(fb_file$datapath, ".xlsx", sep=""), sheet = "Fieldbook")
    
  #fb_data <- readxl::read_excel(fb_file <- paste(fb_file$datapath,".xls",sep = "") ,sheet = "Fieldbook")
  }
})

output$ui.action <- renderUI({
  if (is.null(fb_data())) return()
  actionButton("action", "Calculate your Fieldbook Variables")
})

output$independents <- renderUI({
  df <- fb_data()
  if (is.null(df)) return(NULL)
  items=names(df)
  names(items)=items
  #selectInput("independents","Select the Variables of your fieldbook:",choices = items,selected = items,multiple=TRUE)
  #selectInput("independents","Select the Variables to summaryze",items,items,multiple=TRUE,selectize = TRUE)
  selectInput("independents","Select the Variables to summaryze",items,multiple=TRUE,selectize = TRUE)
  })

# output$ui_action_sum <- renderUI({
#   if (is.null(fb_data())) return()
#   actionButton("action_sum", "Choose Variables to Sumarize:")
# })


# output$dataviewer <- renderTable({
#   if(is.null(fb_data)){iris}
#    if(!is.null(fb_data)){
#   fb_data()
#    }
# })

output$dataviewer <- renderDataTable({
  if (is.null(input$action)) return()
  if (input$action==0) return()
  #isolate({ #begin isolate
  
  if(is.null(fb_data)){iris}
  if(!is.null(fb_data)){
    
    #if(is.null(input$independents)) fb_data()
    #if(!is.null(input$independents)) fb_data()[,input$independents]
    fb_data()
  }
 
  
  
  
 # })#end isolate
  
})


# use DT to add dplyr - server side code
# output$dataviewer <- DT::renderDataTable({
# 
#   if(input$view_vars %>% not_available) return()
#   select_(.getdata(), .dots = input$view_vars) %>%
#   DT::datatable(., server = TRUE)
# })
# 
# options = list(orderClasses = TRUE, caseInsensitive = TRUE,
#   lengthMenu = list(c(10, 25, 50, -1),c('10','25','50','All')),
#   pageLength = 10, search = list(regex = TRUE)))
# 
# fin de nuevo codigo