#library(htmltools)
# addUIDep <- function(x) {
#   jqueryUIDep <- htmlDependency("jqueryui", "1.10.4", c(href="shared/jqueryui/1.10.4"),
#                                 script = "jquery-ui.min.js",
#                                 stylesheet = "jquery-ui.min.css")
#   
#   attachDependencies(x, c(htmlDependencies(x), list(jqueryUIDep)))
# }

output$ui_Summary <- renderUI({
  list(
    wellPanel(
      #uiOutput("uiView_vars")
      # shiny::fileInput(inputId = "view_vars",label ="Select your fieldbook" ,accept = c(".xlsx",".xls"))  
    ),
    #uiOutput("dependent"),
    #uiOutput("independents"),
    #tags$hr(),
    #uiOutput('ui.action'), # instead of conditionalPanel
    #tags$hr(),
    uiOutput("independents"),
    #tags$hr(),
    #     uiOutput("ui_action_sum"), 
    #     tags$hr(),
    help_modal('View','viewHelp',inclMD(file.path("..",app_dir,"tools/help/view.md")))
  )})



# output$ui_action_sum <- renderUI({
#   if (is.null(fb_data())) return()
#   actionButton("action_sum", "Choose Variables to Sumarize:")
# })

# output$independents <- renderUI({
#   df <- fb_data()
#   if (is.null(df)) return(NULL)
#   items <- names(df)
#   names(items) <- items
#   #selectInput("independents","Select the Variables of your fieldbook:",choices = items,selected = items,multiple=TRUE)
#   #selectInput("independents","Select the Variables to summaryze",items,items,multiple=TRUE,selectize = TRUE)
#   selectInput("independents","Select the Variables to summaryze",items,multiple=TRUE,selectize = TRUE)
# })

summary_dframe <- reactive({
  
  if(is.null(fb_data())){return()}
  if(!is.null(fb_data())){
 
    fb_data1 <- as.data.frame(fb_data())
    fp12 <- "Z:\\hidap\\inst\\hidap\\ontologies\\ontologies_potato.xlsx" 
    datadict <- readxl::read_excel(fp12,sheet = "Template for submission",skip = 5)    
    datadict <- as.data.frame(datadict)   

    resu <- sbformula::sb_summary_excel(data = fb_data1,idx = 4,groupfactors = "INSTN",na.rm = TRUE,datadict = datadict)[1]  
    for (i in 4:ncol(fb_data1)){
      resu = merge(resu, sbformula::sb_summary_excel(data =fb_data1,idx = i,groupfactors = "INSTN",na.rm = TRUE,datadict = datadict))
    }  
    resu   
  }
    
})           
                           
output$independents <- renderUI({
  df <- fb_data()
  if (is.null(df)) return(NULL)
  items <- names(df)
  names(items) <- items
  selectizeInput("independents", label = "Select the Variables to summaryze",
                 #choices =  names(summary_dframe()),
                 choices = items,
                 selected = items, 
                 multiple = TRUE,
                 options = list(placeholder = 'Select Variables',
                                plugins = list('remove_button'))
  )

# addUIDep(selectizeInput("independents", label = "Select the Variables to summaryze",
#                         choices = items, 
#                         selected = items, 
#                         multiple = TRUE, 
#                         options = list(placeholder = 'Select Variables',
#                                        plugins = list("drag_drop", "remove_button"))))


})

output$datasummary <- renderDataTable({
  
  if (is.null(input$action)) return()
  if (input$action==0) return()
  #isolate({ #begin isolate
  #summary_dframe()
  #summary_dframe()
  sumvar <- input$independents

    refresh_sumvar <- unlist(lapply(sumvar, function(x) paste(x,c("_n","_Mean","_Mode","_sd"),sep ="")))
    #refresh_sumvar <- refresh_sumvar[refresh_sumvar %in% names(summary_dframe())]
    refresh_sumvar <- names(summary_dframe())[ names(summary_dframe()) %in% refresh_sumvar]
    refresh_sumvar  <- c("INSTN",refresh_sumvar)
    print(refresh_sumvar)

# #    print("===1====")
# #     print(refresh_sumvar)

   if(length(sumvar)==0){ summary_dframe()}
   if(length(sumvar)>0){ summary_dframe()[, refresh_sumvar, drop=FALSE]}

})
