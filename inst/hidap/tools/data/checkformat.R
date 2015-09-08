output$ui_Check <- renderUI({
  list(
    wellPanel(
      #uiOutput("uiView_vars")
      # shiny::fileInput(inputId = "view_vars",label ="Select your fieldbook" ,accept = c(".xlsx",".xls"))  
    ),

    #uiOutput("independents"),
    #tags$hr(),
    #     uiOutput("ui_action_sum"), 
    #     tags$hr(),
    #help_modal('View','viewHelp',inclMD(file.path("..",app_dir,"tools/help/view.md")))
    help_modal('Fieldbook','viewHelp',inclMD(file.path("..",app_dir,"tools/help/view.md"))) 
    )})
           

# output$independents <- renderUI({
#   df <- fb_data()
#   if (is.null(df)) return(NULL)
#   items <- names(df)
#   names(items) <- items
#   selectizeInput("independents", label = "Select the Variables to summaryze",
#                  #choices =  names(summary_dframe()),
#                  choices = items,
#                  selected = items, 
#                  multiple = TRUE,
#                  options = list(placeholder = 'Select Variables',
#                                 plugins = list('remove_button'))
#   )
#     
# })

fb_checks <- reactive({
  
  fp12 <- "Z:\\hidap\\inst\\hidap\\ontologies\\ontologies_potato.xlsx" 
  datadict <- readxl::read_excel(fp12,sheet = "Template for submission",skip = 5)    
  datadict <- as.data.frame(datadict)   
  fb_check <- as.data.frame(fb_data())
  
  cipnumber_checks<- sbformula::sb_cipnumberquality(fb_check,"INSTN")
  check_table<- lapply(4:ncol(fb_check),function(i) as.data.frame(sbformula::sb_qualityvar(data = fb_check,trait = names(fb_check)[i],datadict = datadict)))  
  check_table <- as.data.frame(data.table::rbindlist(check_table)) 
  names(check_table) <- "Checks"
  print(check_table)
  
  check_general <- rbind(cipnumber_checks,check_table)
  
  
})


output$datacheckformat<- renderDataTable({
  
  if (is.null(input$action)) return()
  if (input$action==0) return()

#   fp12 <- "Z:\\hidap\\inst\\hidap\\ontologies\\ontologies_potato.xlsx" 
#   datadict <- readxl::read_excel(fp12,sheet = "Template for submission",skip = 5)    
#   datadict <- as.data.frame(datadict)   
#   fb_check <- as.data.frame(fb_data())
#     
#   cipnumber_checks<- sbformula::sb_cipnumberquality(fb_check,"INSTN")
#   
#   check_table<- lapply(4:ncol(fb_check),function(i) as.data.frame(sbformula::sb_qualityvar(data = fb_check,trait = names(fb_check)[i],datadict = datadict)))  
#   check_table <- as.data.frame(data.table::rbindlist(check_table)) 
#   names(check_table) <- "Checks"
#   print(check_table)
#   
#   check_general <- rbind(cipnumber_checks,check_table)
    fb_checks()
   
})
