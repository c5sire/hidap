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
    help_modal('View','viewHelp',inclMD(file.path("..",app_dir,"tools/help/view.md")))
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

output$datacheckformat<- renderDataTable({
  
  if (is.null(input$action)) return()
  if (input$action==0) return()

  fp12 <- "Z:\\hidap\\inst\\hidap\\ontologies\\ontologies_potato.xlsx" 
  datadict <- readxl::read_excel(fp12,sheet = "Template for submission",skip = 5)    
  datadict <- as.data.frame(datadict)   
  fb_check <- as.data.frame(fb_data())
    #for (i in 4:ncol(fb_data1)){
      #resu = merge(resu, sbformula::sb_qualityvar(data = fb_data,trait = "NMTP",datadict = datadict))
    #} 
   print(fb_check)
   resu <- sbformula::sb_qualityvar(data = fb_check,trait = "MTWP",datadict = datadict)
   #resu  <- as.data.frame(resu)  
  #}

})
