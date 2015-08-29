
# # alternative hypothesis options
# sm_alt <- c("Two sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")
# sm_plots <- c("Histogram" = "hist", "Simulate" = "simulate")

output$met <- renderUI({
  
  
  sidebarLayout(
    sidebarPanel(
      
      strong("Analysis: Multi-Environment (MET)"),
      hr(),
      shiny::fileInput(inputId = "met_fbvars_input",label ="Select your fieldbook",multiple = TRUE,accept = c(".xlsx")),
      hr(),
      #strong("Statistical Design"),
      #strong("Statistical Design"),
      #textOutput("met.fb.design"),
      #hr(),
      #uiOutput("met.fbtraits"),
      #uiOutput("met.fbgenotypes"),
      #uiOutput("met.fbrep"),
      #uiOutput('ui.met.run'),
      uiOutput('met.export.action')
    ),
    
    mainPanel(
      #if (input$met_run==0) return()
      
      verbatimTextOutput('met.report')            
      
    )  
  )
})

met_fbdata <- reactive({
  

})

met_fbvars <- reactive({

  
})

output$met.fb.design <- renderText({ 

})

output$met.fbtraits <- renderUI({
 
  
})

output$met.fbgenotypes <- renderUI({

})

output$met.fbrep <- renderUI({
 
})

output$ui.met.run <- renderUI({

})

output$met.report  <-  reactivePrint(func = function(){
  
  p <- input$met_fbvars_input
  print(p)
  print(str(p))
  print(str(p))
  print(p$datapath)
  print(p$name)
 
})

output$met.export.action <- renderUI({
  if(is.null(input$met_fbvars_input)) return()
  actionButton("met_export_button", "Export Anova Analysis")
  
})

shiny::observeEvent(input$met_export_button, function(){
  isolate({ 
     
    met_file <- input$met_fbvars_input
    print(met_file)
    met_file_name <- met_file$name
    print(met_file_name)
    met_file_datapath <- met_file$datapath
    print(met_file_datapath)
    #### met parameters #####
    n <- length(met_file_name)
    fp <- list()
    for(i in 1:n){
      
      crop <- getcrop_file(met_file_name[i])
     
      met_folder_file <- getfolder_file(met_file_name[i]) 
     
      met_trial_abb_file <- gettrial_abb_file(met_file_name[i])  
     
      folder_to <- folderPath("data")
     
      met_temp_excel <- tempfile_name(met_file_datapath[i]) 
      
      met_folder_path <- folder_path(folder_to= folder_to,crop=crop,folder_file=met_folder_file[i])
             
      met_file_path <- new_file_path(folder_path=met_folder_path,file_name=met_file_name)
      
      ############
      
      fp[[i]] <- met_folder_path[i] #file point
      #print(fp)
      if(!file.exists(met_folder_path[i])) dir.create(met_folder_path[i],rec=T)    
      file.copy(from= met_temp_excel[i],to=fp[[i]])
      
     print(fp[[i]])
    }
    
        
    
    
  })
})