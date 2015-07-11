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



# output$ui.action <- renderUI({
#   if (is.null(fb_data())) return()
#   actionButton("action", "Calculate your Fieldbook Variables")
# })

output$independents <- renderUI({
  df <- fb_data()
  if (is.null(df)) return(NULL)
  items <- names(df)
  names(items) <- items
  #selectInput("independents","Select the Variables of your fieldbook:",choices = items,selected = items,multiple=TRUE)
  #selectInput("independents","Select the Variables to summaryze",items,items,multiple=TRUE,selectize = TRUE)
  selectInput("independents","Select the Variables to summaryze",items,multiple=TRUE,selectize = TRUE)
})

output$datasummary <- renderDataTable({
  if (is.null(input$action)) return()
  if (input$action==0) return()
  #isolate({ #begin isolate
 
  
  if(is.null(fb_data())){return()}
  if(!is.null(fb_data())){
    
    #if(is.null(input$independents)) fb_data()
    #if(!is.null(input$independents)) fb_data()[,input$independents]
    #fb_data()
    #sbformula::sbsummary(data = fb_data(),idx = "NMTP",groupfactors = "INSTN" ,na.rm = TRUE)
#     print(fb_data())
    col <- "NMTP"
#     print(fb_data()[,col])
#     fb_data()[,col]
    fb_data1 <- as.data.frame(fb_data())
    
    #resu<- calcula.split(vvv[,5]~INSTN+FACTOR,data=ndatos,length)[-3]
    resu <- sbformula::sbsummary(data = fb_data1,idx = 4,groupfactors = "INSTN" ,na.rm = TRUE)[1]
    for (i in 4:ncol(fb_data1)){
      #resu = merge(resu,calcula.split(datos,i))
      resu = merge(resu,sbformula::sbsummary(data = fb_data1,idx = i,groupfactors = "INSTN",na.rm = TRUE))
      } 
    
    resu

#     if(is.null(input$independents)) resu
#     if(!is.null(input$independents)){
#       
#       #var_sum <- input$independents
#       #var_sum_mean <- paste(input$independents,"_Mean",sep ="")
#       resu[, input$independents]
#       
#     } 



    #sbformula::sbsummary(data = fb_data(),idx = var,groupfactors = "INSTN" ,na.rm = TRUE)
    #sbformula::sbsummary(fb_data(),col,groupfactors = c("INSTN"),na.rm = TRUE)
  }
  
  # })#end isolate
  
})
