
# # alternative hypothesis options
# sm_alt <- c("Two sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")
# sm_plots <- c("Histogram" = "hist", "Simulate" = "simulate")

output$single_anova <- renderUI({
  
  
  sidebarLayout(
    sidebarPanel(
      
      strong("Analysis: ANOVA"),
      hr(),
      shiny::fileInput(inputId = "aov_fbvars_input",label ="Select your fieldbook" ,accept = c(".xlsx",".xls")),
      hr(),
      strong("Statistical Design"),
      textOutput("aov.fb.design"),
      hr(),
      uiOutput("aov.fbtraits"),
      uiOutput("aov.fbgenotypes"),
      uiOutput("aov.fbrep"),
      uiOutput('ui.aov.run')
    ),
    
    mainPanel(
      )  
  )
 })

aov_fbdata <- reactive({
  fb_file <- input$aov_fbvars_input
 
  if(is.null(input$aov_fbvars_input)){return()}
  if(!is.null(fb_file)){
    #t <- paste(fb_file$datapath, ".xlsx", sep="")
    #str(t)
    #print(t)
    file.copy(fb_file$datapath,paste(fb_file$datapath, ".xlsx", sep=""))
    
    
    fieldbook <- readxl::read_excel(paste(fb_file$datapath, ".xlsx", sep=""), sheet = "Fieldbook")
    fieldbook <- as.data.frame(fieldbook)
    
    design <- get.fb.param(paste(fb_file$datapath, ".xlsx", sep=""),"Installation","Experimental design")  
    
    list(fieldbook=fieldbook,design=design)
    
    #print(fieldbook)
  }
})

aov_fbvars <- reactive({
  df <- aov_fbdata()$fieldbook
  #if (is.null(df)) return(NULL)
  if (is.null(df)) return(NULL)
  items <- names(df)
  names(items) <- items
  
})

output$aov.fb.design <- renderText({ 
  aov_fbdata()$design
})

output$aov.fbtraits <- renderUI({
  items <- aov_fbvars()
  selectizeInput("aov_fb_trait", label = "Trait",
                 #choices =  names(summary_dframe()),
                 choices = items,
                 selected = "", 
                 multiple = TRUE,
                 options = list(placeholder = 'Select trait(s)',
                                plugins = list('remove_button'))
  )

})

output$aov.fbgenotypes <- renderUI({
   items <- aov_fbvars()
  selectizeInput("aov_fb_genotypes", label = "Genotypes",
                 #choices =  names(summary_dframe()),
                 choices = "INSTN",
                 selected = "", 
                 multiple = TRUE,
                 options = list(placeholder = 'Select genotypes',
                                plugins = list('remove_button'))
  )
})

output$aov.fbrep <- renderUI({
  items <- aov_fbvars()
  selectizeInput("aov_fb_rep", label = "Repetitions",
                 #choices =  names(summary_dframe()),
                 choices = "REP",
                 selected = "", 
                 multiple = TRUE,
                 options = list(placeholder = 'Select Repetition Column',
                                plugins = list('remove_button'))
  )
})


output$ui.aov.run <- renderUI({
  #if (is.null(fb_data())) return()
  actionButton("aov_run", "Run ANOVA")
})
