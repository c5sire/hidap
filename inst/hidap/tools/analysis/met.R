
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
      uiOutput('ui.aov.run'),
      uiOutput('aov.export.action')
    ),
    
    mainPanel(
      #if (input$aov_run==0) return()
      
      verbatimTextOutput('aov.report')            
      
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
  out_item <- c("INSTN", "REP", "PLOT")
  items <- items[!(items %in% out_item)]
  
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
  
  trait <- input$aov_fb_trait
  genotypes <- input$aov_fb_genotypes
  rep <- input$aov_fb_rep  
  
  if(length(trait)==0 || length(genotypes)==0 || length(rep)==0 || is.null(aov_fbdata())) return()
  
  #if(is.null(trait) & is.null(genotypes) & is.null(fbdata)) return()
  
  actionButton("aov_run", "Run ANOVA")
})

output$aov.report  <-  reactivePrint(function() {
  
  if (is.null(input$aov_run)) return()
  if (input$aov_run==0) return()
  
  traits <- input$aov_fb_trait
  #print(input$aov_fb_trait)
  genotypes <-input$aov_fb_genotypes
  #   print(input$aov_fb_genotypes)
  rep <- input$aov_fb_rep
  #   print(input$aov_fb_rep)
  fbdata  <-  aov_fbdata()$fieldbook
  #   print(aov_fbdata)
  #   str(aov_fbdata)
  #   print(input$aov_run)
  #   
  if(is.null(traits) & is.null(genotypes) & is.null(fbdata)) return()
  
  #out <- capture.output(rcbd_anova(trait = "MTYNA",genotypes = "INSTN",repetitions = "REP",data = datos))
  #if(is.null(input$aov_run)) return()
  
  #lapply(traits, function(x) rcbd_anova(trait = x,genotypes = genotypes,repetitions = rep,data = fbdata))  
  aov_output_data <- list()
  for(i in 1:length(traits)){
    aov_output_data[[i]] <- capture.output(rcbd_anova(trait = traits[i],genotypes = genotypes,repetitions = rep,data = fbdata))
  }  
  names(aov_output_data) <- traits
  aov_output_data
  #capture.output(rcbd_anova(trait = traits,genotypes = genotypes,repetitions = rep,data = fbdata))
})

output$aov.export.action <- renderUI({
  trait <- input$aov_fb_trait
  #print(input$aov_fb_trait)
  genotypes <-input$aov_fb_genotypes
  rep <- input$aov_fb_rep
  fbdata  <-  aov_fbdata()$fieldbook
  if(length(trait)==0 || length(genotypes)==0 || length(rep)==0 || is.null(aov_fbdata())) return()
  actionButton("aov_export_button", "Export Anova Analysis")
})

shiny::observeEvent(input$aov_export_button, function(){
  isolate({ 
    fp <- "D:\\Users\\obenites\\Desktop\\Fieldbooks_Examples\\PTYL200211_CHIARA.xlsx"
  
    
    
  })
})