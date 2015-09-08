
output$ammi <- renderUI({
  
  
  sidebarLayout(
    sidebarPanel(
      
      strong("Analysis: Multi-Environment"),
      hr(),
      #shiny::fileInput(inputId = "ammi_fbvars_input",label ="Select your fieldbook" ,multiple = TRUE,accept = c(".xlsx",".xls")),
      selectizeInput("ammi_fb_crops", label = "Crop",
                      #choices =  names(summary_dframe()),
                       choices = c("potato","sweetpotato"),
                       selected = "potato", 
                       multiple = FALSE,
                       options = list(placeholder = 'Select crop',
                                      plugins = list('remove_button'))
        ),       
      hr(),
      #strong("Statistical Design"),
      #textOutput("ammi.fb.design"),
      hr(),
      #uiOutput("ammi.fbcrops"),
      uiOutput("ammi.fbtrials"),
      uiOutput("ammi.fbbooks"),
      uiOutput("ammi.fbtraits"), 
      uiOutput('ui.ammi.run'),
      uiOutput('ammi.export.action')
    ),
    
    mainPanel(
        # ,
      tabsetPanel(id = "ammi_tab_id",
        tabPanel("Combine Books", shiny::dataTableOutput("ammiviewer")),
        tabPanel("AMMI Analysis", verbatimTextOutput("ammi.report"))
        #tabPanel("AMMI Plot", tableOutput("ammi_plot"))
      )
      
    )  
  )
})

output$ammi.fbtrials <- renderUI({
  
   crop <- input$ammi_fb_crops
   trial_choices <- croptrials_name(crop)
   #print(trial_choices)
   selectizeInput("ammi_fb_trials", label = "Type of trial",
                 #choices =  names(summary_dframe()),
                 choices = trial_choices,
                 selected = "", 
                 multiple = FALSE,
                 options = list(placeholder = 'Select trial',
                                plugins = list('remove_button'))
  )
  
  #print(input$ammi_fb_trials)
})

output$ammi.fbbooks <- renderUI({
  
  crop <- input$ammi_fb_crops
  crop_folder <- file.path(folderPath("data"),crop)
  trial <- input$ammi_fb_trials 
  cropfiles_list <- list.files(path = crop_folder,pattern = trial,
                               full.names = FALSE,recursive = TRUE,)
  print(cropfiles_list)
  cropfiles_list2 <- list.files(path = crop_folder,pattern = trial,
                               full.names = TRUE,recursive = TRUE,include.dirs = FALSE)
  print(cropfiles_list2)
    
  selectizeInput("ammi_fb_books", label = "Fieldbooks for ammi",
                 #choices =  names(summary_dframe()),
                 choices = cropfiles_list,
                 selected = "", 
                 multiple = TRUE,
                 options = list(placeholder = 'Select fieldbook (minimum 3 books)',
                                plugins = list('remove_button'))
  )
})

ammi_data <- reactive({  
  
  #Values from SHINY widgets.
  crop <- input$ammi_fb_crops
  select_books <- input$ammi_fb_books
  
  #selecting fieldbook from folder files.
  select_books_pattern <- paste(select_books,collapse = "|")
  crop_folder <- file.path(folderPath("data"),crop)
  cropfiles_list <- list.files(path = crop_folder,
                               full.names = TRUE,recursive = TRUE) 
  cropfiles_list <- cropfiles_list[str_detect(cropfiles_list,pattern = select_books_pattern)]
  
  #to extract ammiadata from filenames
  ammiafiles_list <- list.files(path = crop_folder,
                               full.names = FALSE,recursive = TRUE) 
  
  n <- length(cropfiles_list)
  combine <- list()
  
  for(i in 1:n){  
    
    combine[[i]] <- readxl::read_excel(cropfiles_list[i], sheet = "Fieldbook")        
    ENV <- getfilename_book(ammiafiles_list[i])
    YEAR <- getdate_file(ENV)$year
    MONTH <- getdate_file(ENV)$month
    LOCATION <- getlocation_file(ENV)
    combine[[i]] <- cbind(ENV,YEAR,MONTH,LOCATION,combine[[i]])
  } 
  
  join_books <- data.table::rbindlist(combine)
  join_books <- as.data.frame(join_books)
  join_books    
  #output <- st4gi::ammi(trait = "MTYA", geno = "INSTN", env = "LOCATION", rep = "REP", data = comb_books)
  #print(join_books)
  
})

output$ammiviewer <- renderDataTable({
  #if (is.null(input$ammi_fb_books)) return()
  if (is.null(input$ammi_run)) return()
  if (input$ammi_run==0) return()
  #Init ProgressBar
    withProgress(message = "Checking Fieldbook...", value=0, {
      ammi_data()
      
    })
  #}
})

ammi_fbvars <- reactive({
#  if(is.null(ammi_data()) return()
  df <- ammi_data()
  #if (is.null(df)) return(NULL)
  if (is.null(df)) return(NULL)
  items <- names(df)
  names(items) <- items
  out_item <- c("ENV","YEAR","MONTH","LOCATION","PLOT","REP","INSTN")
  items <- items[!(items %in% out_item)]
})

output$ammi.fbtraits <- renderUI({
  
  if(is.null(input$ammi_fb_books)) return()
  items <- ammi_fbvars()
  #if(is.null(items)) return()
  selectizeInput("ammi_fb_trait", label = "Trait",
                 #choices =  names(summary_dframe()),
                 choices = items,
                 selected = "", 
                 multiple = TRUE,
                 options = list(placeholder = 'Select trait(s)',
                                plugins = list('remove_button'))
  )
  
})

output$ammi.report  <-  reactivePrint(function() {
#        traits <- input$ammi_fb_trait
#        print(traits)
  
  #     
    if (is.null(input$ammi_run) & is.null(input$ammi_fb_trait)) return()
    if (input$ammi_run==0 & is.null(input$ammi_fb_trait)) return()
    traits <- input$ammi_fb_trait
    print(traits)
    #if (is.null(ammi_data()) & is.null(input$ammi_fb_trait)) return()
    ammi_data1 <- ammi_data()
    #print(ammi_data)
    #ammmi_data
    ammi_output_data <- list()
    
#     for(i in 1:length(traits)){
#       ammi_output_data[[i]] <- st4gi::ammi(trait = traits[i] , geno = "INSTN", env = "LOCATION", rep = "REP", data = ammi_data1)
# #       names(ammi_output_data[[i]]) <- traits[i]
# #       print(ammi_output_data[[i]])
#     }  
  
  })


#Buttons 

output$ui.ammi.run <- renderUI({
  if (is.null(input$ammi_fb_books)) return()
  actionButton("ammi_run", "Run AMMI")
})

output$ammi.export.action <- renderUI({
  if(length(input$ammi_fb_books)>0){
    actionButton("ammi_export_button", "Export AMMI Analysis")
  }
})

# shiny::observeEvent(input$ammi_export_button, function(){
#   isolate({ 
# 
#   crop <- input$ammi_fb_crops
#   select_books <- input$ammi_fb_books
#   
#   #matching the filnames selected with shiny widgets
#    select_books_pattern <- paste(select_books,collapse = "|")
#    #print(select_books_pattern)
#    crop_folder <- file.path(folderPath("data"),crop)
#    #print(crop_folder)
#    cropfiles_list <- list.files(path = crop_folder,
#                                full.names = TRUE,recursive = TRUE) 
#    #print(cropfiles_list)
#    cropfiles_list <- cropfiles_list[str_detect(cropfiles_list,pattern = select_books_pattern)]
#    #print(cropfiles_list)
#    
#    ammiafiles_list <- list.files(path = crop_folder,
#                                full.names = FALSE,recursive = TRUE) 
#    print(ammiafiles_list)
#    
#    n <- length(cropfiles_list)
#    combine <- list()
#   
#        for(i in 1:n){  
#         
#          combine[[i]] <- readxl::read_excel(cropfiles_list[i], sheet = "Fieldbook")        
#          ENV <- getfilename_book(ammiafiles_list[i])
#          YEAR <- getdate_file(ENV)$year
#          MONTH <- getdate_file(ENV)$month
#          LOCATION <- getlocation_file(ENV)
#          combine[[i]] <- cbind(ENV,YEAR,MONTH,LOCATION,combine[[i]])
#        } 
#    
#    comb_data <- data.table::rbindlist(combine)
#    comb_data <- as.data.frame(comb_data)
#    str(comb_data) 
#   
#   # st4gi::ammi(trait = "MTYA", geno = "INSTN", env = "LOCATION", rep = "REP", data = join)
#   #st4gi::tai(trait = "MTYA",geno = "INSTN",env = "LOCATION",rep = "REP",data = join,maxp = 10,)
#   #openxlsx::write.xlsx(x = join,file = "combine_data_africa.xlsx")
#   #st4gi::tai(trait = "MTYA",geno = "INSTN",env = "LOCATION",rep = "REP",data = join,plot.img = TRUE)
#   st4gi::ammi(trait = "MTYA", geno = "INSTN", env = "LOCATION", rep = "REP", data = comb_data)
#   
#   })
# })



