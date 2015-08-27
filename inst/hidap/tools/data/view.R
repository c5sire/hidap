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
    uiOutput('ui.action'), 
    uiOutput('ui.exportfb.action'),
    #print(output$ui.action),# instead of conditionalPanel
    tags$hr(),
    #uiOutput("independents"),
    #tags$hr(),
#     uiOutput("ui_action_sum"), 
#     tags$hr(),
    #help_modal('View','viewHelp',inclMD(file.path("..",app_dir,"tools/help/view.md"))) 'Fieldbook'=='View'
    help_modal('Fieldbook','viewHelp',inclMD(file.path("..",app_dir,"tools/help/view.md")))
)})

fb_data <- reactive({
#vars <- fb_data
  #fb_file <- input$view_vars
#   print("===")
#   print(input$view_vars)
  #fb_file <- input$view_vars
  fb_file <- input$view_vars_input
  #print(fb_file)
  #str(fb_file)
  #print(fb_file$name)
  folder_file <- fb_file$name %>% gsub(pattern = "_.*","",.) %>% gsub(pattern = "[^0-9]*","",.)
  print(folder_file)
  
#   print("---2---")
#   print(input$view_vars)
#   print("---3---")
#   print(fb_file)
  #if(is.null(input$view_vars)){return()}
  #if(!is.null(fb_file)){
  if(is.null(input$view_vars_input)){return()}
  if(!is.null(fb_file)){
    #t <- paste(fb_file$datapath, ".xlsx", sep="")
    #str(t)
    #print(t)
    file.copy(fb_file$datapath,paste(fb_file$datapath, ".xlsx", sep=""))
    
    #fb_data <- read_excel(paste(fb_file$datapath, ".xlsx", sep=""), sheet = "Fieldbook")
    
    fieldbook <- readxl::read_excel(paste(fb_file$datapath, ".xlsx", sep=""), sheet = "Fieldbook") 
    inst <- readxl::read_excel(paste(fb_file$datapath, ".xlsx", sep=""),"Installation") # reverted to xlsx so all formulas are read as values
    mgt  <-  readxl::read_excel(paste(fb_file$datapath, ".xlsx", sep=""),"Crop_management")
    mtl	<-  readxl::read_excel(paste(fb_file$datapath, ".xlsx", sep=""),"Material List")
    mml	<-  readxl::read_excel(paste(fb_file$datapath, ".xlsx", sep=""),"Minimal") # reverted to xlsx so all formulas are read as values
    typ	<-  as.character(mml[mml$Factor=="Type of Trial","Value"])
    plot_size <- as.numeric(inst[stringr::str_detect(inst$Factor,"Plot size"),"Value"])
    plant_den <- as.numeric(inst[stringr::str_detect(inst$Factor,"Planting density"),"Value"])
      
    fb_data <-  sbformula::sbcalculate(fb = fieldbook, plot.size = plot_size,plant.den = plant_den)   
    #save(fb_data,file = "fieldbook.Rdata")
    fb_data
    
  #fb_data <- readxl::read_excel(fb_file <- paste(fb_file$datapath,".xls",sep = "") ,sheet = "Fieldbook")
  }
})

output$ui.action <- renderUI({
  if (is.null(fb_data())) return()
  actionButton("action", "Process your Fieldbook")
})

output$ui.exportfb.action <- renderUI({
  if (is.null(summary_dframe())) return()
  actionButton("exportfb_button", "Export Fieldbook")
})

shiny::observeEvent(input$exportfb_button, function(){
  isolate({ 
    #fp <-  "D:\\Users\\obenites\\Desktop\\Fieldbooks_Examples\\PTYL200211_CHIARA.xlsx"
    #fp <- "D:\\Users\\obenites\\Desktop\\Fieldbooks_Examples\\PTYL200211_CHIARA.xlsx"
    
    
    fb_file <- input$view_vars_input
    #print(input$view_vars_input)
    fb_file_name <- fb_file$name
    #print(fb_file)
    #str(fb_file)
    #print(fb_file$name)
    #folder_file <- fb_file$name %>% gsub(pattern = "_.*","",.) %>% gsub(pattern = "[^0-9]*","",.)
    #t <- paste(fb_file$datapath, ".xlsx", sep="")
    #print(input$view_vars_input)
    #str(input$view_vars_input)
    #print(fb_file_name)
    fb_folder_file <- folder_file(fb_file_name) 
    #print(fb_folder_file)
    fb_trial_abb_file <- trial_abb_file(fb_file_name)  
    #print(fb_trial_abb_file)
    
    folder_to <- folderPath("data")
    fb_temp_excel <- paste(fb_file$datapath,".xlsx",sep = "")
        
    dir_name <- file.path(folder_to,tolower(input$doe_type_crop),fb_folder_file,sep = "") 
    print(dir_name)
    to <- file.path(dir_name,fb_file_name,sep = "")
    print(to)
    fp <- to
    
    if(!file.exists(dir_name)) dir.create(dir_name,rec=T)
    
    #if(!file.exists(to)){
      
      
      file.copy(from= fb_temp_excel,to=fp)
    #}
 
    fieldbook2 <- fb_data()
    summaryfb <- summary_dframe()
    fbchecks <- fb_checks()
    #print(b)
    wb <- openxlsx::loadWorkbook(fp)
    sheets <- readxl::excel_sheets(path = fp)
    fieldbook <- readxl::read_excel(fp,sheet = "Fieldbook")
    fieldbook <- as.data.frame(fieldbook)
    print(sheets)
    
    if("Fieldbook" %in% sheets){    
    openxlsx::removeWorksheet(wb, "Fieldbook")
    print("ok-1")
    }
    if("Summary by clone" %in% sheets){    
    openxlsx::removeWorksheet(wb, "Summary by clone")
    print("ok-2")
    }
    if("Check Format" %in% sheets){
      openxlsx::removeWorksheet(wb, "Check Format")
      print("ok-3")
    }
    
    openxlsx::addWorksheet(wb = wb,sheetName = "Fieldbook",gridLines = TRUE)
    openxlsx::addWorksheet(wb = wb,sheetName = "Summary by clone",gridLines = TRUE)
    openxlsx::addWorksheet(wb = wb,sheetName = "Check Format",gridLines = TRUE)
    #openxlsx::writeData(wb, sheet = "Fieldbook2", x = fieldbook2)
    openxlsx::writeDataTable(wb,sheet = "Fieldbook",x = fieldbook2,colNames = TRUE,withFilter = FALSE)
    openxlsx::writeDataTable(wb,sheet = "Summary by clone",x = summaryfb ,colNames = TRUE,withFilter = FALSE)
    openxlsx::writeDataTable(wb,sheet = "Check Format",x = fbchecks ,colNames = TRUE,withFilter = FALSE)
    openxlsx::saveWorkbook(wb = wb,file = fp,overwrite = TRUE) 
    #if potato
    #fp12 <- "Z:\\hidap\\inst\\hidap\\ontologies\\ontologies_potato.xlsx"
    
    #
    crop <- input$doe_type_crop
    print(crop)
    crop_dict <- getResourcePath("dictionary",crop)
    print(crop_dict)
    
    
    datadict <- readxl::read_excel(crop_dict,sheet = "Template for submission",skip = 5)    
    nombres <- names(fieldbook)[4:ncol(fieldbook)]
    
    #Init ProgressBar
    withProgress(message = "Downloading fieldbook...", value=0, {
    
            
    lapply(nombres,function(x) por <- conditionalformat_trait(fp=fp,trait= x, datadict = datadict) )
          
    
    conditionalFormat_cipnumber(fp,sheet="Fieldbook",cip_colname = "INSTN")
    conditionalFormat_cipnumber(fp,sheet="Summary by clone",cip_colname = "INSTN")
    
    }) #Close progressBar
    
    shell.exec(fp)
    #}
    
    
    }) 
   
})


# output$independents <- renderUI({
#   df <- fb_data()
#   if (is.null(df)) return(NULL)
#   items=names(df)
#   names(items)=items
#   #selectInput("independents","Select the Variables of your fieldbook:",choices = items,selected = items,multiple=TRUE)
#   #selectInput("independents","Select the Variables to summaryze",items,items,multiple=TRUE,selectize = TRUE)
#   selectInput("independents","Select the Variables to summaryze",items,multiple=TRUE,selectize = TRUE)
#   })

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
  
    #Init ProgressBar
    withProgress(message = "Checking Fieldbook...", value=0, {
    
    #if(is.null(input$independents)) fb_data()
    #if(!is.null(input$independents)) fb_data()[,input$independents]
    fb_data()
    
    })
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