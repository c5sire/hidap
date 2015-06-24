#source("../hidap/tools/design/doe.R")
#source("Z:\\hidap\\inst\\hidap\\data_sites.R")


doe_args <- as.list(formals(doe))
#cat("hi")
#print("doe_args")
# print(doe_args)#los parametros pasados para el libro de campo
save(doe_args,file = "doe.args.Rdata")

doe_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(doe_args)) {
    doe_args[[i]] <- input[[i]]
  }
  #print(doe_args)
  #if(!input$show_filter) doe_args$data_filter = ""
  #print second doe_args de doe_inputs
  #print(doe_args)
  doe_args
})

.summary_doe <- reactive({
#   if(not_available(input$doe_var))
#     return("This analysis requires a variable of type numeric or interval.\nIf none are available please select another dataset")
  #   if(is.na(input$ma_comp_value))
  #     return("Please choose a comparison value")
  out <- summary(.doe())
  out
})

.fieldbook_doe <- reactive({
  out <- fieldbook.doe(.doe())
  
  out
})

#.doe :: la funcion doe usa como argumento doe_inputs()
.doe <- reactive({
  do.call(doe, doe_inputs())
  
})

observe({
  if(not_pressed(input$doe_report)) return()
  isolate({
    outputs <- c("summary", "fb_draft", "plot")
    inp_out <- list(doe_plots = input$doe_plots) %>% list("",.)
    figs <- TRUE
    if(length(input$doe_plots) == 0) {
      figs <- FALSE
      outputs <- c("summary")
      inp_out <- list("","")
    }
    update_report(inp_main = clean_args(doe_inputs(), doe_args),
                  fun_name = "doe",
                  inp_out = inp_out,
                  outputs = outputs,
                  figs = figs,
                  fig.width = round(7 * doe_plot_width()/650,2),
                  fig.height = round(7 * doe_plot_height()/650,2))
   })
})

#var_list:: variable list extracted from Potato and Sweetpotato Ontology.

#var_list <- data_dictionary(fp)



# var_list <- data_dictionary(fp1)
# 



datasites <- data_sites()
lcountries <- list_countries(datasites)

###VISUAL INTERFACE

output$ui_doe <- renderTree({
  list(
    Cassava = "",
    Potato = list(
      DT = list(Stage1 = list(
        Trial_1 = "",
        Trial_2 = "",
        Trial_3 = ""
      ), 
      LB = list(Stage2 = list(
        Trial_1 ="",
        Trial_2 ="",
        Trial_3=""
      ))
      , stselected=TRUE)
    ),
    Sweetpotato = list(
      OF = ""
    )
  )
#     list(
#       cassava = "",
#       potato = list(
#         ProgramA = list(
#           StageA = list(
#             Trial1="",
#             Trial2="",
#             Trial3=""
#           ),
#           StageB = "", leaf3=""),
#         ProgramB = list(leafA = "", leafB = "")
#       )
#    )
})

output$ui_doe_par <- renderUI({
  choices <- c("Randomized Complete Block Design" = "RCBD",
               "Completely Randomized Design" = "CRD",
               "Latin Square Design" = "LSD",
               "Split-plot Design" = "SPPD",
               "Strip-plot Design" = "STPD",
               "Augmented Block Design" = "ABD",
               "Balanced Incomplete Block Design" = "BIBD",
               "Graeco-Latin Design" = "GLD",
               #"Youden Design" = "YD",
               #"Cyclic Design" = "CD",
               "Lattice Design" = "LD",
               "Alpha Design" = "AD",
               "Augmented Partially Replicated Design" = "APRD",
               "Factorial Design" = "F2SPPD"
               )
  selectInput("design", "Design method:", choices, multiple = FALSE)
})

# .doe_r_low <- reactive({
#   rl = 1
#   #if(input$design == "RCBD") rl = 2
#   rl
# })

# get_r_range <- function(doe) {
#   des <-doe$design
#   if(is.null(des)) des = "RCBD"
#   print(des)
#   if(des == "CRD") return(2:4)
#   if(des == "RCBD") return(2:10)
# }


output$fieldbook_doe <- renderDataTable(
  
   try(.fieldbook_doe())
  )

output$options_doe <- renderUI({
 #BEGIN tagList
  tagList(
#     conditionalPanel(condition = "input.tabs_doe == 'Plot'",
#                      wellPanel(
#                        selectizeInput(inputId = "doe_plots", label = "Select plots:",
#                                       choices = ma_plots,
#                                       selected = state_single("ma_plots", ma_plots, "hist"),
#                                       multiple = TRUE,
#                                       options = list(plugins = list('remove_button', 'drag_drop')))
#                      )
#     ),
    wellPanel(style = "background-color: #DAEFEF;",
      
      selectInput(inputId = "doe_type_crop", label = "Type of Crop", choices = c("Potato","Sweetpotato"),
                 multiple = FALSE ),
       
#       conditionalPanel(
#         condition = "input.doe_type_crop=='Potato'",
#            selectInput(inputId = "doe_template_pt",label = "Type of Trial (Template)",choices = c("Potato Healthy Tuber Yield"="PTYL"),
#                            selected = c("PTLY"), multiple = FALSE)     
#                
#       ),
# #       
#       conditionalPanel(
#         condition = "input.doe_type_crop=='Sweetpotato'",
#                selectInput(inputId = "doe_template_sp",label = "Type of Trial (Template)",choices = c("Sweetpotato Healthy Tuber Yield"="SPYL"),
#                            selected = c("SPYL"), multiple = FALSE)    
#         
#       ),
     selectInput(inputId = "doe_template",label = "Type of Trial (Template)", choices = ""),
                 
      
#       dateInput('doe_date',
#                 label = 'Date input: yyyy-mm-dd',startview = "year",
#                 value = Sys.Date()
#       ),
     
      dateRangeInput('doe_date',
                     label = 'Date range input: yyyy-mm-dd',
                     start = Sys.Date() - 2, end = Sys.Date() + 2,startview = "year"
      )
  ),    
     
wellPanel(style = "background-color: #F5F5DC;",
      
      shiny::selectInput(inputId = "CNTRY",label = "Country",choices =lcountries,selectize = TRUE),
       
      #shiny::selectInput(inputId = "doe_trialSite",label = "Localities",choices = c("Chinga","Ecunha","Humpata","Chibia")),      
      selectInput(inputId = 'doe_trialSite', label = 'Localities', choices = "",selectize = TRUE),
  
      shiny::selectInput(inputId = "doe_expenv",label = "Experimental Environment",choices = c("Field","Greenhouse","Laboratory"),selected = "Field"), 
      
     h5("File name preview",style = "font-family: 'Arial', cursive;font-weight: 500; line-height: 1.1; 
        color: #4d3a7d;"),
      
      verbatimTextOutput(outputId = "doe_full_fieldbook_name")  
    
    ), 
    
    
    wellPanel(
      h4("Define Genotypes",style = "font-family: 'Arial', cursive;
        font-weight: 500; line-height: 1.1; 
        color: #4d3a7d;"),
      br(),
      
      fluidRow(
        column(3,
             tags$textarea(id="doe_germ_txtarea", rows="10", style="width:300px;", "")),
      br(),
      
        column(6,offset = 3,
            fileInput(inputId = "doe_germ_inputfile",label = "Genotypes list"),
            #tableOutput("doe_germ_table")
            tableOutput("doe_germ_table")
            
            )
      )
  ),
    
    
    wellPanel(
      uiOutput("ui_doe_par"), ##SELECT THE STATISTICAL DESIGN
      
      checkboxInput("zigzag", "Zigzag:", TRUE),
      radioButtons("serie", "Label series:", 
                   #get_series_labels(), "101, 102, ...", #get_series_labels()[[2]], 
                   1:3, 2, 
                   inline = TRUE),
#       selectInput("trt", "Treatment (Germplasm)", get_germplasm_lists() , 
#                     multiple = FALSE),
      conditionalPanel(condition =  "input.design == 'CRD'",
        #radioButtons("r", "r:", 2:9, 2, inline = TRUE)
        wellPanel(
          selectInput("crd_r", "Number of Replication:", 2:1000, 3),
          checkboxInput("crd_first", "Randomize first block", FALSE), 
          checkboxInput("crd_continue", "Use continuous numeration", FALSE) 
        )
        
      ),
      conditionalPanel(condition =  "input.design == 'RCBD' ", 
        wellPanel(
        selectInput("rcbd_r", "Number of Replication:", 2:1000, 3),
        checkboxInput("rcbd_first", "Randomize first block", FALSE), 
        checkboxInput("rcbd_continue", "Use continuous numeration", FALSE) 
       )
      ),

      conditionalPanel(condition =  "input.design == 'LSD' ",
        wellPanel(                        
         selectInput("lsd_r", "Number of Replication:", 2:1000, 3),
         checkboxInput("lsd_first", "Randomize first block", FALSE) 
        )
      ),
      
        conditionalPanel(condition =  "input.design == 'ABD' ", 

            wellPanel(
             selectInput("abd_r", "Number of Replication:", 2:1000, 3),
#              checkboxInput("abd_first", "Randomize first block", FALSE), 
#              checkboxInput("abd_continue", "Use continuous numeration", FALSE),
             br(),
                                                      
               fluidRow(
                 column(3,
                  tags$textarea(id="abd_check_txtarea", rows="10", style="width:300px;", "")),
                   br(),
                   column(6,offset = 2,
                          fileInput(inputId = "abd_check_inputfile",label = "Check-Genotipes List")
                                    #tableOutput("doe_germ_table") 
                                    #tableOutput("doe_germ_table")                                
                       )
                 )                      
               )
        ),

      conditionalPanel(condition =  "input.design == 'GLD' ", 
#          selectInput("gld_trt2", "Treatment 2 (Germplasm)", get_germplasm_lists() , 
#                                            multiple = FALSE)
           wellPanel(
             selectInput("gld_r", "Number of Replication:", 2:1000, 3),
#              checkboxInput("gld_first", "Randomize first block", FALSE), 
#              checkboxInput("gld_continue", "Use continuous numeration", FALSE),
             br(),
             fluidRow(
               column(3,
                      tags$textarea(id="gld_check_txtarea", rows="10", style="width:300px;", "")),
               br(),
               column(6,offset = 2,
                      fileInput(inputId = "gld_check_inputfile",label = "Check-Genotipes List")
                      #tableOutput("doe_germ_table") 
                      #tableOutput("doe_germ_table")                                
               )
             )                       
           ) 
      ),
     conditionalPanel(condition = "input.design == 'SPPD'",
          
          wellPanel(
            selectInput("sppd_r", "Number of Replication:", 2:1000, 3),
            checkboxInput("sppd_first", "Randomize first block", FALSE), 
            checkboxInput("sppd_continue", "Use continuous numeration", FALSE),
            br(),
            
            fluidRow(
              column(3,
               selectInput(inputId = "sppd_stat_design",label = "Choose a statistical design",choices = 
                                c("Randomized Complete Block Design" = "rcbd",
                                 "Completely Randomized Design" = "crd",
                                 "Latin Square Design" = "lsd")),   
                     
               textInput(inputId = "sppd_factor_name", label = "Additional Factor Name",""),
               br(),
               textInput(inputId = "sppd_factor_lvl1", label = "First Level for Additional Factor",value = ""),
               textInput(inputId = "sppd_factor_lvl2", label = "Second Level for Additional Factor",value = ""),       
               textInput(inputId = "sppd_factor_lvl3", label = "Third Level for Additional Factor",value = "")      
                     )           
              )   
          )
      ),
###
        conditionalPanel(condition = "input.design == 'STPD'",
                         
                   wellPanel(
                   selectInput("stpd_r", "Number of Replication:", 2:1000, 3),
                   checkboxInput("stpd_first", "Randomize first block", FALSE), 
                   checkboxInput("stpd_continue", "Use continuous numeration", FALSE),
                   br(),
                           
                   fluidRow(
                   column(3,
                   textInput(inputId = "stpd_factor_name", label = "Additional Factor Name",""),
                   br(),
                   textInput(inputId = "stpd_factor_lvl1", label = "First Level for Additional Factor",value = ""),
                   textInput(inputId = "stpd_factor_lvl2", label = "Second Level for Additional Factor",value = ""),       
                   textInput(inputId = "stpd_factor_lvl3", label = "Third Level for Additional Factor",value = "")      
                       )           
                   )   
             )
        ),
###
conditionalPanel(condition =  "input.design == 'BIBD' ", 
                 wellPanel(
                   selectInput("bibd_r", "Number of Replication:", 2:1000, 3)
#                    checkboxInput("bibd_first", "Randomize first block", FALSE), 
#                    checkboxInput("bibd_continue", "Use continuous numeration", FALSE) 
                 )
),



###    
      conditionalPanel(condition =  "input.design == 'YD' ", 
         wellPanel(
           selectInput("yd_r", "r:", 2:11, 2),
           checkboxInput("yd_first", "Randomize first block", TRUE)
         )
      ),
#       conditionalPanel(condition =  "input.design == 'BIB' ", 
#          selectInput("bib_r", "r:", 4:30, 4)
#       ),
      conditionalPanel(condition =  "input.design == 'CD' ", 
         selectInput("cd_r", "r:", 6:30, 6)
      ),
      conditionalPanel(condition =  "input.design == 'CD' ", 
         selectInput("cd_k", "k:", 2:10, 2)
      ),
      conditionalPanel(condition =  "input.design == 'CD' ", 
         checkboxInput("rowcol", "Row or column", FALSE)
      ),
      conditionalPanel(condition =  "input.design == 'LD' ", 
         selectInput("ld_r", "r:", 2:3, 2)
      ),
      conditionalPanel(condition =  "input.design == 'AD' ", 
         selectInput("ad_r", "r:", 2:4, 2)
      ),
      conditionalPanel(condition =  "input.design == 'AD' ", 
         selectInput("ad_k", "k:", 2:30, 2)
      ),

#       wellPanel(
#       selectInput("abd_r", "r:", 2:1000, 3),
#       checkboxInput("abd_first", "Randomize first block", FALSE), 
#       checkboxInput("abd_continue", "Use continuous numeration", FALSE),
#       br(),
#                                                       
#       fluidRow(
#                column(3,
#                tags$textarea(id="abd_check_txtarea", rows="10", style="width:300px;", "")),
#                br(),
#                       
#                 column(6,offset = 2,
#                                     fileInput(inputId = "abd_check_inputfile",label = "Check-Genotipes List")
#                                     #tableOutput("doe_germ_table") 
#                                     #tableOutput("doe_germ_table")                                
#                              )
#                            )                      
#             )
        #),

      conditionalPanel(condition =  "input.design == 'GLD' ", 
         selectInput("gld_trt2", "Treatment 2 (Germplasm)", get_germplasm_lists() , 
                                           multiple = FALSE)

      )
      
      
        
#       conditionalPanel(condition =  "input.design == 'YD' ", 
#          wellPanel(
#            selectInput("yd_r", "r:", 2:11, 2),
#            checkboxInput("yd_first", "Randomize first block", TRUE)
#          )
#       ),
#       conditionalPanel(condition =  "input.design == 'BIB' ", 
#          selectInput("bib_k", "k:", 4:30, 4)
#       ),
#       conditionalPanel(condition =  "input.design == 'CD' ", 
#          selectInput("cd_r", "r:", 6:30, 6)
#       ),
#       conditionalPanel(condition =  "input.design == 'CD' ", 
#          selectInput("cd_k", "k:", 2:10, 2)
#       ),
#       conditionalPanel(condition =  "input.design == 'CD' ", 
#          checkboxInput("rowcol", "Row or column", FALSE)
#       ),
#       conditionalPanel(condition =  "input.design == 'LD' ", 
#          selectInput("ld_r", "r:", 2:3, 2)
#       ),
#       conditionalPanel(condition =  "input.design == 'AD' ", 
#          selectInput("ad_r", "r:", 2:4, 2)
#       ),
#       conditionalPanel(condition =  "input.design == 'AD' ", 
#          selectInput("ad_k", "k:", 2:30, 2)
#       )
       
    )
    
      #     ,
#     help_and_report(modal_title = 'Experimental design', fun_name = 'doe',
#                     help_file = inclMD(file.path("..",app_dir,"tools","help",
#                                                  "my_analysis.md"))
#     )
#END tagList
  )
})


output$fb_variables_doe <- renderUI({
  tagList(
    
     
    conditionalPanel(#begin conditional panel
     # condition = "input.crop_type=='Potato'",
       condition = "input.doe_type_crop=='Potato'",
#          wellPanel(style = "background-color: #99CC99;",
#            checkboxGroupInput("vars_doe_pt",label = "Fiedlbook Variables" ,
#                     #choices = list("Number of Plants Harvested" = "PPH", "Number of Plants Planted" = "NPP", "Choice 3" = 3),
#                     choices = var_list,
#                     selected = "") #var_list[1]
#               ),
wellPanel(style = "background-color: #99CC99 ;", 
   shinyBS::bsCollapse(id = "collapseExample", open = c("Healthy Tuber and Yield"),
                shinyBS::bsCollapsePanel("Healthy Tuber and Yield", "This is a panel with just text ",
                               "and has the default style. You can change the style in ",
                               "the sidebar.", 
                              checkboxGroupInput("vars_doe_pt",label = "Fiedlbook Variables",
                              #choices = list("Number of Plants Harvested" = "PPH", "Number of Plants Planted" = "NPP", "Choice 3" = 3),
                              #choices = var_list, selected = ""),
                              choices = var_list(crop="Potato"), selected = ""),
                              style = "info")
                )

 )

    ), #end conditional panel
    
    conditionalPanel(
      #condition = "input.crop_type=='Sweetpotato'",
      condition = "input.doe_type_crop=='Sweetpotato'",
#           wellPanel(style = "background-color: #99CC99;",
#             checkboxGroupInput("vars_doe_sp",label = "Fiedlbook Variables" ,
#                                choices = list("Number of Plants Harvested" = "PPH", "Number of Plants Planted" = "NPP", "Choice 3" = 3),
#                                selected = ""))
#begin well panel
wellPanel(style = "background-color: #99CC99 ;", 
      shinyBS::bsCollapse(id = "collapseExample", open = c("Healthy Tuber and Yield"),
                    shinyBS::bsCollapsePanel("Healthy Tuber and Yield", "This is a panel with just text ",
                                             "and has the default style. You can change the style in ",
                                             "the sidebar.", 
                                checkboxGroupInput("vars_doe_sp",label = "Fiedlbook Variables",
                                #choices = list("Number of Plants Harvested" = "PPH", "Number of Plants Planted" = "NPP", "Choice 3" = 3),
                                #choices = list("Number of Plants Harvested" = "PPH", "Number of Plants Planted" = "NPP", "Choice 3" = 3), 
                                choices = var_list(crop="Sweetpotato"),selected = ""),
                                style = "info")
                         )
       )#end conditional panel
)
  )  
  
})


# output$tabed_doe <- renderRHandsontable({
#   DF <- .getdata()#.fieldbook_doe()
#   #if (!is.null(input$tabed_doe)) {
#   #setHot(DF)
#   rhandsontable(DF) %>%
#     hot_table(highlightCol = TRUE, highlightRow = TRUE ) %>%
#     hot_cols( fixedColumnsLeft = 3)
#   #}
# })


output$doe <- renderUI({
  sidebarLayout(
    sidebarPanel(
      shinyTree("ui_doe", checkbox = TRUE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        id = "tabs_doe",
        
        tabPanel("Options", uiOutput("options_doe"),icon = icon("fa fa-file-excel-o fa-2x"))
        ,
#         withProgress(message = 'Creating fieldbook', value = 0.1, {
#           tabPanel("Summary", verbatimTextOutput("summary_doe"))
#         })  ,
         
        tabPanel("Fieldbook Variables", uiOutput("fb_variables_doe"),icon = icon("fa fa-building-o fa-2x")), 
        withProgress(message = 'Creating fieldbook', value = 0.1, {  

        tabPanel("Fielbook draft", 
                 dataTableOutput("fieldbook_doe"),
                 downloadButton('downloadData', 'Download'),
                 actionButton("fieldbook_export_button_doe", "Click to export your fieldbook"),
                 p("Click on the procces"),               
                 icon = icon("fa fa-table fa-2x")
               
             )

#         tabPanel("Fielbook draft", dataTableOutput("fieldbook_doe"),icon = icon("fa fa-table fa-2x"))

          
        })
         
#         tabPanel("Table edit", rHandsontableOutput("tabed_doe"))
        
        #,
        #tabPanel("Plot", plotOutput("plot_my_analysis", height = "100%"))
     


      )
    )
    
  )
})


# output$doe <- renderUI({
#   crop_tree <- shinyTree("ui_doe", checkbox = TRUE)
#   
#   register_print_output("summary_doe", ".summary_doe")
#   #register_print_output("fieldbook_doe", ".fieldbook_doe")
# 
# #   register_plot_output("plot_my_analysis", ".plot_my_analysis",
# #                        height_fun = "ma_plot_height")
#   # two separate tabs
#   
#   doe_output_panels <- tabsetPanel(
#     id = "tabs_doe",
#     
#       tabPanel("Options", uiOutput("options_doe"))
#       ,
#     withProgress(message = 'Creating fieldbook', value = 0.1, {
#       tabPanel("Summary", verbatimTextOutput("summary_doe"))
#     })  ,
#     withProgress(message = 'Creating fieldbook', value = 0.1, {  
#       tabPanel("Fielbook draft", dataTableOutput("fieldbook_doe"))
#     })
#     #,
#     #tabPanel("Plot", plotOutput("plot_my_analysis", height = "100%"))
#   )
#   
#   # one output with components stacked
#   # sm_output_panels <- tagList(
#   # tabPanel("Summary", verbatimTextOutput("summary_single_mean")),
#   # tabPanel("Plot", plotOutput("plot_single_mean", height = "100%"))
#   # )
#   stat_tab_panel(menu = "Design",
#                  tool = "Design of experiments",
#                  tool_ui = crop_tree ,
#                  output_panels = doe_output_panels,
#                  data = NULL)
#   # add "data = NULL" if the app doesn't doesn't use data
#  
# })



