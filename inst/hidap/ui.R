library(radiant)
app_title <- "HIDAP"

shinyUI(navbarPage(app_title, id = "nav_radiant", inverse = TRUE,
        collapsible = TRUE,
  
  #tabPanel("Dashboard", withMathJax(), uiOutput("ui_dashboard")), 
  #tabPanel("Dashboard", withMathJax(), uiOutput("ui_dashboard")),   book 
  tabPanel("Design", withMathJax(), uiOutput("doe"),icon = icon("book","fa-2x")),      
  #tabPanel("Design", uiOutput("doe")),      
  
  tabPanel("Data", withMathJax(), uiOutput('ui_data'),icon = icon("list-alt",class = "fa-2x")),
  #tabPanel("Data", uiOutput('ui_data')),icon("fa fa-table fa-2x")fa-medium glyphicon glyphicon-font
  navbarMenu("Analysis",
             shiny::tabPanel(" ANOVA",uiOutput("single_anova"),icon = icon("font",lib="glyphicon")),
             shiny::tabPanel("Main effects and Multiplicative Interaction ",uiOutput("ammi"),icon = icon("cubes","fa-1x")),
             shiny::tabPanel("Principal Component Analysis",icon = icon("arrows","fa-1x")),
             shiny::tabPanel("Single mean", uiOutput("single_mean")),
             shiny::tabPanel("GWAS", uiOutput("ui_gwas")), icon = icon("line-chart","fa-2x")),
             
  navbarMenu("R",
    tabPanel("Report", uiOutput("report")),
    tabPanel("Code", uiOutput("rcode"))
  ),
  
  tabPanel("Quit", uiOutput("savequit")),
  
  navbarMenu("Help",
    tabPanel(paste(app_title, "help"), uiOutput("help_base")),
    tabPanel(paste(app_title, "videos"), uiOutput("help_videos")),
    tabPanel("About", uiOutput("help_about"))
  ),
  
  tags$head(tags$link(rel="shortcut icon", href="imgs/icon.png"))
))

