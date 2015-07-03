library(radiant)
app_title <- "HIDAP"

shinyUI(navbarPage(app_title, id = "nav_radiant", inverse = TRUE,
        collapsible = TRUE,
  
  #tabPanel("Dashboard", withMathJax(), uiOutput("ui_dashboard")), 
  #tabPanel("Dashboard", withMathJax(), uiOutput("ui_dashboard")),    
  tabPanel("Design", withMathJax(), uiOutput("doe")),      
  #tabPanel("Design", uiOutput("doe")),      
  
  tabPanel("Data", withMathJax(), uiOutput('ui_data')),
  #tabPanel("Data", uiOutput('ui_data')),
  navbarMenu("Analysis",
             tabPanel("ANOVA"),
             tabPanel("MET-MultiEnviormental Analysis Trial"),
             tabPanel("Principal Component Analysis"),
             tabPanel("Single mean", uiOutput("single_mean")),
             tabPanel("GWAS", uiOutput("ui_gwas"))
             
  ),
    
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