library(radiant)
app_title <- "HIDAP"

shinyUI(navbarPage(app_title, id = "nav_radiant", inverse = TRUE,
        collapsible = TRUE,

  tabPanel("Data", withMathJax(), uiOutput('ui_data')),
  
  navbarMenu("Analysis",
             tabPanel("Single mean", uiOutput("single_mean")),
             tabPanel("My analysis", uiOutput("my_analysis"))
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
