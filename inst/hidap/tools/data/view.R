#############################################
# View table output of the selected dataset
#############################################

vars <- reactive({
  validate(
    need(!is.null(varnames()), "No dataset chosen!")
  )
  get(varnames())
})

output$uiView_vars <- renderUI({
  vars <- varnames()
  #vars <- vars()
  #print(vars)
  selectInput("view_vars", "Select variables to show:", choices  = vars,
    selected = state_multiple("view_vars",vars, vars), multiple = TRUE,
    selectize = FALSE, size = min(15, length(vars)))
})

#observe(output$uiView_vars)({
output$ui_View <- renderUI({
  list(
    wellPanel(
      uiOutput("uiView_vars")
    ),
    help_modal('View','viewHelp',inclMD(file.path("..",app_dir,"tools/help/view.md")))
)}
)

output$dataviewer <- renderDataTable({

  if(input$view_vars %>% not_available) return()
  select_(.getdata(), .dots = input$view_vars)

}, options = list(orderClasses = TRUE, caseInsensitive = TRUE,
  lengthMenu = list(c(10, 25, 50, -1),c('10','25','50','All')),
  pageLength = 10, search = list(regex = TRUE)))


# use DT to add dplyr - server side code
# output$dataviewer <- DT::renderDataTable({

#   if(input$view_vars %>% not_available) return()
#   select_(.getdata(), .dots = input$view_vars) %>%
#   DT::datatable(., server = TRUE)
# })

# options = list(orderClasses = TRUE, caseInsensitive = TRUE,
#   lengthMenu = list(c(10, 25, 50, -1),c('10','25','50','All')),
#   pageLength = 10, search = list(regex = TRUE)))
