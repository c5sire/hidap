#renderInfoBox <- renderValueBox
renderInfoBox <- function (expr, env = parent.frame(), quoted = FALSE) 
{
  vbox_fun <- shiny::exprToFunction(expr, env, quoted)
  shiny::renderUI({
    vbox <- vbox_fun()
    tagAssert(vbox, type = "div")
    vbox$children[[1]]
  })
}
