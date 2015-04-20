library("readxl")

v <- reactiveValues(msg = "")

locs <- "inst/hidap/tools/dashboard/Master-list-trial-sites.xlsx"
locs <- "../hidap/tools/dashboard/Master-list-trial-sites.xlsx"
locs <- readxl::read_excel(locs,1)
locs$LATD <- as.numeric(locs$LATD)
locs$LOND <- as.numeric(locs$LOND)
lng1 <- min(locs$LOND)
lng2 <- max(locs$LOND)
lat1 <- min(locs$LATD)
lat2 <- max(locs$LATD)


nvar <- abs(round(rnorm(length(LOND))*100, 0)) + 10
locs <- cbind(locs, nvar)

observeEvent(input$map1_marker_click, {
  v$msg <- input$map1_marker_click
})

output$hist_nvar <- renderPlot({
  data <- locsInBounds()$nvar
  x <- 1:length(data)
  if(length(x) != length(data)) return(NULL)
  #plot(x, data)
  hist(data)
})

output$dot_yield <- renderPlot({
  db <- length(locsInBounds()$nvar)
  yvar <- rpois(10, db)
  nmsv <- paste("Clon", 10:19)
  dta <- as.data.frame(cbind(nmsv, yvar))
  
  lattice::dotplot(nmsv ~ yvar, data = dta, horizontal=TRUE)
})

output$site_desc <- renderUI({
  rec <- subset(locs,
                LATD == as.numeric(v$msg[2]) & LOND == as.numeric(v$msg[3]))
  #paste(v$msg[2],"/", v$msg[3])
  if(nrow(rec) != 1) return("")
  #paste(rec, collapse - ", ")
  x <- ""
  y <- paste(names(rec),": ", rec, "<br/>", sep="")
  z <- ""
  el <- div(HTML(y))
  #cat(as.character(el))
  el
})

output$progressBox <- renderInfoBox({
  infoBox(
    "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
    color = "purple"
  )
})

locsInBounds <- reactive({
  if (is.null(input$map1_bounds))
    return(locs[FALSE,])
  bounds <- input$map1_bounds
  latRng <- range(bounds$north, bounds$south)
  lngRng <- range(bounds$east, bounds$west)
  #print(bounds)
  subset(locs,
           LATD >= latRng[1] & LATD <= latRng[2] &
           LOND >= lngRng[1] & LOND <= lngRng[2])
})

# observe({
#   bounds <- input$map1_bounds
#   print(input$map1_bounds)
#   print(range(bounds$north, bounds$south) )
#   print(range(bounds$east, bounds$west) )
#   #print(locsInBounds())  
# })
# 


output$map1 <- renderLeaflet({
  m = leaflet(width = "50%") %>% addTiles()
  m  # a map with the default OSM tile layer
  
  m = m %>% fitBounds(lng1,lat1, lng2, lat2)
  m
  
  m %>% addMarkers(LOND, LATD,popup=locs$FULLN)
})

output$ui_dashboard <- renderUI({
  list(
    includeCSS("../hidap/www/shinydashboard.css"),
    includeScript("../hidap/www/shinydashboard.js"),
    
  fluidPage(
    
    # Application title
    #titlePanel("Hello Shiny!"),
   leafletOutput("map1"),
    
   fluidRow(
  
              # box(title = "Histogram", plotOutput("db_plot1")),
        #infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
        #infoBoxOutput("progressBox"),
        box(width = 4, title="Site",htmlOutput("site_desc")),
        box(width = 4, title="Yield across sites",plotOutput("hist_nvar", height = 250)),
        box(width = 4, title="Top ten varieties",plotOutput("dot_yield", height = 250))
        #box(width = 4, actionButton("count", "Increment progress")),
        
        
  )
  ))
}
)
    