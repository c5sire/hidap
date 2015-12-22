################################################################
# Create dynamic reports using Radiant and the shinyAce editor
################################################################
rmd_example <- "
```{r setup,echo=FALSE, results='hide', message=FALSE}
library(datacheck)
library(xtable)
options(xtable.type = 'html')
```

A vignette for *datacheck* (version `r pkg_version('datacheck')`)
========================================================
Reinhard Simon, International Potato Center, Lima, Peru

The library *datacheck* provides some simple functions to check the consistency of a dataset. It assumes data are available in tabular format - typically a csv file with
objects or records in rows and attributes or variables in the columns.

In a database setting the variables would be controlled by the database - at least
conformance to types (character, numeric, etc) and allowed min/maximum values.
However, often data are gathered in simple spreadsheets or are for other reasons 
without such constraints. Here, data constraints like allowed types or values, expected
values and relationships can be defined using R commands and syntax. This allows much
more flexibility and fine grained control. Typically it demands also a lot of domain
knowledge from the user. It is therefore often useful to re-use such domain aware rule files across tables with similar content. Therefore this tool is foregiving if rules cannot be executed if a variable is not present in the table to be analyzed allowing the reuse of such rule files.

Using the HTML interface
-----------------------


Use the following commands to copy some example files to your current working directory (uncomment the file.copy command):
```{r}
atable = system.file('examples/soilsamples.csv', package='datacheck')
srules = system.file('examples/soil_rules.R', package='datacheck')

# Uncomment the next two lines

# file.copy(atable, 'soilsamples.csv')
# file.copy(srules, 'soil_rules.R')

```
Then type in the command *run_datacheck()* in the R editor.

Use the upload buttons to load the respective files in your working directory.
Review the results.


Using the command line interface
--------------------------------
Assuming you have copied the above mentioned files in your working directory proceed to
read in the data.

```{r message=FALSE, results='hide'}


atable = read.csv(atable, header = TRUE, stringsAsFactors = FALSE)
srules = read_rules(srules)
profil = datadict_profile(atable, srules)
```

You can inspect a graphical summary of rules per variable:

```{r fig.width=7, fig.height=6}
rule_coverage(profil)
```

The cumulative number of records with increasing scores.
```{r fig.width=7, fig.height=6}
score_sum(profil)
```

Or see the tables (only the first 20 records and first 6 columns shown):

```{r results = 'asis'}
xtable(atable[1:20, 1:6])
```

Similarly for the score table; however, this table contains also the total counts of scores by records and variables. In addition, the maximum score by variable.
```{r results='asis'}
ps = profil$scores
recs = c(1:10, nrow(ps)-1, nrow(ps))
cols = c(1:4,  ncol(ps))
xtable(ps[recs, cols])
```

A last visualization is a heatmap of the score table to organize similar records and similar rule profiles to help detect any patterns,

```{r echo=FALSE, fig.width=7, fig.height=8}
#filter out only records with less than maximum points
mp = max(ps$Record.score[nrow(ps)-2])

heatmap_quality(profil, scoreMax = mp)

```

Checking tables with data _inconsistencies_
--------------------------

For comparative purposes we purposely introduce a few errors in our table as below. We also exclude a rule on soil types for better display.
```{r message=FALSE, results='hide'}
atable$P[1]  = -100
atable$pH[11]= -200
srule1 = srules[-c(33),]
profil = datadict_profile(atable, srule1)
```

To get a better handle on the data it is always informative to review simple descriptive
summaries of the data. A custom summary function is included in the package to display this summary in tabular form:

```{r results='asis'}
xtable(short_summary(atable))
```

A summary of the results by rule can be seen from the profil object:

```{r results = 'asis'}
xtable(profil$checks)
```

The *checks* part lists all erroneous records in the last column for each rule. This may be too long for printing. To this end a custom print report function only displays the first n records where n=5 is the default. 

```{r message=FALSE, results = 'hide'}
atable$Sand[20:30] = -1
profil = datadict_profile(atable, srule1)
```

```{r results='asis'}

xtable(prep4rep(profil$checks))
```


Using rules that cannot be executed
----------------------------------
This may happen if the syntax is wrong. Another reason - particularly if re-using rule files across tables - maybe that a particular variable name is not present amongst the column names of the present table. The tool will just ignore it and report a 'failed' execution. Let us simply modify an existing rule as below:

```{r message = FALSE, results='hide'}
srule1$Variable[25] = 'caCO3'
srule1$Rule[25] = 'caCO3 >= 0'
profil = datadict_profile(atable, srule1)

```

Now let us just look at an excerpt of the results table:

```{r results = 'asis'}
xtable(prep4rep(profil$checks[20:30,]))
```

_End of tutorial_
"

knitr::opts_chunk$set(echo=FALSE, comment=NA, cache=FALSE, message=FALSE, warning=FALSE,
               fig.path = "~/radiant_figures/")
knitr::opts_knit$set(progress = TRUE)

output$report <- renderUI({

  list(
    actionButton("evalRmd", "Update"),
    downloadButton("saveHTML", "Save HTML"),
    downloadButton("saveRmd", "Save Rmd"),
    checkboxInput("manualPaste", "Manual paste",
                  value = state_init("manualPaste", FALSE)),
    fileInput("loadRmd", "", multiple=TRUE),

    div(class="row",
      div(class="col-xs-6",
        shinyAce::aceEditor("rmd_report", mode="markdown",
                  wordWrap = TRUE,
                  height = "600px",
                  selectionId = "rmd_selection",
                  value=state_init("rmd_report",rmd_example),
                  hotkeys=list(runKeyRmd=list(win="Ctrl-R|Ctrl-Shift-Enter", mac="CMD-ENTER|CMD-SHIFT-ENTER"))
                  )),
      div(class="col-xs-6", htmlOutput("rmd_knitDoc"))
    )
  )
})

valsRmd <- reactiveValues(knit = 0)

knitIt <- function(text) knitr::knit2html(text = text, quiet = TRUE, options=c("mathjax", "base64_images"),
                                          stylesheet = file.path("..",app_dir,"www/rmarkdown.css")) %>% HTML

# rmarkdown requires pandoc install
# knitIt <- function(text) rmarkdown::render(input = tmpfile(text))

knitIt2 <- function(text) paste(knitr::knit2html(text = text, fragment.only = TRUE, quiet = TRUE),
                                "<script type='text/javascript' src='https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'></script>",
                                "<script>MathJax.Hub.Typeset();</script>", sep = '\n') %>% HTML

observe({
  input$runKeyRmd
  if(!is.null(input$evalRmd)) isolate(valsRmd$knit <- valsRmd$knit + 1)
})

output$rmd_knitDoc <- renderUI({
  if(valsRmd$knit == 1) return()

  isolate({
    if(!running_local) {
      return(HTML("<h2>Rmd file is not evaluated when running Radiant on a server</h2>"))
    }
    if(input$rmd_report != "") {
      withProgress(message = 'Knitting report', value = 0, {
        # return(knitIt2(input$rmd_report))
        ifelse(is.null(input$rmd_selection) || input$rmd_selection == "",
               return(knitIt2(input$rmd_report)),
               return(knitIt2(input$rmd_selection)))
      })
    }
  })
})

output$saveHTML <- downloadHandler(
  filename = function() {"report.html"},
  content = function(file) {
    if(running_local) {
      isolate({
        # text <- input$rmd_report
        ifelse(is.null(input$rmd_selection) || input$rmd_selection == "",
               text <- input$rmd_report, text <- input$rmd_selection)
        knitIt(text) %>% cat(.,file=file,sep="\n")
      })
    }
  }
)

output$saveRmd <- downloadHandler(
  filename = function() {"report.Rmd"},
  content = function(file) {
    isolate({
      input$rmd_report %>% cat(.,file=file,sep="\n")
    })
  }
)

observe({
  # loading rmd report from disk
  inFile <- input$loadRmd
  if(!is.null(inFile) && !is.na(inFile)) {
    isolate({
      rmdfile <- paste0(readLines(inFile$datapath), collapse = "\n")
      shinyAce::updateAceEditor(session, "rmd_report", value = rmdfile)
    })
  }
})

# updating the report when called
update_report <- function(inp_main = "", fun_name = "", inp_out = list("",""), pre_cmd = "result <- ",
                          outputs = c("summary", "plot"),
                          figs = TRUE, fig.width = 7, fig.height = 7, xcmd = "") {

  cmd <- ""
  if(inp_main[1] != "") {
    cmd <- deparse(inp_main, control = c("keepNA"), width.cutoff = 500L) %>%
             sub("list", fun_name, .) %>%
             paste0(pre_cmd, .)
  }

  lout <- length(outputs)
  if(lout > 0) {
    for(i in 1:lout) {
      if(inp_out[i] != "" && length(inp_out[[i]]) > 0) {
        cmd <- deparse(inp_out[[i]], control = c("keepNA"), width.cutoff = 500L) %>%
                 sub("list\\(", paste0(outputs[i], "\\(result, "), .) %>%
                 paste0(cmd, "\n", .)
      } else {
        cmd <- paste0(cmd, "\n", outputs[i], "(result)")
      }
    }
  }

  if(xcmd != "") cmd <- paste0(cmd, "\n", xcmd)

  if(figs)
    cmd <- paste0("\n```{r fig.width=",fig.width,", fig.height=",fig.height,"}\n",cmd,"\n```\n")
  else
    cmd <- paste0("\n```{r}\n",cmd,"\n```\n")

  update_report_fun(cmd)
}

update_report_fun <- function(cmd) {

  if(!is.null(input$manualPaste) && input$manualPaste) {
    os_type <- Sys.info()["sysname"]
    if (os_type == 'Windows') {
      cat(cmd, file = "clipboard")
    } else if (os_type == "Darwin") {
      cat(cmd, file = pipe("pbcopy"))
    } else if (os_type == "Linux") {
      cat("Clipboard not supported on linux")
      # nothing yet
    }
    # by setting cmd to "" nothing is added to the report
    cmd <- ""
  }

  if(cmd != "") {
    if(is.null(input$rmd_report)) {
      if(is.null(r_state$rmd_report)) {
        r_state$rmd_report <<- cmd
      } else {
        r_state$rmd_report <<- paste0(r_state$rmd_report,"\n",cmd)
      }
    } else {
      shinyAce::updateAceEditor(session, "rmd_report",
                      value = paste0(input$rmd_report,"\n",cmd))
    }
  }

  # move to the report panel
  updateTabsetPanel(session, "nav_radiant", selected = "Report")
}


################################################################
# Run R-code within Radiant using the shinyAce editor
################################################################
r_example <- "# to get the currently active data
dat <- .getdata()

# show the first observations
head(dat)

# to access a specific dataset by name
dat <- r_data[['diamonds']]

# add a variable to the data used by Radiant
r_data[['diamonds']]$log.price <- log(dat$price)
dat <- r_data[['diamonds']]

# show the first observations
head(dat)

# plotting a figure
plot(1:10)

# run a regression
reg <- lm(price ~ carat + clarity, data = dat)
summary(reg)
"

output$rcode <- renderUI({
  div(class="row", div(class="col-xs-6",
    shinyAce::aceEditor("r_code", mode="r",
                        selectionId = "r_code_selection",
                        value=state_init("r_code",r_example),
                        hotkeys=list(runKeyCode=list(win="Ctrl-R|Ctrl-Shift-Enter", mac="CMD-ENTER|CMD-SHIFT-ENTER"))
              ),
    actionButton("rEval", "Run"),
    downloadButton('saveCode', 'Save R-code'), tags$br(), tags$br(),
    fileInput('loadCode', 'Load R-code', multiple=FALSE)
    #, fileInput('sourceCode', 'Source R-code', multiple=TRUE)
  ),
  div(class="col-xs-6", htmlOutput("rCodeEval"))
  )
})

valsCode <- reactiveValues(code = 0)

observe({
  input$runKeyCode
  if(!is.null(input$rEval)) isolate(valsCode$code <- valsCode$code + 1)
})

output$rCodeEval <- renderPrint({
  if(valsCode$code == 1) return()
  isolate({
    if(running_local) {
      if(is.null(input$r_code_selection) || input$r_code_selection == "") {
        r_code <- input$r_code
      } else {
        r_code <- input$r_code_selection
      }

      r_output <- paste0("```{r cache = FALSE, echo = TRUE}\n",r_code,"\n```")
      return(HTML(paste(knitr::knit2html(text = r_output, fragment.only = TRUE, quiet = TRUE),
             '<script>', 'MathJax.Hub.Typeset();', '</script>', sep = '\n')))
    } else {
      return(HTML("<h2>Code is not evaluated when running Radiant on a server</h2>"))
    }
  })
})

output$saveCode <- downloadHandler(
  filename = function() {"rcode.R"},
  content = function(file) {
    isolate({
      cat(input$r_code,file=file,sep="\n")
    })
  }
)

# loading r-code from disk
observe({
  inFile <- input$loadCode
  if(!is.null(inFile)) {
    isolate({
      paste0(readLines(inFile$datapath), collapse = "\n") %>%
        shinyAce::updateAceEditor(session, "r_code", value = .)
    })
  }
})

# source r-code from disk
# observe({
#   inFile <- input$sourceCode
#   isolate({
#     if(!is.null(inFile)) source(inFile$datapath)
#   })
# })
