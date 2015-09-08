library("radiant")

shinyServer(function(input, output, session) {

  # source shared functions
	source('init.R', local = TRUE)
	source('radiant.R', local = TRUE)
  source('renderInfoBox.R',local=TRUE) #added by Omar Benites
  source('data_dictionary.R',local=TRUE)#added by Omar Benites
  source('data_sites.R',local=TRUE)#added by Omar Benites
  source('utils.R',local=TRUE)#added by Omar Benites
  source('vals_params_design.R',local=TRUE)#added by Omar Benites
  
	# for shiny-server
 	if(!"package:radiant" %in% search()) {
	  for(file in list.files("../../R",
	      pattern="\\.(r|R)$",
	      full.names = TRUE)) {

	  	source(file, local = TRUE)
	  }
	}

	# source data & analysis tools
  for(file in list.files(c("tools/app","tools/data", "tools/analysis", 
                           "tools/design", "tools/dashboard","tools/utils"),
      pattern="\\.(r|R)$",
      full.names = TRUE)) {

  	source(file, local = TRUE)
  }
  
  
  
  #print(getwd())
  # save state on refresh or browser close
  saveStateOnRefresh(session)
})
