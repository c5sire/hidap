#Values of Statistical Designs Parameters
vals_design <- function(stat_design,nrep){
  
  if(design=="RCBD"){
    sdesign_name <- "Randomized Complete Block Design (RCBD)"
    #number_rep <- input$rcbd_r #in this case blocks will be reps  blocks=reps
    number_rep <- as.numeric(param_rep)
  }
  
  if(design=="CRD"){
    sdesign_name <- "Completely Randomized Design (CRD)"
    #number_rep <- input$crd_r
    number_rep <- as.numeric(param_rep)  
  }
     
  if(design=="LSD"){
    sdesign_name <- "Latin Square Design (LSD)"
    #number_rep <- input$lsd_r
    number_rep <- as.numeric(param_rep)
  }
  
  if(design=="ABD"){
    sdesign_name <- "Augmented Block Design (ABD)"
    number_rep <- as.numeric(param_rep)  #in this case blocks are reps blocks = reps
  }
  
  #if(design=="SPPD") #split plot design
  
 
# data_hidap[data_hidap$Factor=="Experimental design",col.name] <- paste(as.character(sdesign_name))
# data_hidap[data_hidap$Factor=="Number of repetitions or blocks",col.name] <- paste(as.character(number_rep))
 out <- list(sdesign=sdesign_name,nrep=number_rep)
 
}





