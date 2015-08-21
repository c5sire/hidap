#load("../../../data/primes.rda")
#fp_sites <- "Z:\\hidap\\inst\\hidap\\sites\\Master-list-trial-sites.xlsx"
#fp_sites <- "Z:\\hidap\\inst\\hidap\\sites\\Master-list-trial-sites.xlsx"
#source("Z:\\hidap\\inst\\hidap\\data_sites.R")

################
library(agricolae)
library(stringr)

is_odd <- function(v){
  v %% 2 == 1
}

guess_k <- function(n) {
  n = as.integer(n)
  if(n == 0) return(2)
  #cat(n, "\n")
  k_upper <- sqrt(n)
  k_g <- round(k_upper,0)
  #print(k_g)
  res <- logical(k_g)
  
  for(k in 3:k_g) {
    #cat(n, "\n")
    #cat(k, "\n")
    if(n %% k ==0) res[k] <- TRUE
  }
  x <- 1:k_g
  r <- x[res]
  if(length(r)==0) return(NULL)
  r
}

valid_k <- function(trt, r = c(2,3,4), k){
  #cat(r)
  n <- length(trt)
  if(n < 8) return(FALSE)
  if(n %% k != 0 ) return(FALSE)
  s <- n / k
  
  if( (r == 2 & k <= s) == TRUE ) return(TRUE)
  
  if( (r == 3 & k <= s  & is_odd(s)) == TRUE ) return(TRUE)
  
  if( (r == 3 & (k <= s-1)  & !is_odd(s)) == TRUE) return(TRUE)
  
  if( (r == 4 & is_odd(s) & (s %% 3 !=0)) == TRUE) return(TRUE)
  
  FALSE
}

guess_k_by_r <- function(n){
  n = as.integer(n)
  gk <- guess_k(n)
  if(is.null(gk)) return(NULL)
  rs <- list()
  
  for(r in 2:4){
    rr <- logical(length(gk))
    for(i in 1:length(gk)){
      if(valid_k(1:n, r, gk[i])) rr[i] <- TRUE
    }
  
    re <- gk[rr]
    #print(re)
    if(length(re)==0) re = NULL
    rs[[r]] <- re
  }
#   if(length(r)==3){
#     names(rs)[1] = paste("n:",n)
#     names(rs)[2] = paste("r:",2)
#     names(rs)[3] = paste("r:",3)
#   }
  rs
}

get_vr <- function(n){
  # get valid repetitions
  n = length(n)
  x <- guess_k_by_r(n)
  y <- 1:length(x)
  z <- sapply(x, length)
  y[z>0]
}

get_vk <- function(n, r){
  n = length(n)
  r = as.integer(r)
  x <- guess_k_by_r(n)
  if(length(x)>0) {
    return(x[[r]])  
  } else {
    return(0)
  }
  
}

#designs = c("RCBD", "CRD", "LSD", "GLD","YD","BIB", "CD","LD","AD","ABD", "SPPD", "STPD", "F2SPPD")

doe <- function(design = "RCBD",# "CRD", "LSD", "GLD","YD","BIB", 
                           #"CD","LD","AD","ABD", "SPPD", "STPD", "F2SPPD"), 
                #trt = "A", trt2 = letters[1:8],
                trt = "A" ,trt2 = letters[1:8],
                r = 2, k = 2,
                rowcol = FALSE,
                name = "", 
                sub_design = "rcbd", #c("rcbd", "lsd", "crd"),
                serie = 1, zigzag = TRUE,
                seed = 0, kinds = "Super-Duper", 
                
                #crd_r=2 , crd_first = FALSE, crd_continue = FALSE,
                crd_r=2 ,crd_continue = FALSE,
                #rcbd_r=2, rcbd_first = FALSE, rcbd_continue = FALSE,
                rcbd_r=2, rcbd_continue = FALSE,
                                    
                #lsd_r=2, lsd_first = FALSE,
                lsd_r=2, 
                #abd_r=2, abd_first =FALSE, abd_continue = FALSE, 
                #abd_r=2, #abd_first =FALSE

                #abd_trt2= "A", abd_r=2, abd_first =FALSE, abd_continue = FALSE, #to run an example
                abd_trt2= "A", abd_r=2, abd_continue = FALSE,
                #abd_r=2, abd_first =FALSE, abd_continue = FALSE,
                
                gld_trt2 = "A",
                
                sppd_r=2, sppd_continue=FALSE,
                #sppd_r=2, sppd_first=FALSE, sppd_continue=FALSE, 
                sppd_stat_design="rcbd",sppd_factor_lvl1="level1",sppd_factor_lvl2="level2",sppd_factor_lvl3="level3",
                
                #stpd_r=2, stpd_first=FALSE, stpd_continue=FALSE, 
                stpd_r=2, stpd_continue=FALSE, 
                stpd_factor_lvl1="level1",stpd_factor_lvl2="level2",stpd_factor_lvl3="level3",
                
                bib_r= 3,
                
                #yd_r = 2, yd_first = FALSE,
                yd_r = 2,
               
                cd_k = 2, cd_r = 6,
                ld_r = 2,
                ad_r = 2, ad_k = 2
                ){
  out <- NULL
  
  r <- as.integer(r)
  k <- as.integer(k)
  #trt  <- get_germplasm_ids(trt)
  trt <- germlist()
  #trt2 <- get_germplasm_ids(trt2)
  #trt2 <- genochecks()
  first <- FALSE
  continue <- FALSE
  rowcol = as.logical(rowcol)
  
  if(design == "RCBD"){
    r <- as.integer(rcbd_r) 
    #first <- as.logical(rcbd_first)
    continue <- as.logical(rcbd_continue)
  }
  if(design == "CRD"){
    r <- as.integer(crd_r)
    #first <- as.logical(crd_first)
    continue <- as.logical(crd_continue)
  }
  if(design == "LSD"){
    r <- as.integer(lsd_r) 
    #first <- as.logical(lsd_first)
  }
 
  if(design == "ABD"){
#     trt <- trt2
#     trt2 <- trt
    trt2 <- genochecks()
    r <- as.integer(abd_r)
    #trt2 <- get_germplasm_ids(abd_trt2)
    #first <- as.logical(abd_first)
    continue <- as.logical(abd_continue)
  }
  
  if(design == "SPPD"){
    r <- as.integer(sppd_r)
    #first <- as.logical(sppd_first)
    continue <- as.logical(sppd_continue) 
#     
    sub_design <- as.character(sppd_stat_design)
    sppd_factor_lvl1 <- sppd_factor_lvl1 %>% as.character() %>% str_trim(.,side = "both")
    sppd_factor_lvl2 <- sppd_factor_lvl2 %>% as.character() %>% str_trim(.,side = "both")
    sppd_factor_lvl3 <- sppd_factor_lvl3 %>% as.character() %>% str_trim(.,side = "both")
    print(sppd_factor_lvl1)
    print(sppd_factor_lvl2)
    print(sppd_factor_lvl3)
    trt2 <- c(sppd_factor_lvl1,sppd_factor_lvl2,sppd_factor_lvl3)
    print(trt2)
    trt2 <- trt2[!is.na(trt2) & trt2!=""]
    print(trt2)
  }
###
  if(design == "STPD"){
      r <- as.integer(stpd_r)
      #first <- as.logical(stpd_first)
      continue <- as.logical(stpd_continue) 
      #     
      stpd_factor_lvl1 <- stpd_factor_lvl1 %>% as.character() %>% str_trim(.,side = "both")
      stpd_factor_lvl2 <- stpd_factor_lvl2 %>% as.character() %>% str_trim(.,side = "both")
      stpd_factor_lvl3 <- stpd_factor_lvl3 %>% as.character() %>% str_trim(.,side = "both")
      print(stpd_factor_lvl1)
      print(stpd_factor_lvl2)
      print(stpd_factor_lvl3)
      trt2 <- c(stpd_factor_lvl1,stpd_factor_lvl2,stpd_factor_lvl3)
      print(trt2)
      trt2 <- trt2[!is.na(trt2) & trt2!=""]
      print(trt2)
    }
  
  if(design == "BIBD"){
    r <- as.integer(bib_r)  
  }
###
  if(design == "GLD"){
    trt2 <- get_germplasm_ids(gld_trt2)
  }

  if(design == "YD"){
    r <- as.integer(yd_r) 
    #first <- as.logical(yd_first)
  }

  if(design == "CD"){
    k <- as.integer(cd_k)  
    r <- as.integer(cd_r)
  }

  if(design == "LD"){
    r <- as.integer(ld_r)
  }

  if(design == "AD"){
    k <- as.integer(ad_k)  
    r <- as.integer(ad_r)
  }

  zigzag <- as.logical(zigzag)
  rowcol <- as.logical(rowcol)
#  first <- as.logical(first)
  
  #print(serie)
  serie <- as.integer(serie)
   
  if(design == "CRD"){
    #out <- design.crd(trt, r, serie, seed, kinds)
    out <- design_crd(trt, r, serie, seed, kinds)
  }
  
  if(design == "RCBD"){
    out <- design_rcbd(trt, r, serie, seed, kinds, first = TRUE, continue)
    #out <- design_rcbd(trt, r, serie, seed, kinds, first, continue)
  }
  
  if(design == "LSD"){
    out <- design_lsd(trt, serie, seed, kinds, first=TRUE)
  }

  if(design == "ABD"){ #trt2::genotypes & trt:: genotypes
  out <- design_abd(trt2,trt, r, serie, seed, kinds)
  }
  
  if(design == "GLD"){
    out <- design_gld(trt, trt2, serie, seed, kinds)
  }

  if(design == "SPPD"){
    #out <- design.split(trt, trt2, r, sub_design, serie, seed, kinds, first)
    out <- design.split(trt, trt2, r, sub_design, serie, seed, kinds ,first = TRUE)
  }

  if(design == "STPD"){
    print(trt)
    print(trt2)
    print(r)
    print(serie)
    print(seed)
    print(kinds)
    #out <- design_split(trt, trt2, r, serie, seed, kinds)
    print(out)
    out <- design.strip(trt, trt2, r, serie, seed, kinds)
  }
   
  if(design == "YD"){
    out <- design_yd(trt, r, serie, seed, kinds, first)
  }

  if(design == "BIBD"){
    out <- design_bib(trt, r, serie, seed, kinds)
  }

  if(design == "CD"){
    out <- design_cd(trt, r, k, rowcol, serie, seed, kinds)
  }

  if(design == "LD"){
    out <- design_ld(trt, r, serie, seed, kinds)
  }

  if(design == "AD"){
    out <- design_ad(trt, k, r, serie, seed, kinds)
  }
#   if(design == "ABD"){
#     out <- design.dau(trt2, trt, r, serie, seed, kinds, name)
#   }

  if(design == "F2SPPD"){ ##Factorial De
    out <- design.ab(trt, r, serie, sub_design, seed, kinds, first=FALSE)
  }


  if(design == "APRD"){
    out <- design_aprd(trt, trt2, frac, r, serie, seed, kinds)
  }
  
  if(zigzag & (design != "CRD")){
    out$book <- zigzag(out)
    out$parameter$zigzag <- TRUE
  } else {
    out$parameter$zigzag <- FALSE
  }
  out -> res
  
#   print("res")
#   print(res)
#   save(res,file = "res.Rdata")
#   print("out")
#   print(out)
#   save(out,file = "out.Rdata")

  environment() %>% as.list %>% set_class(c("doe",class(.)))
 
#   print("el doe")
#   t <- environment() %>% as.list %>% set_class(c("doe",class(.)))
#   save(t,file = "doe.Rdata")

}


summary.doe <- function(object, ...){
  
  x <- object$res
  if(length(x) == 0) return("")
  if("parameter" %in% names(x)) {
    p <- x$parameter  
    
  } else {
    p <- x$parameters
  }
  if(length(p) == 0) return("")
  
  if(length(p)>=2)  names(p)[2] = "trt"
  
  cat("Summary experimental design\n")
  cat("Design:", p$design, "\n")
  cat("Label series:", p$serie, "\n")
  cat("Zigzag:", p$zigzag, "\n")
  cat("Treatment 1 (n):", length(p$trt), "\n")
  if(toupper(p$design) %in% c("GLD", "ABD", "SPPD", "STPD", "F2SPPD" )){
    cat("Treatment 2 (n):", length(p$trt2), "\n")  
  }
  if(toupper(p$design) %in% c("RCBD", "CRD", "YOUDEN", "CYCLIC", 
                              "LD", "ALPHA", "ABD", "SPPD", "STPD", "F2SPPD" )){
    cat("r:", p$r, "\n")  
  }
  if(toupper(p$design) %in% c("BIBD","CYCLIC", "ALPHA" )){
    cat("k:", p$k, "\n")  
  }
  if(toupper(p$design) %in% c("RCBD", "YOUDEN", "SPPD", "F2SPPD" )){
    cat("Randomize first row:", p$first, "\n")  
  }
  if(toupper(p$design) %in% c("RCBD" )){
    cat("Continuous numeration:", p$continue, "\n")  
  }
  if(toupper(p$design) %in% c("CYCLIC" )){
    cat("Row or column:", p$rowcol, "\n")  
  }
  
  cat("randomization algorithm:", p$kind, "\n")
  cat("randomization seed number:", p$seed, "\n")
  cat("book length:",nrow(x$book),"\n")
  #print(x$book)
}

var_selected <- reactive({
  
  #if(input$crop_type=='Potato'){
  if(input$doe_type_crop=='Potato'){
  vars <- input$vars_doe_pt 
  vars <- as.character(as.vector(vars))
  vars
  }
  #if(input$crop_type=="Sweetpotato"){
  if(input$doe_type_crop=="Sweetpotato"){
  vars <- input$vars_doe_sp  
  vars <- as.character(as.vector(vars))
  }
  vars
})#####################Added by Omar Benites


fieldbook.doe <- function(object, ...){
   
  x <- object$res #assigng all the features of the fieldbook
  
  fieldbook <- x$book #extract the fieldbook design
  print("fb before")
  print(head(fieldbook))
  
  if(input$design=="RCBD"){ 
    names(fieldbook) <- c("PLOT","REP","INSTN") 
  }
  if(input$design=="CRD"){ 
    names(fieldbook) <- c("PLOT","REP","INSTN") 
  }
  if(input$design=="LSD"){ 
    names(fieldbook) <- c("PLOT","REP","CBLOCK","INSTN") #column block
  }
  if(input$design=="SPPD"){
    
    if(input$sppd_stat_design=="crd"){
      fieldbook <-  fieldbook[,c(1,3,5,4)]
      names(fieldbook) <- c("PLOT","REP","FACTOR","INSTN") #column block
    }
    if(input$sppd_stat_design=="rcbd"){
      fieldbook <-  fieldbook[,c(1,3,5,4)]
      names(fieldbook) <- c("PLOT","REP","FACTOR","INSTN") #column block
    }    
    if(input$sppd_stat_design=="lsd"){
    fieldbook <-  fieldbook[,c(1,3,4,6,5)]
    names(fieldbook) <- c("PLOT","REP","CBLOCK","FACTOR","INSTN") #column block
    }
            
  }
  if(input$design=="STPD"){
    fieldbook <-  fieldbook[,c(1,2,4,3)]
    names(fieldbook) <- c("PLOT","REP","FACTOR","INSTN")
    
  }
  if(input$design=="BIBD"){
    names(fieldbook) <- c("PLOT","REP","INSTN") 
  }
  
  
  print("fb after")
  print(head(fieldbook)) 
   
#   if(is.null(var_selected())){
#     fieldbook
#   }else{
#     var_names <- var_selected() 
#     fieldbook[,vars_names] <- NA
#     fieldbook
#   }
  if(is.null(var_selected())){
    fieldbook
  }else{
    fieldbook[,var_selected()] <- NA 
    fieldbook 
  } 


#   l <- nrow(x$book) 
#   TTWP <- rep(46,times = l)
#   TNTP <- rep(15,times=l)
#   ATW <- rep(15,times=l)
#   DM1 <- rep(15,times=l)
#   AVDM <- rep(23,times = l)
#   AOCP <- rep(46,times = l)
#   FE1 <- rep(15,times=l)
#   FE2 <- rep(15,times=l)
#   FE3 <- rep(15,times=l)
#   CAR4 <- rep(23,times = l)
#   CAR5 <- rep(46,times = l)
#   CAR6 <- rep(23,times = l)
#   CAR7 <- rep(46,times = l)
#   CAR8 <- rep(23,times = l)
#   CAR9 <- rep(46,times = l)
#   
#   cos <-data.frame(x$book, TTWP,TNTP,ATW,DM1,AVDM,AOCP,FE1,FE2,FE3,CAR4,CAR5,CAR6,CAR7,CAR8,CAR9) 
#   cos
}


#####################Added by Omar Benites

# lsites <- reactive({
#   
#   cntry <- input$CNTRY
#   fsites <- filter_sites(data_sites(),country_input = cntry)
#   fsites
# })
#For TYPE OF TRIAL
observe({       
  if(is.null(input$doe_type_crop)){return()}
   # else{
  #  if(is.null(input$doe_trialSite)){return()}
  if(input$doe_type_crop=="Potato"){
    updateSelectInput(session,inputId = "doe_template",label = "Type of Trial (Template)",
                      choices = c("Potato Trial Healthy Tuber Yield (PTYL)"="PTYL"),selected = c("PTYL"))
  }
  
  if(input$doe_type_crop=="Sweetpotato"){
      updateSelectInput(session,inputId = "doe_template",label = "Type of Trial (Template)",
                   choices = c("SweetPotato Trial Healthy Tuber Yield (SPYL)"="SPYL"),selected = c("SPYL"))
    }
  
})

#For TRIAL SITSE
observe({
  #if(input$CNTRY==""){return()}
  if(is.null(input$CNTRY)){return()}
  
    
  #if(!is.null(input$CNTRY)){
  cntry <- input$CNTRY
  fsites <- filter_sites(data_sites(),country_input = cntry)
  fsites <- sort(fsites)    
  # else{
  #  if(is.null(input$doe_trialSite)){return()}
#   if(input$CNTRY=="Angola"){
#     updateSelectInput(session,inputId="doe_trialSite", label = "Localities",choices = lsites())
#   }
  #if(!is.null(input$doe_trailSite)){
    updateSelectInput(session,inputId="doe_trialSite", label = "Localities",choices = fsites)
  #}
  #}
})


full_fieldbook_name_reactive <- reactive({
  .template <- input$doe_template     
  .date <- input$doe_date 
  
  .trialSite <- input$doe_trialSite 
  #.trialSite <- unlist(str_split(.trialSite, pattern = " \\("))[2]
  .trialSite <- unlist(str_split(.trialSite, pattern = " \\("))[2] %>% str_replace(.,pattern = ")",replacement="")
  
  begin_date <- unlist(str_split(.date[1],pattern = "-",n = 3))
  begin_date_year <- begin_date[1]
  begin_date_month <- begin_date[2]
  
  if(is.null(.template))({ return() })
  if(is.null(.date))({return()})
  if(is.null(.trialSite))({return()})
  #paste(.template,.date[1],"_",.trialSite,sep="")
  paste(.template,begin_date_year,begin_date_month,"_",.trialSite,sep="")
 
  
})


output$doe_full_fieldbook_name <- renderText({
  full_fieldbook_name_reactive()
})

#list of the genotypes on the fieldbook
 

germlist <- reactive({
  
  file1 <- input$doe_germ_inputfile
  if(is.null(file1)){return()}
  germ_list <- read.csv(file = file1$datapath,header = TRUE)[["INSTN"]] %>% as.character()

})


#begin genotypes that we use as checks in the field
genochecks <- reactive({
  file2 <- input$abd_check_inputfile
  if(is.null(file2)){return()}
  geno_checks <- read.csv(file = file2$datapath,header = TRUE)[["CHECKS"]] %>% as.character()

})
#end genochecks


#begin genotypes that we use as checks in the field
# genochecks <- reactive({
#   file2 <- input$abd_check_inputfile
#   if(is.null(file2)){return()}
#   geno_checks <- read.csv(file = file2$datapath,header = TRUE)[["CHECKS"]] %>% as.character()
# })
#end genochecks


output$doe_germ_table <- renderTable({
  pol <- as.data.frame(germlist())  
  #head(germlist(),n = 4)
  head(pol,n=4)
})


output$doe_genochecks_table <- renderTable({
  pol <- as.data.frame(genochecks())  
  #head(germlist(),n = 4)
  head(pol,n=4)
  
  
})

# observe({ #begin observe
#   if(is.null(input$fieldbook_export_button_doe)){return(NULL)}
#   if(!is.null(input$fieldbook_export_button_doe)){ #not run being inizialited
#     print("error1")
#     isolate({
#       
#       file_from <- "Z:\\hidap\\inst\\hidap\\templates\\potato\\template_PTYL.xlsx"
#       
#       print(file_from)
#       from <- file.path(file_from)
#       #print(from)
#       file_to <- "Z:/hidap/inst/hidap/data"
#       print("error2")  
# #       directorio <- file.path(file_to,"potato","201545",sep = "")
# #       dir.create(directorio)
#       
#       to <- file.path(file_to, paste(full_fieldbook_name_reactive(),".xlsx",sep = ""))
#       #print(to)
#       #file <- to
#       print("error3")
#       if(!file.exists(to)){
#         file.copy(from=from,to=to)
#         
# #         add.date(to, "Minimal", row.label = "Begin date", col.name = "Value",input$doe_date,1)
# #         add.date(to, "Minimal", row.label = "End date", col.name = "Value",input$doe_date,2)
#         add.vals.to.fb(to = to,col.name = "Value",reactive_value = full_fieldbook_name_reactive(),
#                        input_value= input$doe_date,fb_reactive=.fieldbook_doe())        
# 
# 
#         #add.short.name(to = to,sheetName = "Minimal",row.label = "Short name or Title",col.name = "Value",reactive_value = full_fieldbook_name_reactive())        
# 
#         print("print 1 step: Minimal data")
#         
# #         wb <- openxlsx::loadWorkbook(to)
# #         openxlsx::addWorksheet(wb, "Fieldbook")
# #         openxlsx::writeDataTable(wb = wb,sheet = "Fieldbook",x = isolate({.fieldbook_doe()}))
# #         openxlsx::saveWorkbook(wb,file = to, overwrite = TRUE)
# #         
#         print("print 2 step: Fieldbook")
#         
#         shell.exec(to)
#       }
#     })
#     
#   }
#   
# }) #end observe

observeEvent(input$fieldbook_export_button_doe, function() {
  
  
  isolate({
    
    cropPath <- cropPath(input$doe_type_crop)
    print(cropPath(input$doe_type_crop))
    
    fr = paste("template_",input$doe_template,".xlsx", sep="")
    print(fr)
    file_from <- file.path(cropPath,fr,paste="")
    print(file_from)
    #file_from <- "Z:\\hidap\\inst\\hidap\\templates\\potato\\template_PTYL.xlsx"
    
    from <- file.path(file_from)
    #file_to <- "Z:/hidap/inst/hidap/data"
    folder_to <- folderPath("data")
    
    .date <- input$doe_date 
    begin_date <- unlist(str_split(.date[1],pattern = "-",n = 3))
    folder_file_name <- paste(begin_date[1],begin_date[2],sep="")#folder where fieldbook goes to    
    dir_name <- file.path(folder_to,tolower(input$doe_type_crop),folder_file_name,sep = "") #File path of the folder_file 
    
    print(dir_name)
    #dir.create(dir_name)
    #if(!file.exists(folder_to)) dir.create(dir_name,rec=T)
    if(!file.exists(dir_name)) dir.create(dir_name,rec=T)

    print(folder_to)
    
    #to <- file.path(folder_to, paste(full_fieldbook_name_reactive(),".xlsx",sep = ""))
    to <- file.path(dir_name,paste(full_fieldbook_name_reactive(),".xlsx",sep = ""))
    
    print(to)
    #file <- to
    
    if(!file.exists(to)){
      file.copy(from=from,to=to)
      
      #         add.date(to, "Minimal", row.label = "Begin date", col.name = "Value",input$doe_date,1)
      #         add.date(to, "Minimal", row.label = "End date", col.name = "Value",input$doe_date,2)
      add.vals.to.fb(to = to,col.name = "Value",reactive_value = full_fieldbook_name_reactive(),
                     input_value= input$doe_date,fb_reactive=.fieldbook_doe())        
      
      
      #add.short.name(to = to,sheetName = "Minimal",row.label = "Short name or Title",col.name = "Value",reactive_value = full_fieldbook_name_reactive())        
      
      print("print 1 step: Minimal data")
      
      #         wb <- openxlsx::loadWorkbook(to)
      #         openxlsx::addWorksheet(wb, "Fieldbook")
      #         openxlsx::writeDataTable(wb = wb,sheet = "Fieldbook",x = isolate({.fieldbook_doe()}))
      #         openxlsx::saveWorkbook(wb,file = to, overwrite = TRUE)
      #         
      print("print 2 step: Fieldbook")
      
      shell.exec(to)
    }
  })
  
 
})




# add.short.name <- function(to,sheetName,row.label,col.name,reactive_value){
#   wb <- openxlsx::loadWorkbook(to) 
#   data_hidap <- readxl::read_excel(path = to,sheet = sheetName)
#   data_hidap[data_hidap$Factor==row.label,col.name] <- paste(isolate(reactive_value))
#   openxlsx::writeDataTable(wb,sheet = sheetName,x = data_hidap,colNames = TRUE,withFilter = FALSE)
#   openxlsx::saveWorkbook(wb,file = to, overwrite = TRUE)
# }
# 
# add.date <- function(to, sheetName, row.label,col.name,input_value,pos){
#   input_val <- input_value
#   input_val <- input_val[pos]
#   wb <- openxlsx::loadWorkbook(to)
#   data_hidap <- readxl::read_excel(path = to,sheet = sheetName)
#   data_hidap[data_hidap$Factor==row.label,col.name] <- paste(as.character(input_val))
#   openxlsx::writeDataTable(wb,sheet = sheetName,x = data_hidap,colNames = TRUE,withFilter = FALSE)
#   openxlsx::saveWorkbook(wb,file = to, overwrite = TRUE)
# }

add.vals.to.fb <- function(to, col.name, reactive_value,input_value,fb_reactive,germ_list){
  wb <- openxlsx::loadWorkbook(to) 
  
  #BEGIN sheet MINIMAL
  data_hidap <- readxl::read_excel(path = to,sheet = "Minimal")
  data_hidap[data_hidap$Factor=="Short name or Title",col.name] <- paste(isolate(reactive_value))
  #openxlsx::writeDataTable(wb,sheet = sheetName,x = data_hidap,colNames = TRUE,withFilter = FALSE)
  #openxlsx::saveWorkbook(wb,file = to, overwrite = TRUE)
  
  print("Short Name Pass")
  
  input_val <- input_value
  #wb <- openxlsx::loadWorkbook(to)
  #data_hidap <- readxl::read_excel(path = to,sheet = sheetName)
  data_hidap[data_hidap$Factor=="Begin date",col.name] <- paste(as.character(input_val[1]))
  data_hidap[data_hidap$Factor=="End date",col.name] <- paste(as.character(input_val[2]))
  
  print("Dates Pass")
  
  #geographic information
  cntry <- input$CNTRY
  tsites <- input$doe_trialSite %>% gsub("\\s*\\([^\\)]+\\)","",.)
 
  #tsites <- tsites %>% stringr::str_trim(.,side = "both")
  print(cntry)
  print(tsites)
  if(is.null(cntry)){return()}
  if(is.null(tsites)){return()}
  if(!is.null(cntry) && !is.null(tsites)){
  print(data_sites())
  geodata <- filter_geodata(data_sites = data_sites(),country_input = cntry,trail_site = tsites)
  print(geodata)
  data_hidap[data_hidap$Factor=="Site short name",col.name] <- paste(as.character(geodata$SHORTN))
  data_hidap[data_hidap$Factor=="Agroecological zone",col.name] <- paste(as.character(geodata$AEZ))
  data_hidap[data_hidap$Factor=="CIP Region",col.name] <- paste(as.character(geodata$CREG))
  data_hidap[data_hidap$Factor=="Continent",col.name] <- paste(as.character(geodata$CONT))
  data_hidap[data_hidap$Factor=="Country",col.name] <- paste(as.character(geodata$CNTRY))
  data_hidap[data_hidap$Factor=="Admin1",col.name] <- paste(as.character(geodata$ADM1))
  data_hidap[data_hidap$Factor=="Admin2",col.name] <- paste(as.character(geodata$ADM2))
  data_hidap[data_hidap$Factor=="Admin3",col.name] <- paste(as.character(geodata$ADM3))
  data_hidap[data_hidap$Factor=="Locality",col.name] <- paste(as.character(geodata$LOCAL))
  data_hidap[data_hidap$Factor=="Elevation",col.name] <- paste(as.character(geodata$ELEV))
  data_hidap[data_hidap$Factor=="Latitude",col.name] <- paste(as.character(geodata$LATD))
  data_hidap[data_hidap$Factor=="Longitude",col.name] <- paste(as.character(geodata$LOND))
  }##end geographic information

  print("Geographic Information Pass")
   openxlsx::writeDataTable(wb,sheet = "Minimal",x = data_hidap,colNames = TRUE,withFilter = FALSE)
  #END MINIMAL
  #openxlsx::saveWorkbook(wb,file = to, overwrite = TRUE)
    print("Minimal Finished")
  
  
#BEGIN Sheet Installation
  expenv <- input$doe_expenv
  stat_design <- input$design
  if(is.null(stat_design)){return()}
  if(!is.null(stat_design)){
  #declare parameter from statistical des  
  data_hidap <- readxl::read_excel(path = to,sheet = "Installation")
  
      if(stat_design=="RCBD"){
        sdesign_name <- "Randomized Complete Block Design (RCBD)"
        nrep <- input$rcbd_r 
      }  
      if(stat_design=="CRD"){
        sdesign_name <- "Completely Randomized Design (CRD)"
        nrep <- input$crd_r  
      }      
      if(stat_design=="LSD"){
        sdesign_name <- "Latin Square Design (LSD)"
        nrep <- "" 
      }      
      if(stat_design=="ABD"){
        sdesign_name <- "Augmented Block Desing (ABD)"
        nrep <- input$abd_r   
      }
      if(stat_design=="SPPD"){
        
        if(input$sppd_stat_design=="crd"){
          sdesign_name <- "Split Plot with Plots in CRD (CRD)"
          nrep <- input$sppd_r        
        }
        
        if(input$sppd_stat_design=="rcbd"){
        sdesign_name <- "Split Plot with Plots in RCBD (SPRCBD)"
        nrep <- input$sppd_r        
        }
        
        if(input$sppd_stat_design=="lsd"){
          sdesign_name <- "Split Plot with Plots in LSD (LSD)"
          nrep <- input$sppd_r        
        }
           
    }     
      if(stat_design=="STPD"){
          #sdesign_name <- "Strip Plot Design (STPD)"
          sdesign_name <- "Strip Plot Design (STRIP)"
          nrep <- input$stpd_r           
      }
      if(stat_design=="BIBD"){
        sdesign_name <- "Balanced Incomplete Block Design (BIBD)"
        nrep <- input$bibd_r       
      } 

print("Disenos Pass")
  
    data_hidap[data_hidap$Factor=="Experimental design",col.name] <- paste(as.character(sdesign_name))
    data_hidap[data_hidap$Factor=="Number of repetitions or blocks",col.name] <- paste(as.character(nrep))
#   data_hidap[data_hidap$Factor=="Experimental design",col.name] <- paste(as.character(stat_design))
#   data_hidap[data_hidap$Factor=="Number of repetitions or blocks",col.name] <- paste(as.character(input$rcbd_r))
}

  if(is.null(expenv)){return()} 
  if(!is.null(expenv)){
  data_hidap[data_hidap$Factor=="Experimental Environment",col.name] <- paste(as.character(expenv))
  }
  #openxlsx::writeDataTable(wb,sheet = "Installation",x = data_hidap,colNames = TRUE,withFilter = FALSE)
  openxlsx::writeDataTable(wb,sheet = "Installation",x = data_hidap,colNames = TRUE,withFilter = FALSE)
#END INSTALLATION

print("Installation Pass")
##Sheet Material List sheet
    file1 <- input$doe_germ_inputfile
    if(is.null(file1)){return()}
    if(!is.null(file1)){
    mat_list_sheet <- openxlsx::read.xlsx(xlsxFile = to,sheet = "Material List",colNames = TRUE,skipEmptyRows = TRUE)
    ##
    print(mat_list_sheet)
    ##
    names_mat_list <- names(mat_list_sheet)
    ##
    print(names_mat_list)
    ##
    germ_list_user<- read.csv(file = file1$datapath,header = TRUE)
    print(germ_list_user)
    Numeration <- 1:nrow(germ_list_user)
    print(Numeration)
    datos <- cbind(Numeration,germ_list_user)
    print(datos)
    names(datos) <- stringr::str_replace_all(string = names(mat_list_sheet),pattern = "\\.",replacement = " ")
    print("final datos Installation")
    print(datos)
    openxlsx::writeDataTable(wb = wb,sheet = "Material List",x = datos,colNames = TRUE,withFilter = FALSE)
    }
print("Crop Material List")        
#Sheet Crop_management 
  rm(data_hidap)
  data_hidap <- readxl::read_excel(path = to,sheet = "Crop_management")
  data_hidap[data_hidap[,"Intervention type"]=="Planting","Date"] <- paste(input_val[1])
  data_hidap[data_hidap[,"Intervention type"]=="Vine cutting / killing","Date"] <- paste(input_val[1])
  data_hidap[data_hidap[,"Intervention type"]=="Harvest","Date"] <- paste(input_val[1])

  if(input$doe_type_crop=="Potato"){
     if(!is.null(input$vars_doe_pt)){
       v_input <- input$vars_doe_pt 
       v_list <- var_list(crop="Potato")
       vars <- v_list %in% v_input
       vars <- names(v_list[vars])
       #print(vars)
       vars <- gsub("\\[[^\\]]*\\]: ", "", vars, perl=TRUE)
       vars <- stringr::str_trim(vars,side = "both")
       #print(vars)
      }
  }
  if(input$doe_type_crop=="Sweetpotato"){
      if(!is.null(input$vars_doe_sp)){vars <- input$vars_doe_sp 
        v_input <- input$vars_doe_pt 
        v_list <- var_list(crop="Sweetpotato")
        vars <- v_list %in% v_input
        vars <- names(v_list[vars])
        #print(vars)
        vars <- gsub("\\[[^\\]]*\\]: ", "", vars, perl=TRUE)
        vars <- stringr::str_trim(vars,side = "both")
        #print(vars)
     } 
  }
  n_row <- length(vars)
  col1 <- rep("Measure",n_row)
  #var <- ddict %>% dplyr::filter(.,ABBR %in% a)
  col2 <- vars
  col3 <- rep(as.character(input_val[1]),n_row)
  col4 <- rep(NA,n_row)
  col5 <- rep(NA,n_row)
  col6 <- rep(NA,n_row)
  col7 <- rep(NA,n_row)
  col8 <- rep(NA,n_row)
  col9 <- rep(NA,n_row)

  temp_data <- data.frame(col1,col2,col3,col4,col5,col6,col7,col8,col9)
  names(temp_data) <- names(data_hidap)
  data_hidap <- rbind(data_hidap,temp_data)
  openxlsx::writeDataTable(wb = wb,sheet = "Crop_management",x = data_hidap,colNames = TRUE,withFilter = FALSE)
      
print("Crop Management Pass")
#Sheet Fieldbook      
  openxlsx::addWorksheet(wb, "Fieldbook")
  openxlsx::writeDataTable(wb = wb,sheet = "Fieldbook",x = isolate({fb_reactive}))
  openxlsx::saveWorkbook(wb,file = to, overwrite = TRUE)  
print("Fieldbook Pass")
shell.exec(to)
}

#print("Fieldbook Pass")
# observe({
#   
# #  if(is.null(input$CNTRY)) {return()}
# #  if(!is.null(input$CNTRY)){
# #       
# #   if(input$CNTRY=="PERU"){
# #     #updateSelectInput(session,inputId="LOCAL",choices=list("CIPHQ","LIMA","ACO"))
# #     updateSelectInput(session,inputId="doe_trialSite",choices=list("CIPHQ","LIMA","ACO"))
# #   }
# #   
# #   if(input$CNTRY=="KENIA"){
# #     updateSelectInput(session,inputId="doe_trialSite",choices=list("LUMURU","NAIROBI"))
# #   }
# #   
# #  }
# #   
#   p <- data_sites(fp_sites)
#   r <- list_countries(p)
#   fsites <- filter_sites(data_sites=p,country_input=input$CNTRY,locality_input=input$doe_trialSite)
#   lsites <- list_sites(fsites)
#   
#   
#  if(is.null(input$CNTRY)) {return()}
#  if(!is.null(input$CNTRY)){
#      if(input$CNTRY=="Angola"){
#        #updateSelectInput(session,inputId="LOCAL",choices=list("CIPHQ","LIMA","ACO"))
#        updateSelectInput(session,inputId="doe_trialSite",choices=list("Chinga","Ecunha","Humpata","Chibia"))
#      }
#      else {
#        updateSelectInput(session,inputId="doe_trialSite",choices= lsites)
#        
#      }
#      
#      
#      
#  }
# })   

# output$columns = renderUI({
#   mydata = get(input$doe_trialSite)
#   selectInput('columns2', 'Columns', names(mydata))
# })




# output$columns = renderUI({
#   mydata = get(input$doe_trialSite)
#   selectInput('columns2', 'Columns', names(mydata)   )
# })


# output$doe_germ_table <- renderText({
#       
#      file <- input$doe_germ_inputfile
#      p <- file$datapath
#      if(is.null(file)){return()}
#       readxl::read_excel(path = file$datapath,1,col_names = TRUE)
#      paste(p)   
# })

################DownloadData ()
# output$downloadData <- downloadHandler(
#   
#   filename = function() { paste("fbelisa", '.csv', sep='') },
#     content = function(file) {
#     write.csv(.fieldbook_doe(), file, na="",row.names=FALSE,col.names=FALSE)
#   }
# 
#   
# )

# 
# output$fieldbook_export <- renderPrint({
#   input$fieldbook_export_button_doe
#   #g <- isolate(.fieldbook_doe())
#  
#   
#   #if(!is.null(input$fieldbook_export_button_doe)){ 
#   file_from <- "Z:\\hidap\\inst\\hidap\\templates\\potato\\template_PTYL.xlsx"
#   
#   print(file_from)
#   #from <- "Z:/hidap/inst/hidap/templates/potato"
#   from <- file.path(file_from)
#   print(from)
#   file_to <- "Z:/hidap/inst/hidap/data"
#   
#   to <- file.path(file_to, paste(full_fieldbook_name_reactive(),".xlsx",sep = ""))
#   print(to)
#   #file <- to
#   file.copy(from=from,to=to)
#   #openxlsx::write.xlsx(.fieldbook_doe(),file = file, sheetName = "Fieldbook",col.names = TRUE,row.names = TRUE,append = TRUE,showNA = FALSE)
#    wb <- openxlsx::loadWorkbook(to)
# #   openxlsx::addWorksheet(wb,sheetName = "Fieldbook")
# # #   #openxlsx::addWorksheet(wb = wb,sheetName = "Fieldbook",header = "center")
# # #   #openxlsx::writeData(wb = wb,sheet = "Fieldbook",x = .fieldbook_doe(),colNames = TRUE,rowNames = FALSE,withFilter = TRUE,keepNA = FALSE)
# # #   x <- as.data.frame(.fieldbook_doe())
# # #   #openxlsx::writeDataTable(wb = wb,sheet = "Fieldbook",x = .fieldbook_doe(),colNames = TRUE,rowNames = FALSE,keepNA = FALSE)
# #   openxlsx::saveWorkbook(wb,file = to, overwrite = TRUE)
# #    wb <- xlsx::loadWorkbook(to)
# #    sheet <- xlsx::createSheet(wb = wb,sheetName="Fieldbook")
# #    #xlsx::addDataFrame(x = .fieldbook_doe(),sheet = sheet ,col.names = TRUE,row.names = FALSE)
# #    xlsx::saveWorkbook(wb = wb,file = to)
# #    #readxl::read_excel(path = to,sheet = "Fieldbook",col_names = TRUE)
# #    #xlsx::write.xlsx(x = .fieldbook_doe(),file = to,sheetName = "Fieldbook",append = TRUE)
# #    xlsx::write.xlsx(.fieldbook_doe(), file=to, sheetName="Fieldbook2", 
# #            col.names=TRUE, row.names=FALSE, append=TRUE, showNA=FALSE)
#   openxlsx::addWorksheet(wb, "Fieldbook")
#   openxlsx::writeDataTable(wb = wb,sheet = "Fieldbook",x = .fieldbook_doe())
#   openxlsx::saveWorkbook(wb,file = to, overwrite = TRUE)
# #   
# #   openxlsx::writeData()
# #   
# #   if(file.exists(to)){
# #     print(to)
# #     shell.exec(to)
# #   }
#   getwd()
#   paste("Fieldbook succesfully created")
# #}
#   
# 
# })




# output$test <- renderPrint({
#   input$aa
#   
#   isolate(class(iris))
# })





# 
# output$downloadData  <-  downloadHandler(
#   filename = function() { paste(full_fieldbook_name_reactive(),".xls",sep = "")},
#   content = function(file){
#     
#     file_from <- "Z:\\hidap\\inst\\hidap\\templates\\potato\\template_PTYL.xls"
#     #from <- "Z:/hidap/inst/hidap/templates/potato"
#     from <- file.path(file_from)
#     file_to <- "Z:/hidap/inst/hidap/data"
#     to <- file.path(file_to,full_fieldbook_name_reactive
#     #file <- to
#     file.copy(from=from,to=to,overwrite = TRUE)
#     
#     #openxlsx::write.xlsx(.fieldbook_doe(),file = file, sheetName = "Fieldbook",col.names = TRUE,row.names = TRUE,append = TRUE,showNA = FALSE)
#     openxlsx::write.xlsx(.fieldbook_doe(),file = to, sheetName = "Fieldbook",col.names = TRUE,row.names = TRUE,append = TRUE,showNA = FALSE)
#     
#   }
#   
#   
#   )

###############################################################
#  output$downloadData <- downloadHandler(
# #    file_pot <- "Z:\\hidap\\inst\\hidap\\templates\\potato\\template_PTYL.xls"
# #    
# #    from <- "Z:/hidap/inst/hidap/templates/potato"
# #    to <- "Z:/hidap/inst/hidap/data"
# #    file_name <- input$
# #    
# 
#    
#   filename = function() { 
#     
#     .template <- input$doe_template     
#     .date <- input$doe_date 
#     .trialSite <- input$doe_trialSite 
#     begin_date <- unlist(str_split(.date[1],pattern = "-",n = 3))
#     begin_date_year <- begin_date[1]
#     begin_date_month <- begin_date[2]
#     
#     if(is.null(.template))({ return() })
#     if(is.null(.date))({return()})
#     if(is.null(.trialSite))({return()})
#     #paste(.template,date[1],"_",.trialSite,sep="")
#     
#     file_name <- paste(.template,begin_date_year,begin_date_month,"_",.trialSite,sep="")
#     
#     folder_name <- "Z:/hidap/inst/hidap/templates/potato/"
#     trial_name <- paste("template_",input$doe_template)
#     from <- paste(folder_name,trial_name,".xls",sep="")
#     
#     to <- paste("Z:/hidap/inst/hidap/data/",file_name,".xls",sep = "")
#     
#     file.copy(from,to,overwrite = TRUE)
#   
#     return(to)
#   } ,
#     
#   content = function(file) {
#     openxlsx::write.xlsx(.fieldbook_doe(),file = file,sheetName = "Fieldbook",col.names = TRUE,row.names = TRUE,append = TRUE,showNA = FALSE)
#     
#   }
# 
#   
#   
#   
# )





