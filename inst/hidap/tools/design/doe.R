#load("../../../data/primes.rda")
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
                
                crd_r=2 , crd_first = FALSE, crd_continue = FALSE,
                rcbd_r=2, rcbd_first = FALSE, rcbd_continue = FALSE,
                                    
                lsd_r=2, lsd_first = FALSE,
<<<<<<< HEAD
                abd_r=2, abd_first =FALSE, abd_continue = FALSE, 
                #abd_r=2, #abd_first =FALSE
=======
                abd_trt2= "A", abd_r=2, abd_first =FALSE, abd_continue = FALSE, 
                
>>>>>>> d998a245b322053bfd1c69529772506e698b0f73
                
                gld_trt2 = "A",
                
                sppd_r=2, sppd_first=FALSE, sppd_continue=FALSE, 
                sppd_stat_design="rcbd",sppd_factor_lvl1="level1",sppd_factor_lvl2="level2",sppd_factor_lvl3="level3",
                
                
                yd_r = 2, yd_first = FALSE,
                bib_k=4,
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
  trt2 <- genochecks()
  first <- FALSE
  continue <- FALSE
  rowcol = as.logical(rowcol)
  
  if(design == "RCBD"){
    r <- as.integer(rcbd_r) 
    first <- as.logical(rcbd_first)
    continue <- as.logical(rcbd_continue)
  }
  if(design == "CRD"){
    r <- as.integer(crd_r)
    first <- as.logical(crd_first)
    continue <- as.logical(crd_continue)
  }
  if(design == "LSD"){
    #r <- as.integer(lsd_r) 
    first <- as.logical(lsd_first)
  }
  
<<<<<<< HEAD
  if(design == "ABD"){
#     trt <- trt2
#     trt2 <- trt
    r <- as.integer(abd_r)
    #trt2 <- get_germplasm_ids(abd_trt2)
    first <- as.logical(abd_first)
    continue <- as.logical(abd_continue)
  }
  
  if(design == "SPPD"){
    r <- as.integer(sppd_r)
    first <- as.logical(sppd_first)
    continue <- as.logical(sppd_continue) 
    
    sub_design <- as.character(sppd_stat_design)
    sppd_factor_lvl1 <- sppd_factor_lvl1 %>% as.character() %>% str_trim(.,side = "both")
    sppd_factor_lvl2 <- sppd_factor_lvl2 %>% as.character() %>% str_trim(.,side = "both")
    sppd_factor_lvl3 <- sppd_factor_lvl3 %>% as.character() %>% str_trim(.,side = "both")
    trt2 <- c(sppd_factor_lvl1,sppd_factor_lvl2,sppd_factor_lvl3)
    
  }



=======
  if((design=="ABD")){
    trt2 <- get_germplasm_ids(abd_trt2)
    r <- as.integer(abd_r)
    first <- as.logical(abd_first)
#     continue <- as.logical(abd_continue)
  }
  
>>>>>>> d998a245b322053bfd1c69529772506e698b0f73
  if(design == "GLD"){
    trt2 <- get_germplasm_ids(gld_trt2)
  }
  if(design == "YD"){
    r <- as.integer(yd_r) 
    first <- as.logical(yd_first)
  }
  if(design == "BIB"){
    k <- as.integer(bib_k)  
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
    out <- design_rcbd(trt, r, serie, seed, kinds, first, continue)
  }
  
  if(design == "LSD"){
    out <- design_lsd(trt, serie, seed, kinds)
  }

<<<<<<< HEAD
  if(design == "ABD"){ #trt2::genotypes & trt:: genotypes
    out <- design_abd(trt2,trt, r, serie, seed, kinds)
=======
  if(design == "ABD"){
    out <- design_abd(trt,trt2, serie, seed, kinds)
>>>>>>> d998a245b322053bfd1c69529772506e698b0f73
  }
  
  if(design == "GLD"){
    out <- design_gld(trt, trt2, serie, seed, kinds)
  }

  if(design == "SPPD"){
    out <- design.split(trt, trt2, r, sub_design, serie, seed, kinds, first)
  }

  if(design == "YD"){
    out <- design_yd(trt, r, serie, seed, kinds, first)
  }

  if(design == "BIB"){
    out <- design_bib(trt, k, serie, seed, kinds)
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

  if(design == "STPD"){
    out <- design.strip(trt, trt2, r, serie, seed, kinds)
  }
  
  if(design == "AB"){
    out <- design.ab(trt, r, serie, sub_design, seed, kinds, first)
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
  if(toupper(p$design) %in% c("BIB","CYCLIC", "ALPHA" )){
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
  
  if(input$crop_type=='Potato'){
  vars <- input$vars_doe_pt 
  vars <- as.character(as.vector(vars))
  vars
  }
  if(input$crop_type=="Sweetpotato"){
  vars <- input$vars_doe_sp  
  vars <- as.character(as.vector(vars))
  }
  vars
})


fieldbook.doe <- function(object, ...){
   
  x <- object$res #assigng all the features of the fieldbook
  
  fieldbook <- x$book #extract the fieldbook design
  
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

output$doe_full_fieldbook_name <- renderText({
     .template <- input$doe_template     
     .date <- input$doe_date
     .trialSite <- input$doe_trialSite
    
    if(is.null(.template))({ return() })
    if(is.null(.date))({return()})
    if(is.null(.trialSite))({return()})
    paste(.template,.date,"_",.trialSite,sep="")
    
})

<<<<<<< HEAD
#list of the genotypes on the fieldbook
=======

>>>>>>> d998a245b322053bfd1c69529772506e698b0f73
germlist <- reactive({
  
  
  file1 <- input$doe_germ_inputfile
  if(is.null(file1)){return()}
  germ_list <- read.csv(file = file1$datapath,header = TRUE)[["INSTN"]] %>% as.character()
<<<<<<< HEAD
  
=======
})

#begin genotypes that we use as checks in the field
genochecks <- reactive({
  file2 <- input$abd_check_inputfile
  if(is.null(file2)){return()}
  geno_checks <- read.csv(file = file2$datapath,header = TRUE)[["CHECKS"]] %>% as.character()
>>>>>>> d998a245b322053bfd1c69529772506e698b0f73
})
#end genochecks


#begin genotypes that we use as checks in the field
genochecks <- reactive({
  file2 <- input$abd_check_inputfile
  if(is.null(file2)){return()}
  geno_checks <- read.csv(file = file2$datapath,header = TRUE)[["CHECKS"]] %>% as.character()
})
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




# output$doe_germ_table <- renderText({
#       
# #      file <- input$doe_germ_inputfile
# #      p <- file$datapath
# #      if(is.null(file)){return()}
# # #      readxl::read_excel(path = file$datapath,1,col_names = TRUE)
# #      paste(p)   
# })

##################DownloadData ()


output$downloadData <- downloadHandler(
  filename = function() { paste("fbelisa", '.csv', sep='') },
  content = function(file) {
    write.csv(.fieldbook_doe(), file, na="",row.names=FALSE,col.names=FALSE)
  }
)





