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
  x[[r]]
}

#designs = c("RCBD", "CRD", "LSD", "GLD","YD","BIB", "CD","LD","AD","ABD", "SPPD", "STPD", "F2SPPD")

doe <- function(design = "RCBD",# "CRD", "LSD", "GLD","YD","BIB", 
                           #"CD","LD","AD","ABD", "SPPD", "STPD", "F2SPPD"), 
                trt = "A", trt2 = letters[1:8],
                r = 2, k = 2,
                rowcol = FALSE,
                name = "", 
                sub_design = "rcbd", #c("rcbd", "lsd", "crd"),
                serie = 1, zigzag = TRUE,
                seed = 0, kinds = "Super-Duper", 
                
                rcbd_r=2, rcbd_first = FALSE, rcbd_continue = FALSE,
                lsd_r=2, lsd_first = FALSE
                
                ){
  out <- NULL
  
  r <- as.integer(r)
  trt  <- get_germplasm_ids(trt)
  trt2 <- get_germplasm_ids(trt2)
  first <- FALSE
  continue <- FALSE
  if(design == "RCBD"){
    r <- as.integer(rcbd_r) 
    first <- as.logical(rcbd_first)
    continue <- as.logical(rcbd_continue)
  }
  if(design == "CRD"){
    r <- as.integer(r)  
  }
  if(design == "LSD"){
    r <- as.integer(lsd_r) 
    first <- as.logical(lsd_first)
  }
 
  
  k <- as.integer(k)
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
    out <- design_lsd(trt, serie, seed, kinds, first)
  }
  if(design == "GLD"){
    out <- design_gld(trt, trt2, serie, seed, kinds)
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
    out <- design.alpha(trt, k, r, serie, seed, kinds)
  }
  if(design == "ABD"){
    out <- design.dau(trt, trt2, r, serie, seed, kinds, name)
  }
  if(design == "SPPD"){
    out <- design.split(trt, trt2, r, sub_design, serie, seed, kinds, first)
  }
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
  
  environment() %>% as.list %>% set_class(c("doe",class(.)))
}


summary.doe <- function(object, ...){
  
  x <- object$res
  if("parameter" %in% names(x)) {
    p <- x$parameter  
    
  } else {
    p <- x$parameter
  }
  
  names(p)[2] = "trt"
  
  cat("Summary experimental design\n")
  cat("Design:", p$design, "\n")
  cat("Label series:", p$serie, "\n")
  cat("Zigzag:", p$zigzag, "\n")
  cat("Treatment 1 (n):", length(p$trt), "\n")
  if(p$design %in% c("GLD", "ABD", "SPPD", "STPD", "F2SPPD" )){
    cat("Treatment 1 (n):", length(p$trt2), "\n")  
  }
  if(toupper(p$design) %in% c("RCBD", "CRD", "YD", "CD", "LD", "AD", "ABD", "SPPD", "STPD", "F2SPPD" )){
    cat("r:", p$r, "\n")  
  }
  if(toupper(p$design) %in% c("BIB","CD", "AD" )){
    cat("k:", p$k, "\n")  
  }
  if(toupper(p$design) %in% c("RCBD", "YD", "SPPD", "F2SPPD" )){
    cat("Randomize first row:", p$first, "\n")  
  }
  if(toupper(p$design) %in% c("RCBD" )){
    cat("Continuous numeration:", p$continue, "\n")  
  }
  
  cat("randomization algorithm:", p$kind, "\n")
  cat("randomization seed number:", p$seed, "\n")
  cat("book length:",nrow(x$book),"\n")
  #print(x$book)
}


fieldbook.doe <- function(object, ...){
  
  x <- object$res
  
  x$book
}


