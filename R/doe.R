load("")


is_odd <- function(v){
  v %% 2 == 1
}

guess_k <- function(n) {
  k_upper <- sqrt(n)
  k_g <- round(k_upper,0)
  res <- logical(k_g)
  for(k in 3:k_g) {
    if(n %% k ==0) res[k] <- TRUE
  }
  x <- 1:k_g
  r <- x[res]
  if(length(r)==0) return(NULL)
  r
}

valid_k <- function(trt, r = 2:4, k){
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

design = c("RCBD", "CRD", "LSD", "GLD","YD","BIB", "CD","LD","AD","ABD", "SPPD", "STPD", "F2SPPD")

doe <- function(design = designs, 
                trt = LETTERS[1:5], trt2 = NULL,
                r = 2, k = 1,
                first = FALSE, rowcol = FALSE,
                name = "", continue = FALSE,
                sub_design = c("rcbd", "lsd", "crd"),
                serie = 1, zigzag = TRUE,
                seed = 0, kinds = "Super-Duper"){
  out <- NULL

  if(design == "CRD"){
    out <- design.crd(trt, r, serie, seed, kinds)
  }
  
  if(design == "RCBD"){
    out <- design.rcbd(trt, r, serie, seed, kinds, continue)
  }
  
  if(design == "LSD"){
    out <- design.lsd(trt, serie, seed, kinds, first)
  }
  if(design == "GLD"){
    out <- design.graeco(trt, trt2, serie, seed, kinds)
  }
  if(design == "YD"){
    out <- design.youden(trt, r, serie, seed, kinds, first)
  }
  if(design == "BIB"){
    out <- design.bib(trt, k, serie, seed, kinds)
  }
  if(design == "CD"){
    if(length(trt) < 6 | length(trt) > 30 ) 
      stop("There must be at least 6 and max 30 treatments.")
    if(k < 2 | k > 10 ) 
      stop("K must be > 1 and < 11.")
    out <- design.cyclic(trt, k, r, serie, rowcol, seed, kinds)
  }
  if(design == "LD"){
    out <- design.lattice(trt, r, serie, seed, kinds)
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
  
  if(zigzag & (design != "CRD")){
    out$book <- zigzag(out)
    out$parameter$zigzag <- TRUE
  } else {
    out$parameter$zigzag <- FALSE
  }
  out
  environment() %>% as.list %>% set_class(c("doe",class(.)))
}


summary.doe <- function(object, ...){
  cat("Summary experimental design\n")
  cat("Design:", object$parameter$design, "\n")
  cat("Treatment 1 (n):", length(object$parameter$trt), "\n")
  cat("r:", object$parameter$r, "\n")
  cat("k:", object$parameter$k, "\n")
}





