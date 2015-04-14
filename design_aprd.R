design_crd <- function(trt, ...){
  n = length(trt)
  #stopifnot(n > 3 & n < 10)
  if(n < 4 | n > 9) stop("The number of treatments in a CRD must be > 3 and < 10.")
  design.crd(trt, ...)
}

design_rcbd <- function(trt, ...){
  n = length(trt)
  #stopifnot(n > 3 & n < 10)
  if(n < 4 | n > 30) stop("The number of treatments in a CRD must be > 3 and < 31.")
  design.rcbd(trt, ...)
}

design_lsd <- function(trt, ...){
  n = length(trt)
  #stopifnot(n > 3 & n < 10)
  if(n < 4 | n > 10) stop("The number of treatments in a CRD must be > 3 and < 11.")
  design.lsd(trt, ...)
}

design_gld <- function(trt, trt2, ...){
  n = length(trt)
  #stopifnot(n > 3 & n < 10)
  valid <- c(4, 7:13, 5, 17, 19, 21, 23, 25)
  if(!n %in% valid ) stop("The number of treatments in a GLD must be one of 4, 7-13, 15, 17, 19, 21, 23, 25.")
  if(length(trt) != length(trt2)) stop("The number of entries in a GLD must be equal in both treatments.")
  design.graeco(trt, trt2, ...)
}

design_yd <- function(trt, r, ...){
  n = length(trt)
  #stopifnot(n > 3 & n < 10)
  if(n < 4 | n > 10) stop("The number of treatments in a YD must be > 3 and < 11.")
  if(r < 2 | r > n) stop("The number of replications in a Youden design must be > 1 and <= number of treatments.")
  design.youden(trt, r, ...)
}

design_bib <- function(trt, k, ...){
  n = length(trt)
  #stopifnot(n > 3 & n < 10)
  if(n < 4) stop("The number of treatments in a YD must be > 3.")
  design.bib(trt, k, ...)
}

design_cd <- function(trt, r, k, rowcol, ...){
  n = length(trt)
  if(n < 6 | n > 30) stop("The number of treatments in a CD must be > 5 and < 31.")
  if(k < 2 | k > 10) stop("k in a CD must be > 1 and < 11.")
  if(r %% k !=0) stop("r must be a multiple of k.")
  design.cyclic(trt, k, r, rowcol = rowcol, ...)
}

design_ld <- function(trt, r, ...){
  n = length(trt)
  #stopifnot(n > 3 & n < 10)
  if(n < 4) stop("The number of treatments in a LD must be > 3.")
  if(r < 2 | r > 3) stop("The value of r must be 2 or 3.")
  if(n %% r != 0) stop("The number of treatments must be a multiple of r.")
  design.lattice(trt, r, ...)
}

design_aprd <- function(trt1, trt2, frac = 0.3, r=2, ... 
                        ){
  stopifnot(is.numeric(frac))
  stopifnot(frac>=0 & frac < 1)
  # checks are in trt1
  # new varieties in trt2
  size <- max(0, trunc(length(trt2) * frac))
  t2 <- sample(trt2, size)
  trt1 <- c(trt1, t2)
  trt2 <- trt2[!trt2 %in% t2]
  
  design.dau(trt1, trt2, r=r, ...)
}
