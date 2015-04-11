library(stringr)

get_germplasm_lists <- function(){
  #mockup - should connect to database
  x <- paste("LGOFAT2015",LETTERS[1:3], sep="" )
  y <- as.list(x)
  names(y) <- x
  y
}

get_germplasm_ids <- function(gp_list_id){
  # mockup : should get from database
  x = 4
  if(str_detect(gp_list_id,"B")) x = 10
  if(str_detect(gp_list_id,"C")) x = 20
  LETTERS[1:x]
}

get_series_labels <- function(){
  x <- as.list(c("11, 12, ...", "101, 102, ...", "1001, 1002, ..." ) )
  names(x) <- 1:3
  x
}



