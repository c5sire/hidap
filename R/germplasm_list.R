library(stringr)

get_germplasm_lists <- function(){
  #mockup - should connect to database
  x <- paste("lgofat2015", LETTERS[1:6], sep="" )
  y <- as.list(x)
  names(y) <- x
  y
}

get_germplasm_ids <- function(gp_list_id){
  # mockup : should get from database
  x = 21
  if(str_detect(gp_list_id[1],"B")) x = 10
  if(str_detect(gp_list_id[1],"C")) x = 20
  
  if(str_detect(gp_list_id[1],"D")) {
    x = 100
  }
  if(str_detect(gp_list_id[1],"E")) {
    x = 30
  }
  if(str_detect(gp_list_id[1],"F")) {
    x = 9
  }
  paste0("id_",1:x)
}

get_series_labels <- function(){
  x <- as.list(c("11, 12, ...", "101, 102, ...", "1001, 1002, ..." ) )
  names(x) <- 1:3
  x
}



