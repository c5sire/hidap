filename = function(fpath){
  fn = list.files(fpath, ".xlsx", r=T)
  if(length(fn)==0) fn = list.files(fpath, ".xls", r=T)
  fn = basename(fn)
  #fn = sapply(fn,strsplit(fn,"\\.")[[1]][1])
  ff = function(x) strsplit(x,"\\.")[[1]][1]
  fn = sapply(fn,ff)
  as.character(fn)
}

data.dir = function(){
  res = getwd()
  res
}

getBaseDir <- function(){
  file.path(data.dir())
  
}

getResourcePath <- function(type="dictionary",crop){
  #path = getBaseDir()
  #subdir  <-  "inst/hidap"
  fil  <-  ""
  if(type=="sites"){
    ext <-"sites" 
    fil  <-  "Master-list-trial-sites.xlsx" 
  }
  if(type=="dictionary"){
    #ontologies_potato.xlsx
    #fil = "crop_ontologies.xlsx" 
      if(crop=="Potato"){
        ext  <-  "ontologies"
        fil  <-  "ontologies_potato.xlsx"
      }
      if(crop=="Sweetpotato"){
        ext <- "ontologies"
        fil <- "ontologies_sweetpotato.xlsx"
      }
  }
  #path = file.path(getBaseDir(),subdir,ext,fil)
  path = file.path(getBaseDir(),ext,fil)
  return(path)
}

folderPath = function(folder){
  #  season = getSeason(pref.defaults)
  #  path = file.path(getBaseDir(),getCurrentCrop())
  file.path(getBaseDir(),folder)
}

cropPath = function(getCurrentCrop){
  #  season = getSeason(pref.defaults)
  #  path = file.path(getBaseDir(),getCurrentCrop())
  file.path(getBaseDir(),"templates",tolower(getCurrentCrop))
}



# 
# cropResourcePath <- function(type="data",crop){
#   #path = getBaseDir()
#   #subdir  <-  "inst/hidap"
#   fil  <-  ""
#   if(type=="data"){
#     if(crop=="potato"){
#       ext <- "potato"
#       fil <- 
#       
#     }
#     if(crop=="sweetpotato"){
#       ext <- 
#         fil <- 
#         
#     }
#     
#      
#     
#    
#   }
#   #path = file.path(getBaseDir(),subdir,ext,fil)
#   path = file.path(getBaseDir(),ext,fil)
#   return(path)
# }





#var_list(crop="Sweetpotato") OK
#a <- var_list(crop="Potato") OK

# var_list(type="dictionary",crop="Potato") not run
# var_list(type="dictionary",crop="Sweetpotato") not run
#getResourcePath(type,doe_type_crop)
