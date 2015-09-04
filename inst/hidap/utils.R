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
#'@description This function gets the folde's path

folderPath = function(folder){
  #  season = getSeason(pref.defaults)
  #  path = file.path(getBaseDir(),getCurrentCrop())
  file.path(getBaseDir(),folder)
}

#'@description This function gets the crop folders path in PC

cropPath = function(getCurrentCrop){
  #  season = getSeason(pref.defaults)
  #  path = file.path(getBaseDir(),getCurrentCrop())
  file.path(getBaseDir(),"templates",tolower(getCurrentCrop))
}


#### Utils functions to tranfers xlsx temp files to HIDAP data folders

#'@description Function to get the folder file name given a shiny object (fileInput)
getfolder_file <-  function(file_name){
  folder_file <- file_name %>% gsub(pattern = "_.*","",.) %>%  gsub(pattern = "[^0-9]*","",.)
  folder_file
  print(folder_file)
}

#'@description Function to get the trial abbreviation file given a shiny object (fileInput)
gettrial_abb_file <- function(file_name){  
  croptrial_abb <- gsub(pattern = "[a-z0-9].*","",file_name) #crop and trial abbreviation of files
  pt_fix <- "PT" #for potato trial
  sp_fix <- "SP" #for sweetpotato trial
  cva_fix <- "CVA" # for cassava trial
  #stringr::str_detect(string = croptrial ,pattern = r)
  if(stringr::str_detect(croptrial_abb, pt_fix)) trial_abb <- gsub(pattern = pt_fix,"",croptrial_abb)
  if(stringr::str_detect(croptrial_abb, sp_fix)) trial_abb  <- gsub(pattern = sp_fix, "",croptrial_abb ) 
 trial_abb
 print(trial_abb)
} #get the abbreviation file 

#'@description Function to get the trial abbreviation file given a shiny object (fileInput)
getcrop_file <- function(file_name){
  if(grepl("PT",file_name))  crop <- "potato"
  if(grepl("SP",file_name))  crop <- "sweetpotato"
  if(grepl("CVA",file_name)) crop <- "cassava"
  #crop
  #if(stringr::str_detect(string = file_name,"PT"))  crop <- "potato"
  #if(stringr::str_detect(string = file_name,"SP"))  crop <- "sweetpotato"
  #if(stringr::str_detect(string = file_name,"CVA")) crop <- "cassava"
  crop
  print(crop)
}

gettrial_name <- function(file){ 
  
  if(grep) trial_name <- list(trial_name = "Yield")
  if(trial_abb=="PTLB") trial_name <- list(trial_name = "Late Blight")
  if(trial_abb=="STYL") trial_name <- list(trial_name = "Sweetpotato Yield" )
  trial_name
  
}

croptrials_name <- function(crop){
     if(crop=="potato"){out <- list("Yield(PTYL)"="YL","Late Blight(LB)" ="LB")}
     if(crop=="sweetpotato"){out <- list("Yield(STYL)"="YL","Morphology(SPMOR)" ="MOR")} 
     out
}

#'@description This function gives a xlsx file name for temporary files uplodad using SHINY.
tempfile_name <-  function(file_name){
  fb_temp_excel <- paste(file_name,".xlsx",sep = "")
  fb_temp_excel
  print(fb_temp_excel)
}

#dir_name <- file.path(folder_to,tolower(input$doe_type_crop),fb_folder_file,sep = "") 
#'@description This function gives the new folder_path for the temporary file
folder_path  <- function(folder_to,crop,folder_file){
  path <- file.path(folder_to,crop,folder_file,sep = "")
#   newpath <- file.path(path,file_name,sep = "")
#   newpath
  path
  print(path)
}

new_file_path <- function(folder_path,file_name){
  newpath <- file.path(folder_path,file_name,sep = "")
  newpath
  print(newpath)
}

####FILENAME'S METADATA 

#'@description  function to get the fieldbook's file name
getfilename_book <- function(file_path){
  output <- gsub(".*/",replacement = "",file_path) 
  output
}

#'@description function to get data file name
getdate_file<-function(file_name){ 
  year <- str_sub(getfolder_file(file_name),1,4)
  month <- str_sub(getfolder_file(file_name),5,6)
  output <- list(year=year,month=month)
}

#'@description fuction to get the location o trial site of the file
getlocation_file <- function(file_name){
  output <-gsub(pattern = ".*_",replacement = "",x = file_name)
  output <- gsub(pattern = ".xlsx",replacement = "",x = output)
  #output <-grep(pattern = ".*_",x = file_name,value = "")
  output
}



####################
#'@description Function to get the trait type given a data dictionary.
trait_type <- function(trait,datadict)
{
  tp <- as.character(datadict[datadict$ABBR==trait,c("TYPE")]) 
  stringr::str_trim(tp,side="both")
  #   if(tp=="Continuous" || tp=="Discrete"){
  #   #tp <- "quantitative"   
  #   tp <- tp
  #   }
  #   if(tp=="Categorical"){
  #   #tp <- "categorical"  
  #   tp <- tp
  #     }
  if(is.na(tp)){
    tp <- "Continuous"
  }
  return(tp)
}

#'@description Function to get the scale of differents trait, 
#'dependig if Its continous/discrete/categorical variable
scale_trait <- function(trait,datadict){
  
  tp <- trait_type(trait = trait,datadict = datadict)
  
  if(tp=="Continuous"||tp=="Discrete"){
    
    ll <- as.numeric(datadict[datadict$ABBR==trait,c("LOWER")])
    ul <- as.numeric(datadict[datadict$ABBR==trait,c("UPPER")])
    output <- list(ll=ll,ul=ul)
  }
  
  if(tp=="Categorical"){
    cat_scale <- datadict[datadict$ABBR == trait, c("CLASS1","CLASS2","CLASS3","CLASS4","CLASS5","CLASS6","CLASS7","CLASS8","CLASS9","CLASS10")]
    pattern <- "= .*$"
    cat_scale <- gsub(pattern=pattern,replacement = "",x = cat_scale)
    cat_scale <- suppressWarnings(as.numeric(cat_scale))
    cat_scale <- as.numeric(stringr::str_trim(cat_scale[!is.na(cat_scale)],side="both"))
    output <- list(cat_scale=cat_scale)
  }
  
  invisible(output)
  
}

#'@description This function use openxlsx package to give conditional format (paint with colours) 
#'for every trait column based on their own data dictionary.
conditionalformat_trait <- function(fp,trait,datadict,fbSheet="Fieldbook"){ 
  
  wb <- openxlsx::loadWorkbook(fp)
  #fieldbook <- readxl::read_excel(fp,sheet = "Fieldbook")
  fieldbook <- readxl::read_excel(fp,sheet = fbSheet)
  fieldbook <- as.data.frame(fieldbook)
  #sheet <- "Fieldbook"  
  sheet <- fbSheet
  # flag=TRUE #quantitative
  tp <- trait_type(trait=trait,datadict=datadict)
  out <-scale_trait(trait=trait,datadict=datadict) 
  print(trait)
  col_trait <- fieldbook[,trait]
  col_number <- which(names(fieldbook)==trait)
  nc <- nrow(fieldbook)+1
  
  negStyle <- openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  posStyle <- openxlsx::createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
  
  if(tp == "Continuous"|| tp == "Discrete"){
    print(out$ll)
    print(out$ul)
    #       openxlsx::conditionalFormatting(wb, sheet = sheet, cols=col_number, rows=2:nc, rule = sprintf("<%s", out$ul), style = posStyle)#OK
    #       openxlsx::conditionalFormatting(wb, sheet = sheet, cols=col_number, rows=2:nc, rule = sprintf(">%s", out$ll), style = posStyle)#OK
    openxlsx::conditionalFormatting(wb, sheet = sheet, cols=col_number, rows=2:nc, rule=sprintf(">%s", out$ul), style = negStyle)#WRONG
    openxlsx::conditionalFormatting(wb, sheet = sheet, cols=col_number, rows=2:nc, rule=sprintf("<%s", out$ll), style = negStyle)#WRONG
    openxlsx::conditionalFormatting(wb, sheet = sheet, cols = col_number, rows = 2:nc, rule = c(out$ll,out$ul), style = posStyle,type = "between" )
  } 
  
  if(tp =="Categorical"){
    
    print(out$cat_scale)
    out_values <- col_trait[!is.element(el = col_trait,set = out$cat_scale)]
    print("ok")
    print("out_values")
    for(i in out_values)
      openxlsx::conditionalFormatting(wb, sheet = sheet, cols = col_number, rows = 2:nc, rule = sprintf("==%s", i),style = negStyle)     
  }
  
  
  openxlsx::saveWorkbook(wb,file = fp,overwrite = TRUE)
}

#'@description This function paint wrong cip numbers in fieldbook using openxlsx package and regular expressions.
conditionalFormat_cipnumber <- function(fp,sheetName,cip_colname="INSTN"){
  wb <- openxlsx::loadWorkbook(fp)
  book <- readxl::read_excel(path = fp,sheet=sheetName)
  book <- as.data.frame(book)
  sheet <- sheetName
  nc <- nrow(book)+1
  col_number <- which(names(book)==cip_colname)
  a <- sbformula::cip_number_check(book[,cip_colname])
  cipwrong <- a$cipnumber_wrong
  if(length(cipwrong)>0){
    regla <-  unique(cipwrong)
    print(regla)
    for(i in regla){
      openxlsx::conditionalFormatting(wb, sheet = sheet, cols = col_number, rows = 2:nc, type = "contains", rule = i )
    }
    openxlsx::saveWorkbook(wb,file = fp,overwrite = TRUE)
  }
}

#'@description This function gets parameters or values from fieldbook excel file. Do an excel scrapping.
get.fb.param <-function(fp,sheet,param){
  params <- readxl::read_excel(path = fp, sheet = sheet)
  params <- as.data.frame(params)
  lapply(x <- 1:ncol(params), function(x)  params[,x]<-as.character(params[,x]))
  #for(i in 1:ncol(params)) params[,i]<-as.character(params[,i])
  params[params$Factor==param,2]
}

#get.fb.param(fp,"Installation","Experimental design")
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
