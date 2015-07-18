#fp <- "D:\\Users\\obenites\\Desktop\\ontologies-potato-XLSX.xlsx"
#fp <- "Z:\\hidap\\inst\\hidap\\ontologies\\ontologies_potato.xlsx"
#fp1 <- "Z:\\hidap\\inst\\hidap\\ontologies\\ontologies_potato.xlsx"
#fp1 <- "Z:\\hidap\\inst\\hidap\\ontologies\\ontologies_sweetpotato.xlsx"
data_dictionary <- function(fp,trial){
#                  if(crop=="Potato"){fp <- "Z:\\hidap\\inst\\hidap\\ontologies\\ontologies_potato.xlsx"}
#                  if(crop=="Sweetpotato"){fp <- "Z:\\hidap\\inst\\hidap\\ontologies\\ontologies_sweetpotato.xlsx"}
#                               
                   ddict <- readxl::read_excel(path = fp,sheet = "Template for submission",skip = 5)
                   ddict <- dplyr::select(ddict,VAR,ABBR)
                   var <- stringr::str_trim(ddict$VAR,side="both")
                   abbr <- stringr::str_trim(ddict$ABBR,side = "both")
                   var_labels<- paste("[",abbr,"]",": ",var,sep="")
                   var_list_checkbox <- as.list(abbr)
                   names(var_list_checkbox) <- var_labels
                   #                    
                   output <- var_list_checkbox
                   
                   #out <- list(ddict,)
}

var_list <- function(crop,trial){
  getResourcePath(type="dictionary",crop) %>% data_dictionary(.)
}


crop_vars <- function(crop,vars){
fp <- getResourcePath(type="dictionary",crop)
cropvars <- readxl::read_excel(path = fp,sheet = "Template for submission",skip = 5)
cropvars <- dplyr::select(cropvars,VAR,ABBR)
cropvars <- dplyr::filter(cropvars, ABBR %in% vars)
cropvars <- cropvars$VAR
}

#Mas adelante usare el argumento "trial" para separar por trials

#data_dictionary(fp, crop = "potato",trial="yield")
#data_dictionary(fp, crop = "sweetpotato",trial="yield")