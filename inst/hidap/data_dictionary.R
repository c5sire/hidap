fp <- "D:\\Users\\obenites\\Desktop\\ontologies-potato-XLSX.xlsx"
data_dictionary <- function(fp){
                   ddict <- readxl::read_excel(path = fp,sheet = "Template for submission",skip = 5)
                   ddict <- dplyr::select(ddict,VAR,ABBR)
                   var <- ddict$VAR
                   abbr <- ddict$ABBR
                   var_labels<- paste("[",ddict$ABBR,"]"," :",ddict$VAR,sep="")
                   var_list_checkbox <- as.list(abbr)
                   names(var_list_checkbox) <- var_labels
                  #                    
                   output <- var_list_checkbox   
                   #out <- list(ddict,)
}
