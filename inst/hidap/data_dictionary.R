fp <- "D:\\Users\\obenites\\Desktop\\ontologies-potato-XLSX.xlsx"
data_dictionary <- function(fp){
                   ddict <- readxl::read_excel(path = fp,sheet = "Template for submission",skip = 5)
                   ddict <- dplyr::select(ddict,VAR,ABBR)
                   ddict <- paste("[",ddict$ABBR,"]",": ",ddict$VAR,sep="")
}