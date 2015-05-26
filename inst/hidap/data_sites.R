fp_sites <- "Z:\\hidap\\inst\\hidap\\sites\\Master-list-trial-sites.xlsx"

data_sites <-function() {
  #fp_sites <- "C:\\Omar-2015\\hidap\\inst\\hidap\\sites\\Master-list-trial-sites.xlsx"
  fp_sites <- "Z:\\hidap\\inst\\hidap\\sites\\Master-list-trial-sites.xlsx"
  
  d_sites <- readxl::read_excel(path = fp_sites,sheet = "Sites",skip = 1)
  d_sites <- dplyr::select_(d_sites,"SHORTN","FULLN","LOCAL","LATD","LOND","ELEV",
                            "CROPS","AEZ","CONT","CREG","CNTRY","ADM4","ADM3","ADM2","ADM1") #%>%
  d_sites
} 

list_countries <- function(data_sites){
  out <- as.list((unique((sort(data_sites$CNTRY)))))
  out
}

filter_sites <- function(data_sites,country_input){
  
  filter_country_locality <-  dplyr::select_(data_sites,"SHORTN","FULLN","LOCAL","LATD","LOND","ELEV",
                                             "CROPS","AEZ","CONT","CREG","CNTRY","ADM4","ADM3","ADM2","ADM1") %>%
    dplyr::filter(.,CNTRY==country_input) 
  #   %>%
  #     dplyr::filter(.,LOCAL==locality_input) 
  #   
  filter_sites <- filter_country_locality
  
  sites_labels<- paste(filter_sites$LOCAL," ","(",filter_sites$SHORTN,")",sep="")
  sites_list_inputs <- as.list(filter_sites$LOCAL)
  names(sites_list_inputs) <- sites_labels
  #out <- sites_list_inputs
  out <- sites_labels
  out
  
  
  #out <- filter_country_locality
  #out <- list(countries=countries,filter_country=filter_country, sites_list_inputs= sites_list_inputs)
  
}


