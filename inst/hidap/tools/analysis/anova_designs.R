# datos <- aov_fbdata()$fieldbook
# datos$INSTN <- as.factor(datos$INSTN) #as.factor
# datos$REP <- as.factor(datos$REP) #as .factor

# output <- rcbd_anova(trait="MTYNA",genotypes="INSTN",repetitions="REP",data=datos)

# trait <- input$aov_trait
# genotypes <-input$aov_fb_genotypes
# rep <- input$aov_fb_rep
# data = aov_fbdata

# output <- rcbd_anova(trait=trait,genotypes=genotypes,repetitions=rep,data=data)

rcbd_anova <- function(trait,genotypes,repetitions,data) {
  # trait <- "MTYNA"
  # genotypes <- "INSTN"
  # repetitions <- "REP" 
  trait <- as.character(trait)
  genotypes <- as.character(genotypes)
  repetitions <- as.character(repetitions)
  datos <- data
  #print(trait);print(genotypes);print(repetitions);
  #print(datos)
  
  data[,genotypes] <-as.factor(data[,genotypes]) 
  #print(data[,genotypes])
  data[,repetitions] <-as.factor(data[,repetitions]) 
  #print(data[,repetitions])
#   data[,genotypes] <- as.factor(data[,genotypes])
#   data[,repetitions] <- as.factor(data[,repetitions])
  
  rcbd_lmf <- paste(trait,"","~",genotypes,"+",repetitions,sep = "") #rcbd linear model formula
  rcbd_lmf <- as.formula(rcbd_lmf)
  modelo <- aov(rcbd_lmf, data = data)
  
  #modelo <- aov(trait ~ factor(genotypes) + factor(REP), data=aov_fbdata) if genotypes and REP 
  #are not factors in fbdata
  tabla <- anova(modelo)
  row.names(tabla)[1] <- "INSTN" #colnames(datos)["INSTN"]
  attr(tabla,"heading")[2] <- paste("Response:",trait)
  nn=names(na.omit(rstandard(modelo)))
  # levene.test seria mas adecuado
  btest <- bartlett.test(na.omit(rstandard(modelo)) ~ datos[nn,genotypes])
  pv2=btest$p.value
  btest$data.name <- "standardized residuals by treatments"
  ntest <- shapiro.test(rstandard(modelo))
  pv1=ntest$p.value
  ntest$data.name <- "standardized residuals"
  
  cat("################################################################################","\n")
  cat(paste("Analysis for", trait), "\n")
  cat("################################################################################","\n")
  cat("################################################################################","\n")
  print(btest)
  cat("################################################################################","\n")
  print(ntest)
  
  #Here start the part of 
  modelo <- aov(rcbd_lmf, data=data) #trait ~ INSTN + REP , data=fielbook
  tabla <- anova(modelo)
  cat("################################################################################","\n")
  print(tabla)
  cat("oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo","\n")
   
  Fc<- tabla[1,5]
  dfree<- df.residual(modelo) #degree freedom
  #glib<- df.residual(modelo) # grados de libertad
  
  #cm<- deviance(modelo)/glib #cuadrado medio del error
  mserror <-deviance(modelo)/dfree #mean square error
  
  if(pv1 >= 0.05 && pv2 >= 0.05)
  {
    glib<- df.residual(modelo)
    cm <- deviance(modelo)/glib
    cat('\n')
    #cat('Analysis of variance','\n')
    cat('---------------------','\n')
    #print(tabla)
    
    if(tabla[1,5]<0.05)
    {   
      cat('\n')
      cat('Tukey multiple comparisons','\n')
      cat('---------------------------------','\n')
      #Variable=data[,trait]  
      #compara<-HSD.test(y = data[,trait],trt = data[,genotypes],glib,cm)
      comparison<-HSD.test(y = data[,trait],trt = data[,genotypes],dfree,cm)
      print(comparison)
      cat('\n')
      #ngraf=paste(getwd(),"/temp/","V ",trait,".jpeg",sep="")
      
    } 
  }
  
}

#example
#rcbd_anova(trait = "MTYNA",genotypes = "INSTN",repetitions = "REP",data = datos)
