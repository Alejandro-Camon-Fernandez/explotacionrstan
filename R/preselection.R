#' @name preselection
#' @rdname preselection
#' @title Algorithm of preselection of linear models
#' @description Five different linear models are fitted and compared to represent the maximum daily temperature data.
#' @param place String. Name of the place whose data we are modeling: "Tx.(place)".
#' @param data List of data. The data set must have the next columns:
#'
#' -(place): Data of the maximum daily temperature of a specific place.
#'
#' -sin1,cos1,sin2,cos2,sin3,cos3,sin4,cos4,sin5,cos5,sin6,cos6: Daily values of the harmonic functions of periods 1 year, 1/2 year, 1/3 year, 1/4 year, 1/5 year and 1/6 year.
#'
#' -year: Year of each temperature.
#'
#' -day.year: Day of the year of each temperature.

#' @param pendiente Boolean. If TRUE the annual trend of the temperature and its interaction with the harmonic functions will be plotted.
#' @param rstanarm Boolean. If TRUE a model Bayesian model will be fitted using the function stan_glm and the parameters of the best linear model.
#' @param bamlss Boolean. If TRUE a model Bayesian model will be fitted using the function bamlss and the parameters of the best linear model.
#' @param verbal Boolean. If TRUE the function will print the results.
#' @return A list of the five models fitted and the answer to which one of them is the best.
#' @aliases preselection preselecion premodel localfits
#' @importFrom rstanarm stan_glm
#' @importFrom bamlss bamlss
#' @examples
#' auxiliar<- read.table( 'Tmax_Aragon.txt', header=T, dec='.'  )
#' auxiliar$sin1 <- sin( (auxiliar$day.year-1)/366  *pi *2 )
#' auxiliar$cos1 <- cos( (auxiliar$day.year-1)/366  *pi *2 )
#' auxiliar$sin2 <- sin( (auxiliar$day.year-1)/366  *pi *4 )
#' auxiliar$cos2 <- cos( (auxiliar$day.year-1)/366  *pi *4 )
#' auxiliar$sin3 <- sin( (auxiliar$day.year-1)/366  *pi *6 )
#' auxiliar$cos3 <- cos( (auxiliar$day.year-1)/366  *pi *6 )
#' auxiliar$sin4 <- sin( (auxiliar$day.year-1)/366  *pi *8 )
#' auxiliar$cos4 <- cos( (auxiliar$day.year-1)/366  *pi *8 )
#' auxiliar$sin5 <- sin( (auxiliar$day.year-1)/366  *pi *10 )
#' auxiliar$cos5 <- cos( (auxiliar$day.year-1)/366  *pi *10 )
#' auxiliar$sin6 <- sin( (auxiliar$day.year-1)/366  *pi *12 )
#' auxiliar$cos6 <- cos( (auxiliar$day.year-1)/366  *pi *12 )

#' preselection("Tx.zaragoza")
NULL

#' @rdname preselection
#' @export
ajuste.parada<-function(place,data = auxiliar,pendiente=FALSE,rstanarm=FALSE,bamlss=FALSE,verbal=TRUE){
  datos<-data
  datos$y<-data[,place]
  datos$y.lag<-c(NA,datos$y[1:(length(datos$y)-1)])
  ajus<-list()
  aux.ajus<-list()
  tipo <- c('year','sin1','cos1','sin2','cos2','sin3','cos3','sin4','cos4','sin5','cos5','sin6','cos6')

  aux.marcador<-!is.na(datos$y.lag)

  ajus$lm0.0 <- lm(y ~ y.lag, data= datos[aux.marcador,])
  ajus$lm1.0 <- lm(y ~ y.lag+sin1+cos1, data= datos[aux.marcador,])
  for (n.arm in 2:6){
    aux.paste<-paste(".~. ",tipo[2*n.arm],tipo[2*n.arm+1],sep="+")
    aux.ajus<-update(ajus$lm1.0,aux.paste,data= datos[aux.marcador,])
    if(anova(ajus$lm1.0,aux.ajus,test='F')[2,6]<=0.001){
      ajus$lm1.0<-aux.ajus
    }else{
      n.arm<-n.arm-1
      break
    }
  }

  ajus$lm1.1 <- update(ajus$lm1.0,.~. +y.lag:(sin1+cos1),data= datos[aux.marcador,])
  if(n.arm>1){
    for (n.int.lag in 2:min(3,n.arm)){
      aux.paste<-paste(".~. ",paste("y.lag:",tipo[2*n.int.lag]),paste("y.lag:",tipo[2*n.int.lag+1]),sep="+")
      aux.ajus<-update(ajus$lm1.1,aux.paste,data= datos[aux.marcador,])
      if(anova(ajus$lm1.1,aux.ajus,test='F')[2,6]<=0.001){
        ajus$lm1.1<-aux.ajus
      }else{
        n.int.lag<-n.int.lag-1
        break
      }
    }
  }else{
    n.int.lag<-1
  }

  ajus$lm2.0 <- update(ajus$lm1.1,.~. +year,data= datos[aux.marcador,])
  ajus$lm2.1 <- update(ajus$lm2.0,.~. +year:(sin1+cos1),data= datos[aux.marcador,])
  if(n.arm>1){
    for (n.int.year in 2:min(3,n.arm)){
      aux.paste<-paste(".~.",paste("year:",tipo[2*n.int.year]),paste("year:",tipo[2*n.int.year+1]),sep="+")
      aux.ajus<-update(ajus$lm2.1,aux.paste,data= datos[aux.marcador,])
      if(anova(ajus$lm2.1,aux.ajus,test='F')[2,6]<=0.001){
        ajus$lm2.1<-aux.ajus
      }else{
        n.int.year<-n.int.year-1
        break
      }
    }
  }else{
    n.int.year<-1
  }


  if(verbal==TRUE){
    print("?Es lm1.0 mejor que lm0.0?")
    print(anova(ajus$lm0.0,ajus$lm1.0,test='F')[2,6]<=0.001)

    print("?Es lm1.1 mejor que lm1.0?")
    print(anova(ajus$lm1.0,ajus$lm1.1,test='F')[2,6]<=0.001)

    print("?Es lm2.0 mejor que lm1.1?")
    print(anova(ajus$lm1.1,ajus$lm2.0,test='F')[2,6]<=0.001)

    print("?Es lm2.1 mejor que lm2.0?")
    print(anova(ajus$lm2.0,ajus$lm2.1,test='F')[2,6]<=0.001)

  }


  if(anova(ajus$lm0.0,ajus$lm1.0,test='F')[2,6]<=0.001){
    aux.ajus<-ajus$lm1.0
    aux.lm <-'lm1.0'
  }else{
    aux.ajus<-ajus$lm0.0
    aux.lm <-'lm0.0'
  }

  if(anova(aux.ajus,ajus$lm1.1,test='F')[2,6]<=0.001){
    aux.ajus<-ajus$lm1.1
    aux.lm <-'lm1.1'
  }

  if(anova(aux.ajus,ajus$lm2.0,test='F')[2,6]<=0.001){
    aux.ajus<-ajus$lm2.0
    aux.lm <-'lm2.0'
  }

  if(anova(aux.ajus,ajus$lm2.1,test='F')[2,6]<=0.001){
    aux.ajus<-ajus$lm2.1
    aux.lm <-'lm2.1'
  }
  ajus$mejor_modelo <- aux.lm

  if(verbal==TRUE){
    cat("n.arm",n.arm,"n.int.lag",n.int.lag,"n.int.year",n.int.year,fill=T,sep=" , ")
  }

  datos$y.var<-c(NA,log((aux.ajus$residuals)^2))

  ajus$var.lm0.0 <- lm(y.var ~ y.lag, data= datos[aux.marcador,])
  ajus$var.lm1.0 <- lm(y.var ~ y.lag+sin1+cos1, data= datos[aux.marcador,])
  for (n.arm in 2:6){
    aux.paste<-paste(".~. ",tipo[2*n.arm],tipo[2*n.arm+1],sep="+")
    aux.ajus<-update(ajus$var.lm1.0,aux.paste,data= datos[aux.marcador,])
    if(anova(ajus$var.lm1.0,aux.ajus,test='F')[2,6]<=0.001){
      ajus$var.lm1.0<-aux.ajus
    }else{
      n.arm<-n.arm-1
      break
    }
  }

  ajus$var.lm1.1 <- update(ajus$var.lm1.0,.~. +y.lag:(sin1+cos1),data= datos[aux.marcador,])
  if(n.arm>1){
    for (n.int.lag in 2:min(3,n.arm)){
      aux.paste<-paste(".~. ",paste("y.lag:",tipo[2*n.int.lag]),paste("y.lag:",tipo[2*n.int.lag+1]),sep="+")
      aux.ajus<-update(ajus$var.lm1.1,aux.paste,data= datos[aux.marcador,])
      if(anova(ajus$var.lm1.1,aux.ajus,test='F')[2,6]<=0.001){
        ajus$var.lm1.1<-aux.ajus
      }else{
        n.int.lag<-n.int.lag-1
        break
      }
    }
  }else{
    n.int.lag<-1
  }

  ajus$var.lm2.0 <- update(ajus$var.lm1.1,.~. +year,data= datos[aux.marcador,])
  ajus$var.lm2.1 <- update(ajus$var.lm2.0,.~. +year:(sin1+cos1),data= datos[aux.marcador,])
  if(n.arm>1){
    for (n.int.year in 2:min(3,n.arm)){
      aux.paste<-paste(".~.",paste("year:",tipo[2*n.int.year]),paste("year:",tipo[2*n.int.year+1]),sep="+")
      aux.ajus<-update(ajus$var.lm2.1,aux.paste,data= datos[aux.marcador,])
      if(anova(ajus$var.lm2.1,aux.ajus,test='F')[2,6]<=0.001){
        ajus$var.lm2.1<-aux.ajus
      }else{
        n.int.year<-n.int.year-1
        break
      }
    }
  }else{
    n.int.year<-1
  }

  if(verbal==TRUE){
    print("?Es var.lm1.0 mejor que var.lm0.0?")
    print(anova(ajus$var.lm0.0,ajus$var.lm1.0,test='F')[2,6]<=0.001)

    print("?Es var.lm1.1 mejor que var.lm1.0?")
    print(anova(ajus$var.lm1.0,ajus$var.lm1.1,test='F')[2,6]<=0.001)

    print("?Es var.lm2.0 mejor que var.lm1.1?")
    print(anova(ajus$var.lm1.1,ajus$var.lm2.0,test='F')[2,6]<=0.001)

    print("?Es var.lm2.1 mejor que var.lm2.0?")
    print(anova(ajus$var.lm2.0,ajus$var.lm2.1,test='F')[2,6]<=0.001)

  }


  if(anova(ajus$var.lm0.0,ajus$var.lm1.0,test='F')[2,6]<=0.001){
    aux.ajus<-ajus$var.lm1.0
    aux.lm <-'var.lm1.0'
  }else{
    aux.ajus<-ajus$var.lm0.0
    aux.lm <-'var.lm0.0'
  }

  if(anova(aux.ajus,ajus$var.lm1.1,test='F')[2,6]<=0.001){
    aux.ajus<-ajus$var.lm1.1
    aux.lm <-'var.lm1.1'
  }

  if(anova(aux.ajus,ajus$var.lm2.0,test='F')[2,6]<=0.001){
    aux.ajus<-ajus$var.lm2.0
    aux.lm <-'var.lm2.0'
  }

  if(anova(aux.ajus,ajus$var.lm2.1,test='F')[2,6]<=0.001){
    aux.ajus<-ajus$var.lm2.1
    aux.lm <-'var.lm2.1'
  }
  ajus$mejor_modelo_var <- aux.lm

  if(verbal==TRUE){
    cat("n.arm_var",n.arm,"n.int.lag_var",n.int.lag,"n.int.year_var",n.int.year,fill=T,sep=" , ")
  }

  #defino el array de r cuadrados y la escribo intercalando "&" para poder copiar la tabla en LaTeX
  r2.array<-c(summary(ajus$lm0.0)$r.squared,
              summary(ajus$lm1.0)$r.squared,
              summary(ajus$lm1.1)$r.squared,
              summary(ajus$lm2.0)$r.squared,
              summary(ajus$lm2.1)$r.squared)
  if(verbal==TRUE){
    cat("R cuadrado:",round(r2.array, digits=2),fill=T,sep=" & ")
  }

  r2.var.array<-c(summary(ajus$var.lm0.0)$r.squared,
                  summary(ajus$var.lm1.0)$r.squared,
                  summary(ajus$var.lm1.1)$r.squared,
                  summary(ajus$var.lm2.0)$r.squared,
                  summary(ajus$var.lm2.1)$r.squared)
  if(verbal==TRUE){
    cat("R cuadrado de varianzas:",round(r2.var.array, digits=2),fill=T,sep=" & ")
  }


  if(pendiente==TRUE){
    #hacemos un plot de las pendientes asociadas en lm2.1 a todos los t?rminos con "year"
    indices <- grep("year",rownames(summary(ajus$lm2.1)$coeff))
    pendiente.lm2.1 <- summary(ajus$lm2.1)$coeff[indices[1],1]

    l=1+2*n.int.year
    for(i in 2:l){
      pendiente.lm2.1 <- pendiente.lm2.1 + summary(ajus$lm2.1)$coeff[indices[i],1]*datos[,grep(tipo[i],names(datos))]
    }
    plot(datos$day.year,pendiente.lm2.1, ylab='Incremento en grados/d?cada', xlab='D?a del a?o')
    title(main=paste('lm2.1',place))


    ajus$pendiente.lm2.1 <- pendiente.lm2.1
  }

  if(rstanarm==TRUE){
    modelo.lm.aux.formula<-formula(ajus[[ajus$mejor_modelo]])
    start_time<-Sys.time()
    ajus$mejor_modelo.rstanarm <- stan_glm(modelo.lm.aux.formula, data = datos)
    end_time <- Sys.time()
    cat("Duraci?n del ajuste rstanarm: ",end_time - start_time,fill=T,sep="")
  }

  if(bamlss==TRUE){
    #modelo.lm.aux.formula<-bamlss.formula(list(formula(ajus[[ajus$mejor_modelo]]),formula(ajus[[ajus$mejor_modelo_var]])))
    modelo.lm.aux.formula<-formula(ajus[[ajus$mejor_modelo]])
    start_time<-Sys.time()
    ajus$mejor_modelo.bamlss <- bamlss(modelo.lm.aux.formula, data = datos)
    end_time <- Sys.time()
    cat("Duraci?n del ajuste bamlss: ",end_time - start_time,fill=T,sep="")
  }

  return(ajus)
} #end function





#ajuste.parada<-function(place,data = auxiliar,pendiente=FALSE,rstanarm=FALSE,bamlss=FALSE){
#  datos<-data
#  datos$y<-data[,place]
#  datos$y.lag<-c(NA,datos$y[1:(length(datos$y)-1)])
#  ajus<-list()
#  aux.ajus<-list()
#  tipo <- c('year','sin1','cos1','sin2','cos2','sin3','cos3','sin4','cos4','sin5','cos5','sin6','cos6')
#
#  aux.marcador<-!is.na(datos$y.lag)
#
#  ajus$lm0.0 <- lm(y ~ y.lag, data= datos[aux.marcador,])
#  ajus$lm1.0 <- lm(y ~ y.lag+sin1+cos1, data= datos[aux.marcador,])
#  for (n.arm in 2:6){
#    aux.paste<-paste(".~. ",tipo[2*n.arm],tipo[2*n.arm+1],sep="+")
#    aux.ajus<-update(ajus$lm1.0,aux.paste,data= datos[aux.marcador,])
#    if(anova(ajus$lm1.0,aux.ajus,test='F')[2,6]<=0.001){
#      ajus$lm1.0<-aux.ajus
#    }else{
#      n.arm<-n.arm-1
#      break
#    }
#  }
#
#  ajus$lm1.1 <- update(ajus$lm1.0,.~. +y.lag:(sin1+cos1),data= datos[aux.marcador,])
#  if(n.arm>1){
#    for (n.int.lag in 2:min(3,n.arm)){
#      aux.paste<-paste(".~. ",paste("y.lag:",tipo[2*n.int.lag]),paste("y.lag:",tipo[2*n.int.lag+1]),sep="+")
#      aux.ajus<-update(ajus$lm1.1,aux.paste,data= datos[aux.marcador,])
#      if(anova(ajus$lm1.1,aux.ajus,test='F')[2,6]<=0.001){
#        ajus$lm1.1<-aux.ajus
#      }else{
#        n.int.lag<-n.int.lag-1
#        break
#      }
#    }
#  }else{
#    n.int.lag<-1
#  }
#
#  ajus$lm2.0 <- update(ajus$lm1.1,.~. +year,data= datos[aux.marcador,])
#  ajus$lm2.1 <- update(ajus$lm2.0,.~. +year:(sin1+cos1),data= datos[aux.marcador,])
#  if(n.arm>1){
#    for (n.int.year in 2:min(3,n.arm)){
#      aux.paste<-paste(".~.",paste("year:",tipo[2*n.int.year]),paste("year:",tipo[2*n.int.year+1]),sep="+")
#      aux.ajus<-update(ajus$lm2.1,aux.paste,data= datos[aux.marcador,])
#      if(anova(ajus$lm2.1,aux.ajus,test='F')[2,6]<=0.001){
#        ajus$lm2.1<-aux.ajus
#      }else{
#        n.int.year<-n.int.year-1
#        break
#      }
#    }
#  }else{
#    n.int.year<-1
#  }
#
#
#  print("?Es lm1.0 mejor que lm0.0?")
#  print(anova(ajus$lm0.0,ajus$lm1.0,test='F')[2,6]<=0.001)
#
#  print("?Es lm1.1 mejor que lm1.0?")
#  print(anova(ajus$lm1.0,ajus$lm1.1,test='F')[2,6]<=0.001)
#
#  print("?Es lm2.0 mejor que lm1.1?")
#  print(anova(ajus$lm1.1,ajus$lm2.0,test='F')[2,6]<=0.001)
#
#  print("?Es lm2.1 mejor que lm2.0?")
#  print(anova(ajus$lm2.0,ajus$lm2.1,test='F')[2,6]<=0.001)
#
#
#  if(anova(ajus$lm0.0,ajus$lm1.0,test='F')[2,6]<=0.001){
#    aux.ajus<-ajus$lm1.0
#    aux.lm <-'lm1.0'
#  }else{
#    aux.ajus<-ajus$lm0.0
#    aux.lm <-'lm0.0'
#  }
#
#  if(anova(aux.ajus,ajus$lm1.1,test='F')[2,6]<=0.001){
#    aux.ajus<-ajus$lm1.1
#    aux.lm <-'lm1.1'
#  }
#
#  if(anova(aux.ajus,ajus$lm2.0,test='F')[2,6]<=0.001){
#    aux.ajus<-ajus$lm2.0
#    aux.lm <-'lm2.0'
#  }
#
#  if(anova(aux.ajus,ajus$lm2.1,test='F')[2,6]<=0.001){
#    aux.ajus<-ajus$lm2.1
#    aux.lm <-'lm2.1'
#  }
#  ajus$mejor_modelo <- aux.lm
#
#  cat("n.arm",n.arm,"n.int.lag",n.int.lag,"n.int.year",n.int.year,fill=T,sep=" , ")
#
#  datos$y.var<-c(NA,log((aux.ajus$residuals)^2))
#
#  ajus$var.lm0.0 <- lm(y.var ~ y.lag, data= datos[aux.marcador,])
#  ajus$var.lm1.0 <- lm(y.var ~ y.lag+sin1+cos1, data= datos[aux.marcador,])
#  for (n.arm in 2:6){
#    aux.paste<-paste(".~. ",tipo[2*n.arm],tipo[2*n.arm+1],sep="+")
#    aux.ajus<-update(ajus$var.lm1.0,aux.paste,data= datos[aux.marcador,])
#    if(anova(ajus$var.lm1.0,aux.ajus,test='F')[2,6]<=0.001){
#      ajus$var.lm1.0<-aux.ajus
#    }else{
#      n.arm<-n.arm-1
#      break
#    }
#  }
#
#  ajus$var.lm1.1 <- update(ajus$var.lm1.0,.~. +y.lag:(sin1+cos1),data= datos[aux.marcador,])
#  if(n.arm>1){
#    for (n.int.lag in 2:min(3,n.arm)){
#      aux.paste<-paste(".~. ",paste("y.lag:",tipo[2*n.int.lag]),paste("y.lag:",tipo[2*n.int.lag+1]),sep="+")
#      aux.ajus<-update(ajus$var.lm1.1,aux.paste,data= datos[aux.marcador,])
#      if(anova(ajus$var.lm1.1,aux.ajus,test='F')[2,6]<=0.001){
#        ajus$var.lm1.1<-aux.ajus
#      }else{
#        n.int.lag<-n.int.lag-1
#        break
#      }
#    }
#  }else{
#    n.int.lag<-1
#  }
#
#  ajus$var.lm2.0 <- update(ajus$var.lm1.1,.~. +year,data= datos[aux.marcador,])
#  ajus$var.lm2.1 <- update(ajus$var.lm2.0,.~. +year:(sin1+cos1),data= datos[aux.marcador,])
#  if(n.arm>1){
#    for (n.int.year in 2:min(3,n.arm)){
#      aux.paste<-paste(".~.",paste("year:",tipo[2*n.int.year]),paste("year:",tipo[2*n.int.year+1]),sep="+")
#      aux.ajus<-update(ajus$var.lm2.1,aux.paste,data= datos[aux.marcador,])
#      if(anova(ajus$var.lm2.1,aux.ajus,test='F')[2,6]<=0.001){
#        ajus$var.lm2.1<-aux.ajus
#      }else{
#        n.int.year<-n.int.year-1
#        break
#      }
#    }
#  }else{
#    n.int.year<-1
#  }
#
#  print("?Es var.lm1.0 mejor que var.lm0.0?")
#  print(anova(ajus$var.lm0.0,ajus$var.lm1.0,test='F')[2,6]<=0.001)
#
#  print("?Es var.lm1.1 mejor que var.lm1.0?")
#  print(anova(ajus$var.lm1.0,ajus$var.lm1.1,test='F')[2,6]<=0.001)
#
#  print("?Es var.lm2.0 mejor que var.lm1.1?")
#  print(anova(ajus$var.lm1.1,ajus$var.lm2.0,test='F')[2,6]<=0.001)
#
#  print("?Es var.lm2.1 mejor que var.lm2.0?")
#  print(anova(ajus$var.lm2.0,ajus$var.lm2.1,test='F')[2,6]<=0.001)
#
#
#  if(anova(ajus$var.lm0.0,ajus$var.lm1.0,test='F')[2,6]<=0.001){
#    aux.ajus<-ajus$var.lm1.0
#    aux.lm <-'var.lm1.0'
#  }else{
#    aux.ajus<-ajus$var.lm0.0
#    aux.lm <-'var.lm0.0'
#  }
#
#  if(anova(aux.ajus,ajus$var.lm1.1,test='F')[2,6]<=0.001){
#    aux.ajus<-ajus$var.lm1.1
#    aux.lm <-'var.lm1.1'
#  }
#
#  if(anova(aux.ajus,ajus$var.lm2.0,test='F')[2,6]<=0.001){
#    aux.ajus<-ajus$var.lm2.0
#    aux.lm <-'var.lm2.0'
#  }
#
#  if(anova(aux.ajus,ajus$var.lm2.1,test='F')[2,6]<=0.001){
#    aux.ajus<-ajus$var.lm2.1
#    aux.lm <-'var.lm2.1'
#  }
#  ajus$mejor_modelo_var <- aux.lm
#
#  cat("n.arm_var",n.arm,"n.int.lag_var",n.int.lag,"n.int.year_var",n.int.year,fill=T,sep=" , ")
#
#  #defino el array de r cuadrados y la escribo intercalando "&" para poder copiar la tabla en LaTeX
#  r2.array<-c(summary(ajus$lm0.0)$r.squared,
#              summary(ajus$lm1.0)$r.squared,
#              summary(ajus$lm1.1)$r.squared,
#              summary(ajus$lm2.0)$r.squared,
#              summary(ajus$lm2.1)$r.squared)
#  cat("R cuadrado:",round(r2.array, digits=2),fill=T,sep=" & ")
#
#  r2.var.array<-c(summary(ajus$var.lm0.0)$r.squared,
#                  summary(ajus$var.lm1.0)$r.squared,
#                  summary(ajus$var.lm1.1)$r.squared,
#                  summary(ajus$var.lm2.0)$r.squared,
#                  summary(ajus$var.lm2.1)$r.squared)
#  cat("R cuadrado de varianzas:",round(r2.var.array, digits=2),fill=T,sep=" & ")
#
#
#  if(pendiente==TRUE){
#    #hacemos un plot de las pendientes asociadas en lm2.1 a todos los t?rminos con "year"
#    indices <- grep("year",rownames(summary(ajus$lm2.1)$coeff))
#    pendiente.lm2.1 <- summary(ajus$lm2.1)$coeff[indices[1],1]
#
#    l=1+2*n.int.year
#    for(i in 2:l){
#      pendiente.lm2.1 <- pendiente.lm2.1 + summary(ajus$lm2.1)$coeff[indices[i],1]*datos[,grep(tipo[i],names(datos))]
#    }
#    plot(datos$day.year,pendiente.lm2.1, ylab='Incremento en grados/d?cada', xlab='D?a del a?o')
#    title(main=paste('lm2.1',place))
#
#
#    ajus$pendiente.lm2.1 <- pendiente.lm2.1
#  }
#
#  if(rstanarm==TRUE){
#    modelo.lm.aux.formula<-formula(ajus[[ajus$mejor_modelo]])
#    ajus$mejor_modelo.rstanarm <- stan_glm(modelo.lm.aux.formula, data = datos)
#  }
#
#  if(bamlss==TRUE){
#    #modelo.lm.aux.formula<-bamlss.formula(list(formula(ajus[[ajus$mejor_modelo]]),formula(ajus[[ajus$mejor_modelo_var]])))
#    modelo.lm.aux.formula<-formula(ajus[[ajus$mejor_modelo]])
#    ajus$mejor_modelo.bamlss <- bamlss(modelo.lm.aux.formula, data = datos)
#  }
#
#  return(ajus)
#} #end function

