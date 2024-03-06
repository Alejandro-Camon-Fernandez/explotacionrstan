#' @name explotacion
#' @rdname explotacion
#' @title Sampling for explotation
#' @description Given a fitted stan model and the dataframe used to fit it, we obtain a matrix of samples for every day.
#' @param aux.X Columns of data used for the model of the mean.
#' @param aux.X.sigma2 Columns of data used for the model of the variance.
#' @param stanfit Stanfit object. Bayesian model that we want to sample from.
#' @param extractedsamples An integer that says how many simulated samples of the stanfit object are used.
#' @param randomsamples An integer that says how many new random samples are obtained from each extracted sample.
#' @param samplefrom An integer that says from how many of the last extracted samples are obtained the extractedsamples. It must be a multiple of the number of chains simulated in the model greater than extractedsamples.
#' @param justParams Boolean. If TRUE the function will return the parameters $\mu$ and $\sigma^2$ instead of the new samples.
#' @importFrom rstan extract
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
#'
#' #Zaragoza
#' modelo.lm<- ajuste.parada(places[13], auxiliar)
#' aux.X <- model.matrix(modelo.lm[[modelo.lm$mejor_modelo]])[-1,]
#' aux.X.sigma2 <- model.matrix(modelo.lm[[modelo.lm$mejor_modelo_var]])[-1,]
#'
#' samples<-samplerstan(aux.X,aux.X.sigma2,modelos.rstan.jul[[places[13]]])
#'
#' #Yesa (simplest model)
#' modelo.lm<- ajuste.parada(places[18], auxiliar)
#' aux.X <- model.matrix(modelo.lm[[modelo.lm$mejor_modelo]])[-1,]
#' aux.X.sigma2 <- model.matrix(modelo.lm[[modelo.lm$mejor_modelo_var]])[-1,]
#'
#' samples<-samplerstan(aux.X,aux.X.sigma2,modelos.rstan.jul[[places[18]]])
#'
#' #Pallaruelo (most complex model)
#' modelo.lm<- ajuste.parada(places[15], auxiliar)
#' aux.X <- model.matrix(modelo.lm[[modelo.lm$mejor_modelo]])[-1,]
#' aux.X.sigma2 <- model.matrix(modelo.lm[[modelo.lm$mejor_modelo_var]])[-1,]
#'
#' samples<-samplerstan(aux.X,aux.X.sigma2,modelos.rstan.jul[[places[15]]],20,1000)
#'
#' monthmeans<-NA
#' monthsd<-NA
#' for(i in 1:12){
#'   sampmonthmeans[i]<-mean(samples[auxiliar$month[-c(1,2)]==i,])
#'   obsmonthmeans[i]<-mean(auxiliar[auxiliar$month[-c(1,2)]==i,places[15]])
#'   sampmonthsd[i]<-sd(samples[auxiliar$month[-c(1,2)]==i,])
#'   obsmonthsd[i]<-sd(auxiliar[auxiliar$month[-c(1,2)]==i,places[15]])
#' }
#' difmonthmeans<-abs(obsmonthmeans-sampmonthmeans)
#' difmonthsd<-obsmonthsd/sampmonthsd
#' min(difmonthsd)
#' max(difmonthsd)
#'
#' #Residuos de las medias y media de los residuos
#' #fechas<-which((auxiliar$month %in% c(6,7,8))&(auxiliar$year>=1981)&(auxiliar$year<=2010))
#' modelo.lm<- ajuste.parada(places[13], auxiliar)
#' aux.X <- model.matrix(modelo.lm[[modelo.lm$mejor_modelo]])[-1,]
#' aux.X.sigma2 <- model.matrix(modelo.lm[[modelo.lm$mejor_modelo_var]])[-1,]
#'
#' params<-samplerstan(aux.X,aux.X.sigma2,modelos.rstan.jul[[places[13]]],justParams=TRUE)
#'
#' meanparams<-list()
#' meanparams$mu<-tapply(params$mu,matrix(rep(1:dim(params$mu)[1],dim(params$mu)[2]),ncol=dim(params$mu)[2],byrow=FALSE),FUN=mean)
#' meanparams$sigma<-tapply(sqrt(params$sigma2),matrix(rep(1:dim(params$sigma2)[1],dim(params$sigma2)[2]),ncol=dim(params$sigma2)[2],byrow=FALSE),FUN=mean)
#' residualsmean<-(auxiliar[-c(1,2),places[13]]-meanparams$mu)/sqrt(meanparams$sigma)
#' meanresiduals<-tapply((auxiliar[-c(1,2),places[13]]-params$mu)/sqrt(params$sigma2),matrix(rep(1:dim(params$mu)[1],dim(params$mu)[2]),ncol=dim(params$mu)[2],byrow=FALSE),FUN=mean)
#'
#' #Histogramas de los residuos (figura TFM)
#' library(lattice)
#' i.place<-1
#' meses<-c('Enero','Febrero','Marzo','Abril','Mayo','Junio','Julio','Agosto','Septiembre','Octubre','Noviembre','Diciembre')
#' #Tarda unos 5 minutos por figura
#'
#' modelo.lm<- ajuste.parada(places[i.place], auxiliar)
#' aux.X <- model.matrix(modelo.lm[[modelo.lm$mejor_modelo]])[-1,]
#' aux.X.sigma2 <- model.matrix(modelo.lm[[modelo.lm$mejor_modelo_var]])[-1,]
#'
#' params<-samplerstan(aux.X,aux.X.sigma2,modelos.rstan.jul[[places[i.place]]],justParams=TRUE)
#' residuals<-(auxiliar[-c(1,2),places[i.place]]-params$mu)/sqrt(params$sigma2)
#' meanresiduals<-tapply(residuals,matrix(rep(1:dim(residuals)[1],dim(residuals)[2]),ncol=dim(residuals)[2],byrow=FALSE),FUN=mean)
#'
#' aux.factor<-factor(auxiliar$month[-(1:2)], levels=c(1:12), labels=meses)
#' pdf(paste("C:/Universidad/TFG/TFG Analisis Bayesiano/TFM imagenes/histogramas residuos mensuales ",places[i.place],".pdf",sep=""),height=4.5,width=5.5)
#' histogram(~meanresiduals|aux.factor,xlab="Residuos",ylab="Porcentaje del total")
#' dev.off()
#'
#'
#' #boxplot(split(basura$desvest[[places[i.station]]],aux.df$month),xlab="Meses",ylab="ºC",cex.lab=1.5)
#' #lines(1:12,tapply(basura$desvest[[places[i.station]]], aux.df$month, mean))
#' #lines(1:12,tapply(exp(c(NA,predict(modelos.lm[[places[i.station]]][[modelos.lm[[places[i.station]]]$mejor_modelo_var]])/2)), aux.df$month, mean))
#'
#' residualsmonthmean<-tapply(residuals,matrix(rep(aux.factor,dim(residuals)[2]),ncol=dim(residuals)[2],byrow=FALSE),FUN=mean)
#' residualsmonthsd<-tapply(residuals,matrix(rep(aux.factor,dim(residuals)[2]),ncol=dim(residuals)[2],byrow=FALSE),FUN=sd)
#'
#'
#' #Distribuciones a posteriori de enero (o junio) 1956-1960 (o 2009-2013) en función de los datos observados en Zaragoza
#' i.place<-1
#' firstyear<-2009
#' lastyear<-2013
#' meses<-c('Enero','Febrero','Marzo','Abril','Mayo','Junio','Julio','Agosto','Septiembre','Octubre','Noviembre','Diciembre')
#' i.month<-6
#' #Tarda unos 20 segundos por figura
#'
#' for(i.place in 1:18){
#'  for(i.month in c(12)){
#'   for(firstyear in c(1955,2008)){
#'    lastyear<-firstyear+4
#'    pdf(paste("C:/Universidad/TFG/TFG Analisis Bayesiano/TFM imagenes/distribucion estimada en funcion de datos observados ",meses[i.month]," ",firstyear,"-",lastyear," ",places[i.place],".pdf",sep=""),height=4.5,width=5.5)
#'    fechas<-which((auxiliar$month==i.month)&(auxiliar$year>=firstyear)&(auxiliar$year<=lastyear))
#'    modelo.lm<- ajuste.parada(places[i.place], auxiliar)
#'    aux.X <- model.matrix(modelo.lm[[modelo.lm$mejor_modelo]])[fechas-1,]
#'    aux.X.sigma2 <- model.matrix(modelo.lm[[modelo.lm$mejor_modelo_var]])[fechas-1,]
#'    samples<-samplerstan(aux.X,aux.X.sigma2,modelos.rstan.jul[[places[i.place]]])
#'    y1<-NULL
#'    y2<-NULL
#'    for(i in 1:nrow(samples)){
#'      y1<-cbind(y1,quantile(samples[i,],0.01))
#'      y2<-cbind(y2,quantile(samples[i,],0.99))
#'    }
#'    plot(auxiliar[fechas,places[i.place]]/10,rowMeans(samples)/10,xlab="Tx observadas (ºC)",ylab="Tx estimadas (ºC)",ylim=c(min(y1)/10,max(y2)/10))
#'    segments(auxiliar[fechas,places[i.place]]/10,y1/10,auxiliar[fechas,places[i.place]]/10,y2/10)
#'    abline(a=0,b=1)
#'    dev.off()
#'   }
#'  }
#' }
#'
NULL

#' @rdname explotacion
#' @export
samplerstan<-function(aux.X,aux.X.sigma2,stanfit,extractedsamples=2000,randomsamples=10,samplefrom=2000,justParams=FALSE){

  parnames<-names(rstan::extract(stanfit,permuted=FALSE)[1,1,])
  betasigma2index<-grep("betasigma",parnames)
  betaindex<-grep("beta",parnames[-betasigma2index])

  betasamples<-rstan::extract(stanfit,permuted=FALSE)[,,betaindex] #La primera dimensión son las simulaciones, la segunda las cadenas y la tercera los parámetros
  betasigma2samples<-rstan::extract(stanfit,permuted=FALSE)[,,betasigma2index]


  if((samplefrom%%dim(betasamples)[2]==0)&&(dim(betasamples)[1]>=samplefrom/dim(betasamples)[2])&&(samplefrom>=extractedsamples)){
    sfperchain<-samplefrom/dim(betasamples)[2]
    esperchain<-extractedsamples/dim(betasamples)[2]
    betasamples<-betasamples[seq(dim(betasamples)[1]-sfperchain+1,dim(betasamples)[1],length.out=esperchain),,]
    betasigma2samples<-betasigma2samples[seq(dim(betasigma2samples)[1]-sfperchain+1,dim(betasigma2samples)[1],length.out=esperchain),,]

    betasamples<-matrix(betasamples,dim(betasamples)[1]*dim(betasamples)[2],dim(betasamples)[3])
    betasigma2samples<-matrix(betasigma2samples,dim(betasigma2samples)[1]*dim(betasigma2samples)[2],dim(betasigma2samples)[3])

    mu<-matrix(nrow=dim(aux.X)[1],ncol=dim(betasamples)[1])
    sigma2<-matrix(nrow=dim(aux.X.sigma2)[1],ncol=dim(betasigma2samples)[1])
    newsamples<-matrix(nrow=dim(aux.X)[1],ncol=dim(betasamples)[1]*randomsamples)

    for(i in 1:dim(aux.X)[1]){
      for(j in 1:dim(betasamples)[1]){ #bucle lento, seguramente se pueda acelerar con algún apply
        mu[i,j]<-betasamples[j,]%*%aux.X[i,]
        sigma2[i,j]<-exp(betasigma2samples[j,]%*%aux.X.sigma2[i,])
      }
      if(justParams==FALSE){
        newsamples[i,]<-rnorm(dim(mu)[2]*randomsamples,rep(mu[i,],each=randomsamples),rep(sqrt(sigma2[i,]),each=randomsamples))
      }
    }

    if(justParams==FALSE){
      return(newsamples)
    }else if(justParams==TRUE){ #Devuelvo solo las matrices mu y sigma2
      params<-list()
      params$mu<-mu
      params$sigma2<-sigma2
      return(params)
    }
  }else{warning("extractedsamples must be equal or lower than 2000 and the number of the samples of the stanfit object must be equal or bigger than 2000. And samplefrom must be a multiple of the number of chainssimulated in the stan model")}

}


