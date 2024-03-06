#' @name sampleqqplot
#' @rdname sampleqqplot
#' @title Q-Q plot of the quantile distributions
#' @description Using the empiric and sampled quantiles, prepare a Q-Q plot with the chosen simple credibility interval and calculate the bias.
#' @param marginquantilematrix List with the arrays of samples for each marginal quantile.
#' @param empiricquantilevector List of the empiric marginal quantiles.
#' @param qqplot Boolean. If TRUE the function will represent the q-q plot. Otherwise it will just calculate its important summary information.
#' @param range Radius of the interval around the y=x (sampled=empirical)function where we consider the sampled marginal quantiles as correct. Its default value is 1.
#' @param xlab,ylab Axis labels of the plot. Their default values are "Empiric margin quantiles" and "Simulated margin quantiles", respectively.
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
#' #####10 de enero de 1955, Zaragoza#####
#' marginquantilematrix2<-samplequant2(places[13], auxiliar,modelos.rstan.jul[[places[13]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[740,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),extractedsamples=200,randomsamples=200)
#' empiricquantilevector<-quantmarginal(auxiliar[places[13]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[740,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),randomsamples=1)
#'
#' needdatesindex<-NULL
#' aux.needdatesindex<-which((auxiliar$year[-c(1,2)]>=1953)&(auxiliar$year[-c(1,2)]<=1957)&(auxiliar$day.year[-c(1,2)]==10))
#' for(j in aux.needdatesindex){
#'   needdatesindex<-c(needdatesindex,(j-7):(j+7))
#' }
#' sampleqqplot(marginquantilematrix2,empiricquantilevector)
#'
#' #####deciles 7 de julio de 1955, Zaragoza#####
#' i.place<-13
#' fecha<-which(auxiliar$year==1955 & auxiliar$month==7 & auxiliar$day.month==7)
#' marginquantilematrix2<-samplequant2(places[i.place], auxiliar,modelos.rstan.jul[[places[i.place]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[fecha,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),extractedsamples=200,randomsamples=200)
#' empiricquantilevector<-quantmarginal(auxiliar[places[i.place]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[fecha,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),randomsamples=1)
#' sampleqqplot(marginquantilematrix2,empiricquantilevector)
#'
#' #####centiles 7 de julio de 2013, Zaragoza##### (EJEMPLOS PARA TFM)
#' i.place<-13
#' fecha<-which(auxiliar$year==2013 & auxiliar$month==7 & auxiliar$day.month==7)
#' marginquantilematrix2<-samplequant2(places[i.place], auxiliar,modelos.rstan.jul[[places[i.place]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[fecha,c(1,4)],(1:99)/100,extractedsamples=200,randomsamples=200)
#' empiricquantilevector<-quantmarginal(auxiliar[places[i.place]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[fecha,c(1,4)],(1:99)/100,randomsamples=1)
#' sampleqqplot(marginquantilematrix2,empiricquantilevector,xlab="Cuantiles marginales empíricos",ylab="Cuantiles marginales posteriori")
#'
#' #####1 de mayo de 1991, Pamplona#####
#' i<-2922
#' j<-1
#' marginquantilematrix2<-samplequant2(places[j], auxiliar,modelos.rstan.jul[[places[j]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[i,c(1,4)],(1:99)/100,extractedsamples=200,randomsamples=200)
#' empiricquantilevector<-quantmarginal(auxiliar[places[j]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[i,c(1,4)],(1:99)/100,randomsamples=1)
#' sampleqqplot(marginquantilematrix2,empiricquantilevector,xlab="Cuantiles marginales empíricos",ylab="Cuantiles marginales posteriori")
#'
#' j<-1
#' year<-1972
#' days_INDEX<-which((auxiliar[-c(1,2),1]==year & auxiliar[-c(1,2),4]>=357)|(auxiliar[-c(1,2),1]==year+1 & auxiliar[-c(1,2),4]<=8))+2
#' for(i in days_INDEX){
#' print(auxiliar[i,c(1:4)])
#' marginquantilematrix2<-samplequant2(places[j], auxiliar,modelos.rstan.jul[[places[j]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[i,c(1,4)],(1:99)/100,extractedsamples=200,randomsamples=200)
#' empiricquantilevector<-quantmarginal(auxiliar[places[j]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[i,c(1,4)],(1:99)/100,randomsamples=1)
#' output<-sampleqqplot(marginquantilematrix2,empiricquantilevector,xlab="Cuantiles marginales empíricos",ylab="Cuantiles marginales posteriori")
#' }
#'
#' #####7 de julio de 1965, Zaragoza (con ventana de 3 días y 5 años)#####
#' marginquantilematrix2<-samplequant2(places[13], auxiliar,modelos.rstan.jul[[places[13]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[4571,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),w.days=3,w.years=5,extractedsamples=200,randomsamples=200)
#' empiricquantilevector<-quantmarginal(auxiliar[places[13]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[4571,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),w.days=3,w.years=5,randomsamples=1)
#' sampleqqplot(marginquantilematrix2,empiricquantilevector)
#'
#' ###centiles###
#' marginquantilematrix2<-samplequant2(places[13], auxiliar,modelos.rstan.jul[[places[13]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[4571,c(1,4)],seq(0.01,0.99,0.01),w.days=3,w.years=5,extractedsamples=200,randomsamples=200)
#' empiricquantilevector<-quantmarginal(auxiliar[places[13]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[4571,c(1,4)],seq(0.01,0.99,0.01),w.days=3,w.years=5,randomsamples=1)
#' needdatesindex<-NULL
#' aux.needdatesindex<-which((auxiliar$year[-c(1,2)]>=1960)&(auxiliar$year[-c(1,2)]<=1970)&(auxiliar$day.year[-c(1,2)]==188))
#' for(j in aux.needdatesindex){
#'   needdatesindex<-c(needdatesindex,(j-3):(j+3))
#'   }
#' graph<-sampleqqplot(marginquantilematrix2,empiricquantilevector)
#' seq(0.01,0.99,0.01)[graph$inside]
#'
#' abline(v=empiricquantilevector$`0.25`)
#' abline(v=empiricquantilevector$`0.5`)
#' abline(v=empiricquantilevector$`0.75`)
#'
#'###Decada 1961-1970 febreros
#' fechas<-which((auxiliar$month==2)&(auxiliar$year>=1961)&(auxiliar$year<=1970))
#' firstinside<-NULL
#' lastinside<-NULL
#' for(i in fechas){
#'   marginquantilematrix2<-samplequant2(places[13], auxiliar,modelos.rstan.jul[[places[13]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[i,c(1,4)],seq(0.01,0.99,0.01),w.days=7,w.years=2,extractedsamples=200,randomsamples=200)
#'   empiricquantilevector<-quantmarginal(auxiliar[places[13]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[i,c(1,4)],seq(0.01,0.99,0.01),w.days=7,w.years=2,randomsamples=1)
#'   graph<-sampleqqplot(marginquantilematrix2,empiricquantilevector,qqplot=FALSE)
#'   firstinside<-c(firstinside,seq(0.01,0.99,0.01)[graph$inside[1]])
#'   lastinside<-c(lastinside,seq(0.01,0.99,0.01)[graph$inside[length(graph$inside)]])
#' }
#' boxplot(firstinside,xlab="February 1961-1970",ylab="Centil",cex.lab=1.5)
#' boxplot(lastinside,xlab="February 1961-1970",ylab="Centil",cex.lab=1.5)
#' #boxplot(split(firstinside,auxiliar$month[-c(1,2)]),xlab="Months",ylab="ºC",cex.lab=1.5)
#'
#' #El 18 de febrero de 1970, por ejemplo, tiene el primer y último centil dentro, pero después del primer centil hay varios que quedan fuera
#'
#'###Decada 1961-1970 febreros (alternativa más rápida)
#' fechas<-which((auxiliar$month==2)&(auxiliar$year>=1961)&(auxiliar$year<=1970))
#' marginquantilematrix2<-samplequant2(places[13], auxiliar,modelos.rstan.jul[[places[13]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[fechas,c(1,4)],seq(0.01,0.99,0.01),w.days=7,w.years=2,extractedsamples=200,randomsamples=200)
#' empiricquantilevector<-quantmarginal(auxiliar[places[13]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[fechas,c(1,4)],seq(0.01,0.99,0.01),w.days=7,w.years=2,randomsamples=1)
#'
#' firstinside<-NULL
#' lastinside<-NULL
#' for(i in 1:length(fechas)){
#'   marginsample<-list()
#'   marginempiric<-list()
#'   for(j in 1:99){
#'     marginsample[[j]]<-marginquantilematrix2[[j]][i,]
#'     marginempiric[[j]]<-empiricquantilevector[[j]][i]
#'
#'   }
#'   graph<-sampleqqplot(marginsample,marginempiric,qqplot=FALSE)
#'   firstinside<-c(firstinside,seq(0.01,0.99,0.01)[graph$inside[1]])
#'   lastinside<-c(lastinside,seq(0.01,0.99,0.01)[graph$inside[length(graph$inside)]])
#' }
#' boxplot(firstinside,xlab="February 1961-1970",ylab="Centil",cex.lab=1.5)
#' boxplot(lastinside,xlab="February 1961-1970",ylab="Centil",cex.lab=1.5)
#' #boxplot(split(firstinside,auxiliar$month[-c(1,2)]),xlab="Months",ylab="ºC",cex.lab=1.5)
#'
#'###Decada 2003-2012 entera (alternativa más rápida)
#' i.place<-13
#' firstyear<-2003
#' lastyear<-2012
#' #firstyear<-1956
#' #lastyear<-1965
#' fechas<-which((auxiliar$year>=firstyear)&(auxiliar$year<=lastyear))
#'
#' #for(i.place in c(1,4,5,6,9,13)){
#' for(i.place in c(11,12)){
#'   start_time <- Sys.time()
#'   marginquantilematrix2<-samplequant2(places[i.place], auxiliar,modelos.rstan.jul[[places[i.place]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[fechas,c(1,4)],seq(0.01,0.99,0.01),w.days=7,w.years=2,extractedsamples=200,randomsamples=200)
#'   end_time <- Sys.time()
#'   timemarginquantilematrix2<-end_time - start_time
#'   #Time difference of 1.127681 hours
#'
#'   start_time <- Sys.time()
#'   empiricquantilevector<-quantmarginal(auxiliar[places[i.place]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[fechas,c(1,4)],seq(0.01,0.99,0.01),w.days=7,w.years=2,randomsamples=1)
#'   end_time <- Sys.time()
#'   timeempiricquantilevector<-end_time - start_time
#'   #Time difference of 5.843876 secs
#'
#'   firstinside<-NULL
#'   lastinside<-NULL
#'   quantityinside<-NULL
#'   start_time <- Sys.time()
#'   for(i in 1:length(fechas)){
#'     marginsample<-list()
#'     marginempiric<-list()
#'     for(j in 1:99){
#'       marginsample[[j]]<-marginquantilematrix2[[j]][i,]
#'       marginempiric[[j]]<-empiricquantilevector[[j]][i]
#'
#'     }
#'     graph<-sampleqqplot(marginsample,marginempiric,qqplot=FALSE)
#'     firstinside<-c(firstinside,seq(0.01,0.99,0.01)[graph$inside[1]])
#'     if(length(graph$inside)>0){
#'       lastinside<-c(lastinside,seq(0.01,0.99,0.01)[graph$inside[length(graph$inside)]])
#'     }else{
#'       lastinside<-c(lastinside,NA)
#'     }
#'     quantityinside<-c(quantityinside,length(graph$inside))
#'   }
#'   end_time <- Sys.time()
#'   timebucle<-end_time - start_time
#'   #Time difference of 1.511343 mins
#'   pdf(paste("C:/Universidad/TFG/TFG Analisis Bayesiano/Trabajo TFM/TFM imagenes/firstquant",firstyear,"-",lastyear,places[i.place],".pdf",sep=""),height=4.5,width=5.5)
#'   boxplot(split(firstinside,auxiliar$month[fechas]),xlab="Meses",ylab="Centil",cex.lab=1.5)
#'   dev.off()
#'   pdf(paste("C:/Universidad/TFG/TFG Analisis Bayesiano/Trabajo TFM/TFM imagenes/lastquant",firstyear,"-",lastyear,places[i.place],".pdf",sep=""),height=4.5,width=5.5)
#'   boxplot(split(lastinside,auxiliar$month[fechas]),xlab="Meses",ylab="Centil",cex.lab=1.5)
#'   dev.off()
#'   pdf(paste("C:/Universidad/TFG/TFG Analisis Bayesiano/Trabajo TFM/TFM imagenes/quantityquant",firstyear,"-",lastyear,places[i.place],".pdf",sep=""),height=4.5,width=5.5)
#'   boxplot(split(quantityinside,auxiliar$month[fechas]),xlab="Meses",ylab="Nº de centiles",cex.lab=1.5)
#'   dev.off()
#'
#'   boxplot(split(firstinside,auxiliar$month[fechas]),xlab="Meses",ylab="Centil",cex.lab=1.5)
#'   boxplot(split(lastinside,auxiliar$month[fechas]),xlab="Meses",ylab="Centil",cex.lab=1.5)
#'   boxplot(split(quantityinside,auxiliar$month[fechas]),xlab="Meses",ylab="Nº de centiles",cex.lab=1.5)
#'
#' }
#'
#'
#' ###Sesgos 0.01 y 0.99 medios mensuales, década 1956-1965 (la primera que se puede simular entera con la ventana de 2 años y 7 días)
#' #fechas<-which((auxiliar$year>=1956)&(auxiliar$year<=1965)) #década 1956-1965
#' fechas<-which((auxiliar$year>=2003)&(auxiliar$year<=2012)) #década 2003-2012
#' i.place<-13
#' start_time <- Sys.time()
#' marginquantilematrix2<-samplequant2(places[i.place], auxiliar,modelos.rstan.jul[[places[i.place]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[fechas,c(1,4)],seq(0.01,0.99,0.01),w.days=7,w.years=2,extractedsamples=200,randomsamples=200)
#' end_time <- Sys.time()
#' timemarginquantilematrix2<-end_time - start_time
#'
#'
#' start_time <- Sys.time()
#' empiricquantilevector<-quantmarginal(auxiliar[places[i.place]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[fechas,c(1,4)],seq(0.01,0.99,0.01),w.days=7,w.years=2,randomsamples=1)
#' end_time <- Sys.time()
#' timeempiricquantilevector<-end_time - start_time
#'
#'
#' firstinside<-NULL
#' lastinside<-NULL
#' quantityinside<-NULL
#' fistsbiases<-NULL
#' lastsbiases<-NULL
#' start_time <- Sys.time()
#' for(i in 1:length(fechas)){
#'   marginsample<-list()
#'   marginempiric<-list()
#'   for(j in 1:99){
#'     marginsample[[j]]<-marginquantilematrix2[[j]][i,]
#'     marginempiric[[j]]<-empiricquantilevector[[j]][i]
#'
#'   }
#'   graph<-sampleqqplot(marginsample,marginempiric,qqplot=FALSE)
#'   #print(length(graph$inside))
#'   #print(graph$inside[length(graph$inside)])
#'   #print(seq(0.01,0.99,0.01)[graph$inside[length(graph$inside)]])
#'   firstinside<-c(firstinside,seq(0.01,0.99,0.01)[graph$inside[1]])
#'   if(length(graph$inside)>0){
#'     lastinside<-c(lastinside,seq(0.01,0.99,0.01)[graph$inside[length(graph$inside)]])
#'   }else{
#'     lastinside<-c(lastinside,NA)
#'   }
#'   quantityinside<-c(quantityinside,length(graph$inside))
#'   fistsbiases<-c(fistsbiases,graph$firstbias)
#'   lastsbiases<-c(lastsbiases,graph$lastbias)
#' }
#' end_time <- Sys.time()
#' timebucle<-end_time - start_time
#' boxplot(split(firstinside,auxiliar$month[fechas]),xlab="Months",ylab="ºC",cex.lab=1.5)
#' boxplot(split(lastinside,auxiliar$month[fechas]),xlab="Months",ylab="ºC",cex.lab=1.5)
#' boxplot(split(quantityinside,auxiliar$month[fechas]),xlab="Months",ylab="Centiles",cex.lab=1.5)
#' fistsbiasesmean<-tapply(fistsbiases,INDEX=auxiliar$month[fechas],FUN=mean)
#' lastsbiasesmean<-tapply(lastsbiases,INDEX=auxiliar$month[fechas],FUN=mean)
#'
#' #1956-1965 Zaragoza (places[13])
#' Sesgo medio mensual quantile 0.01: 21.83997, 25.04205, 18.10378, 21.42236, 19.31271, 24.73456, 20.92259, 21.28395, 16.34479, 13.00127, 12.08881, 23.29790
#' Sesgo medio mensual quantile 0.99: 20.43178, 17.04510, 17.56713, 14.98806, 23.90456, 16.36720, 18.05223, 22.29651, 25.17272, 19.03923, 20.26754, 23.91033
#'
#' #1956-1965 Panticosa (places[9])
#' Sesgo medio mensual quantile 0.01: 13.69592 21.77554 17.20263 17.94510 18.03371 20.12398 15.55221 14.75076 17.12699 14.33704 16.66977 12.94941
#' Sesgo medio mensual quantile 0.99: 15.64837 17.84930 18.83639 14.71380 17.90318 11.65209 12.22496 15.96510 20.47040 17.54178 19.05935 20.07329
#'
#' #1956-1965 Pamplona (places[1])
#' Sesgo medio mensual quantile 0.01: 30.07219 27.74517 37.93163 45.99106 51.99507 51.45125 45.24811 51.87466 64.28204 55.15001 50.56075 37.67611
#' Sesgo medio mensual quantile 0.99: 62.17974  81.02913 110.43040 112.59610 117.04631 110.55593 119.75638 114.86643 114.47237  96.29258 55.36246  47.13312
#'
#' #1956-1965 Tornos (places[6])
#' Sesgo medio mensual quantile 0.01: 18.83708 22.08024 18.21108 23.65532 21.23727 25.28644 22.27203 21.26345 19.07785 17.71485 14.53666 14.01426
#' Sesgo medio mensual quantile 0.99: 10.84156 19.75492 22.95580 17.06053 25.30329 15.87776 18.53895 19.93460 24.43865 20.32832 16.99393 15.75102
#'
#' #2003-2012 Zaragoza (places[13])
#' Sesgo medio mensual quantile 0.01: 26.04636 22.91254 22.71727 21.11158 23.92947 21.60475 20.93521 19.18615 18.01474 13.75210 15.11447 24.97223
#' Sesgo medio mensual quantile 0.99: 18.43280 11.00908 14.52452 15.85436 18.69324 19.69130 16.29938 22.41038 20.13141 24.92620 20.71620 15.44869
#'
#' #2003-2012 Panticosa (places[9])
#' Sesgo medio mensual quantile 0.01: 23.82452 26.45691 22.76794 16.00753 20.29811 18.91501 16.04593 19.21094 15.43988 12.81388 15.13530 19.33341
#' Sesgo medio mensual quantile 0.99: 18.41628 16.40672 19.71083 16.79340 17.29136 13.99323 14.24213 17.68265 17.68723 19.84291 17.45814 15.01884
#'
#' #2003-2012 Pamplona (places[1])
#' Sesgo medio mensual quantile 0.01: 20.60727 23.23309 19.36343 23.12170 21.56189 25.02705 25.96257 28.08875 20.30581 18.08341 15.09805 13.87417
#' Sesgo medio mensual quantile 0.99: 16.23712 12.66621 19.30132 21.55909 30.05743 25.12414 30.85321 32.01133 31.33772 24.40746 16.77188 14.62366
#'
#' #2003-2012 Tornos (places[6])
#' Sesgo medio mensual quantile 0.01: 18.83491 22.07483 18.20011 23.66772 21.23311 25.28915 22.27961 21.27424 19.08121 17.71217 14.53473 14.01903
#' Sesgo medio mensual quantile 0.99: 10.83968 19.75567 22.94902 17.05976 25.30402 15.87528 18.54682 19.92318 24.43368 20.32341 16.99055 15.74977
#'
#'###Decada 1955-2013 febreros día 15 de cada mes
#' fechas<-which((auxiliar$day.month==15)&(auxiliar$year>=1955)&(auxiliar$year<=2013))
#' firstinside<-array(,dim=c(18,59,12),dimnames=list(places,1955:2013,1:12))
#' lastinside<-array(,dim=c(18,59,12),dimnames=list(places,1955:2013,1:12))
#' quantityinside<-array(,dim=c(18,59,12),dimnames=list(places,1955:2013,1:12))
#' firstbias<-array(,dim=c(18,59,12),dimnames=list(places,1955:2013,1:12))
#' lastbias<-array(,dim=c(18,59,12),dimnames=list(places,1955:2013,1:12))
#'
#' timemarginquantilematrix.places<-array(,dim=c(18),dimnames=list(places))
#' timeempiricquantilevector.places<-array(,dim=c(18),dimnames=list(places))
#' timebucle.places<-array(,dim=c(18),dimnames=list(places))
#'
#' for(i.place in 13){
#'   start_time <- Sys.time()
#'   marginquantilematrix2<-samplequant2(places[i.place], auxiliar,modelos.rstan.jul[[places[i.place]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[fechas,c(1,4)],seq(0.01,0.99,0.01),w.days=7,w.years=2,extractedsamples=200,randomsamples=200)
#'   end_time <- Sys.time()
#'   timemarginquantilematrix.places[i.place]<-end_time - start_time
#'
#'   start_time <- Sys.time()
#'   empiricquantilevector<-quantmarginal(auxiliar[places[i.place]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[fechas,c(1,4)],seq(0.01,0.99,0.01),w.days=7,w.years=2,randomsamples=1)
#'   end_time <- Sys.time()
#'   timeempiricquantilevector.places[i.place]<-end_time - start_time
#'
#'   aux.firstinside<-NULL
#'   aux.lastinside<-NULL
#'   aux.quantityinside<-NULL
#'   aux.firstbias<-NULL
#'   aux.lastbias<-NULL#'
#'   start_time <- Sys.time()
#'   for(i in 1:length(fechas)){
#'     marginsample<-list()
#'     marginempiric<-list()
#'     for(j in 1:99){
#'       marginsample[[j]]<-marginquantilematrix2[[j]][i,]
#'       marginempiric[[j]]<-empiricquantilevector[[j]][i]
#'
#'     }
#'     graph<-sampleqqplot(marginsample,marginempiric,qqplot=FALSE)
#'     aux.firstinside<-c(aux.firstinside,seq(0.01,0.99,0.01)[graph$inside[1]])
#'     if(length(graph$inside)>0){
#'       aux.lastinside<-c(aux.lastinside,seq(0.01,0.99,0.01)[graph$inside[length(graph$inside)]])
#'     }else{
#'       aux.lastinside<-c(aux.lastinside,NA)
#'     }
#'     aux.quantityinside<-c(aux.quantityinside,length(graph$inside))
#'     aux.firstbias<-c(aux.firstbias,graph$firstbias)
#'     aux.lastbias<-c(aux.lastbias,graph$lastbias)
#'   }
#'   end_time <- Sys.time()
#'   timebucle.places[i.place]<-end_time - start_time
#'   firstinside[i.place,,]<-matrix(aux.firstinside,ncol=12)
#'   lastinside[i.place,,]<-matrix(aux.lastinside,ncol=12)
#'   quantityinside[i.place,,]<-matrix(aux.quantityinside,ncol=12)
#'   firstbias[i.place,,]<-matrix(aux.firstbias,ncol=12)
#'   lastbias[i.place,,]<-matrix(aux.lastbias,ncol=12)
#'
#' }
#'
#' saveRDS(firstinside, file = "firstinside.rds")
#' firstinside<-readRDS(file = "firstinside.rds")
#' saveRDS(lastinside, file = "lastinside.rds")
#' lastinside<-readRDS(file = "lastinside.rds")
#' saveRDS(quantityinside, file = "quantityinside.rds")
#' quantityinside<-readRDS(file = "quantityinside.rds")
#' saveRDS(firstbias, file = "firstbias.rds")
#' firstbias<-readRDS(file = "firstbias.rds")
#' saveRDS(lastbias, file = "lastbias.rds")
#' lastbias<-readRDS(file = "lastbias.rds")
#'
#' saveRDS(timemarginquantilematrix.places, file = "timemarginquantilematrix.places.rds")
#' timemarginquantilematrix.places<-readRDS(file = "timemarginquantilematrix.places.rds")
#' saveRDS(timeempiricquantilevector.places, file = "timeempiricquantilevector.places.rds")
#' timeempiricquantilevector.places<-readRDS(file = "timeempiricquantilevector.places.rds")
#' saveRDS(timebucle.places, file = "timebucle.places.rds")
#' timebucle.places<-readRDS(file = "timebucle.places.rds")
#'
#' #boxplot(split(firstinside,auxiliar$month[fechas]),xlab="Meses",ylab="Centil",cex.lab=1.5)
#' #boxplot(split(lastinside,auxiliar$month[fechas]),xlab="Meses",ylab="Centil",cex.lab=1.5)
#' #boxplot(split(quantityinside,auxiliar$month[fechas]),xlab="Meses",ylab="Nº de centiles",cex.lab=1.5)
#'
NULL

#' @rdname sampleqqplot
#' @export
sampleqqplot<-function(marginquantilematrix,empiricquantilevector,qqplot=TRUE,range=1,xlab="Empiric margin quantiles",ylab="Simulated margin quantiles"){

  nquant<-length(marginquantilematrix)
  x<-NULL
  y1<-NULL
  y2<-NULL
  for(i in 1:nquant){
    x<-cbind(x,empiricquantilevector[[i]])
    y1<-cbind(y1,quantile(marginquantilematrix[[i]],0.01))
    y2<-cbind(y2,quantile(marginquantilematrix[[i]],0.99))
  }

  if(qqplot==TRUE){
    plot(min(cbind(x,y1,y2)):max(cbind(x,y1,y2)),min(cbind(x,y1,y2)):max(cbind(x,y1,y2)),type="l",xlab=xlab,ylab=ylab)
    segments(x,y1,x,y2)
    abline(c(range*10,1),lty=2)
    abline(c(-range*10,1),lty=2)
  }

  output<-list()
  output$inside<-which(((x-range*10)<=y2)&((x+range*10)>=y1))
  output$firstbias<-abs(mean(marginquantilematrix[[1]])-empiricquantilevector[[1]])
  output$lastbias<-abs(mean(marginquantilematrix[[nquant]])-empiricquantilevector[[nquant]])

  return(output)

}
