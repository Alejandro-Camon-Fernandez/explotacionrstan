#' @name samplequant
#' @rdname samplequant
#' @title Quantile distribution from stanfit
#' @description Given a fitted stan model and the dataframe used to fit it, a matrix of samples for every day.
#' @param aux.X Columns of data used for the model of the mean.
#' @param aux.X.sigma2 Columns of data used for the model of the variance.

#' @param place Name of the column modeled of the data frame fiven in data.
#' @param data Complete data frame.

#' @param stanfit Stanfit object. Bayesian model that we want to sample from.
#' @param extractedsamples An integer that says how many simulated samples of the stanfit object are used.
#' @param randomsamples An integer that says how many new random samples are obtained from each extracted sample.
#' @param samplefrom An integer that says from how many of the last extracted samples are obtained the extractedsamples. It must be a multiple of the number of chains simulated in the model greater than extractedsamples.
#' @param dates Year and day of the year data of, at least, all the days needed to obtain the quantiles desired.
#' @param choosedates Year and day of the year data of the days that we want to obtain the quantiles from.
#' @param quant Array of the quantiles that we want to obtain.
#' @param w.days For the marginal quantiles, a window of "close or similar" days data is considered to avoid the previous day dependence. Then, to obtain the marginal quantiles of each day, we consider the data of that day plus the data of the w.days next days and the w.days previous days.
#' @param w.years Similar to the use of w.days, besides the window of 2*w.days+1 days, the marginal quantiles will also be obtained using the data of the respective days window on the next w.years years and the previous w.years years.
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
#' marginquantilematrix<-samplequant(aux.X,aux.X.sigma2,modelos.rstan.jul[[places[13]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[c(1500,1600,1700,1800),c(1,4)],0.95,extractedsamples=200,randomsamples=200)
#'
#' #Sacar los índices necesarios de la función
#' empiricquantilevector<-quantmarginal(auxiliar[places[13]],auxiliar[-c(1,2),c(1,4)],0.95,randomsamples=1)
#'
#' marginquantilematrix2<-samplequant2(places[13], auxiliar,modelos.rstan.jul[[places[13]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[c(1500,1600,1700,1800),c(1,4)],0.95,extractedsamples=200,randomsamples=200)
#'
#' #Test de distribución de quantiles
#' aux.day1<-sum(marginquantilematrix[1,]>empiricquantilevector[1,])/length(marginquantilematrix[1,])
#' for(i in 1:length(marginquantilematrix[,1])){
#'  print(sum(marginquantilematrix[i,]>empiricquantilevector[i,])/length(marginquantilematrix[i,]))
#' }
#'
#'
#' #5 de enero de 1956, Zaragoza
#' placeindex<-13
#' marginquantilematrix2<-samplequant2(places[placeindex], auxiliar,modelos.rstan.jul[[places[placeindex]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[1100,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),extractedsamples=200,randomsamples=200)
#' empiricquantilevector<-quantmarginal(auxiliar[places[placeindex]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[1100,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),randomsamples=1)
#'
#' needdatesindex<-NULL
#' aux.needdatesindex<-which((auxiliar$year[-c(1,2)]>=1954)&(auxiliar$year[-c(1,2)]<=1958)&(auxiliar$day.year[-c(1,2)]==5))
#' for(j in aux.needdatesindex){
#'   needdatesindex<-c(needdatesindex,(j-7):(j+7))
#' }
#'
#' plot(density(auxiliar[[places[placeindex]]][needdatesindex]))
#' for(i in 1:9){
#'   lines(density(marginquantilematrix2[[i]]),col=i)
#'   abline(v=empiricquantilevector[[i]])
#' }
#'
#' #8 de enero de 1955, Zaragoza
#' placeindex<-13
#' marginquantilematrix2<-samplequant2(places[placeindex], auxiliar,modelos.rstan.jul[[places[placeindex]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[740,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),extractedsamples=200,randomsamples=200)
#' empiricquantilevector<-quantmarginal(auxiliar[places[placeindex]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[740,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),randomsamples=1)
#'
#' needdatesindex<-NULL
#' aux.needdatesindex<-which((auxiliar$year[-c(1,2)]>=1953)&(auxiliar$year[-c(1,2)]<=1957)&(auxiliar$day.year[-c(1,2)]==10))
#' for(j in aux.needdatesindex){
#'   needdatesindex<-c(needdatesindex,(j-7):(j+7))
#' }
#'
#' plot(density(auxiliar[[places[placeindex]]][needdatesindex]))
#' for(i in 1:9){
#'   lines(density(marginquantilematrix2[[i]]),col=i)
#'   abline(v=empiricquantilevector[[i]])
#' }
#'
#'
#' #7 de julio de 1955, Zaragoza
#' placeindex<-13
#' marginquantilematrix2<-samplequant2(places[placeindex], auxiliar,modelos.rstan.jul[[places[placeindex]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[918,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),extractedsamples=200,randomsamples=200)
#' empiricquantilevector<-quantmarginal(auxiliar[places[placeindex]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[918,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),randomsamples=1)
#'
#' needdatesindex<-NULL
#' aux.needdatesindex<-which((auxiliar$year[-c(1,2)]>=1953)&(auxiliar$year[-c(1,2)]<=1957)&(auxiliar$day.year[-c(1,2)]==188))
#' for(j in aux.needdatesindex){
#'   needdatesindex<-c(needdatesindex,(j-7):(j+7))
#' }
#'
#' plot(density(auxiliar[[places[placeindex]]][needdatesindex]))
#' for(i in 1:9){
#'   lines(density(marginquantilematrix2[[i]]))
#'   abline(v=empiricquantilevector[[i]],col=i+1)
#' }
#'
#'
#' #7 de julio de 2013, Zaragoza
#' placeindex<-13
#' marginquantilematrix2<-samplequant2(places[placeindex], auxiliar,modelos.rstan.jul[[places[placeindex]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[22103,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),extractedsamples=200,randomsamples=200)
#' empiricquantilevector<-quantmarginal(auxiliar[places[placeindex]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[22103,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),randomsamples=1)
#'
#' needdatesindex<-NULL
#' aux.needdatesindex<-which((auxiliar$year[-c(1,2)]>=2011)&(auxiliar$year[-c(1,2)]<=2015)&(auxiliar$day.year[-c(1,2)]==188))
#' for(j in aux.needdatesindex){
#'   needdatesindex<-c(needdatesindex,(j-7):(j+7))
#' }
#'
#' plot(density(auxiliar[[places[placeindex]]][needdatesindex]))
#' for(i in 1:9){
#'   lines(density(marginquantilematrix2[[i]]),col=i)
#'   abline(v=empiricquantilevector[[i]])
#' }
#'
#'
#' #7 de julio de 1965, Zaragoza (con ventana de 3 días y 5 años)
#' placeindex<-13
#' marginquantilematrix2<-samplequant2(places[placeindex], auxiliar,modelos.rstan.jul[[places[placeindex]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[4571,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),w.days=3,w.years=5,extractedsamples=200,randomsamples=200)
#' empiricquantilevector<-quantmarginal(auxiliar[places[placeindex]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[4571,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),w.days=3,w.years=5,randomsamples=1)
#'
#' needdatesindex<-NULL
#' aux.needdatesindex<-which((auxiliar$year[-c(1,2)]>=1960)&(auxiliar$year[-c(1,2)]<=1970)&(auxiliar$day.year[-c(1,2)]==188))
#' for(j in aux.needdatesindex){
#'   needdatesindex<-c(needdatesindex,(j-3):(j+3))
#' }
#'
#' sesgos<-NULL
#' plot(density(auxiliar[[places[placeindex]]][needdatesindex]))
#' for(i in 1:9){
#'   lines(density(marginquantilematrix2[[i]]))
#'   abline(v=empiricquantilevector[[i]],col=i+1)
#'   sesgos<-cbind(sesgos,mean(marginquantilematrix2[[i]])-empiricquantilevector[[i]])
#' }
#'
#'
#' #Sesgos medios mensuales de Zaragoza de la década 1961-1970
#' placeindex<-13
#' fechas<-which((auxiliar$month==2)&(auxiliar$year>=1961)&(auxiliar$year<=1970))
#' start_time <- Sys.time()
#' marginquantilematrix2<-samplequant2(places[placeindex], auxiliar,modelos.rstan.jul[[places[placeindex]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[fechas,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),w.days=7,w.years=2,extractedsamples=200,randomsamples=200)
#' end_time <- Sys.time()
#' timeesf<-end_time - start_time
#' Time difference of 5.299719 mins
#'
#' start_time <- Sys.time()
#' empiricquantilevector<-quantmarginal(auxiliar[places[placeindex]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[fechas,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),w.days=7,w.years=2,randomsamples=1)
#' end_time <- Sys.time()
#' timeesf<-end_time - start_time
#' Time difference of 0.403789 secs
#'
#' w.years<-2
#' w.days<-7
#' needdatesindex<-NULL
#' for(i in fechas){
#' aux.needdatesindex<-which((auxiliar$year>=auxiliar$year[i]-w.years)&(auxiliar$year<=auxiliar$year[i]+w.years)&(auxiliar$day.year==auxiliar$day.year[i]))
#'  for(j in aux.needdatesindex){
#'    needdatesindex<-c(needdatesindex,(j-w.days):(j+w.days))
#'  }
#' }
#' needdatesindex<-sort(unique(needdatesindex))
#'
#' sesgos<-NULL
#' plot(density(auxiliar[[places[placeindex]]][needdatesindex]))
#' for(i in 1:9){
#'   #lines(density(marginquantilematrix2[[i]]))
#'   #abline(v=empiricquantilevector[[i]],col=i+1)
#'   sesgos<-cbind(sesgos,rowMeans(marginquantilematrix2[[i]])-empiricquantilevector[[i]])
#' }
#' meansesgos<-colMeans(sesgos)
#' #Sesgos de enero 1961-1970 (Zaragoza): (15.8185054  9.7708481  5.4428089  2.1721025  -0.7592067  -3.2203477  -6.7462737  -9.7521370  -13.5979871)
#' #Sesgos de febrero 1961-1970 (Zaragoza): (15.3438000  10.6336527  7.9971702  4.8145742  1.8147032 -0.5900367 -3.5776822 -6.1117081 -9.4799386)
#'
#' #31 de diciembre del 2000 (fila 17532 de auxiliar)
#' marginquantilematrix2<-samplequant2(places[placeindex], auxiliar,modelos.rstan.jul[[places[13]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[17532,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),w.days=7,w.years=2,extractedsamples=200,randomsamples=200)
#'
#'
#' #31 de diciembre del 1998 (fila 16801 de auxiliar)
#' marginquantilematrix2<-samplequant2(places[placeindex], auxiliar,modelos.rstan.jul[[places[13]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[16801,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),w.days=7,w.years=2,extractedsamples=200,randomsamples=200)
#'
#'
#' #1 de enero del 1999 (fila 16802 de auxiliar)
#' marginquantilematrix2<-samplequant2(places[placeindex], auxiliar,modelos.rstan.jul[[places[13]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[16802,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),w.days=7,w.years=2,extractedsamples=200,randomsamples=200)
#'
#'#Revisar el día 365 de un año bisiesto y el 364 de un bisiesto+2
#'
#' #30 de diciembre del 2000 (fila 17531 de auxiliar)
#' marginquantilematrix2<-samplequant2(places[placeindex], auxiliar,modelos.rstan.jul[[places[13]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[17532,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),w.days=7,w.years=2,extractedsamples=200,randomsamples=200)
#'
#'
#' #30 de diciembre del 1998 (fila 16800 de auxiliar)
#' marginquantilematrix2<-samplequant2(places[13], auxiliar,modelos.rstan.jul[[places[13]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[16801,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),w.days=7,w.years=2,extractedsamples=200,randomsamples=200)
#'
#'
#' #Sesgos medios mensuales de Zaragoza, Pamplona, Panticosa y Tornos de las décadas 1961-1970 1981-1990 y 2001-2010
#' placeindex<-6
#' #fechas<-which((auxiliar$year>=1961)&(auxiliar$year<=1970))
#' fechas<-which((auxiliar$year>=1981)&(auxiliar$year<=1990))
#' #fechas<-which((auxiliar$year>=2001)&(auxiliar$year<=2010))
#'
#' start_time <- Sys.time()
#' marginquantilematrix2<-samplequant2(places[placeindex], auxiliar,modelos.rstan.jul[[places[placeindex]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[fechas,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),w.days=7,w.years=2,extractedsamples=200,randomsamples=200)
#' end_time <- Sys.time()
#' timeesf<-end_time - start_time
#' #Time difference of 1.800704 hours
#'
#' start_time <- Sys.time()
#' empiricquantilevector<-quantmarginal(auxiliar[places[placeindex]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[fechas,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),w.days=7,w.years=2,randomsamples=1)
#' end_time <- Sys.time()
#' timeesf2<-end_time - start_time
#' #Time difference of 10.01929 secs
#'
#' w.years<-2
#' w.days<-7
#' needdatesindex<-NULL
#' for(i in fechas){
#' aux.needdatesindex<-which((auxiliar$year>=auxiliar$year[i]-w.years)&(auxiliar$year<=auxiliar$year[i]+w.years)&(auxiliar$day.year==auxiliar$day.year[i]))
#'  for(j in aux.needdatesindex){
#'    needdatesindex<-c(needdatesindex,(j-w.days):(j+w.days))
#'  }
#' }
#' needdatesindex<-sort(unique(needdatesindex))
#'
#' INDEX<-auxiliar$month[fechas]
#' meansesgos<-matrix(nrow=12,ncol=9)
#' for(i in 1:9){
#'   #lines(density(marginquantilematrix2[[i]]))
#'   #abline(v=empiricquantilevector[[i]],col=i+1)
#'   sesgos<-rowMeans(marginquantilematrix2[[i]])-empiricquantilevector[[i]]
#'   meansesgos[,i]<-tapply(sesgos,INDEX=INDEX,FUN=mean)
#' }
#' #Sesgos de enero 1961-1970 (Zaragoza): 15.818572  9.770922  5.441974  2.167943 -0.7619930 -3.22104884  -6.745028  -9.749717 -13.598958
#' #Sesgos de febrero 1961-1970 (Zaragoza): 15.357117 10.645130  8.027204  4.823200  1.8486631 -0.54901530  -3.567839  -6.081415  -9.439444
#' #Sesgos de marzo 1961-1970 (Zaragoza): 15.350872 11.322688  7.853057  5.133067  2.5819627 -0.51077053  -3.260609  -6.149936  -9.938811
#' #Sesgos de abril 1961-1970 (Zaragoza): 14.968150 11.140123  8.000702  5.261163  2.9187951  0.07326197  -3.085561  -6.092200  -9.354359
#' #Sesgos de mayo 1961-1970 (Zaragoza): 15.681066 11.612795  7.460855  3.971932  0.2597548 -3.11830519  -6.993832 -11.296926 -15.932197
#' #Sesgos de junio 1961-1970 (Zaragoza): 16.736400 12.762155  9.741957  6.790653  3.7968715  0.94885611  -2.992420  -6.920146 -11.608186
#' #Sesgos de julio 1961-1970 (Zaragoza): 16.918866 10.482516  5.837544  1.391740 -2.0062985 -5.26814527  -8.744181 -11.694371 -15.669274
#' #Sesgos de agosto 1961-1970 (Zaragoza): 14.633584 10.387696  7.146796  2.519470 -1.4984811 -4.76209093  -7.831609 -11.818259 -16.267129
#' #Sesgos de septiembre 1961-1970 (Zaragoza): 13.359720  8.974830  3.165956 -1.100588 -4.8682064 -8.37734086 -11.762815 -16.174002 -20.357599
#' #Sesgos de octubre 1961-1970 (Zaragoza):  5.250512  0.810027 -2.094913 -4.921234 -7.3431486 -9.73921190 -12.347684 -15.223163 -19.172317
#' #Sesgos de noviembre 1961-1970 (Zaragoza): 10.001399  5.175440  1.915256 -1.306504 -3.5744033 -5.96679170  -8.032526 -10.685319 -14.412342
#' #Sesgos de diciembre 1961-1970 (Zaragoza): 14.468186  9.246363  5.004142  1.627714 -1.6923828 -4.95463134  -8.151720 -11.190086 -15.981417
#'
#' #Sesgos de enero 1981-1990 (Zaragoza): 18.343905 12.550916  7.40729035  3.04781001  0.06475415 -2.91807898  -5.0082708  -7.777573 -12.023809
#' #Sesgos de enero 1981-1990 (Zaragoza): 16.749709 10.877977  7.30990417  4.45846190  2.01511264 -0.09083381  -3.8126956  -6.837510 -10.367425
#' #Sesgos de enero 1981-1990 (Zaragoza): 15.025624 10.964215  8.05571538  5.15808961  2.61882547  0.07789778  -2.7937399  -6.460511 -10.719270
#' #Sesgos de enero 1981-1990 (Zaragoza): 17.657923 13.486089 10.41079021  7.61194786  4.69270543  2.39729531  -0.8694144  -4.742434  -9.617897
#' #Sesgos de enero 1981-1990 (Zaragoza): 19.543193 15.802456 13.08155365  9.67325117  5.41789865  2.36802389  -1.3367077  -5.884002 -10.962561
#' #Sesgos de enero 1981-1990 (Zaragoza): 20.770715 16.457176 11.99044691  8.40383145  4.98659179  1.89351214  -2.4544056  -6.647841 -11.024718
#' #Sesgos de enero 1981-1990 (Zaragoza): 16.669484 11.160736  6.99810237  2.76044029 -0.78899784 -3.55622202  -6.4803891 -10.165026 -12.766288
#' #Sesgos de enero 1981-1990 (Zaragoza): 14.024107  9.004599  5.21077755  1.23954588 -2.05022964 -5.53842588  -8.3750561 -11.334717 -15.634236
#' #Sesgos de enero 1981-1990 (Zaragoza): 9.964920  4.906276 -0.54776727 -4.19375089 -7.08624551 -9.23367944 -11.3631978 -14.471800 -19.031817
#' #Sesgos de enero 1981-1990 (Zaragoza): 7.696444  2.631519 -0.90249546 -4.11376392 -6.48438559 -8.43300908 -10.9372212 -13.303436 -16.645369
#' #Sesgos de enero 1981-1990 (Zaragoza): 6.596663  2.823633  0.04003344 -2.27476211 -4.46394679 -7.26050882  -9.6944203 -12.260279 -15.310795
#' #Sesgos de enero 1981-1990 (Zaragoza): 12.611432  7.164258  2.91901015  0.05894562 -1.96315042 -4.27708070  -7.1133175 -11.106224 -16.671717
#'
#' #Sesgos de enero 2001-2010 (Zaragoza): 18.889306 13.368967  8.4199742  4.9321358  1.38585603 -1.764008  -4.913134  -7.851835 -12.910920
#' #Sesgos de febrero 2001-2010 (Zaragoza): 17.371899 12.674339  9.2575244  6.7525435  4.93598858  2.141588  -0.222137  -4.226145  -7.681901
#' #Sesgos de marzo 2001-2010 (Zaragoza): 17.239511 12.012114  8.5656286  4.9614917  1.67433385 -1.481483  -3.970986  -7.241931 -11.321880
#' #Sesgos de abril 2001-2010 (Zaragoza): 16.512648 12.880698  9.2291454  5.5295659  2.24968677 -1.078702  -3.940389  -6.695261 -10.477143
#' #Sesgos de mayo 2001-2010 (Zaragoza): 16.151208 11.740841  7.7357265  4.3217346  0.04209174 -3.673893  -6.934348 -10.689047 -13.810208
#' #Sesgos de junio 2001-2010 (Zaragoza): 14.804057  9.601118  4.4358605  0.6497107 -2.86648966 -6.826164 -10.236814 -13.158801 -16.574316
#' #Sesgos de julio 2001-2010 (Zaragoza): 15.712183  9.848371  5.2786563  1.2295958 -2.09392019 -5.661301  -8.743112 -11.965837 -16.071987
#' #Sesgos de agosto 2001-2010 (Zaragoza): 13.987750  8.526427  3.5630041 -0.4680387 -3.52675868 -7.015582 -10.349977 -13.821904 -19.020226
#' #Sesgos de septiembre 2001-2010 (Zaragoza): 10.505620  6.576675  2.5564031 -0.8742780 -3.86736967 -5.821817  -8.140865 -11.513732 -15.392792
#' #Sesgos de octubre 2001-2010 (Zaragoza): 7.540664  2.942106 -0.1829761 -3.4010460 -5.92977619 -8.112340 -10.550818 -13.764631 -16.951392
#' #Sesgos de noviembre 2001-2010 (Zaragoza): 8.269791  3.790115  1.1419632 -0.8369388 -2.84436784 -5.166914  -7.479082 -10.291037 -14.980963
#' #Sesgos de diciembre 2001-2010 (Zaragoza): 14.108649  8.615887  5.1226086  2.2794326 -0.45666919 -2.818928  -5.358593  -8.476554 -12.801642
#'
#' #Sesgos de enero 1961-1970 (Pamplona): 11.949621  8.380053  5.5727644  3.087713  1.3212682  -0.83739259  -3.215845  -5.343835  -8.658720
#' #Sesgos de febrero 1961-1970 (Pamplona): 14.436175 11.154773  7.6056422  4.644594  2.0073980   0.01824582  -2.507723  -5.188544  -8.530468
#' #Sesgos de marzo 1961-1970 (Pamplona): 15.677314 11.754113  8.0521993  4.945692  2.0762356  -0.71060693  -3.463771  -6.820318 -10.732467
#' #Sesgos de abril 1961-1970 (Pamplona): 18.702691 14.506087 10.6977112  7.019051  3.6766441  -0.04842046  -3.669276  -7.758208 -12.533070
#' #Sesgos de mayo 1961-1970 (Pamplona): 21.153720 16.507056 10.1087236  5.300978  1.2445961  -2.76180741  -7.228363 -12.733470 -19.381897
#' #Sesgos de junio 1961-1970 (Pamplona): 21.864434 17.296868 13.2866847  9.743072  5.7167502   0.87015068  -3.665541  -9.258287 -15.691698
#' #Sesgos de julio 1961-1970 (Pamplona): 24.416682 18.809148 12.2445841  5.781988 -0.8630871  -6.66007408 -10.174307 -14.834832 -20.861566
#' #Sesgos de agosto 1961-1970 (Pamplona): 25.093911 19.480806 13.3596050  7.383991  2.3830483  -2.97630530  -7.789033 -13.016874 -20.476400
#' #Sesgos de septiembre 1961-1970 (Pamplona): 16.929543 12.944957  7.3169059  1.992749 -3.0917841  -7.84332519 -14.403182 -19.589146 -26.140150
#' #Sesgos de octubre 1961-1970 (Pamplona): 7.885971  3.611796 -0.3372987 -5.231478 -7.9063471 -11.06575447 -14.760022 -19.340777 -24.365624
#' #Sesgos de noviembre 1961-1970 (Pamplona): 10.580022  6.013492  2.8946677  0.254557 -2.6487391  -5.26522083  -7.712580 -10.942165 -14.950969
#' #Sesgos de diciembre 1961-1970 (Pamplona): 10.734593  7.416143  4.5488757  2.360373  0.2280689  -1.54611757  -3.628565  -5.984889  -9.903927
#'
#' #Sesgos de enero 1981-1990 (Pamplona): 13.464517  9.485688  6.2860970  3.301895359  1.0789994  -1.3360083  -3.146019  -5.851790  -9.070566
#' #Sesgos de febrero 1981-1990 (Pamplona): 13.495820  8.912153  5.5759880  2.799385300  0.4474069  -1.2843305  -3.489516  -6.169035  -9.965599
#' #Sesgos de marzo 1981-1990 (Pamplona): 15.911068 12.132798  8.1071704  5.055219762  2.7225113   0.2203981  -2.810019  -6.451363 -10.820991
#' #Sesgos de abril 1981-1990 (Pamplona): 20.368173 15.738493 11.7648657  8.669766521  5.7463337   2.5035496  -1.793328  -6.587600 -11.649826
#' #Sesgos de mayo 1981-1990 (Pamplona): 25.687690 20.523385 16.6082696 11.984787936  7.3594425   2.7597409  -2.128373  -7.629933 -13.692580
#' #Sesgos de junio 1981-1990 (Pamplona): 26.767292 21.759454 17.0005917 12.174312372  7.1535870   1.3441733  -4.030512  -9.441867 -15.505219
#' #Sesgos de julio 1981-1990 (Pamplona): 25.127275 18.896624 13.6158150  7.680687001  1.6198353  -4.1778385  -9.774609 -15.618134 -21.675729
#' #Sesgos de agosto 1981-1990 (Pamplona): 22.801036 17.301385 12.7309257  6.872106683  1.6020015  -4.1793059 -10.060363 -15.142103 -20.939305
#' #Sesgos de septiembre 1981-1990 (Pamplona): 17.396578 11.096888  5.2812833 -1.255805607 -6.0153130 -10.0215536 -13.633079 -17.850540 -22.867259
#' #Sesgos de octubre 1981-1990 (Pamplona): 10.908003  6.506415  3.4176405 -1.664557016 -6.0988103  -9.8819322 -12.697462 -15.768803 -18.960280
#' #Sesgos de noviembre 1981-1990 (Pamplona): 6.907298  2.708377 -0.4727216 -2.955153707 -5.9451758  -8.4713293 -10.689564 -12.935938 -15.760796
#' #Sesgos de diciembre 1981-1990 (Pamplona): 7.411132  3.983749  1.8544501 -0.004513963 -2.0125673  -5.0839447  -7.504607 -10.262700 -14.294127
#'
#' #Sesgos de enero 2001-2010 (Pamplona): 10.441249  6.843019 4.378448  2.3253763  0.05043267  -2.09695406  -4.056806  -6.336727  -9.674123
#' #Sesgos de febrero 2001-2010 (Pamplona): 13.610622  9.761048 6.654449  4.1892484  1.97958973   0.07904653  -2.305098  -4.026390  -8.139423
#' #Sesgos de marzo 2001-2010 (Pamplona): 13.535109  9.436984 5.395272  1.9597890 -1.24817008  -4.48457238  -7.722795 -11.110424 -14.967531
#' #Sesgos de abril 2001-2010 (Pamplona):  16.013708 12.029235 8.200720  3.9624283  0.17977536  -3.12735912  -6.472095 -10.620297 -14.340766
#' #Sesgos de mayo 2001-2010 (Pamplona): 18.175912 12.305741 8.165909  2.7084278 -1.78353235  -6.46424519 -10.202756 -15.022541 -21.185392
#' #Sesgos de junio 2001-2010 (Pamplona): 17.558709 11.127253 5.463153  0.3194366 -5.01612675 -10.98714425 -15.436315 -20.467317 -25.960106
#' #Sesgos de julio 2001-2010 (Pamplona): 20.846305 13.956163 8.689053  4.0713141 -1.52711061  -7.42301468 -13.537934 -20.019157 -25.454133
#' #Sesgos de agosto 2001-2010 (Pamplona): 19.165709 13.613870 7.525529  2.6684365 -2.28875676  -8.02024195 -14.177709 -20.897372 -28.279689
#' #Sesgos de septiembre 2001-2010 (Pamplona): 14.530245 10.329472 5.954770  1.9694591 -2.39377422  -7.32843214 -11.311756 -15.475733 -20.573542
#' #Sesgos de octubre 2001-2010 (Pamplona): 9.272235  5.014693 1.022492 -2.4663038 -5.84981719  -8.50479003 -10.938170 -13.310972 -16.866932
#' #Sesgos de noviembre 2001-2010 (Pamplona): 8.601537  4.282169 1.398568 -0.4701152 -2.64749511  -5.07211914  -7.881975 -10.645725 -14.353285
#' #Sesgos de diciembre 2001-2010 (Pamplona): 9.978958  6.616782 3.990857  1.3553966 -0.94111170  -2.99050566  -5.534412  -7.217937 -10.955127
#'
#' #Sesgos de enero 1961-1970 (Panticosa): 11.878816  7.655138  5.1085359  2.6382632  0.2604592 -2.1829724  -5.050928825  -7.320185 -11.044090
#' #Sesgos de febrero 1961-1970 (Panticosa): 14.425902  9.861512  7.3382673  3.9707533  1.1992825 -1.6813010  -4.606884572  -8.163483 -11.625154
#' #Sesgos de marzo 1961-1970 (Panticosa): 13.980787  9.125311  6.3101839  3.8993676  1.4434960 -1.3410812  -3.856548074  -7.054915 -10.721398
#' #Sesgos de abril 1961-1970 (Panticosa): 14.447133 10.618110  7.1065827  4.3769966  2.3662927 -0.4994486  -2.858607968  -5.782321  -9.320977
#' #Sesgos de mayo 1961-1970 (Panticosa): 16.172094 12.409652  8.9973180  4.9935767  1.7887106 -0.9576313  -3.643698500  -7.961629 -12.061569
#' #Sesgos de junio 1961-1970 (Panticosa): 18.624811 14.823599 11.6472076  8.3996891  5.7235942  2.5873804   0.004082181  -3.073180  -6.575969
#' #Sesgos de julio 1961-1970 (Panticosa): 11.548788  7.310639  3.7248774  1.2102772 -0.4467569 -2.6922527  -4.398613076  -6.948268  -9.096213
#' #Sesgos de agosto 1961-1970 (Panticosa): 11.100325  7.701410  4.9531268  2.1844239 -0.2916569 -2.1248372  -5.157220250  -7.697868 -10.888948
#' #Sesgos de septiembre 1961-1970 (Panticosa): 13.325275  8.079865  4.7327555  0.5084139 -3.5235574 -7.2142149 -10.291318252 -12.825008 -16.221518
#' #Sesgos de octubre 1961-1970 (Panticosa): 6.987526  3.160168 -0.8923425 -4.0562594 -6.8269942 -9.1845771 -12.688915910 -15.065240 -17.012258
#' #Sesgos de noviembre 1961-1970 (Panticosa): 10.828649  7.813591  4.4161944  1.3044278 -1.1975308 -4.2971978  -7.288287903 -10.834970 -16.146036
#' #Sesgos de diciembre 1961-1970 (Panticosa): 12.597822  7.624090  5.2052568  2.6881225  0.3311684 -2.1181841  -4.811195665  -8.562209 -12.599818
#'
#' #Sesgos de enero 1981-1990 (Panticosa): 13.919331  7.935234  5.863274  3.25417598  0.3058638 -1.827479 -3.7613287  -6.209765 -10.292784
#' #Sesgos de febrero 1981-1990 (Panticosa): 14.278610 10.856164  7.023157  3.93247192  0.9929023 -1.742025 -4.5167352  -6.813894  -9.681413
#' #Sesgos de marzo 1981-1990 (Panticosa): 15.103158 10.943038  7.611236  4.73605891  1.2582548 -1.556220 -4.2328693  -8.142631 -11.887570
#' #Sesgos de abril 1981-1990 (Panticosa): 15.733061 12.372952  9.255893  6.86535558  4.1879272  1.177955 -1.8651722  -5.309742  -8.356188
#' #Sesgos de mayo 1981-1990 (Panticosa): 19.720592 15.877113 12.979030 10.16553669  7.5232067  4.853569  0.8312414  -3.327530  -8.569299
#' #Sesgos de junio 1981-1990 (Panticosa): 18.323746 14.347496 11.115745  7.99285139  4.9597090  1.744337 -1.2198108  -4.465202  -9.664832
#' #Sesgos de julio 1981-1990 (Panticosa): 11.258899  5.926607  2.938990 -0.36641201 -2.3376547 -4.370445 -6.5718078  -8.772729 -11.264621
#' #Sesgos de agosto 1981-1990 (Panticosa): 12.987761  7.580417  2.589040 -0.29437700 -2.8996524 -5.077507 -7.1692776  -9.468630 -13.130663
#' #Sesgos de septiembre 1981-1990 (Panticosa): 9.267753  5.283995  2.318686  0.00497837 -2.7266412 -4.321894 -7.2104732 -10.183279 -14.490899
#' #Sesgos de octubre 1981-1990 (Panticosa): 9.209851  4.983451  2.203518 -0.62123921 -2.3367717 -5.205763 -7.6313157  -9.886077 -13.598184
#' #Sesgos de noviembre 1981-1990 (Panticosa): 8.677407  4.802228  1.500878 -1.92207260 -4.0975024 -6.504389 -8.6713813 -10.772474 -14.574913
#' #Sesgos de diciembre 1981-1990 (Panticosa): 10.216650  6.601934  3.358709  1.08462600 -1.4275873 -3.913314 -5.1780778  -7.872766 -11.361970
#'
#' #Sesgos de enero 2001-2010 (Panticosa): 15.114503 10.087739 6.388715  2.95770182  0.4413715 -2.8144984 -5.386694  -8.841107 -12.27062
#' #Sesgos de febrero 2001-2010 (Panticosa): 16.849271 11.794249 8.215361  5.46793727  2.4618425 -0.4364138 -4.443533  -7.716017 -12.19363
#' #Sesgos de marzo 2001-2010 (Panticosa): 14.497425  9.733676 5.966760  2.74553144 -0.5570169 -3.7994597 -7.293079 -10.743976 -14.68098
#' #Sesgos de abril 2001-2010 (Panticosa): 16.094178 11.774352 8.281202  5.21399405  2.0784114 -1.1772767 -4.127492  -6.789513 -10.11152
#' #Sesgos de mayo 2001-2010 (Panticosa): 15.079473 10.783546 7.545202  4.37971426  0.6695524 -2.6455817 -5.653493  -8.792416 -11.98178
#' #Sesgos de junio 2001-2010 (Panticosa): 14.243895  8.763365 5.125804  1.53452468 -2.0084064 -5.0174323 -8.193329 -10.769495 -13.01684
#' #Sesgos de julio 2001-2010 (Panticosa): 11.389609  6.050085 2.650129 -0.52665585 -2.2565211 -5.0530258 -6.899793  -8.644829 -10.40287
#' #Sesgos de agosto 2001-2010 (Panticosa): 11.986309  7.283786 3.137386  0.07012205 -2.3717415 -5.3231348 -7.644931 -10.090152 -13.54793
#' #Sesgos de septiembre 2001-2010 (Panticosa): 10.546789  6.109093 2.844229 -0.35391517 -3.3274478 -5.9768759 -8.486930 -10.704534 -13.86238
#' #Sesgos de octubre 2001-2010 (Panticosa): 9.059047  5.821504 2.556933 -0.06529924 -2.4375672 -5.6892965 -8.850736 -11.752665 -14.65717
#' #Sesgos de noviembre 2001-2010 (Panticosa): 12.939764  8.602765 4.980641  1.83380629 -1.5456524 -4.4834442 -7.631541 -10.466102 -14.36326
#' #Sesgos de diciembre 2001-2010 (Panticosa): 15.241801 11.810011 8.399191  5.68623196  2.7507712  0.5274593 -2.136066  -5.408750 -10.26937
#'
#' #Sesgos de enero 1961-1970 (Tornos): 12.435321  9.753033  7.5143696  5.3125414  3.0561459   1.0783613  -0.8319417  -3.413862  -7.574877
#' #Sesgos de febrero 1961-1970 (Tornos): 14.856157 11.806320  8.9930291  6.8890910  4.1835064   1.2472204  -1.7819636  -5.369783 -10.996986
#' #Sesgos de marzo 1961-1970 (Tornos): 15.377746 10.669561  7.4984887  4.6800153  1.9002205  -1.2917957  -4.1768897  -8.922841 -13.925466
#' #Sesgos de abril 1961-1970 (Tornos): 17.820520 12.739089  9.0131890  5.9890730  2.6399575  -0.5095758  -3.7449114  -8.506635 -13.905755
#' #Sesgos de mayo 1961-1970 (Tornos): 18.843562 13.431125  8.4741971  4.3821261  0.4194998  -3.9515943  -7.9863388 -12.458763 -19.066220
#' #Sesgos de junio 1961-1970 (Tornos): 20.573963 15.464728 11.6164100  8.3243513  4.1635290   0.1381451  -2.8707785  -7.696024 -13.284715
#' #Sesgos de julio 1961-1970 (Tornos): 14.245099  8.815093  4.5074038  0.8741379 -2.6996555  -5.5232926  -9.7497289 -13.331761 -18.161264
#' #Sesgos de agosto 1961-1970 (Tornos): 14.944797  9.533038  5.1018982  1.3053870 -2.0859915  -5.1091634  -8.6787101 -12.769443 -17.972076
#' #Sesgos de septiembre 1961-1970 (Tornos): 15.413892 10.180386  5.2767831 -0.2815969 -4.8738339  -9.2618637 -13.5348129 -17.705974 -21.277133
#' #Sesgos de octubre 1961-1970 (Tornos): 9.890872  5.480821  0.9239678 -3.5769585 -8.2142258 -12.8773672 -16.1271799 -20.404927 -23.587437
#' #Sesgos de noviembre 1961-1970 (Tornos): 10.887243  7.247814  3.5873831  1.8250911 -0.6076028  -3.5242604  -6.8115986 -10.835255 -16.822284
#' #Sesgos de diciembre 1961-1970 (Tornos): 11.910100  8.923167  6.4561318  4.1950407  1.9511275  -0.2620427  -2.2532916  -5.131295  -9.713282
#'
#' #Sesgos de enero 1981-1990 (Tornos): 9.909092  5.728744  3.11013323  0.4928400  -2.1067813  -4.9288448  -7.949093 -11.424631 -15.40650
#' #Sesgos de febrero 1981-1990 (Tornos): 14.850239 10.049351  6.96510053  2.8292206  -0.1302911  -4.0394033  -7.771246 -12.309895 -14.67507
#' #Sesgos de marzo 1981-1990 (Tornos): 14.931946 10.976185  7.16450822  2.7509098  -0.6729978  -4.8665391  -9.120153 -13.142929 -19.45299
#' #Sesgos de abril 1981-1990 (Tornos): 22.487633 16.886017 12.52846199  8.7475482   4.6532276   0.4715963  -3.493207  -8.229786 -14.74895
#' #Sesgos de mayo 1981-1990 (Tornos): 26.196668 20.336918 14.84700573 11.4640597   6.6610241   3.3728234  -1.947166  -7.513891 -13.04828
#' #Sesgos de junio 1981-1990 (Tornos): 23.023128 17.150555 11.37948054  7.1557899   3.6894278  -0.2749600  -4.563379  -8.734822 -14.06226
#' #Sesgos de julio 1981-1990 (Tornos): 18.189557 10.295582  4.18913949  0.2554925  -4.6448822  -7.5632676 -10.069086 -13.145118 -15.79697
#' #Sesgos de agosto 1981-1990 (Tornos): 13.435719  6.566964  2.47509761 -2.1541271  -5.9096843  -8.4944961 -10.675075 -14.632619 -18.52190
#' #Sesgos de septiembre 1981-1990 (Tornos): 10.674545  3.379161 -2.93575013 -7.2960813 -10.4854369 -12.8804372 -15.852531 -18.073642 -21.88962
#' #Sesgos de octubre 1981-1990 (Tornos): 8.563885  5.078327 -0.06140443 -5.2925350  -9.1165036 -14.0016609 -17.064038 -20.422175 -23.83493
#' #Sesgos de noviembre 1981-1990 (Tornos): 5.936802  1.397541 -3.29183043 -6.7996819  -9.3910381 -12.6226431 -15.442694 -18.975556 -24.16817
#' #Sesgos de diciembre 1981-1990 (Tornos): 5.138926  1.617098 -1.40843477 -3.7108290  -5.2915469  -8.0342510 -11.129732 -14.287470 -17.94278
#'
#' #Sesgos de enero 2001-2010 (Tornos): 13.297347  9.163179  6.428217  4.98499288  3.5379436  1.1590567  -1.491207  -3.402682  -5.031202
#' #Sesgos de febrero 2001-2010 (Tornos): 17.498517 14.147961 10.155793  7.48351862  5.4886697  2.4633691  -1.107337  -4.287417  -7.990685
#' #Sesgos de marzo 2001-2010 (Tornos): 17.577063 12.611358  8.810511  4.02972040 -0.6571526 -3.5373289  -6.877077 -10.670383 -16.144147
#' #Sesgos de abril 2001-2010 (Tornos): 20.029976 16.865607 13.609737  8.15320541  3.9445700 -1.5188932  -4.483814  -9.530648 -12.641204
#' #Sesgos de mayo 2001-2010 (Tornos): 23.320774 18.015844 12.712807  7.12521369  2.1408181 -2.2522391  -7.164788 -12.034651 -16.445958
#' #Sesgos de junio 2001-2010 (Tornos): 19.424498 13.679649  8.621548  3.86257502 -0.9745644 -5.1774146  -9.499978 -13.489843 -16.977136
#' #Sesgos de julio 2001-2010 (Tornos): 15.563997  9.505566  3.955748 -0.05762521 -3.0062758 -6.5366517  -9.118636 -12.424347 -14.797211
#' #Sesgos de agosto 2001-2010 (Tornos): 15.793193 10.006261  4.791525  0.41964744 -2.9788668 -6.7150038 -10.183999 -13.480253 -16.605914
#' #Sesgos de septiembre 2001-2010 (Tornos): 13.691213  7.747725  3.361674 -0.21615760 -3.7863221 -7.2371121  -8.790599 -12.005326 -15.487380
#' #Sesgos de octubre 2001-2010 (Tornos): 9.755498  6.165426  1.302897 -2.02363935 -4.9304009 -8.0442354 -10.739167 -12.909455 -16.600313
#' #Sesgos de noviembre 2001-2010 (Tornos): 9.607418  7.177740  5.272235  1.77976132 -0.8223670 -3.4775401  -6.290732  -8.744484 -13.031720
#' #Sesgos de diciembre 2001-2010 (Tornos): 12.321033  8.863175  4.639656  3.53747276  2.0827598  0.2042298  -2.648537  -4.317789  -6.342590
#'
#' #Sesgos medios mensuales de Zaragoza de las décadas 1961-1970, 1971-1980, 1981-1990, 1991-2000 y 2001-2010
#' placeindex<-13
#' fechas<-which((auxiliar$year>=1961)&(auxiliar$year<=2010))
#'
#' start_time <- Sys.time()
#' marginquantilematrix2<-samplequant2(places[placeindex], auxiliar,modelos.rstan.jul[[places[placeindex]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[fechas,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),w.days=7,w.years=2,extractedsamples=200,randomsamples=200)
#' end_time <- Sys.time()
#' timeesf<-end_time - start_time
#' #Time difference of 7.530833 hours
#'
#' start_time <- Sys.time()
#' empiricquantilevector<-quantmarginal(auxiliar[places[placeindex]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[fechas,c(1,4)],c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),w.days=7,w.years=2,randomsamples=1)
#' end_time <- Sys.time()
#' timeesf2<-end_time - start_time
#' #Time difference of 1.549967 mins
#'
#' w.years<-2
#' w.days<-7
#' needdatesindex<-NULL
#' for(i in fechas){
#' aux.needdatesindex<-which((auxiliar$year>=auxiliar$year[i]-w.years)&(auxiliar$year<=auxiliar$year[i]+w.years)&(auxiliar$day.year==auxiliar$day.year[i]))
#'  for(j in aux.needdatesindex){
#'    needdatesindex<-c(needdatesindex,(j-w.days):(j+w.days))
#'  }
#' }
#' needdatesindex<-sort(unique(needdatesindex))
#'
#' sesgos<-NULL
#' for(i in 1:9){
#'   #lines(density(marginquantilematrix2[[i]]))
#'   #abline(v=empiricquantilevector[[i]],col=i+1)
#'   sesgos<-cbind(sesgos,rowMeans(marginquantilematrix2[[i]])-empiricquantilevector[[i]])
#' }
#' INDEX<-(round((auxiliar$year-1)/10)*1000+auxiliar$month)[fechas]
#' meansesgos<-tapply(sesgos,INDEX=INDEX,FUN=mean)#' #Sesgos de enero 1961-1970 (Zaragoza): (15.8185054   9.7708481   5.4428089   2.1721025  -0.7592067  -3.2203477  -6.7462737  -9.7521370 -13.5979871)
#' #meansesgos<-colMeans(sesgos)
#'
#'
#' #15 de enero y 15 de junio de 1960, 1980 y 2000 (cuantiles 0.5 y 0.75) (FIGURA TFM)
#' placeindex<-13
#' fechas<-which((auxiliar$year %in% c(1960,1980,2000))&(auxiliar$day.year %in% c(15,167)))
#'
#' start_time <- Sys.time()
#' marginquantilematrix2<-samplequant2(places[placeindex], auxiliar,modelos.rstan.jul[[places[placeindex]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[fechas,c(1,4)],c(0.25,0.5,0.75),w.days=7,w.years=2,extractedsamples=200,randomsamples=200)
#' end_time <- Sys.time()
#' timeesf<-end_time - start_time
#' #Time difference of 14.32921 secs
#'
#' start_time <- Sys.time()
#' empiricquantilevector<-quantmarginal(auxiliar[places[placeindex]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[fechas,c(1,4)],c(0.25,0.5,0.75),w.days=7,w.years=2,randomsamples=1)
#' end_time <- Sys.time()
#' timeesf2<-end_time - start_time
#' #Time difference of 0.0233171 secs
#'
#' par(mfrow=c(2,3))
#' plot(density(marginquantilematrix2$`0.5`["1960-15",]), xlim=c(65,140),col=2,main="1960",ylab="15 enero",xlab="")
#' lines(density(marginquantilematrix2$`0.25`["1960-15",]),col=1)
#' lines(density(marginquantilematrix2$`0.75`["1960-15",]),col=4)
#'
#' plot(density(marginquantilematrix2$`0.5`["1980-15",]), xlim=c(65,140),col=2,main="1980",ylab="15 enero",xlab="")
#' lines(density(marginquantilematrix2$`0.25`["1980-15",]),col=1)
#' lines(density(marginquantilematrix2$`0.75`["1980-15",]),col=4)
#'
#' plot(density(marginquantilematrix2$`0.5`["2000-15",]), xlim=c(65,140),col=2,main="2000",ylab="15 enero",xlab="")
#' lines(density(marginquantilematrix2$`0.25`["2000-15",]),col=1)
#' lines(density(marginquantilematrix2$`0.75`["2000-15",]),col=4)
#'
#'
#' plot(density(marginquantilematrix2$`0.25`["1960-167",]), xlim=c(245,340),col=1,main="1960",ylab="15 junio",xlab="")
#' lines(density(marginquantilematrix2$`0.5`["1960-167",]),col=2)
#' lines(density(marginquantilematrix2$`0.75`["1960-167",]),col=4)
#'
#' plot(density(marginquantilematrix2$`0.25`["1980-167",]), xlim=c(245,340),col=1,main="1980",ylab="15 junio",xlab="")
#' lines(density(marginquantilematrix2$`0.5`["1980-167",]),col=2)
#' lines(density(marginquantilematrix2$`0.75`["1980-167",]),col=4)
#'
#' plot(density(marginquantilematrix2$`0.25`["2000-167",]), xlim=c(245,340),col=1,main="2000",ylab="15 junio",xlab="")
#' lines(density(marginquantilematrix2$`0.5`["2000-167",]),col=2)
#' lines(density(marginquantilematrix2$`0.75`["2000-167",]),col=4)
#'
#'
#' w.years<-2
#' w.days<-7
#' needdatesindex<-NULL
#' for(i in fechas){
#' aux.needdatesindex<-which((auxiliar$year>=auxiliar$year[i]-w.years)&(auxiliar$year<=auxiliar$year[i]+w.years)&(auxiliar$day.year==auxiliar$day.year[i]))
#'  for(j in aux.needdatesindex){
#'    needdatesindex<-c(needdatesindex,(j-w.days):(j+w.days))
#'  }
#' }
#' needdatesindex<-sort(unique(needdatesindex))
#'
#' sesgos<-NULL
#' for(i in 1:9){
#'   #lines(density(marginquantilematrix2[[i]]))
#'   #abline(v=empiricquantilevector[[i]],col=i+1)
#'   sesgos<-cbind(sesgos,rowMeans(marginquantilematrix2[[i]])-empiricquantilevector[[i]])
#' }
#' INDEX<-(round((auxiliar$year-1)/10)*1000+auxiliar$month)[fechas]
#' meansesgos<-tapply(sesgos,INDEX=INDEX,FUN=mean)#' #Sesgos de enero 1961-1970 (Zaragoza): (15.8185054   9.7708481   5.4428089   2.1721025  -0.7592067  -3.2203477  -6.7462737  -9.7521370 -13.5979871)
#' #meansesgos<-colMeans(sesgos)
#'
#'
#'
#' #Boxplot de las diferencias de media a posteriori de los cuantiles 0.25, 0.5 y 0.75 entre el 15 de junio de 1955,1980 y 2013 para los 18 lugares juntos (FIGURA TFM)
#' fechas<-which((auxiliar$year %in% c(1955,1980,2013))&(auxiliar$day.month == 15)&(auxiliar$month == 6))
#' #fechas<-which((auxiliar$year %in% c(1955,1980,2013))&(auxiliar$day.month == 15)&(auxiliar$month == 1))
#' quantiles<-c(0.25,0.5,0.75)
#' marginquantilelist<-sapply(as.character(quantiles),function(x) NULL)
#' INDEX<-matrix(rep(1:length(fechas),200),ncol=200)
#'
#' start_time <- Sys.time()
#' for(i.place in 1:18){
#'   newmarginquantilelist<-samplequant2(places[i.place], auxiliar,modelos.rstan.jul[[places[i.place]]],auxiliar[-c(1,2),c(1,4)],choosedates=auxiliar[fechas,c(1,4)],c(0.25,0.5,0.75),w.days=7,w.years=2,extractedsamples=200,randomsamples=200)
#'   for(i.quant in as.character(quantiles)){
#'     marginquantilelist[[i.quant]]<-cbind(marginquantilelist[[i.quant]],tapply(newmarginquantilelist[[i.quant]][paste(auxiliar[fechas,1],auxiliar[fechas,4],sep="-"),],INDEX=INDEX,FUN=mean))
#'   }
#' }
#' end_time <- Sys.time()
#' timeesf<-end_time - start_time
#' #Time difference of 1.410661 mins
#'
#' #diferencias_names<-paste(rep(paste("q",quantiles,sep=""),each=2),rep(c("1955-1980","1980-2013"),3))
#' diferencias_names<-NULL
#' diferencias_quant<-NULL
#' diferencias_years<-NULL
#' diferencias<-NULL
#'
#' for(i in 1:2){
#'   if(i==1){
#'     i.firstyear<-1955
#'     i.lastyear<-1980
#'   } else{
#'     i.firstyear<-1980
#'     i.lastyear<-2013
#'   }
#'   for(i.quant in 1:length(quantiles)){
#'     #diferencias_names<-c(diferencias_names,rep(paste(paste("q",quantiles[i.quant],sep=""),paste(i.firstyear,i.lastyear,sep="-")),18))
#'     diferencias_quant<-c(diferencias_quant,rep(paste("q",quantiles[i.quant],sep=""),18))
#'     diferencias_years<-c(diferencias_years,rep(paste(i.firstyear,i.lastyear,sep="-"),18))
#'     diferencias<-c(diferencias,marginquantilelist[[i.quant]][i+1,]-marginquantilelist[[i.quant]][i,])
#'   }
#' }
#' diferencias
#'
#' myplot <- boxplot(diferencias/10~diferencias_years*diferencias_quant ,
#' boxwex=0.4 , ylab="ºC", col=c("red","blue"), xaxt="n",xlab="")
#' axis(1, at = seq(1.5 , 6 , 2), labels = paste("q",quantiles,sep="") , tick=FALSE , cex=0.3)
#'
#' for(i in seq(0.5 , 8, 2)){
#'  abline(v=i,lty=1, col="grey")
#'  }
#'
#' #legend("bottomleft", legend = c("1955-1980", "1980-2013"),
#' #col=c("red","blue"), pch = 15, bty = "n", pt.cex = 3, cex = 1.2,  horiz = F, inset = c(0.1, 0.1))
#'
#'
NULL

#' @rdname samplequant
#' @export
samplequant<-function(aux.X,aux.X.sigma2,stanfit,dates,choosedates=dates,quant=0.5,w.days=7,w.years=2,extractedsamples=2000,randomsamples=10,samplefrom=2000){

  ini<-which((dates$year==(dates$year[1]+w.years))&(dates$day.year==(dates$day.year[1])))+w.days
  fin<-which((dates$year==(dates$year[length(dates$year)]-w.years))&(dates$day.year==(dates$day.year[length(dates$day.year)])))-w.days
  if(((dates$year[1]+w.years)%%4==0)&(w.years%%4!=0)&(dates$day.year[1]>=365-w.days)){
    ini<-which((dates$year==(dates$year[1]+w.years))&(dates$day.year==(dates$day.year[1]+1)))+w.days
  }
  if((dates$year[1]%%4==0)&(w.years%%4!=0)&(dates$day.year[1]>=366-w.days)){
    ini<-which((dates$year==(dates$year[1]+w.years))&(dates$day.year==(dates$day.year[1]-1)))+w.days
  }
  # # if((dates$day.year[1]==366)&(w.years%%4!=0)){
  # #   ini<-which((dates$year==(dates$year[1]+w.years))&(dates$day.year==365))+w.days
  # # }
  # if(((dates$year[length(dates$year)]-w.years-1)%%4==0)&(w.years%%4!=0)&(dates$day.year[length(dates$day.year)]<=w.days)){
  #   fin<-which((dates$year==(dates$year[length(dates$year)]-w.years))&(dates$day.year==(dates$day.year[length(dates$day.year)]+1)))-w.days
  # }
  if(((dates$year[length(dates$year)]-1)%%4==0)&(w.years%%4!=0)&(dates$day.year[length(dates$day.year)]<=w.days)){
    fin<-which((dates$year==(dates$year[length(dates$year)]-w.years))&(dates$day.year==(dates$day.year[length(dates$day.year)]-1)))-w.days
  }
  # if((w.years%%4!=0)&(dates$day.year[length(dates$day.year)]==366)){
  #   fin<-which((dates$year==(dates$year[length(dates$year)]-w.years))&(dates$day.year==365))-w.days+1
  # }
  # # if((dates$day.year[length(dates$day.year)]==366)&(w.years%%4!=0)){
  # #   fin<-which((dates$year==(dates$year[length(dates$year)]-w.years))&(dates$day.year==365))-w.days
  # # }
  maxvector<-ini:fin
  choosedatesindex<-maxvector[maxvector %in% which((dates$year %in% choosedates$year)&(dates$day.year %in% choosedates$day.year))]
  choosedates<-dates[choosedatesindex,]

  needdatesindex<-NULL
  for(i in choosedatesindex){
    aux.needdatesindex<-which((dates$year>=dates$year[i]-w.years)&(dates$year<=dates$year[i]+w.years)&(dates$day.year==dates$day.year[i]))
    for(j in aux.needdatesindex){
      needdatesindex<-c(needdatesindex,(j-w.days):(j+w.days))
    }
  }
  needdatesindex<-sort(unique(needdatesindex))

  samples<-samplerstan(aux.X[needdatesindex,],aux.X.sigma2[needdatesindex,],stanfit=stanfit,extractedsamples=extractedsamples,randomsamples=randomsamples,samplefrom=samplefrom)
  marginquantilematrix<-quantmarginal(samples,dates[needdatesindex,],choosedates=choosedates,quant=quant,w.days=w.days,w.years=w.years,randomsamples=randomsamples)

  return(marginquantilematrix)
}

#' @rdname samplequant
#' @export
samplequant2<-function(place,data,stanfit,dates,choosedates=dates,quant=0.5,w.days=7,w.years=2,extractedsamples=2000,randomsamples=10,samplefrom=2000){

  ini<-which((dates$year==(dates$year[1]+w.years))&(dates$day.year==dates$day.year[1]))+w.days
  fin<-which((dates$year==(dates$year[length(dates$year)]-w.years))&(dates$day.year==dates$day.year[length(dates$day.year)]))-w.days
  if(((dates$year[1]+w.years)%%4==0)&(w.years%%4!=0)&(dates$day.year[1]>365-w.days)){
    # ini<-which((dates$year==(dates$year[1]+w.years))&(dates$day.year==(dates$day.year[1]+1)))+w.days
    ini<-which((dates$year==(dates$year[1]+w.years+1))&(dates$day.year==(dates$day.year[1]+w.days+1-365)))
  } else if(((dates$year[1]+w.years)%%4==0)&(w.years%%4!=0)&(dates$day.year[1]==365-w.days)){
    # ini<-which((dates$year==(dates$year[1]+w.years))&(dates$day.year==(dates$day.year[1]+1)))+w.days
    ini<-which((dates$year==(dates$year[1]+w.years))&(dates$day.year==366))
  }
  if((dates$year[1]%%4==0)&(w.years%%4!=0)&(dates$day.year[1]>366-w.days)){
    # ini<-which((dates$year==(dates$year[1]+w.years))&(dates$day.year==(dates$day.year[1]-1)))+w.days
    ini<-which((dates$year==(dates$year[1]+w.years+1))&(dates$day.year==(dates$day.year[1]+w.days-366)))
  } else if((dates$year[1]%%4==0)&(w.years%%4!=0)&(dates$day.year[1]==366-w.days)){
    ini<-which((dates$year==(dates$year[1]+w.years+1))&(dates$day.year==1))
  }
  # # if((dates$day.year[1]==366)&(w.years%%4!=0)){
  # #   ini<-which((dates$year==(dates$year[1]+w.years))&(dates$day.year==365))+w.days
  # # }
  if(((dates$year[length(dates$year)]-w.years-1)%%4==0)&(w.years%%4!=0)&(dates$day.year[length(dates$day.year)]<=w.days)){
    # fin<-which((dates$year==(dates$year[length(dates$year)]-w.years))&(dates$day.year==(dates$day.year[length(dates$day.year)]-1)))-w.days
    fin<-which((dates$year==(dates$year[length(dates$year)]-w.years-1))&(dates$day.year==(dates$day.year[length(dates$day.year)]-w.days-1+366)))
    # if(dates$day.year[length(dates$day.year)]==1){
    #   fin<-which((dates$year==(dates$year[length(dates$year)]-w.years-1))&(dates$day.year==365))-w.days+1
    # }
  }
  if(((dates$year[length(dates$year)]-1)%%4==0)&(w.years%%4!=0)&(dates$day.year[length(dates$day.year)]<=w.days)){
    fin<-which((dates$year==(dates$year[length(dates$year)]-w.years-1))&(dates$day.year==(dates$day.year[length(dates$day.year)]-w.days+1+365)))
  } else if(((dates$year[length(dates$year)])%%4==0)&(w.years%%4!=0)&(dates$day.year[length(dates$day.year)]==366)){
    fin<-which((dates$year==(dates$year[length(dates$year)]-w.years))&(dates$day.year==(366-w.days)))
  }
  # if((w.years%%4!=0)&(dates$day.year[length(dates$day.year)]==366)){
  #   fin<-which((dates$year==(dates$year[length(dates$year)]-w.years))&(dates$day.year==365))-w.days+1
  # }
  # # if((dates$day.year[length(dates$day.year)]==366)&(w.years%%4!=0)){
  # #   fin<-which((dates$year==(dates$year[length(dates$year)]-w.years))&(dates$day.year==365))-w.days
  # # }
  maxvector<-ini:fin
  choosedatesindex<-maxvector[maxvector %in% which((dates$year %in% choosedates$year)&(dates$day.year %in% choosedates$day.year))]
  choosedates<-dates[choosedatesindex,]

  needdatesindex<-NULL
  for(i in choosedatesindex){
    if(dates$day.year[i]==366){
      aux.needdatesindex<-which((dates$year>=dates$year[i]-w.years)&(dates$year<=dates$year[i]+w.years)&(dates$day.year==365))
      for(j in 1:length(aux.needdatesindex)){
        if(dates$year[aux.needdatesindex[j]]%%4==0){
          aux.needdatesindex[j]<-aux.needdatesindex[j]+1
        }
      }
    }else{
      aux.needdatesindex<-which((dates$year>=dates$year[i]-w.years)&(dates$year<=dates$year[i]+w.years)&(dates$day.year==dates$day.year[i]))
    }

    for(j in aux.needdatesindex){
      needdatesindex<-c(needdatesindex,(j-w.days):(j+w.days))
    }
  }
  needdatesindex<-sort(unique(needdatesindex))

  modelo.lm<- ajuste.parada(place,data,verbal=FALSE)
  aux.X <- model.matrix(modelo.lm[[modelo.lm$mejor_modelo]])[-1,]
  aux.X.sigma2 <- model.matrix(modelo.lm[[modelo.lm$mejor_modelo_var]])[-1,]

  samples<-samplerstan(aux.X[needdatesindex,],aux.X.sigma2[needdatesindex,],stanfit=stanfit,extractedsamples=extractedsamples,randomsamples=randomsamples,samplefrom=samplefrom)

  marginquantilematrix<-quantmarginal(samples,dates[needdatesindex,],choosedates=choosedates,quant=quant,w.days=w.days,w.years=w.years,randomsamples=randomsamples)


  return(marginquantilematrix)
}

