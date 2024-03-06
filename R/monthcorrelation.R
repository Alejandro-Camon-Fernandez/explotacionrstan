#' @name persistence
#' @rdname persistence
#' @title Autocorrelation by months
#' @description A matrix of the autocorrelation coefficients of the data that can be separated by months.
#' @param data Array of the data.
#' @param dates Data frame of dates asociated to the data with columns for the year, the day of the year (day.year) and the month.
#' @param lag Number of days of lag that we want to obtain the correlation coefficients for.
#' @param monthly If monthly=TRUE the correlation will be calculated by months. If monthly=FALSE,the function will calculate the same autocorrelation coefficients that can be aobtained by the function acf.
#' @aliases mescorrelation
#' @examples
#' data<-c(30,31,33,30,29,28,29,32,35)
#' dates<-data.frame(row.names=1:9)
#' dates$year<-rep(1999,9)
#' dates$month<-rep(1,9)
#' dates$day.year<-1:9
#' monthcorrelation(data,dates)
#'
#' auxiliar<- read.table( 'Tmax_Aragon.txt', header=T, dec='.'  )
#' cormatrix<-monthcorrelation(auxiliar[,places[13]],auxiliar[,1:4],lags=c(1:5))
#' monthcorrelation(auxiliar[,places[13]],auxiliar[,1:4],lags=c(1:5),monthly=FALSE)
#' monthcorrelation(auxiliar[,places[13]],auxiliar[,1:4],lags=c(1:5),monthly=TRUE)
#' acf(auxiliar[,places[13]])[1:5]
NULL

#' @rdname persistence
#' @export
monthcorrelation<-function(data, dates, lags=1, monthly=TRUE){
  ini<-which((dates$year==(dates$year[1]))&(dates$day.year==(dates$day.year[1])))+max(lags)
  fin<-length(data)
  aux.data<-data.frame(data[ini:fin])
  names(aux.data)[1]<-"original"
  for(i in lags){
    aux.data[[paste("lag",i)]]<-data[(ini:fin)-i]
  }

  if(monthly==TRUE){
    cormatrix<-matrix(ncol=12,nrow=length(lags))
    for(i in 1:12){
      cormatrix[,i]<-cor(aux.data[dates$month[ini:fin]==i,])[1,-1]
    }
    return(cormatrix)
  }else if(monthly==FALSE){
    cormatrix<-matrix(ncol=1,nrow=length(lags))
    cormatrix[,1]<-cor(aux.data)[1,-1]
    return(cormatrix)
  }
}
