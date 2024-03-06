#' @name boxmonths
#' @rdname boxmonths
#' @title Boxplot by months
#' @description A boxplot of the data separated in months.
#' @param data Array of two columns: one with the data that we want to represent and the other with the months linked to each value.
#' @param title String that will appear as the title of the boxplot.
#' @param english Boolean. If TRUE, the axis labels will be in english. Otherwise, it will be in spanish.
#' @aliases boxmeses
#' @examples
#' x<-c(30,31,33,30,29,28,29,32,35)
#' y<-rep(c(1,2,3),3)
#' boxmonths(cbind(x,y))
#'
#' data<- read.table( 'Tmax_Aragon.txt', header=T, dec='.'  )
#' aux.data<-cbind(data$Tx.zaragoza,data$month)
#' boxmonths(aux.data,english=FALSE)
#'
#' aux.threshold<-tapply(data$Tx.zaragoza,data$month,quantile,0.50)
#' aux.marcador<-(data$Tx.zaragoza>aux.threshold[data$month])
#' boxmonths(aux.data[aux.marcador],english=FALSE)
#'
#' anomaly<-(data$Tx.zaragoza-aux.threshold[data$month])
#' aux.data<-cbind(anomaly,data$month)
#' boxmonths(aux.data,english=FALSE)
NULL

#' @rdname boxmonths
#' @export
boxmonths<-function(data,title="",english=TRUE){
  if(english==FALSE){
    boxplot(split(data[,1],data[,2]),xlab="Meses",ylab="ºC",cex.lab=1.5)
  }else if(english==TRUE){
    boxplot(split(data[,1],data[,2]),xlab="Months",ylab="ºC",cex.lab=1.5)
  }else{warning("english must be TRUE or FALSE")}
  title(main=paste(title),cex.main=2)
  abline(h = 0, lty=2, col="red")
}

