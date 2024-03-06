#' @name annualtrend
#' @rdname annualtrend
#' @title Annual linear trends
#' @description A plot of the annual linear trends or lowess fits of the data in the places desired or in the chosen months of a specific place.
#' @param data Data frame or matrix of the data that we are representing (when monthly=TRUE we can only plot one place or column of data).
#' @param datamonth Array of the months associated to the data.
#' @param datayear Array of the year associated to the data.
#' @param showmonths Array of the months that we want to plot when monthly=TRUE.
#' @param monthly If monthly=TRUE the plot will represent the annual trend of the specified months (monthly means) of a place. If monthly=FALSE, the trends will fit annual means of each one of the chosen places.
#' @param limaxisy The range represented of the vertical axis.
#' @param title The title of the plot. If TRUE, the title will have its default value, if FALSE there won't be a title. Otherwise, the title will be the string value of this parameter.
#' @param linear Boolean. If TRUE, the plot will show the fitted trends.
#' @param low Boolean. If TRUE, the plot will show the fitted lowess.
#' @param width Width of the window of data in the lowess fit.
#' @param legend The legend of the plot. If TRUE, the legend will have its default value, if FALSE there won't be a legend. Otherwise, the legend will be the array of strings assigned of this parameter.
#' @param legpos Position of the legend.
#' @param legfont Font size of the legend.
#' @param legcols Number of columns of the legend.
#' @param english Boolean. If TRUE, the axis labels will be in english. Otherwise, it will be in spanish.
#' @aliases annualfit annualtrendmean annualtrendvariance annualtrendquantile
#' @importFrom quantreg rq
#' @examples
#' x<-c(30,31,33,30,29,28,29,32,35)
#' data<-data.frame("test"=c(30,31,33,30,29,28,29,32,35),"test2"=c(30,31,33,30,29,28,29,32,35))
#' y<-rep(c(1,2,3),3)
#' year<-c(rep(1,3),rep(2,3),rep(3,3))
#' annualtrend(data=data,datamonth=y,datayear=year,showmonths=c(1,2,3))
#' annualtrend(data=data[1],datamonth=y,datayear=year,showmonths=c(1,2,3),monthly=TRUE)
#'
#' aux.df<- read.table( 'Tmax_Aragon.txt', header=T, dec='.'  )
#' annualtrendmean(aux.df[5:22],aux.df$month,aux.df$year)
#' annualtrendmean(aux.df[17],aux.df$month,aux.df$year)
#' annualtrendmean(aux.df[17],aux.df$month,aux.df$year,monthly=TRUE)
#' annualtrendmean(aux.df[17],aux.df$month,aux.df$year,monthly=TRUE,showmonths = c(6,9),limaxisy=c(25,32),english=FALSE,title=FALSE)
#' annualtrendmean(aux.df[c(17,18)],aux.df$month,aux.df$year,monthly=FALSE,limaxisy=c(18,23),english=FALSE,title=FALSE)
#'
#' annualtrendsd(aux.df[5:22],aux.df$month,aux.df$year)
#' annualtrendsd(aux.df[17],aux.df$month,aux.df$year)
#' annualtrendsd(aux.df[17],aux.df$month,aux.df$year,monthly=TRUE,limaxisy=c(3,5))
#'
#' annualtrendquan(aux.df[5:22],aux.df$month,aux.df$year)
#' annualtrendquan(aux.df[17],aux.df$month,aux.df$year)
#' annualtrendquan(aux.df[17],aux.df$month,aux.df$year,monthly=TRUE)
#' annualtrendquan(aux.df[17],aux.df$month,aux.df$year,monthly=TRUE,showmonths = 6,quan=c(0.05,0.5,0.95),limaxisy=c(15,40),english=FALSE,title=FALSE)
#' annualtrendquan(aux.df[17],aux.df$month,aux.df$year,monthly=TRUE,showmonths = c(6,9),quan=c(0.05,0.5,0.95),limaxisy=c(15,45),english=FALSE,title=FALSE,legcols=2)
#' annualtrendquan(aux.df[c(17,18)],aux.df$month,aux.df$year,monthly=FALSE,quan=c(0.05,0.5,0.95),limaxisy=c(0,50),english=FALSE,title=FALSE,legcols=2)
#'
#' ?Cu?ndo el cuantil 0.9 de junio en zaragoza alcanza el valor original del cuantil 0.95?
#' table<-annualtrendquan(aux.df[17],aux.df$month,aux.df$year,monthly=TRUE,showmonths = 6,quan=c(0.9,0.95),limaxisy=c(15,40))
#' abline(v=(table[2,1]+table[2,2]*1953-table[1,1])/table[1,2])
#'
#' ?El cuantil 0.5 de enero de 2015 alcanza el quantil 0.5 de febrero de 1953?
#' table<-annualtrendquan(aux.df[17],aux.df$month,aux.df$year,monthly=TRUE,showmonths = c(1,2),quan=0.5,limaxisy=c(5,20))
#' (table[2,1]+table[2,2]*1953-table[1,1])/table[1,2]<2015 #No lo alcanza
#'
#' aux.threshold<-tapply(aux.df$Tx.zaragoza,aux.df$month,quantile,0.50)
#' aux.marcador<-(aux.df[17]>aux.threshold[aux.df$month])
#' annualtrendmean(aux.df[5:22][aux.marcador,],aux.df$month[aux.marcador],aux.df$year[aux.marcador])
#' annualtrendmean(aux.df[17][aux.marcador,],aux.df$month[aux.marcador],aux.df$year[aux.marcador])
#' annualtrendmean(aux.df[17][aux.marcador,],aux.df$month[aux.marcador],aux.df$year[aux.marcador],monthly=TRUE)
#'
#' annualtrendsd(aux.df[5:22][aux.marcador,],aux.df$month[aux.marcador],aux.df$year[aux.marcador])
#' annualtrendsd(aux.df[17][aux.marcador,],aux.df$month[aux.marcador],aux.df$year[aux.marcador])
#' annualtrendsd(aux.df[17][aux.marcador,],aux.df$month[aux.marcador],aux.df$year[aux.marcador],monthly=TRUE,limaxisy = c(1,3))
#'
#' annualtrendquan(aux.df[5:22][aux.marcador,],aux.df$month[aux.marcador],aux.df$year[aux.marcador])
#' annualtrendquan(aux.df[17][aux.marcador,],aux.df$month[aux.marcador],aux.df$year[aux.marcador])
#' annualtrendquan(aux.df[17][aux.marcador,],aux.df$month[aux.marcador],aux.df$year[aux.marcador],monthly=TRUE)
#'
#' aux.marcador<-(aux.df$month==1)|(aux.df$month==2)
#' annualtrendmean(aux.df[5:22][aux.marcador,],aux.df$month[aux.marcador],aux.df$year[aux.marcador])
#' annualtrendmean(aux.df[17][aux.marcador,],aux.df$month[aux.marcador],aux.df$year[aux.marcador])
#' annualtrendmean(aux.df[17][aux.marcador,],aux.df$month[aux.marcador],aux.df$year[aux.marcador],showmonths=c(1,2),monthly=TRUE)
#'
#' annualtrendsd(aux.df[5:22][aux.marcador,],aux.df$month[aux.marcador],aux.df$year[aux.marcador],limaxis=c(2,6))
#' annualtrendsd(aux.df[17][aux.marcador,],aux.df$month[aux.marcador],aux.df$year[aux.marcador],limaxis=c(2,6))
#' annualtrendsd(aux.df[17][aux.marcador,],aux.df$month[aux.marcador],aux.df$year[aux.marcador],limaxis=c(2,6),showmonths=c(1,2),monthly=TRUE)
NULL

#' @rdname annualtrend
#' @export
annualtrendmean<-function(data, datamonth, datayear, showmonths=c(1:12), monthly=FALSE, limaxisy=c(0,40), title=TRUE, linear=T, low=F, width=0.2, legend=TRUE,legpos='topleft', legfont=0.7, legcols=3, english=TRUE){
  data=data.frame(data)
  nplace=dim(data)[2]
  nmonth=length(showmonths)
  #if((dim(data)[1]==length(datamonth)) & (length(datamonth)==length(datayear))){
  par(mfrow=c(1,1))
  if(monthly==TRUE){
    if(nplace==1){
      for(i.month in 1:nmonth){
        aux.marcador <- (datamonth==showmonths[i.month])
        aux.x <- tapply(datayear[aux.marcador], datayear[aux.marcador], mean, na.rm=T)
        aux.y <- tapply(data[aux.marcador,], datayear[aux.marcador], mean, na.rm=T)/10
        if(i.month==1){
          if(english==TRUE){
            plot(aux.x, aux.y , xlab='Year',ylab="\u00B0C",type='n',ylim=limaxisy,cex.lab=1.5)
            namemonths<-c("January","February","March","April","May","June","July","August","September","October","November","December")
          }else if(english==FALSE){
            plot(aux.x, aux.y , xlab='Anio',ylab="\u00B0C",type='n',ylim=limaxisy,cex.lab=1.5)
            namemonths<-c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
          }else{warning("english must be TRUE or FALSE")}

          if(legend==TRUE){
            legend(legpos,lty=1,col=1:nmonth,legend=namemonths[showmonths[1:nmonth]],cex=legfont, ncol=legcols)
          } else if(legend==FALSE){
          } else{
            legend(legpos,lty=1,col=1:nmonth,legend=legend[1:nmonth],cex=legfont, ncol=legcols)
          }

          if(title==TRUE){
            title<-paste(names(data))
            title(main=paste(title),cex.main=2)
          } else if(title==FALSE){
          } else{
            title(main=paste(title),cex.main=2)
          }
        }
        if(linear==T){
          abline(lm(aux.y~aux.x), col=i.month,lwd=3)
        } #end if
        if(low==T){
          lines(lowess(aux.y~aux.x, f=width), col=i.month,lwd=3)
        } #end if
      } #end for i.month
    }else{warning("When monthly=TRUE data can't have more than ones column")}


  }else if(monthly==FALSE){
    for(i.place in 1:nplace){
      aux.x <- tapply(datayear, datayear, mean, na.rm=T)
      aux.y <- tapply(data[,i.place], datayear, mean, na.rm=T)/10
      if(i.place==1){
        if(english==TRUE){
          plot(aux.x, aux.y , xlab='Year',ylab="\u00B0C",type='n',ylim=limaxisy,cex.lab=1.5)
          #title(main=paste(title),cex.main=2)
        }else if(english==FALSE){
          plot(aux.x, aux.y , xlab='Anio',ylab="\u00B0C",type='n',ylim=limaxisy,cex.lab=1.5)
          #title(main=paste(title),cex.main=2)
        }else{warning("english must be TRUE or FALSE")}

        if(legend==TRUE){
          legend<-names(data)
          legend(legpos,lty=1,col=1:nplace,legend=legend[1:nplace], cex=legfont, ncol=legcols)
        } else if(legend==FALSE){
        } else{
          legend(legpos,lty=1,col=1:nplace,legend=legend[1:nplace], cex=legfont, ncol=legcols)
        }

        if(title==TRUE){
          title<-paste("Annual trend")
          title(main=paste(title),cex.main=2)
        } else if(title==FALSE){
        } else{
          title(main=paste(title),cex.main=2)
        }
      }
      if(linear==T){
        abline(lm(aux.y~aux.x), col=i.place, lwd=3) #ajuste lineal
      } #end if
      if(low==T){
        lines(lowess(aux.y~aux.x, f=width), col=i.place, lwd=3)
      } #end if
    } #end for i.place
  }else{warming("monthly must be TRUE or FALSE")}
  #}else{warming("arrays data columns, month and year must have the same length")}

} #end function


#' @rdname annualtrend
#' @export
annualtrendsd<-function(data, datamonth, datayear, showmonths=c(1:12), monthly=FALSE, desv=1, limaxisy=c(0,12), title=TRUE, linear=T, low=F, width=0.2, legend=TRUE,legpos='topleft', legfont=0.7, legcols=3, english=TRUE){
  data=data.frame(data)
  nplace=dim(data)[2]
  nmonth=length(showmonths)
  #if((dim(data)[1]==length(datamonth)) & (length(datamonth)==length(datayear))){
  par(mfrow=c(1,1))
  if(monthly==TRUE){
    if(nplace==1){
      for(i.month in 1:nmonth){
        aux.marcador <- (datamonth==showmonths[i.month])
        aux.x <- tapply(datayear[aux.marcador], datayear[aux.marcador], mean, na.rm=T)
        if(desv==1){
          aux.y <- tapply(data[aux.marcador,]/10, datayear[aux.marcador], sd, na.rm=T)
        } else if(desv==2){
          aux.y <- tapply(data[aux.marcador,]/10, datayear[aux.marcador], var, na.rm=T)
        } else{warning("desv must be 1 or 2")}
        if(i.month==1){
          if(english==TRUE){
            plot(aux.x, aux.y , xlab='Year',ylab="\u00B0C",type='n',ylim=limaxisy,cex.lab=1.5)
            namemonths<-c("January","February","March","April","May","June","July","August","September","October","November","December")
          }else if(english==FALSE){
            plot(aux.x, aux.y , xlab='Anio',ylab="\u00B0C",type='n',ylim=limaxisy,cex.lab=1.5)
            namemonths<-c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
          }else{warning("english must be TRUE or FALSE")}

          if(legend==TRUE){
            legend(legpos,lty=1,col=1:nmonth,legend=namemonths[showmonths[1:nmonth]],cex=legfont, ncol=legcols)
          } else if(legend==FALSE){
          } else{
            legend(legpos,lty=1,col=1:nmonth,legend=legend[1:nmonth],cex=legfont, ncol=legcols)
          }

          if(title==TRUE){
            title<-paste(names(data))
            title(main=paste(title),cex.main=2)
          } else if(title==FALSE){
          } else{
            title(main=paste(title),cex.main=2)
          }
        }
        if(linear==T){
          abline(lm(aux.y~aux.x), col=i.month,lwd=3)
        } #end if
        if(low==T){
          lines(lowess(aux.y~aux.x, f=width), col=i.month,lwd=3)
        } #end if
      } #end for i.month
    }else{warning("When monthly=TRUE data can't have more than ones column")}


  }else if(monthly==FALSE){
    for(i.place in 1:nplace){
      aux.x <- tapply(datayear, datayear, mean, na.rm=T)
      if(desv==1){
        aux.y <- tapply(data[,i.place]/10, datayear, sd, na.rm=T)
      } else if(desv==2){
        aux.y <- tapply(data[,i.place]/10, datayear, var, na.rm=T)
      } else{warning("desv must be 1 or 2")}
      if(i.place==1){
        if(english==TRUE){
          plot(aux.x, aux.y , xlab='Year',ylab="\u00B0C",type='n',ylim=limaxisy,cex.lab=1.5)
          #title(main=paste(title),cex.main=2)
        }else if(english==FALSE){
          plot(aux.x, aux.y , xlab='Anio',ylab="\u00B0C",type='n',ylim=limaxisy,cex.lab=1.5)
          #title(main=paste(title),cex.main=2)
        }else{warning("english must be TRUE or FALSE")}

        if(legend==TRUE){
          legend<-names(data)
          legend(legpos,lty=1,col=1:nplace,legend=legend[1:nplace], cex=legfont, ncol=legcols)
        } else if(legend==FALSE){
        } else{
          legend(legpos,lty=1,col=1:nplace,legend=legend[1:nplace], cex=legfont, ncol=legcols)
        }

        if(title==TRUE){
          title<-paste("Annual trend")
          title(main=paste(title),cex.main=2)
        } else if(title==FALSE){
        } else{
          title(main=paste(title),cex.main=2)
        }
      }
      if(linear==T){
        abline(lm(aux.y~aux.x), col=i.place, lwd=3) #ajuste lineal
      } #end if
      if(low==T){
        lines(lowess(aux.y~aux.x, f=width), col=i.place, lwd=3)
      } #end if
    } #end for i.place
  }else{warming("monthly must be TRUE or FALSE")}
  #}else{warming("arrays data columns, month and year must have the same length")}

} #end function

#' @rdname annualtrend
#' @export
annualtrendquan<-function(data, datamonth, datayear, showmonths=c(1:12), monthly=FALSE, quan=0.5, limaxisy=c(0,40), title=TRUE, linear=T, low=F, width=0.2, legend=TRUE,legpos='topleft', legfont=0.7, legcols=3, english=TRUE){
  data<-data.frame(data)
  nplace<-dim(data)[2]
  nmonth<-length(showmonths)
  nquan<-length(quan)
  table<-data.frame()
  #if((dim(data)[1]==length(datamonth)) & (length(datamonth)==length(datayear))){
  par(mfrow=c(1,1))
  if(monthly==TRUE){
    if(nplace==1){
      for(i.month in 1:nmonth){
        aux.marcador <- (datamonth==showmonths[i.month])
        #aux.x <- tapply(datayear[aux.marcador], datayear[aux.marcador], mean, na.rm=T)
        aux.y <- data[aux.marcador,]/10
        if(i.month==1){
          if(english==TRUE){
            plot(datayear[aux.marcador], data[aux.marcador,] , xlab='Year',ylab="\u00B0C",type='n',ylim=limaxisy,cex.lab=1.5)
            namemonths<-c("January","February","March","April","May","June","July","August","September","October","November","December")
          }else if(english==FALSE){
            plot(datayear[aux.marcador], data[aux.marcador,] , xlab='Anio',ylab="\u00B0C",type='n',ylim=limaxisy,cex.lab=1.5)
            namemonths<-c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
          }else{warning("english must be TRUE or FALSE")}

          #if(legend==TRUE){
          #  legend(legpos,lty=1,col=1:nmonth,legend=namemonths[showmonths[1:nmonth]],cex=legfont, ncol=legcols)
          #} else if(legend==FALSE){
          #} else{
          #  legend(legpos,lty=1,col=1:nmonth,legend=legend[1:nmonth],cex=legfont, ncol=legcols)
          #}

          if(legend==TRUE){
            legend<-namemonths[showmonths]
            legend2<-paste(rep(legend,each=nquan),rep(quan,nmonth))
            legend(legpos,lty=rep(1:nquan,nmonth),col=rep(1:nmonth, each=nquan),legend=legend2, cex=legfont, ncol=legcols)
          } else if(legend==FALSE){
            legend<-namemonths[showmonths]
          } else{
            legend2<-paste(rep(legend,each=nquan),rep(quan,nmonth))
            legend(legpos,lty=rep(1:nquan,nmonth),col=rep(1:nmonth, each=nquan),legend=legend2, cex=legfont, ncol=legcols)
          }

          if(title==TRUE){
            title<-paste(names(data))
            title(main=paste(title),cex.main=2)
          } else if(title==FALSE){
          } else{
            title(main=paste(title),cex.main=2)
          }
        }
        if(linear==T){
          aux.rq<-rq(aux.y~datayear[aux.marcador],tau=quan)
          aux.matrix<-t(aux.rq$coefficients)
          rownames(aux.matrix)<-paste(legend[i.month],rownames(aux.matrix))
          table<-rbind(table,aux.matrix)
          if(nquan==1){
            abline(aux.rq$coefficients, col=i.month,lwd=3)
          }else{
            for(i.quan in 1:nquan){
              abline(aux.rq$coefficients[,i.quan], col=i.month,lwd=3, lty=i.quan)
            }
          }
        } #end if
        #if(low==T){
        #  lines(lowess(aux.y~aux.x, f=width), col=i.month,lwd=3)
        #} #end if
      } #end for i.month
    }else{warning("When monthly=TRUE data can't have more than ones column")}


  }else if(monthly==FALSE){
    for(i.place in 1:nplace){
      #aux.x <- tapply(datayear, datayear, mean, na.rm=T)
      aux.y <- data[,i.place]/10
      if(i.place==1){
        if(english==TRUE){
          plot(datayear, data[,i.place] , xlab='Year',ylab="\u00B0C",type='n',ylim=limaxisy,cex.lab=1.5)
          #title(main=paste(title),cex.main=2)
        }else if(english==FALSE){
          plot(datayear, data[,i.place] , xlab='Anio',ylab="\u00B0C",type='n',ylim=limaxisy,cex.lab=1.5)
          #title(main=paste(title),cex.main=2)
        }else{warning("english must be TRUE or FALSE")}

        if(legend==TRUE){
          legend<-names(data)
          legend2<-paste(rep(legend,each=nquan),rep(quan,nplace))
          legend(legpos,lty=rep(1:nquan,nplace),col=rep(1:nplace, each=nquan),legend=legend2, cex=legfont, ncol=legcols)
        } else if(legend==FALSE){
          legend<-names(data)
        } else{
          legend2<-paste(rep(legend,each=nquan),rep(quan,nplace))
          legend(legpos,lty=rep(1:nquan,nplace),col=rep(1:nplace, each=nquan),legend=legend2, cex=legfont, ncol=legcols)
        }

        if(title==TRUE){
          title<-paste("Annual trend")
          title(main=paste(title),cex.main=2)
        } else if(title==FALSE){
        } else{
          title(main=paste(title),cex.main=2)
        }
      }
      if(linear==T){
        aux.rq<-rq(aux.y~datayear,tau=quan)
        aux.matrix<-t(aux.rq$coefficients)
        rownames(aux.matrix)<-paste(legend[i.place],rownames(aux.matrix))
        table<-rbind(table,aux.matrix)
        if(nquan==1){
          abline(aux.rq$coefficients, col=i.place, lwd=3) #ajuste lineal
        }else{
          for(i.quan in 1:nquan){
            abline(aux.rq$coefficients[,i.quan], col=i.place, lwd=3, lty=i.quan) #ajuste lineal
          }
        }
      } #end if
      #if(low==T){
      #  lines(lowess(aux.y~aux.x, f=width), col=i.place, lwd=3)
      #} #end if
    } #end for i.place
  }else{warming("monthly must be TRUE or FALSE")}
  #}else{warming("arrays data columns, month and year must have the same length")}
  return(table)
} #end function

