#' @name quantdistr
#' @rdname quantdistr
#' @title Quantile distribution from samples
#' @description Given a matrix of samples for every day (like the one obtained by the function samplerstan of this library), we obtain enough samples of the desired conditional or marginal quantiles of each day's distribution to represent distributions of those quantiles.
#' @param samplematrix Matrix of samples for each day obtained by the samplerstan function.
#' @param dates Year and day of the year data of, at least, all the days needed to obtain the quantiles desired.
#' @param choosedates Year and day of the year data of the days that we want to obtain the quantiles from.
#' @param quant Array of the quantiles that we want to obtain.
#' @param w.days For the marginal quantiles, a window of "close or similar" days data is considered to avoid the previous day dependence. Then, to obtain the marginal quantiles of each day, we consider the data of that day plus the data of the w.days next days and the w.days previous days.
#' @param w.years Similar to the use of w.days, besides the window of 2*w.days+1 days, the marginal quantiles will also be obtained using the data of the respective days window on the next w.years years and the previous w.years years.
#' @param randomsamples An integer that says how many new random samples have been obtained from each extracted sample in the samplerstan function.
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
#' samples<-samplerstan(aux.X,aux.X.sigma2,modelos.rstan.jul[[places[13]]],extractedsamples=200,randomsamples=200)
#'
#' quantilematrix<-quantcondicional(samples,0.95)
#' marginquantilematrix<-quantmarginal(samples,auxiliar[-c(1,2),c(1,4)],0.95,randomsamples=200)
#' empiricquantilevector<-quantmarginal(auxiliar[places[13]],auxiliar[-c(1,2),c(1,4)],0.95,randomsamples=1)
#'
#' #Test de distribución de quantiles
#' aux.day1<-sum(marginquantilematrix[1,]>empiricquantilevector[1,])/length(marginquantilematrix[1,])
#' for(i in 1:length(marginquantilematrix[,1])){
#'  print(sum(marginquantilematrix[i,]>empiricquantilevector[i,])/length(marginquantilematrix[i,]))
#' }
#'
#'
#' #Pallaruelo (most complex model)
#' modelo.lm<- ajuste.parada(places[15], auxiliar)
#' aux.X <- model.matrix(modelo.lm[[modelo.lm$mejor_modelo]])[-1,]
#' aux.X.sigma2 <- model.matrix(modelo.lm[[modelo.lm$mejor_modelo_var]])[-1,]
#'
#' samples<-samplerstan(aux.X,aux.X.sigma2,modelos.rstan.jul[[places[15]]])
#'
#' quantilematrix<-quantcondicional(samples,0.95)
#'
#' #Yesa (simplest model)
#' modelo.lm<- ajuste.parada(places[18], auxiliar)
#' aux.X <- model.matrix(modelo.lm[[modelo.lm$mejor_modelo]])[-1,]
#' aux.X.sigma2 <- model.matrix(modelo.lm[[modelo.lm$mejor_modelo_var]])[-1,]
#'
#' samples<-samplerstan(aux.X,aux.X.sigma2,modelos.rstan.jul[[places[18]]])
#'
#' quantilematrix<-quantcondicional(samples,0.95)
#'

NULL

#' @rdname quantdistr
#' @export
quantcondicional<-function(samplematrix,dates,choosedates=dates,quant=0.5,randomsamples=10){

  samplesperday<-dim(samplematrix)[2]
  extractedsamples<-samplesperday/randomsamples
  #totaldays<-dim(samplematrix)[1]

  ini<-which((dates$year==(dates$year[1]+w.years))&(dates$day.year==dates$day.year[1]))+w.days
  fin<-which((dates$year==(dates$year[length(dates$year)]-w.years))&(dates$day.year==dates$day.year[length(dates$day.year)]))-w.days
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

  nquants<-length(quant)
  if(nquant==1){

    quantilematrix<-matrix(nrow=length(choosedatesindex),ncol=extractedsamples)
    for(i in 1:length(choosedatesindex)){
      fac <- factor(rep(1:extractedsamples,each=randomsamples), levels = 1:extractedsamples)
      quantilematrix[i,]<-tapply(samplematrix[choosedatesindex[i],],fac,FUN=quantile,quant)
    }

  }else if(nquant>=2){

    aux.quantilematrix<-matrix(nrow=length(choosedatesindex),ncol=extractedsamples)
    for(i in 1:length(choosedatesindex)){
      fac <- factor(rep(1:extractedsamples,each=randomsamples), levels = 1:extractedsamples)
      aux.quantilematrix[i,]<-tapply(samplematrix[choosedatesindex[i],],fac,FUN=quantile,quant)
    }

    quantilematrix<-list()
    for(k in 1:nquant){
      aux.matrix<-matrix(nrow=length(choosedatesindex),ncol=extractedsamples)
      for(i in 1:length(choosedatesindex)){
        for(j in 1:extractedsamples){
          aux.matrix[i,j]<-aux.quantilematrix[[i,j]][[paste(quant[k])]]
        }
      }
      quantilematrix[[paste(quant[k])]]<-aux.matrix
    }

    # quantilematrix<-list()
    # for(k in 1:nquant){
    #
    #   quantilematrix[paste(quant[k])]<-matrix(nrow=totaldays,ncol=extractedsamples)
    #   for(i in 1:totaldays){
    #     fac <- factor(rep(1:extractedsamples,each=randomsamples), levels = 1:extractedsamples)
    #     quantilematrix[paste(quant[k])][i,]<-tapply(samplematrix[i,],fac,FUN=quantile,quant)
    #   }
    #
    # }

  }else{warning("quant must be a vector of 1 or more elements")}

  return(quantilematrix)
}

#' @rdname quantdistr
#' @export
quantmarginal<-function(samplematrix,dates,choosedates=dates,quant=0.5,w.days=7,w.years=2,randomsamples=10){

  samplesperday<-dim(samplematrix)[2]
  extractedsamples<-samplesperday/randomsamples
  #totaldays<-sum((dates$year>=dates$year[1]+w.years)&(dates$year<=dates$year[length(dates$year)]-w.years))-2*w.days

  ini<-which((dates$year==(dates$year[1]+w.years))&(dates$day.year==dates$day.year[1]))+w.days
  fin<-which((dates$year==(dates$year[length(dates$year)]-w.years))&(dates$day.year==dates$day.year[length(dates$day.year)]))-w.days
  if(((dates$year[1]+w.years)%%4==0)&(w.years%%4!=0)&(dates$day.year[1]>365-w.days)){
    # ini<-which((dates$year==(dates$year[1]+w.years))&(dates$day.year==(dates$day.year[1]+1)))+w.days
    ini<-which((dates$year==(dates$year[1]+w.years+1))&(dates$day.year==(dates$day.year[1]+w.days-365)))
  } else if(((dates$year[1]+w.years)%%4==0)&(w.years%%4!=0)&(dates$day.year[1]==365-w.days)){
    # ini<-which((dates$year==(dates$year[1]+w.years))&(dates$day.year==(dates$day.year[1]+1)))+w.days
    ini<-which((dates$year==(dates$year[1]+w.years))&(dates$day.year==365))
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
  if(((dates$year[length(dates$year)]-w.years-1)%%4==0)&(w.years%%4!=0)&(dates$day.year[length(dates$day.year)]< w.days)){
    # fin<-which((dates$year==(dates$year[length(dates$year)]-w.years))&(dates$day.year==(dates$day.year[length(dates$day.year)]-1)))-w.days
    fin<-which((dates$year==(dates$year[length(dates$year)]-w.years-1))&(dates$day.year==(dates$day.year[length(dates$day.year)]-w.days-1+366)))
    # if(dates$day.year[length(dates$day.year)]==1){
    #   fin<-which((dates$year==(dates$year[length(dates$year)]-w.years-1))&(dates$day.year==365))-w.days+1
    # }
  } else if(((dates$year[length(dates$year)]-w.years-1)%%4==0)&(w.years%%4!=0)&(dates$day.year[length(dates$day.year)]== w.days)){
    # fin<-which((dates$year==(dates$year[length(dates$year)]-w.years))&(dates$day.year==(dates$day.year[length(dates$day.year)]-1)))-w.days
    fin<-which((dates$year==(dates$year[length(dates$year)]-w.years-1))&(dates$day.year==(dates$day.year[length(dates$day.year)]-w.days+366)))
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
  if(fin<100) print(paste(ini,fin,length(ini:fin)))
  print(ini:fin)
  choosedatesindex<-maxvector[maxvector %in% which((dates$year %in% choosedates$year)&(dates$day.year %in% choosedates$day.year))]
  choosedates<-dates[choosedatesindex,]

  nquant<-length(quant)
  if(nquant==1){

    #quantilematrix<-matrix(nrow=totaldays,ncol=extractedsamples)
    #quantilematrix<-matrix(nrow=fin-ini+1,ncol=extractedsamples)
    quantilematrix<-NULL
    #for(i in ini:(ini+totaldays-1)){
    for(i in choosedatesindex){
      if(dates$day.year[i]==366){
        aux.index<-which((dates$year>=dates$year[i]-w.years)&(dates$year<=dates$year[i]+w.years)&(dates$day.year==365))
        for(j in 1:length(aux.index)){
          if(dates$year[aux.index[j]]%%4==0){
            aux.index[j]<-aux.index[j]+1
          }
        }
      }else{
        aux.index<-which((dates$year>=dates$year[i]-w.years)&(dates$year<=dates$year[i]+w.years)&(dates$day.year==dates$day.year[i]))
      }
      index<-NULL
      for(j in aux.index){
        index<-c(index,(j-w.days):(j+w.days))
      }
      aux.fac <- factor(rep(1:extractedsamples,each=randomsamples), levels = 1:extractedsamples)
      fac <- matrix(rep(aux.fac,length(index)),length(index),samplesperday,byrow=TRUE)
      aux<-tapply(samplematrix[index,],fac,FUN=quantile,quant)
      #quantilematrix[i,]<-aux
      quantilematrix<-rbind(quantilematrix,aux)

      #print(paste(i, i-ini+1,(i-ini+1)/totaldays,index[1],index[length(index)],sep=" "))
      print(paste(i-ini+1,"de",fin-ini+1,paste(round((i-ini+1)/(fin-ini+1),5)*100,"%",sep=""),i, "||",index[1],index[length(index)],sep=" "))
    }
    rownames(quantilematrix)<-mapply(paste, sep = "-", choosedates[1],choosedates[2])

    return(quantilematrix)

  }else if(nquant>=2){

    #quantilematrix<-matrix(nrow=totaldays,ncol=extractedsamples)
    #quantilematrix<-matrix(nrow=fin-ini+1,ncol=extractedsamples)
    aux.quantilematrix<-NULL
    #for(i in ini:(ini+totaldays-1)){
    for(i in choosedatesindex){
      if(dates$day.year[i]==366){
        aux.index<-which((dates$year>=dates$year[i]-w.years)&(dates$year<=dates$year[i]+w.years)&(dates$day.year==365))
        for(j in 1:length(aux.index)){
          if(dates$year[aux.index[j]]%%4==0){
            aux.index[j]<-aux.index[j]+1
          }
        }
      }else{
        aux.index<-which((dates$year>=dates$year[i]-w.years)&(dates$year<=dates$year[i]+w.years)&(dates$day.year==dates$day.year[i]))
      }
      index<-NULL
      for(j in aux.index){
        index<-c(index,(j-w.days):(j+w.days))
      }
      aux.fac <- factor(rep(1:extractedsamples,each=randomsamples), levels = 1:extractedsamples)
      fac <- matrix(rep(aux.fac,length(index)),length(index),samplesperday,byrow=TRUE)
      aux<-tapply(samplematrix[index,],fac,FUN=quantile,quant)
      #quantilematrix[i,]<-aux
      aux.quantilematrix<-rbind(aux.quantilematrix,aux)

      #print(paste(i, i-ini+1,(i-ini+1)/totaldays,index[1],index[length(index)],sep=" "))
      print(paste(i-ini+1,"de",fin-ini+1,paste(round((i-ini+1)/(fin-ini+1),5)*100,"%",sep=""),i, "||",index[1],index[length(index)],sep=" "))
    }

    quantilematrix<-list()
    for(k in 1:nquant){
      aux.matrix<-matrix(nrow=length(choosedatesindex),ncol=extractedsamples)
      for(i in 1:length(choosedatesindex)){
        for(j in 1:extractedsamples){
          aux.matrix[i,j]<-aux.quantilematrix[[i,j]][[k]]
        }
      }
      rownames(aux.matrix)<-mapply(paste, sep = "-", choosedates[1],choosedates[2])
      quantilematrix[[paste(quant[k])]]<-aux.matrix
    }

    # quantilematrix<-list()
    # for(k in 1:nquant){
    #
    #   #quantilematrix<-matrix(nrow=totaldays,ncol=extractedsamples)
    #   #quantilematrix<-matrix(nrow=fin-ini+1,ncol=extractedsamples)
    #   quantilematrix[[paste(quant[k])]]<-NULL
    #   #for(i in ini:(ini+totaldays-1)){
    #   for(i in choosedatesindex){
    #     aux.index<-which((dates$year>=dates$year[i]-w.years)&(dates$year<=dates$year[i]+w.years)&(dates$day.year==dates$day.year[i]))
    #     index<-NULL
    #     for(j in aux.index){
    #       index<-c(index,(j-w.days):(j+w.days))
    #     }
    #     aux.fac <- factor(rep(1:extractedsamples,each=randomsamples), levels = 1:extractedsamples)
    #     fac <- matrix(rep(aux.fac,length(index)),length(index),samplesperday,byrow=TRUE)
    #     aux<-tapply(samplematrix[index,],fac,FUN=quantile,quant[k])
    #     #quantilematrix[i,]<-aux
    #     quantilematrix[[paste(quant[k])]]<-rbind(quantilematrix[[paste(quant[k])]],aux)
    #
    #     #print(paste(i, i-ini+1,(i-ini+1)/totaldays,index[1],index[length(index)],sep=" "))
    #     print(paste("quantile",quant[k],":",i-ini+1,"de",fin-ini+1,paste(round((i-ini+1)/(fin-ini+1),5)*100,"%",sep=""),i, "||",index[1],index[length(index)],sep=" "))
    #   }
    #   rownames(quantilematrix[[paste(quant[k])]])<-mapply(paste, sep = "-", choosedates[1],choosedates[2])
    #
    # }

    return(quantilematrix)

  }else{warning("quant must be a vector of 1 or more elements")}
}

# quantmarginal<-function(samplematrix,dates,quant=0.5,w.days=7,w.years=2,randomsamples=10){
#
#   samplesperday<-dim(samplematrix)[2]
#   extractedsamples<-samplesperday/randomsamples
#   #totaldays<-sum((dates$year>=dates$year[1]+w.years)&(dates$year<=dates$year[length(dates$year)]-w.years))-2*w.days
#
#   ini<-which((dates$year==(dates$year[1]+w.years))&(dates$day.year==(dates$day.year[1]+w.days)))
#   fin<-which((dates$year==(dates$year[length(dates$year)]-w.years))&(dates$day.year==(dates$day.year[length(dates$day.year)]-w.days)))
#
#   #quantilematrix<-matrix(nrow=totaldays,ncol=extractedsamples)
#   #quantilematrix<-matrix(nrow=fin-ini+1,ncol=extractedsamples)
#   quantilematrix<-NULL
#   #for(i in ini:(ini+totaldays-1)){
#   for(i in ini:fin){
#     aux.index<-which((dates$year>=dates$year[i]-w.years)&(dates$year<=dates$year[i]+w.years)&(dates$day.year==dates$day.year[i]))
#     index<-NULL
#     for(j in aux.index){
#       index<-c(index,(j-w.days):(j+w.days))
#     }
#     aux.fac <- factor(rep(1:extractedsamples,each=randomsamples), levels = 1:extractedsamples)
#     fac <- matrix(rep(aux.fac,length(index)),length(index),samplesperday,byrow=TRUE)
#     aux<-tapply(samplematrix[index,],fac,FUN=quantile,quant)
#     #quantilematrix[i,]<-aux
#     quantilematrix<-rbind(quantilematrix,aux)
#
#     #print(paste(i, i-ini+1,(i-ini+1)/totaldays,index[1],index[length(index)],sep=" "))
#     print(paste(i-ini+1,"de",fin-ini+1,paste(round((i-ini+1)/(fin-ini+1),5)*100,"%",sep=""),i, "||",index[1],index[length(index)],sep=" "))
#   }
#
#   return(quantilematrix)
# }
