#' A fairly tidy time-series quantile plot
#'
#' @param x Matrix. A time series quantity `[simulation, year]`
#' @param p Numeric vector. The percentiles that are plotted (LB2,LB1,UB1,UB2). LB2 and UB2 are the outer percentiles, LB1 and UB1 are the inner percentiles.
#' @param yrs Numeric vector. The years corresponding to the indexing of x
#' @param qcol Character, color. The color of the inner percentile range
#' @param lcol Character, color. The color of the outer percentile range.
#' @param addline Logical. Should two individual simulations be added to the percentile plots?
#' @param ablines Numeric vector. Horizontal lines to be added to the plot.
#' @author T.Carruthers
#' @export
plotquant<-function(x,p=c(0.05,0.25,0.75,0.95),yrs,qcol,lcol,addline=T,ablines=NA){
  ny<-length(yrs)
  qs<-apply(x,2,quantile,p=p[c(1,4)],na.rm=T)
  qsi<-apply(x,2,quantile,p=p[2:3],na.rm=T)
  if (all(qs[1,]==qs[2,])) {
    lines(yrs, qs[1,], lwd=2, col=qcol)
  } else {
    polygon(c(yrs,yrs[ny:1]),c(qs[1,],qs[2,ny:1]),border=NA,col='#b3ecff')
    polygon(c(yrs,yrs[ny:1]),c(qsi[1,],qsi[2,ny:1]),border=NA,col=qcol)
    lines(yrs,apply(x,2,quantile,p=0.5,na.rm=T),lwd=2,col="white")
  }

  if(!is.na(ablines[1]))abline(h=ablines,col='#99999980')

  if(addline)for(i in 1:2)lines(yrs,x[i,],col=lcol,lty=i)
  
}


# ProjectPlot <- function(MMSE, Y=c('SB', 'F', 'Y', 'Y.fleet'), 
#                                   maxcol = 6, 
#                                   qcol = rgb(0.4, 0.8, 0.95), 
#                                   lcol = "dodgerblue4",
#                                   quants = c(0.05, 0.25, 0.75, 0.95), 
#                                   curyr = 2018, addline = FALSE) {
#   
#   if(is.na(maxcol))maxcol=ceiling(length(MMSE@MPs)/0.5) # defaults to portrait 1:2
#   MPs<-MMSE@MPs
#   MPrefs<-MMSE@MPrefs
#   nMPs<-length(MPrefs[,1,1])
#   yrs<-curyr+(1:MMSE@proyears)
#   ns<-MMSE@nstocks
#   nf<-MMSE@nfleets
#   
#   Y <- match.arg(Y, choices=c('SB', 'F', 'Y', 'Y.fleet'))
#   
#   plots<-split(1:nMPs, ceiling(seq_along(1:nMPs)/maxcol))
#   
#   
#   
# }




#' Standard plot for an object of class MMSE (multi MSE)
#'
#' @description Plot the projected biomass, fishing, mortality rate and yield for all stocks and MPs
#'
#' @param x Object of class \linkS4class{MMSE}. A Multi-OM object created by \code{multiMSE(MOM, ...)}
#' @param maxcol Integer. The maximum number of columns (MPs) to be plotted in each plot
#' @param qcol Character, color. The color of the inner percentile range
#' @param lcol Character, color. The color of the outer percentile range.
#' @param quants Numeric vector. The percentiles that are plotted (LB2,LB1,UB1,UB2). 
#' LB2 and UB2 are the outer percentiles, LB1 and UB1 are the inner percentiles.
#' @param curyr Integer. The current year from which projections start.
#' @param addline Logical. Should two individual simulations be added to the percentile plots?
#' @param ... Not used
#' @author T.Carruthers
#' @method plot MMSE
#' @importFrom grDevices rgb
#' @export
plot.MMSE <- function(x=NULL, maxcol = 6, qcol = rgb(0.4, 0.8, 0.95), lcol = "dodgerblue4",
                      quants = c(0.05, 0.25, 0.75, 0.95), curyr = 2018, addline = FALSE, ...) {
  MMSE <- x
  if (class(MMSE)!='MMSE') stop('Object must be class `MMSE`')
  if(is.na(maxcol))maxcol=ceiling(length(MMSE@MPs)/0.5) # defaults to portrait 1:2
  MPs<-MMSE@MPs
  MPrefs<-MMSE@MPrefs
  nMPs<-length(MPrefs[,1,1])
  yrs<-curyr+(1:MMSE@proyears)
  ns<-MMSE@nstocks
  nf<-MMSE@nfleets
  
  plots<-split(1:nMPs, ceiling(seq_along(1:nMPs)/maxcol))
  
  # --- Biomass projection ---------------------------------------------------
  B_BMSY<-MMSE@SB_SBMSY
  Blims <- c(0,quantile(B_BMSY,0.95))
  
  for(pp in 1:length(plots)){
    
    toplot<-plots[[pp]]
    nt<-length(toplot)
    par(mfcol=c(ns,nt),mai=c(0.3,0.3,0.3,0.01),omi=c(0.4,0.5,0.05,0.05))
    
    for(MP in toplot){
      
      for(ss in 1:ns){
        plot(range(yrs),Blims,col="white",yaxs="i")
        plotquant(B_BMSY[,ss,MP,],p=quants,yrs,qcol,lcol,ablines=c(0.5,1),addline=addline)
        mtext(paste(paste0("F",1:nf),MPrefs[MP,,ss],collapse=", "),3,line=0.2,font=2,cex=0.7)
        
        if(MP==toplot[1])mtext(MMSE@Snames[ss],2,line=2.5)
      }
    }
  }
  
  mtext("Projection Year",1,font=2,outer=T,line=1.2)
  mtext("Biomass / BMSY",2,font=2,outer=T,line=1.9)
  
  
  # --- F projection -----------------------------------------------------------
  
  F_FMSY<-MMSE@F_FMSY
  F_FMSYsum<-apply(F_FMSY,c(1,2,4,5),sum,na.rm=T)
  Flims<- c(0,quantile(F_FMSYsum,0.95,na.rm=T))
  
  for(pp in 1:length(plots)){
    
    toplot<-plots[[pp]]
    nt<-length(toplot)
    par(mfcol=c(ns,nt),mai=c(0.3,0.3,0.3,0.01),omi=c(0.4,0.5,0.05,0.05))
    
    for(MP in toplot){
      
      for(ss in 1:ns){
        plot(range(yrs),Flims,col="white",yaxs="i")
        plotquant(F_FMSYsum[,ss,MP,],p=quants,yrs,qcol,lcol,ablines=c(0.5,1),addline=addline)
        mtext(paste(paste0("F",1:nf),MPrefs[MP,,ss],collapse=", "),3,line=0.2,font=2,cex=0.7)
        
        if(MP==toplot[1])mtext(MMSE@Snames[ss],2,line=2.5)
      }
    }
  }
  
  mtext("Projection Year",1,font=2,outer=T,line=1.2)
  mtext("Total (all fleets) F / FMSY",2,font=2,outer=T,line=1.9)
  
  
  # --- Total yield projection -----------------------------------------------------
  
  Yd<-MMSE@Catch #MMSE@OM$RefY
  Ydsum<-apply(Yd, c(1,2,4,5),sum,na.rm=T)
  Ydsum<-Ydsum/array(rep(Ydsum[,,,1],MMSE@proyears),dim(Ydsum))
  Ylims<- c(0,quantile(Ydsum,0.95,na.rm=T))
  
  for(pp in 1:length(plots)){
    
    toplot<-plots[[pp]]
    nt<-length(toplot)
    par(mfcol=c(ns,nt),mai=c(0.3,0.3,0.3,0.01),omi=c(0.4,0.5,0.05,0.05))
    
    for(MP in toplot){
      
      for(ss in 1:ns){
        
        plot(range(yrs),Ylims,col="white",yaxs="i")
        plotquant(Ydsum[,ss,MP,],p=quants,yrs,qcol,lcol,ablines=c(0.5,1),addline=addline)
        mtext(paste(paste0("F",1:nf),MPrefs[MP,,ss],collapse=", "),3,line=0.2,font=2,cex=0.7)
        
        if(MP==toplot[1])mtext(MMSE@Snames[ss],2,line=2.5)
      }
    }
  }
  
  mtext("Projection Year",1,font=2,outer=T,line=1.2)
  mtext(paste("Total yield (all fleets) relative to",curyr),2,font=2,outer=T,line=1.9)
  
  
  # --- Yield by fleet projection --------------------------------------------------
  
  YdbyF<-apply(Yd, c(2,3,4,5),mean,na.rm=T)
  cols<-rep(c('black','red','green','blue','orange','grey','purple'),3)
  ltys<-rep(1:3,each=7)
  
  
  for(pp in 1:length(plots)){
    
    toplot<-plots[[pp]]
    nt<-length(toplot)
    par(mfcol=c(ns,nt),mai=c(0.3,0.3,0.3,0.01),omi=c(0.4,0.5,0.05,0.05))
    
    for(MP in toplot){
      for(ss in 1:ns){
        matplot(yrs,t(matrix(YdbyF[ss,,MP,],nrow=nf)),type='l',col=cols,lty=ltys,yaxs="i",ylim=c(0,max(YdbyF[ss,,MP,])))
        mtext(paste(paste0("F",1:nf),MPrefs[MP,,ss],collapse=", "),3,line=0.2,font=2,cex=0.7)
        if(MP==toplot[1])mtext(MMSE@Snames[ss],2,line=2.5)
        if(MP==toplot[1])legend("bottomleft",legend=paste(paste0("F",1:nf),MMSE@Fnames[,ss]),text.col=cols,cex=0.8,text.font=2,bty='n')
      }
    }
  }
  
  mtext("Projection Year",1,font=2,outer=T,line=1.2)
  mtext("Expected yield by fleet",2,font=2,outer=T,line=1.9)
  
  
}


#' A basic comparison of runMSE output (MSE) and multiMSE (MMSE)
#'
#' @param MSEsingle An object of class MSE arising from a run of runMSE(OM, ...)
#' @param MSEmulti An object of class MMSE arising from a run of multiMSE(MOM, ...)
#' @param p Integer. The stock number from the MSEmulti object (to be plotted)
#' @param f Integer. The fleet number from the MSEmulti object (to be plotted)
#' @param MPno Integer. The MP number from the MSEmulti and MSEsingle object (to be plotted)
#' @param maxsims Integer. The maximum number of simulations to plot.
#' @author T.Carruthers
#' @export
multidebug<-function(MSEsingle,MSEmulti,p=1,f=1,MPno=1,maxsims=4){

  MPno=1
  p=1
  f=1
  par(mfrow=c(2,2),mar=rep(2,4))

  SSBhs<-apply(slot(MSEsingle,"SSB_hist"),c(1,3),sum)
  SSBhm<-apply(slot(MSEmulti,"SSB_hist")[,p,,,],c(1,3),sum)
  matplot(t(SSBhs),type='l',main="SSBhist: runMSE")
  matplot(t(SSBhm),type='l',main="SSBhist: multiMSE")

  SSBs<-slot(MSEsingle,"SSB")
  SSBm<-slot(MSEmulti,"SSB")
  matplot(t(SSBs[,MPno,]),type='l',main="SSB: runMSE")
  matplot(t(SSBm[,p,MPno,]),type='l',main="SSB: multiMSE")

}


#' A basic SSB plot for debugging runMSE output
#'
#' @param MSEmulti An object of class MMSE arising from a run of multiMSE(MOM, ...)
#' @param maxsim Integer. The number of simulations to plot
#' @author T.Carruthers
#' @export
plotmulti<-function(MSEmulti,maxsim=8){

  np<-MSEmulti@nstocks
  nMPs<-MSEmulti@nMPs
  MPs<-MSEmulti@MPs
  Snames<-MSEmulti@Snames
  par(mfrow=c(np,nMPs),mai=c(0.7,0.7,0.1,0.1),omi=c(0.2,0.5,0.3,0.1))

  MSEmulti@MPrefs

  SSBm<-slot(MSEmulti,"SSB")
  nsim<-dim(SSBm)[1]
  maxsim<-min(nsim,maxsim)

  for(p in 1:np){
    for(m in 1:nMPs){
      matplot(t(SSBm[1:maxsim,p,m,]),type='l',main="",xlab="",ylab="")
      if(p==1)mtext(MPs[[p]][m],2,line=2.5,font=2)
      if(m==1)mtext(Snames[p],font=2,line=0.8)
    }
  }

  mtext("Projection Year",1,outer=T)

}

