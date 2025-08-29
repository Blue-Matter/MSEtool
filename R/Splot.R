
#' A quantile plot
#'
#' @description Plots quantiles and simulations for a stochastic time-series variable
#' @param datmat Matrix of real values with dimensions (simulation, year) (e.g. SB/SBMSY)
#' @param xvals Vector of numerical values of length ncol(datmat). The xaxis labels for datmat. 
#' @param p Vector of quantiles five positions long. Defaults to c(0.05,0.25,0.5,0.75,0.95) so the 90% and 50% intervals with the median plotted  in white. 
#' @param tcol Color of shaded regions (transparent)
#' @param ylim Numerical vector of length 2, lower and upper limits for the yaxis
#' @param sims Vector of positive integers, the individual simulations to plot
#' @param refline Positive real number, a reference line to plot (on scale of y axis)
#' @param dox Logical, should the x axis labels be plotted. 
#' @param doy Logical, should the y axis labels be plotted. 
#' @author T. Carruthers
#' @export
quantile_plot = function(datmat, xvals, p = c(0.05,0.25,0.5,0.75,0.95),  tcol, ylim, sims=1:3, refline=NA, dox=F, doy=F){
  doborder = function(){for(i in 1:4){axis(i,c(-1E10,1E10),c(-1E10,1E10))}}
  qs = apply(datmat, 2,quantile,p=p)
  plot(range(xvals),range(qs),col="white",ylim=ylim, yaxs ="i",xlab="",ylab="",main="", axes=F)
  doborder(); 
  if(dox){axis(1,las=2)}; if(doy){axis(2)}
  grid(col="grey")
  abline(h=refline,lty=1)
  polygon(c(xvals,rev(xvals)), c(qs[1,],rev(qs[5,])), border=NA, col=tcol)
  polygon(c(xvals,rev(xvals)), c(qs[2,],rev(qs[4,])), border=NA, col=tcol)
  lines(xvals,qs[3,],lwd=2,col="white")
  matplot(xvals,t(datmat[sims,]),type="l",col="darkgrey",lty = 1:length(sims),add=T)
}

#' Standard MSE projection plot
#'
#' @description Plots projections of F/FMSY, SB/SBMSY and Yield
#' @param MSEobj Object of class 'MSE' from runMSE() or Project()
#' @param MPs Either a positive integer (the first MPs number of MPs to plot), a character vector (the names of the MPs to plot), or an integer vector (the index of the MPs to plot)
#' @param p Vector of quantiles five positions long. Defaults to c(0.05,0.25,0.5,0.75,0.95) so the 90% and 50% intervals with the median plotted  in white. 
#' @author T. Carruthers
#' @export
Splot = function(MSEobj, MPs = 5,
                 p = c(0.05,0.25,0.5,0.75,0.95)){
  
  if(methods::is(MPs[1],"character")){
    MPind = MSEobj@MPs %in% MPs
  }else if(length(MPs)==1){
    MPind = 1:min(MPs,MSEobj@nMPs)
  }else{
    MPind = MPs
  }
  
  doborder = function(){for(i in 1:4){axis(i,c(-1E10,1E10),c(-1E10,1E10))}}
  subMSE = Sub(MSEobj,MPs = MSEobj@MPs[MPind])
  cols=c("red","green","blue","orange","purple")
  tcols = makeTransparent(cols,60)
  nMPs = subMSE@nMPs
  vars = c("F_FMSY","SB_SBMSY","Catch")
  refs = c(1,1,mean(MSEobj@PPD[[1]]@Cat[,MSEobj@nyears]))
  leglabs = c("P(F>FMSY)","P(B>BMSY)","P(Catch>current)")
  qlabs = c("F/FMSY","B/BMSY","Yield")
  nv = length(vars)
  yrs = subMSE@OM$CurrentYr[1]+1:subMSE@proyears
  
  par(mfrow=c(nv,nMPs+1),mai=c(0.05,0.05,0.02,0.02),omi=c(0.7,0.5,0.35,0.02))
    
  for(vv in 1:nv){
    vnam = vars[vv]
    vval = slot(subMSE, vnam)
    mus = apply(vval,2:3,mean)
    qs = apply(vval,2:3,quantile,p=p)
    ymax = quantile(qs,0.98)
    
    matplot(yrs,t(mus),col="white",xlab="",ylab="",main="",ylim=c(0,ymax),yaxs ="i",axes=F)
    grid(col="grey"); abline(h=refs[vv],lty=1); doborder()
    axis(2); if(vv==nv)axis(1,las=2)
    matplot(yrs,t(mus),col=cols, type="l",lty=1,lwd=2,add=T)
    mtext(qlabs[vv],2,line=2.6)
    Ps = apply(vval,2,function(x){mean(x>refs[vv])})
    legend('topright',paste0(round(Ps*100,2),"%"),text.col=cols,title=leglabs[vv],title.col ="black",bty="n")
    if(vv==1) mtext("Mean values",line=0.4)
    
    for(mp in 1:nMPs){   
      quantile_plot(datmat = vval[, mp, ], xvals = yrs, p =p, tcol = tcols[mp], 
                    ylim=c(0,ymax),refline = refs[vv], dox = vv==nv)
      if(vv ==1) mtext(subMSE@MPs[mp],line=0.4,col=cols[mp])
    }
  }

  mtext("Projection Year",1,line=3.2, outer=T)
  
}