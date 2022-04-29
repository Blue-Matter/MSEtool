# =======================================================================
# === MOM plotting functions ============================================
# =======================================================================


#' @name plot.MOM
#' @title Standard plot for an object of class MOM
#'
#' @description Plot the stocks, fleets, catch fractions and relationships in multi operating model object
#'
#' @param x Object of class \linkS4class{MOM}. A Multi-OM object created by \code{new('MOM', ...)}
#' @param silent Logical. Do you wish to see print outs / warnings?
#' @param maxsims Integer. What are the maximum number of individual simulations you wish to plot?
#' @author T.Carruthers
#' @aliases plot,MOM,missing-method
#' @export
setMethod("plot", signature(x = "MOM", y = "missing"),
          function(x, silent = TRUE, maxsims = 6) {
            MOM <- x
            Rel<-MOM@Rel
            Stocks<-MOM@Stocks
            Fleets<-MOM@Fleets
            Obs<-MOM@Obs
            Imps<-MOM@Imps
            cpars<-MOM@cpars
            nsim<-MOM@nsim
            maxsims<-min(MOM@nsim,maxsims)
            nsim<-200 # for plotting purposes
            nyears <- MOM@Fleets[[1]][[1]]@nyears  # number of historical years
            proyears<-MOM@proyears
            allyears<-nyears+proyears
            
            np<-length(MOM@Stocks)
            nf<-length(Fleets[[1]])
            Snams<-SIL(MOM@Stocks,"Name")
            Fnames<-t(matrix(SIL(MOM@Fleets,"Name"),nrow=nf))
            
            if (inherits(cpars, 'list') & length(cpars)==0) {
              # add empty list for each stock and fleet
              for (p in 1:np) {
                cpars[[p]] <- list()
                for (f in 1:nf) {
                  cpars[[p]][[f]] <- list()
                }
              }
            }
            
            # --- Plot MICE Rels --------------------------------------------------------------------------------------------
            
            if(length(Rel)>0){
              
              # Set up multi-panel plot Rel x independent variable
              nRels<-length(Rel)
              nIVs<-sapply(1:length(Rel),getnIVs,Rel)
              op <- par(mfrow = c(nRels, max(nIVs)), mai = c(0.6, 0.6, 0.15, 0.05), omi = c(0.6, 0.6, 0.3, 0.05), no.readonly=TRUE)
              
              for(Relno in 1:nRels)  plotRel(Stocks,Rel,Relno,Snams,leg=(Relno==1),extras=max(nIVs)-nIVs[Relno])
              
              mtext("Independent variable(s)",1,line=1.5,outer=T,font=2)
              mtext("Dependent variables",2,line=1.5,outer=T,font=2)
              mtext("Inter-stock MICE relationships",3,line=0.3,outer=T,font=2)
              
            }
            
            
            
            # --- Sample everything -----------------------------------------------------------------------------------------
            
            SampCpars<-list() # empty list
            # custom parameters exist - sample and write to list
            
            for(p in 1:np){
              SampCpars[[p]]<-list()
              for(f in 1:nf){
                
                if(length(cpars[[p]][[f]])>0){
                  message(paste(Stocks[[p]]@Name," - ",Fleets[[p]][[f]]@Name))
                  ncparsim<-cparscheck(cpars[[p]][[f]])   # check each list object has the same length and if not stop and error report
                  SampCpars[[p]][[f]] <- SampleCpars(cpars[[p]][[f]], nsim, msg=!silent)
                }else{
                  SampCpars[[p]][[f]] <-list()
                }                
                

              }
            }
            
            # All stocks and sampled parameters must have compatible array sizes (maxage)
            maxage_s<-unique(SIL(MOM@Stocks,"maxage"))
            if(length(maxage_s)>1 & !silent)message(paste("Stocks of varying maximum ages have been specified, all simulations will run to",max(maxage_s),"ages"))
            maxage<-max(maxage_s)
            for(p in 1:np)MOM@Stocks[[p]]@maxage<-maxage
            
            # --- Sample Stock Parameters ----
            StockPars<-FleetPars<-ObsPars<-ImpPars<-new('list')
            for(p in 1:np){
              StockPars[[p]] <- SampleStockPars(MOM@Stocks[[p]], nsim, nyears, proyears,  SampCpars[[p]][[1]], msg=!silent)
            }
            
            # --- Sample Fleet Parameters ----
            for(p in 1:np){
              FleetPars[[p]]<-ObsPars[[p]]<-ImpPars[[p]]<-list()
              for(f in 1:nf){
                FleetPars[[p]][[f]] <- SampleFleetPars(MOM@Fleets[[p]][[f]], Stock=StockPars[[p]], nsim, nyears, proyears, cpars=SampCpars[[p]][[f]])
                #FleetPars[[p]][[f]]$Find<-FleetPars[[p]][[f]]$Find/(apply(FleetPars[[p]][[f]]$Find,1,mean))
              }
            }
            for(p in 1:np)for(f in 1:nf)ObsPars[[p]][[f]] <- SampleObsPars(MOM@Obs[[p]][[f]], nsim, cpars=SampCpars[[p]][[f]])
            for(p in 1:np)for(f in 1:nf) ImpPars[[p]][[f]] <- SampleImpPars(MOM@Imps[[p]][[f]],nsim, cpars=SampCpars[[p]][[f]])
            
            
            
            # --- Plot apical F by fleet and stock ---------------------------------------------------------------------------------------------
            
            op <- par(mfrow = c(np, nf), mai = c(0.25, 0.35, 0.35, 0.05), omi = c(0.6, 0.6, 0.3, 0.05), no.readonly=TRUE)
            
            for(p in 1:np){
              for(f in 1:nf){
                
                matplot(t(FleetPars[[p]][[f]]$Find[1:maxsims,]),type='l',xlab="",ylab="")
                mtext(Fnames[p,f],3,line=0.5)
                if(f==1)mtext(Snams[p],2,line=2.5)
              }
            }
            
            mtext("Historical year",1,line=1.2,outer=T,font=2)
            mtext("Apical fishing mortality rate",2,line=1.5, outer=T,font=2)
            mtext("Pattern of historial exploitation by fleet",3,line=0.7, outer=T,font=2)
            
            
            # --- Plot current vulnerability by fleet and stock ---------------------------------------------------------------------------------------------
            
            op <- par(mfrow = c(np, nf), mai = c(0.25, 0.35, 0.35, 0.05), omi = c(0.6, 0.6, 0.3, 0.05), no.readonly=TRUE)
            
            for(p in 1:np){
              for(f in 1:nf){
                
                matplot(t(FleetPars[[p]][[f]]$V[1:maxsims,,nyears]),type='l',xlab="",ylab="")
                mtext(Fnames[p,f],3,line=0.5)
                if(f==1)mtext(Snams[p],2,line=2.5)
              }
            }
            
            mtext("Age",1,line=1.2,outer=T,font=2)
            mtext("Selectivity at age",2,line=1.5, outer=T,font=2)
            mtext("Fleet age-selectivity in most recent historical year ",3,line=0.7, outer=T,font=2)
            
            
            
            # --- Plot stock attributes F by fleet and stock ---------------------------------------------------------------------------------------------
            
            op <- par(mfcol = c(np, 5), mai = c(0.45, 0.55, 0.35, 0.05), omi = c(0.05, 0.4, 0.3, 0.05), no.readonly=TRUE)
            on.exit(par(op))
            
            # Growth ----------
            for(p in 1:np){
              matplot(t(StockPars[[p]]$Len_age[1:maxsims,,nyears]),type='l',xlab="",ylab="",yaxs='i',ylim=c(0,max(StockPars[[p]]$Len_age[1:maxsims,,nyears])))
              mtext("Age",1,line=2.2)
              mtext("Length",2,line=2.2)
              if(p==1)mtext("Growth",3,line=1,font=2)
              mtext(Snams[p],2,line=4.5,font=2)
            }
            
            # maturity ----------------
            for(p in 1:np){
              matplot(t(StockPars[[p]]$Mat_age[1:maxsims,,nyears]),type='l',xlab="",ylab="",ylim=c(0,1.05),yaxs='i')
              mtext("Age",1,line=2.2)
              mtext("Spawning Fraction",2,line=2.2)
              if(p==1)mtext("Maturity",3,line=1,font=2)
            }
            
            # natural mortality -------
            for(p in 1:np){
              matplot(t(StockPars[[p]]$M_ageArray[1:maxsims,,nyears]),type='l',xlab="",ylab="",ylim=c(0,1.1*max(StockPars[[p]]$M_ageArray[1:maxsims,,nyears])),yaxs='i')
              mtext("Age",1,line=2.2)
              mtext("Natural mortality rate",2,line=2.2)
              if(p==1)mtext("Natural mortality",3,line=1,font=2)
            }
            
            # S-R ---------------------
            
            for(p in 1:np){
              hist(StockPars[[p]]$hs,col='grey',xlab="",ylab="",main="")
              mtext("Steepness",1,line=2.2)
              mtext("Freq.",2,line=2.2)
              if(p==1)mtext("Recruitment compensation",3,line=1,font=2)
            }
            
            
            # Depletion ---------------
            for(p in 1:np){
              hist(StockPars[[p]]$D,col='grey',xlab="",ylab="",main="")
              mtext("SSB(now) / SSB(unfished)",1,line=2.2)
              mtext("Freq.",2,line=2.2)
              if(p==1)mtext("Stock depletion",3,line=1,font=2)
            }
            
            mtext("Stock parameters",3,line=0.7, outer=T,font=2)
            
            
            # --- Catch frac
            
            
            # --- Assumed allocation
            
            
            # --- MSY calcs?
            
          })


#' Count independent variables for a MICE relationship at position x in a Rel list
#'
#' @param x Position of a MICE relationship in the list Rel (MOM@Rel)
#' @param Rel The list of MICE relationships (MOM@Rel)
#' @author T.Carruthers
#' @export
getnIVs<-function(x,Rel){
  
  Reli<-Rel[[x]]
  fnams<-names(attr(Reli$terms,"dataClasses"))
  DV<-fnams[1]
  Dp<-unlist(strsplit(DV,"_"))[2]
  Dnam<-unlist(strsplit(DV,"_"))[1]
  IV<-fnams[2:length(fnams)]
  length(IV)
  
}



#' Plot a relationship between stocks
#'
#' @param Stocks A list of stock objects (MOM@@Stocks)
#' @param Rel A list of inter-stock MICE relationships (MOM@@Rel)
#' @param Relno Integer. The relationship you wish to plot
#' @param Snams A vector of stock names
#' @param leg Logical. Do you want to plot a legend?
#' @param extras Integer. The number of blank plots to create at the end.
#' @author T.Carruthers
#' @export
plotRel<-function(Stocks,Rel,Relno,Snams,leg=F,extras=0){
  
  Reli<-Rel[[Relno]]
  
  IVnams<-c("B","SSB","N")
  IVcode<-c("Bcur","SSBcur","Ncur")
  
  DVnam<-c("M","a", "b", "R0", "hs", "K", "Linf", "t0")
  modnam<-c("Mx","ax","bx","R0x","hsx","Kx","Linfx","t0x")
  
  fnams<-names(attr(Reli$terms,"dataClasses"))
  DV<-fnams[1]
  Dp<-as.numeric(unlist(strsplit(DV,"_"))[2])
  Dnam<-unlist(strsplit(DV,"_"))[1]
  IV<-fnams[2:length(fnams)]
  nIV<-length(IV)
  IVs<-matrix(unlist(strsplit(IV,"_")),ncol=nIV)
  newdat<-NULL
  
  for(iv in 1:nIV){
    
    p<-as.numeric(IVs[2,iv])
    IVcol<-match(IV,names(Reli$model))
    plot(Reli$model[,c(IVcol,1)],pch=19,xlab="",ylab="")
    mtext(paste0(IV[iv]," (",Snams[p],")"),1,line=2.2)
    mtext(paste0(DV," (",Snams[Dp],")"),2,line=2.2)
    preddat<-cbind(Reli$model[,IVcol],predict(Reli))
    preddat<-preddat[order(preddat[,1]),]
    lines(preddat,col='red',lwd=2)
    points(Reli$model[,IVcol],simulate(Reli)[,1],col='green')
    if(leg & iv==1)legend('topleft',legend=c("Data","Fitted","Simulated"),text.col=c('black','red','green'),bty='n')
    
  }
  
  if(extras>0){
    
    for(i in 1:extras)plot(1,1,axes=F,xlab="",ylab="",col="white")
    
  }
  
}

