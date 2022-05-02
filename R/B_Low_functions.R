# Blow optimization code

#' Blow parallel optimization function
#'
#' Find the current biomass at which it would take HZN mean generation times
#' to reach Bfrac x SSBMSY biomass level given zero catches
#'
#' @param x position in a vector
#' @param N array of numbers at age by sim and area (dims nsim, maxage, nyears, nareas)
#' @param Asize matrix of area size (nsim by nareas)
#' @param SSBMSY vector nsim long of spawning biomass at MSY
#' @param SSBpR matrix nsim by nareas with SSB per recruit
#' @param MPA matrix nyears + proyears by nareas of spatial closures
#' @param SSB0 vector of SSB0 length nsim
#' @param nareas numeric. number of areas
#' @param retA array of retention nsim x maxage x nyears
#' @param MGThorizon vector nsim long of MGT x HZN
#' @param Find matrix of fishing mortality rate nsim x nyears
#' @param Perr matrix of recruitment deviations nsim x nyears + maxage -1
#' @param M_ageArray array of natural mortality rate nsim x maxage x nyears + proyears
#' @param hs vector nsim long of steepness values
#' @param Mat_age array nsim x nages x nyears+proyears of maturity at age
#' @param Wt_age matrix nsim x nages of weight at age
#' @param R0a matrix nsim by nareas of unfished recruitment
#' @param V array of vulnerability nsim x maxage x nyears
#' @param nyears integer: number of historical years
#' @param maxage integer: maximum age
#' @param mov array of movement nsim x 2 x 2
#' @param Spat_targ vector of spatial targeting parameters
#' @param SRrel integer representing recruitment dynamics type 1: Bev Holt 2: Ricker
#' @param aR vector of recruitment parameters
#' @param bR vector of recruitment parameters
#' @param Bfrac fraction of SSBMSY that is the target
#' @param maxF maximum apical fishing mortality
#' @param ploty logical: should a plot be produced
#' @param plusgroup Integer. Default = 0 = no plus-group. Use 1 to include a plus-group
#' @author T. Carruthers with modifications by A. Hordyk
#' @keywords internal
getBlow<-function(x, N, Asize, SSBMSY, SSBpR, MPA, SSB0, nareas, retA,MGThorizon,Find,
                  Perr,M_ageArray,hs,Mat_age,Wt_age, Fec_age, R0a,V,nyears,maxage,mov,Spat_targ,SRrel,
                  aR,bR,Bfrac=0.5, maxF, ploty=F, plusgroup=0){
  
  if (SSBMSY[x] > 0) {
    opt <- optimize(Blow_opt,log(c(0.0075,15)),N=N[x,,1,], Asize_c =Asize[x,], SSBMSYc=SSBMSY[x],
                    SSBpRc=SSBpR[x,], MPA=MPA, SSB0c=SSB0[x], nareas, retAc=retA[x,,],
                    MGThorizonc=MGThorizon[x], Fc=Find[x,],Perrc=Perr[x,], Mc=M_ageArray[x,,],
                    hc=hs[x], Mac=Mat_age[x,,], Wac=Wt_age[x,,], Fecac=Fec_age[x,,],
                    R0c=R0a[x,], Vc=V[x,,],
                    nyears=nyears, maxage=maxage, movc=mov[x,,,,], Spat_targc=Spat_targ[x],
                    SRrelc=SRrel[x], aRc=aR[x,], bRc=bR[x,], Bfrac, maxF, mode=1,
                    plusgroup=plusgroup)
    
    if (ploty) {
      Blow_opt(opt$minimum,N=N[x,,1,], Asize_c =Asize[x,], SSBMSYc=SSBMSY[x],
               SSBpRc=SSBpR[x,], MPA=MPA, SSB0c=SSB0[x], nareas, retAc=retA[x,,],
               MGThorizonc=MGThorizon[x], Fc=Find[x,],Perrc=Perr[x,], Mc=M_ageArray[x,,],
               hc=hs[x], Mac=Mat_age[x,,], Wac=Wt_age[x,,], Fecac=Fec_age[x,,], R0c=R0a[x,], Vc=V[x,,],
               nyears=nyears, maxage=maxage, movc=mov[x,,,,], Spat_targc=Spat_targ[x],
               SRrelc=SRrel[x], aRc=aR[x,], bRc=bR[x,], Bfrac, maxF, mode=3,
               plusgroup=plusgroup)
    }
    
    Blow <- Blow_opt(opt$minimum,N=N[x,,1,], Asize_c =Asize[x,], SSBMSYc=SSBMSY[x],
                     SSBpRc=SSBpR[x,], MPA=MPA, SSB0c=SSB0[x], nareas, retAc=retA[x,,],
                     MGThorizonc=MGThorizon[x], Fc=Find[x,],Perrc=Perr[x,], Mc=M_ageArray[x,,],
                     hc=hs[x], Mac=Mat_age[x,,], Wac=Wt_age[x,,], Fecac=Fec_age[x,,],
                     R0c=R0a[x,], Vc=V[x,,],
                     nyears=nyears, maxage=maxage, movc=mov[x,,,,], Spat_targc=Spat_targ[x],
                     SRrelc=SRrel[x], aRc=aR[x,], bRc=bR[x,], Bfrac, maxF, mode=2,
                     plusgroup=plusgroup)
  } else {
    Blow <- 0
  }
  
  return(Blow)
}


#' Blow internal parallel optimization function
#'
#' Find the current biomass at which it would take HZN mean generation times
#' to reach Bfrac x SSBMSY biomass level given zero catches
#'
#' @param lnq number: estimate of log catchability
#' @param N matrix maxage by nareas with initial numbers at age
#' @param Asize_c vector length nareas with size of each area
#' @param SSBMSYc number: spawning biomass at MSY
#' @param SSBpRc vector length nareas with SSBpR by area
#' @param MPA matrix of spatial closures
#' @param SSB0c SSB0
#' @param nareas numeric: number of areas
#' @param retAc matrix maxage by nyears+proyears of retention by age and year
#' @param MGThorizonc number: MGT x HZN
#' @param Fc vector nyears long of fishing mortality rate
#' @param Perrc vector nyears+maxage-1 long of recruitment deviations
#' @param Mc matrix maxage by nyears+proyears of natural mortality rate
#' @param hc number: steepness values
#' @param Mac matrix nages by nyears+proyears of maturity at age
#' @param Wac vector nages long  of weight at age
#' @param Fecac vector nages long  of mature weight at age
#' @param R0c number: unfished recruitment
#' @param Vc matrix of vulnerability maxage x nyears
#' @param nyears integer: number of historical years
#' @param maxage integer: maximum age
#' @param movc matrix of movement 2 x 2
#' @param Spat_targc number: spatial targeting parameters
#' @param SRrelc integer representing recruitment dynamics type 1: Bev Holt 2: Ricker
#' @param aRc number: recruitment parameter
#' @param bRc number: recruitment parameter
#' @param Bfrac fraction of SSBMSY that is the target
#' @param maxF maximum apical fishing mortality
#' @param mode 1: find Blow 2:report blow  3:plot results
#' @param plusgroup Integer. Default = 0 = no plus-group. Use 1 to include a plus-group
#' @author T. Carruthers with modifications by A. Hordyk
#' @keywords internal
Blow_opt<-function(lnq, N, Asize_c, SSBMSYc,SSBpRc, MPA, SSB0c, nareas, retAc,
                   MGThorizonc,Fc,Perrc,Mc,hc,Mac,Wac, Fecac, R0c,Vc,nyears,maxage,movc,Spat_targc,
                   SRrelc,aRc,bRc,Bfrac,maxF, mode=1, plusgroup=0){

  pyears <- nyears + MGThorizonc
  n_age <- maxage + 1

  # Life history parameters fixed at current values for projection years
  M_age <- Mc[,c(1:nyears, rep(nyears, MGThorizonc))]
  MatAge <- Mac[,c(1:nyears, rep(nyears, MGThorizonc))]
  WtAge <- Wac[,c(1:nyears, rep(nyears, MGThorizonc))]
  FecAge <- Fecac[,c(1:nyears, rep(nyears, MGThorizonc))]
  Prec <- c(Perrc[1:(nyears+maxage+1)], rep(1, MGThorizonc)) # no recruitment variability in projection
  Effind <- c(Fc, rep(0, MGThorizonc)) # no fishing mortality in futute

  Vuln <- Vc[,c(1:nyears, rep(nyears, MGThorizonc))]
  Retc <- retAc[,c(1:nyears, rep(nyears, MGThorizonc))]

  MPA <- MPA[c(1:nyears, rep(nyears, MGThorizonc)),]

  movcx <- array(movc[,,,nyears], dim=c(n_age, nareas, nareas, pyears)) # current movement pattern

  simpop <- popdynCPP(nareas, maxage, N, pyears, M_age, Asize_c,
                      MatAge, WtAge, FecAge, Vuln, Retc, Prec, split.along.dim(movcx,4),
                      SRrelc, Effind, Spat_targc, hc,
                      R0c=R0c, SSBpRc=SSBpRc, aRc=aRc, bRc=bRc, Qc=exp(lnq), Fapic=0,
                      maxF=maxF, MPA=MPA, control=1, SSB0c=SSB0c,
                      plusgroup = plusgroup)

  SSBstore <- apply(simpop[[4]],2, sum)
  SBiomass <- SSBstore[pyears]

  if(mode==1){
    pen<-0
    if(SSBstore[nyears]>(0.8*SSBMSYc))pen<-(SSBstore[nyears]-(0.8*SSBMSYc))^2 # penalty to keep inital depletion under SSB for rebuilding
    return(pen+(log(sum(SBiomass))-log(SSBMSYc*Bfrac))^2)
  }else if(mode==2){
    return(SSBstore[nyears])
  }else{
    plot(SSBstore,ylim=c(0,max(SSBstore)),col='red',xlab="Year",ylab="SSB")
    abline(h=0)
    abline(h=SSBMSYc*Bfrac,lty=2)
  }

}
