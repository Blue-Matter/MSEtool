
#' Internal MSE running function for the tune_MP function
int_tune = function(par, MP_parname, MP, Hist_list,minfunc,parallel){
  assign("MPtest",get(MP))
  formals(MPtest)[[MP_parname]] = par
  cat(paste0(MP_parname," = ",round(par,6)," \n"))
  class(MPtest) = "MP"
  if(!parallel){
    MSE_list = lapply(Hist_list,function(X)Project(X,MPs = "MPtest"))
  }else{
    sfExport('MPtest')
    MSE_list = sfLapply(Hist_list,function(X)Project(X,MPs = "MPtest"))
  }
  minfunc(MSE_list)
}

#' Tune MP
#'
#' A generic function that uses optimize to tune a single MP parameter to minimize a 
#' user-specified function (e.g. squared distance from a mean yield, PGK = 60%, etc.)
#'
#' @param Hist_list A list of objects of class Hist - created by runMSE(..., Hist=T)
#' @param MP A character string that is the name of the MP to be tuned 
#' @param MP_parname A character string that is the argument (parameter) of the MP to be tuned
#' @param interval A numeric vector two positions long that is the c(lower.bound, upper.bound) 
#' for the parameter to be tuned (MP_parname)
#' @param minfunc A function to be minimized (e.g. the squared difference between mean yield 
#' obtained by the MP and a desired yield) that takes a list of MSE objects as its first argument.
#' @param tol A positive numerical value that is the tolerance for the optimize procedure (default is 1E-2) 
#' @param parallel Logical: should the MSE projections (over the Hist objects in Hist_list) be calculated in parallel?
#'
#' @return A function of class MP with argument MP_parname tuned by optim to minimize minfunc
#' @examples
#' \dontrun{
#' testOM@cpars$Data = new('Data')
#' testOM@cpars$Data@MPrec=2000
#' Hist_1 = runMSE(testOM,Hist=T)
#' testOM2 = testOM
#' testOM2@D = testOM@D / 2
#' Hist_2 = runMSE(testOM2,Hist=T)
#' 
#' myMP = function(x, Data, reps=1, rate = 1){
#'   CpI = mean(Data@Cat[x,46:50]) / mean(Data@Ind[x,46:50],na.rm=T)
#'   I = Data@Ind[x,]
#'   recI = mean(I[length(I)-((5-1):0)])
#'   Rec=new('Rec')
#'   Rec@TAC = recI * CpI * rate
#'   Rec
#' }
#' class(myMP) = "MP"
#' 
#' C1000 = function(MSE_list){
#'   mucat = mean(sapply(MSE_list,function(X){mean(X@Catch)}))
#'   cat(paste0("mean catch = ",round(mucat,3),"\n"))
#'   (mucat - 1000)^2 # try to match 1,250t mean yield
#' }
#' 
#' myMP_t = tune_MP(list(Hist_1,Hist_2), MP = "myMP", MP_parname = "rate", 
#'                  interval = c(1,1.5), minfunc = C1000, tol=1E-3, parallel =F)
#' 
#' formals(myMP_t)$rate
#' }
#'
#' @author T. Carruthers
#' @export
#'
tune_MP = function(Hist_list, MP, MP_parname, interval, minfunc, tol=1E-2, parallel=F){
 opt = optimize(int_tune, interval=interval,MP_parname = MP_parname, MP = MP, 
                Hist_list=Hist_list, minfunc=minfunc, tol=tol, parallel = parallel)
 MPout = get(MP)
 formals(MPout)[MP_parname] = opt$minimum
 class(MPout) = "MP"
 return(MPout)
}  
  
  
  