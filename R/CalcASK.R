#' Calculate an Age-Size Key
#'
#' Generates an Age-Size key given mean and standard deviation of size-at-age. The size-at-age
#' can be normally or log-normally distributed. By default the distribution is truncated at 2
#' standard deviations.
#'
#' @param MeanAtAge The mean size at age. Either a numeric vector of length `nage`, a 2D array with
#' dimensions `c(nsim, nage)`, or a 3D array with dimensions `c(nsim, nage, nTS)`.
#' @param CVatAge The coefficient of variation (CV) at age. Same structure as `MeanAtAge`.
#' @param Classes A numeric vector with the midpoints of the size classes for the age-size key.
#' @param TruncSD Numeric value indicating the number of standard deviations
#' where the distribution is truncated. Use high values to approximate a non-truncated distribution
#' @param Dist The distribution of `MeanAtAge`. Character, either `normal` or `lognormal`
#' @param Ages Optional. Numeric vector of values for the age classes. Defaults to `0:(nage-1)`.
#'
#' @return A 4D array with dimensions `nsim`, `nage`, `nClasses`, and `nTS`
#'
#' @export
CalcASK <- function(MeanAtAge, 
                    CVatAge, 
                    Classes,
                    TruncSD=2, 
                    Dist=c('normal', 'lognormal'),
                    Ages=NULL, 
                    silent=FALSE,
                    type='Length') {
  
  Dist <- match.arg(Dist)
  
  # Checks
  if (any(Classes<0))
    cli::cli_abort('Some `Classes` < 0 ')
  if (length(Classes)<3)
    cli::cli_abort('`length(Classes)<3`')
  if (length(TruncSD)>1)
    TruncSD <- TruncSD[1]
  if (TruncSD<0)
    cli::cli_abort('`TruncSD` < 0 ')
  
  
  MeanAtAge <- Structure(MeanAtAge)
  SDatAge <- CalcSDatAge(MeanAtAge, CVatAge)
  
  dim_MeanAtAge <- dim(MeanAtAge)
  nage <- dim_MeanAtAge[2]
  
  dim_SDatAge <- dim(SDatAge)
  
  if (dim_MeanAtAge[2] != dim_SDatAge[2]) {
    if (dim_SDatAge[2]==1) {
      SDatAge <- Structure(replicate(nage, SDatAge))
    } else {
      cli::cli_abort('`dim(MeanAtAge)[2] != dim(SDatAge)[2]`')
    }
  }
  
  nsim_MeanAtAge <- dim_MeanAtAge[1]
  nTS_MeanAtAge <- dim_MeanAtAge[3]
  
  nsim_SDatAge <- dim_SDatAge[1]
  nTS_SDatAge <- dim_SDatAge[3]
  
  nsim <- max(nsim_MeanAtAge, nsim_SDatAge) # maximum number of simulations
  nTS <- max(nTS_MeanAtAge, nTS_SDatAge) # maximum number of time-steps
  
  ASK_list <- vector('list', nsim)
  
  by <- Classes[2]-Classes[1]
  ClassLower <- seq(Classes[1]-0.5*by, by=by, length.out=length(Classes))
  nClasses <- length(Classes)
  
  if (is.null(Ages))
    Ages <- 0:(nage-1)
  
  text <- paste('Age', type, sep='-')

  
  if (!silent)
    cli::cli_progress_bar(paste('Calculating', text, 'Key'),
                          total=nsim, type='tasks')
  for (s in 1:nsim) {
    if (!silent)
      cli::cli_progress_update()
    ASK <- array(0, dim=c(nage, nClasses, nTS))
    
    for (t in 1:nTS) {
      sd <- SDatAge[GetIndex(s, nsim_SDatAge),,GetIndex(t, nTS_SDatAge)]
      if (Dist=='normal') {
        mu <- MeanAtAge[GetIndex(s, nsim_MeanAtAge),,GetIndex(t, nTS_MeanAtAge)]
        classlower <- ClassLower
      }
      if (Dist =='lognormal') {
        sd <- SDatAge[GetIndex(s, nsim_SDatAge),,GetIndex(t, nTS_SDatAge)]/
          MeanAtAge[GetIndex(s, nsim_MeanAtAge),,GetIndex(t, nTS_MeanAtAge)]
        
        sd[!is.finite(sd)] <- 0.05
        mu <- log(MeanAtAge[GetIndex(s, nsim_MeanAtAge),,GetIndex(t, nTS_MeanAtAge)]) -
          0.5 * sd^2
        mu[!is.finite(mu)] <- log(1E-6)
        classlower <- log(ClassLower)
      }
      ASK[,1,t] <- ptnorm(classlower[2], mean=mu, sd=sd, truncsd=TruncSD)
      for (l in 2:(nClasses-1)) {
        ASK[,l,t] <- ptnorm(classlower[l+1], mean=mu, sd=sd, TruncSD) -
          ptnorm(classlower[l], mean=mu, sd=sd, TruncSD)
      }
      ASK[,nClasses,t] <- 1 - ptnorm(classlower[nClasses], mean=mu, sd, TruncSD)
      # ASK[,,t] <- ASK[,,t]/matrix(apply(ASK[,,t], 1, sum), nrow=nage, ncol=nClasses)
    }
    ASK_list[[s]] <- ASK
  }
  out <- abind::abind(ASK_list, along=4) |> aperm(c(4,1,2,3))
  attributes(out)$Classes <- Classes
  attributes(out)$Ages <- Ages
  out
}
