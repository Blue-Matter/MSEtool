CalcAgeSizeKey <- function(MeanAtAge, 
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
  
  nsim <- max(dim_MeanAtAge[1], dim_SDatAge[1]) # maximum number of simulations
  
  TimeStepsList <- list(attributes(MeanAtAge)$dimnames[["TimeStep"]],
                        attributes(SDatAge)$dimnames[["TimeStep"]])
  
  ind <- unlist(lapply(TimeStepsList, length)) |> which.max()
  TimeSteps <- TimeStepsList[[ind]]
  
  MeanAtAgeList <- ExpandSims(MeanAtAge, nsim) |> 
    ExpandTimeSteps(TimeSteps) |> 
    Array2List(pos=1)
  SDatAgeList <- ExpandSims(SDatAge, nsim) |> 
    ExpandTimeSteps(TimeSteps) |>
    Array2List(pos=1)
  
  by <- Classes[2]-Classes[1]
  ClassLower <- seq(Classes[1]-0.5*by, by=by, length.out=length(Classes))
  nClasses <- length(Classes)
  
  
  ASKList <-  purrr::map2(MeanAtAgeList, SDatAgeList, \(x,y) 
                          CalcAgeSizeKey_(x, y, Classes, Dist),
                          .progress = list(
                            type = "iterator",
                            format = "Calculating Age-Size Key {cli::pb_bar} {cli::pb_percent}",
                            clear = TRUE))
    
  
  
  
  
  
  
 
  
  
  
  
 
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
