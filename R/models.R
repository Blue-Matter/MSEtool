PrintModelTable <- function(models, print=TRUE, Independent=NULL) {
  df <- list()
  Independent <- c(Independent, slotNames('stock'), slotNames('fleet'))
  for (i in seq_along(models)) {
    mod <- get(models[i])
    args <- names(formals(mod))
    other_index <- rep(FALSE, length(args))
    other_index[args %in% Independent] <- TRUE

    df[[i]] <- data.frame(Model=models[i],
                          Arguments=paste(names(formals(mod)), collapse=', '),
                          Class=class(mod))
    if (print) {
      cli::cli_par()
      cli::cli_text(paste0("{.strong Model:} {.help MSEtool::", models[i],"}"))
      cli::cli_text("{.strong Pars:} {.code {args[!other_index]}}")

      if (sum(other_index))
        cli::cli_text("{.strong Other argument(s):} {.code {args[other_index]}}")

      cli::cli_text("{.strong Class:} {.code {class(mod)}}")
      cli::cli_end()
    }
  }
  do.call('rbind', df)
}

ReturnModels <- function(ModelClass, full=TRUE, print=TRUE, Independent='Independent') {
  models <- FindModels(ModelClass)
  if (!full)
    return(models)

  df <- PrintModelTable(models, print, Independent)
  if (!print)
    return(df)
  invisible(df)
}







## ---- Fecundity ----

#' Fecundity-at-Length or -Age Models
#'
#' Functions for generating fecundity-at-age or -at-length.
#'
#'
#' @return Fecundity-at-length or -age values
#' @name FecundityModels
#' @examples
#' FecundityModels()
NULL

#' @rdname FecundityModels
#' @param full Logical. Provide a complete table (TRUE) or just the model names (FALSE)?
#' @param print Logical. Print out the results (TRUE) or just return the data.frame (FALSE)?
#'
#' @return Prints to console and invisible data.frame or model names
#' @export
FecundityModels <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Fecundity-at-Age-Model',
                            'Fecundity-at-Length-Model'),
               full, print)
}


#' @rdname FecundityModels
#' @export
FecundityModelsLength <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Fecundity-at-Length-Model'),
               full, print)
}

#' @rdname FecundityModels
#' @export
FecundityModelsAge <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Fecundity-at-Age-Model'),
               full, print)
}

#' Logistic Fecundity-at-length model
#' @param Length A numeric vector of lengths
#' @param L50 Length corresponding with 50% maximum fecundity
#' @param L50_95 Interval between `L50` and length at 95% maximum fecundity (`L95`)
#' @param MaxFec Maximum fecundity
#' @export
FecundityAtLength <- function(Length, L50, L50_95, MaxFec) {
  Pars <- list(L50=Structure(L50, out=c('nsim', 'nTS'), req='nsim'),
               L50_95=Structure(L50_95, out=c('nsim', 'nTS'), req='nsim'),
               MaxFec=Structure(MaxFec, out=c('nsim', 'nTS'), req='nsim'))

  Fecundity_at_Length_(Length, Pars)
}
class(FecundityAtLength) <- 'Fecundity-at-Length-Model'

Fecundity_at_Length_ <- function(Length, Pars) {
  L50 <- Pars$L50
  L50_L95 <- Pars$L50_95
  maxFec <- Pars$MaxFec

  dim_L50 <- dim(L50)
  dim_L50_L95 <- dim(L50_L95)
  dim_maxFec <- dim(maxFec)
  nLength <- length(Length)

  nsim_L50 <- dim_L50[1]
  nTS_L50 <- dim_L50[2]
  nsim_L50_L95 <- dim_L50_L95[1]
  nTS_L50_L95 <- dim_L50_L95[2]

  nsim_maxFec <- dim_maxFec[1]
  nTS_maxFec  <- dim_maxFec[2]

  nsim <- max(nsim_L50, nsim_L50_L95)
  nTS <- max(nTS_L50, nTS_L50_L95)

  MAL <- array(0, dim=c(nsim, nLength, nTS))
  for (s in 1:nsim) {
    for (ts in 1:nTS) {
      MAL[s,,ts] <- Logistic1(Length,
                              L50[GetIndex(s, nsim_L50), GetIndex(ts, nTS_L50)],
                              L50_L95[GetIndex(s, nsim_L50_L95), GetIndex(ts, nTS_L50_L95)],
                              maxFec[GetIndex(s, nsim_maxFec), GetIndex(ts, nTS_L50_L95)]
      )
    }
  }
  MAL
}


#' Logistic Fecundity-at-age model
#' @param Ages A numeric vector of ages
#' @param A50 Age corresponding with 50% maturity
#' @param A50_95 Interval between `A50` and age at 95% maturity (`A95`)
#' @export
FecundityAtAge <- function(Ages, A50, A50_95, MaxFec) {
  Pars <- list(A50=Structure(A50, out=c('nsim', 'nTS'), req='nsim'),
               A50_95=Structure(A50_95, out=c('nsim', 'nTS'), req='nsim'),
               MaxFec=Structure(MaxFec, out=c('nsim', 'nTS'), req='nsim'))

  Fecundity_at_Age_(Ages, Pars)
}
class(FecundityAtAge) <- 'Fecundity-at-Age-Model'

Fecundity_at_Age_ <- function(Ages, Pars) {
  A50 <- Pars$A50
  A50_95 <- Pars$A50_95
  maxFec <- Pars$MaxFec

  dim_A50 <- dim(A50)
  dim_A50_A95 <- dim(A50_95)
  nAge <- length(Ages)

  nsim_A50 <- dim_A50[1]
  nTS_A50 <- dim_A50[2]
  nsim_A50_95 <- dim_A50_A95[1]
  nTS_A50_95 <- dim_A50_A95[2]

  dim_maxFec <- dim(maxFec)
  nsim_maxFec <- dim_maxFec[1]
  nTS_maxFec  <- dim_maxFec[2]

  nsim <- max(nsim_A50, nsim_A50_95)
  nTS <- max(nTS_A50, nTS_A50_95)

  MAL <- array(0, dim=c(nsim, nAge, nTS))
  for (s in 1:nsim) {
    for (ts in 1:nTS) {
      MAL[s,,ts] <- Logistic1(Ages,
                              A50[GetIndex(s, nsim_A50), GetIndex(ts, nTS_A50)],
                              A50_95[GetIndex(s, nsim_A50_95), GetIndex(ts, nTS_A50_95)],
                              maxFec[GetIndex(s, nsim_maxFec), GetIndex(ts, nTS_A50_95)]
      )
    }
  }
  MAL
}




## ---- Selectivity ----

#' Selectivity-at-Length or -Age Models
#'
#' Functions for generating selectivity-at-age or -at-length.
#'
#'
#' @return Selectivity-at-length or -age values
#' @name SelectivityModels
#' @examples
#' SelectivityModels()
NULL

#' @describeIn SelectivityModels Print a list of Selectivity-at-Age or Selectivity-at-Length models
#' @param full Logical. Provide a complete table (TRUE) or just the model names (FALSE)?
#' @param print Logical. Print out the results (TRUE) or just return the data.frame (FALSE)?
#'
#' @return Prints to console and invisible data.frame or model names
#' @export
SelectivityModels <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Selectivity-at-Age-Model',
                            'Selectivity-at-Length-Model',
                            'Selectivity-at-Weight-Model'),
               full, print)
}

#' @describeIn SelectivityModels Print a list of valid Selectivity-at-Length models
#'
#' @export
SelectivityModelsLength <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Selectivity-at-Length-Model'),
               full, print)
}

#' @describeIn SelectivityModels Print a list of valid Selectivity-at-Age models
#'
#' @export
SelectivityModelsAge <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Selectivity-at-Age-Model'),
               full, print)
}


#' @describeIn SelectivityModels Print a list of valid Selectivity-at-Weight models
#'
#' @export
SelectivityModelsWeight <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Selectivity-at-Weight-Model'),
               full, print)
}

#' @describeIn SelectivityModels Logistic selectivity-at-length model
#' @param Length A numeric vector of lengths
#' @param SL50 Length corresponding with 50% selectivity
#' @param SL50_95 Interval between `SL50` and length at 95% selectivity (`SL95`)
#' @export
SelectivityAtLength <- function(Length, SL50, SL50_95) {
  Pars <- list(L50=Structure(SL50, out=c('nsim', 'nTS'), req='nsim'),
               L50_95=Structure(SL50_95, out=c('nsim', 'nTS'), req='nsim'))
  Maturity_at_Length_(Length, Pars)
}
class(SelectivityAtLength) <- 'Selectivity-at-Length-Model'

#' @describeIn SelectivityModels Logistic selectivity-at-weight model
#' @param Weight A numeric vector of weights
#' @param SW50 Weight corresponding with 50% selectivity
#' @param SW50_95 Interval between `SW50` and weight at 95% selectivity (`SW95`)
#' @export
SelectivityAtWeight <- function(Weight, SW50, SW50_95) {
  Pars <- list(L50=Structure(SW50, out=c('nsim', 'nTS'), req='nsim'),
               L50_95=Structure(SW50_95, out=c('nsim', 'nTS'), req='nsim'))
  Maturity_at_Length_(Weight, Pars)
}
class(SelectivityAtWeight) <- 'Selectivity-at-Weight-Model'


#' @describeIn SelectivityModels Double-normal selectivity-at-length model
#' @param L5 Shortest length at which 5% of the population is vulnerable to
#' selection by the gear used in this fleet. A single numeric value, a numeric
#' vector length `nsim`, or a matrix `nsim` by `nTS`.
#' @param FS Shortest length at which 100% of the population is vulnerable to
#'  selection by the gear used by this fleet. Same structure as `L5`
#' @param Vmaxlen Proportion of fish selected by the gear at the maximum
#' length specified in `Length`. **Note**: this has changed from previous versions
#' of `MSEtool`, where `Vmaxlen` corresponded to the selectivity at `Linf`.
#' 
#' @export
DoubleNormal <- function(Length, L5, LFS, Vmaxlen) {
  Pars <- list(L5=Structure(L5, out=c('nsim', 'nTS'), req='nsim'),
               LFS=Structure(LFS, out=c('nsim', 'nTS'), req='nsim'),
               Vmaxlen=Structure(Vmaxlen, out=c('nsim', 'nTS'), req='nsim'))
  double_normal_(Length, Pars)
}
class(DoubleNormal) <- 'Selectivity-at-Length-Model'

#' @describeIn SelectivityModels Double-normal selectivity-at-weight model
#' @export
DoubleNormalWeight <- function(Weight, W5, WFS, Vmaxweight) {
  Pars <- list(L5=Structure(W5, out=c('nsim', 'nTS'), req='nsim'),
               LFS=Structure(WFS, out=c('nsim', 'nTS'), req='nsim'),
               Vmaxlen=Structure(Vmaxweight, out=c('nsim', 'nTS'), req='nsim'))
  double_normal_(Weight, Pars)
}
class(DoubleNormalWeight) <- 'Selectivity-at-Weight-Model'


double_normal_ <- function(Length, Pars) {
  
  L5 <- Pars$L5
  LFS <- Pars$LFS
  Vmaxlen <- Pars$Vmaxlen
  RefLength <- max(Length)
  
  DimList <- matrix(c(dim(L5),
                    dim(LFS),
                    dim(Vmaxlen)), 2,3)
  
  nsim <- max(DimList[1,])
  nTS <- max(DimList[2,])
  
  select_at_length <- array(0, dim=c(nsim, length(Length), nTS))
  
  for (s in 1:nsim) {
    for (ts in 1:nTS) {
      l5 <- L5[GetIndex(s, DimList[1,1]), GetIndex(ts, DimList[2,1])] 
      lfs <- LFS[GetIndex(s, DimList[1,2]), GetIndex(ts, DimList[2,2])] 
      vmaxlen <- Vmaxlen[GetIndex(s, DimList[1,3]),GetIndex(ts, DimList[2,3])]
      
      sr <- (RefLength - lfs) / ((-log(vmaxlen,2))^0.5)
      sr[!is.finite(sr)] <- Inf
      sl <- (lfs - l5) /((-log(0.05,2))^0.5)
      
      select_at_length[s,,ts] <- dnormal(Length, lfs, sl, sr)
      select_at_length[s,,ts] <- select_at_length[s,,ts]/max(select_at_length[s,,ts])
    }
  }
  select_at_length
}

#' @describeIn SelectivityModels Logistic selectivity-at-age model
#' @param Ages A numeric vector of ages
#' @param A50 Age corresponding with 50% selectivity
#' @param A50_95 Interval between `A50` and age at 95% selectivity (`A95`)
#' @export
SelectivityAtAge <- function(Ages, A50, A50_95) {

  Pars <- list(A50=Structure(A50, out=c('nsim', 'nTS'), req='nsim'),
               A50_95=Structure(A50_95, out=c('nsim', 'nTS'), req='nsim'))

  Maturity_at_Age_(Ages, Pars)
}
class(SelectivityAtAge) <- 'Selectivity-at-Age-Model'

## ---- Retention ----

#' Retention-at-Length or -Age Models
#'
#' Functions for generating retention-at-age or -at-length.
#'
#'
#' @return Retention-at-length or -age values
#' @name RetentionModels
#' @examples
#' RetentionModels()
NULL

#' @describeIn Retention Print a list of Retention-at-Age or Retention-at-Length models
#' @param full Logical. Provide a complete table (TRUE) or just the model names (FALSE)?
#' @param print Logical. Print out the results (TRUE) or just return the data.frame (FALSE)?
#'
#' @return Prints to console and invisible data.frame or model names
#' @export
RetentionModels <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Retention-at-Age-Model',
                            'Retention-at-Length-Model',
                            'Retention-at-Weight-Model'),
               full, print)
}

#' @describeIn RetentionModels Print a list of valid Retention-at-Length models
#'
#' @export
RetentionModelsLength <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('RetentionModels-at-Length-Model'),
               full, print)
}

#' @describeIn RetentionModels Print a list of valid Retention-at-Age models
#'
#' @export
RetentionModelsAge <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Retention-at-Age-Model'),
               full, print)
}

#' @describeIn RetentionModels Print a list of valid Retention-at-Weight models
#'
#' @export
RetentionModelsWeight <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Retention-at-Weight-Model'),
               full, print)
}

#' @describeIn RetentionModels Logistic retention-at-length model
#' @param Length A numeric vector of lengths
#' @param RL50 Length corresponding with 50% retention
#' @param RL50_95 Interval between `RL50` and length at 95% retention (`RL95`)
#' @export
RetentionAtLength <- function(Length, RL50, RL50_95) {
  Pars <- list(L50=Structure(RL50, out=c('nsim', 'nTS'), req='nsim'),
               L50_95=Structure(RL50_95, out=c('nsim', 'nTS'), req='nsim'))
  Maturity_at_Length_(Length, Pars)
}
class(RetentionAtLength) <- 'Retention-at-Length-Model'

#' @describeIn RetentionModels Logistic retention-at-weight model
#' @param Weight A numeric vector of weights
#' @param RW50 Weight corresponding with 50% retention
#' @param RW50_95 Interval between `RW50` and weight at 95% retention (`RW95`)
#' @export
RetentionAtWeight <- function(Weight, RW50, RW50_95) {
  Pars <- list(L50=Structure(RW50, out=c('nsim', 'nTS'), req='nsim'),
               L50_95=Structure(RW50_95, out=c('nsim', 'nTS'), req='nsim'))
  Maturity_at_Length_(Weight, Pars)
}
class(RetentionAtWeight) <- 'Retention-at-Weight-Model'


#' @describeIn RetentionModels Double-normal retention-at-length model
#' @param LR5 Shortest length at which 5% of the population is vulnerable to
#' retention by the fleet. A single numeric value, a numeric
#' vector length `nsim`, or a matrix `nsim` by `nTS`.
#' @param LFR Shortest length at which 100% of the population is vulnerable to
#'  retention by this fleet. Same structure as `L5`
#' @param Rmaxlen Proportion of fish selected by the gear at the maximum
#' length specified in `Length`. **Note**: this has changed from previous versions
#' of `MSEtool`, where `Rmaxlen` corresponded to the selectivity at `Linf`.
#' 
#' @export
RDoubleNormal <- function(Length, LR5, LFR, Rmaxlen) {
  Pars <- list(L5=Structure(LR5, out=c('nsim', 'nTS'), req='nsim'),
               LFS=Structure(LFR, out=c('nsim', 'nTS'), req='nsim'),
               Vmaxlen=Structure(Rmaxlen, out=c('nsim', 'nTS'), req='nsim'))
  double_normal_(Length, Pars)
}
class(RDoubleNormal) <- 'Retention-at-Length-Model'

double_normal_ <- function(Length, Pars) {
  
  L5 <- Pars$L5
  LFS <- Pars$LFS
  Vmaxlen <- Pars$Vmaxlen
  RefLength <- max(Length)
  
  DimList <- matrix(c(dim(L5),
                      dim(LFS),
                      dim(Vmaxlen)), 2,3)
  
  nsim <- max(DimList[1,])
  nTS <- max(DimList[2,])
  
  select_at_length <- array(0, dim=c(nsim, length(Length), nTS))
  
  for (s in 1:nsim) {
    for (ts in 1:nTS) {
      l5 <- L5[GetIndex(s, DimList[1,1]), GetIndex(ts, DimList[2,1])] 
      lfs <- LFS[GetIndex(s, DimList[1,2]), GetIndex(ts, DimList[2,2])] 
      vmaxlen <- Vmaxlen[GetIndex(s, DimList[1,3]),GetIndex(ts, DimList[2,3])]
      
      sr <- (RefLength - lfs) / ((-log(vmaxlen,2))^0.5)
      sr[!is.finite(sr)] <- Inf
      sl <- (lfs - l5) /((-log(0.05,2))^0.5)
      
      select_at_length[s,,ts] <- dnormal(Length, lfs, sl, sr)
      select_at_length[s,,ts] <- select_at_length[s,,ts]/max(select_at_length[s,,ts])
    }
  }
  select_at_length
}

#' @describeIn RetentionModels Logistic retention-at-age model
#' @param Ages A numeric vector of ages
#' @param A50 Age corresponding with 50% retention
#' @param A50_95 Interval between `A50` and age at 95% retention (`A95`)
#' @export
RetentionAtAge <- function(Ages, A50, A50_95) {
  
  Pars <- list(A50=Structure(A50, out=c('nsim', 'nTS'), req='nsim'),
               A50_95=Structure(A50_95, out=c('nsim', 'nTS'), req='nsim'))
  
  Maturity_at_Age_(Ages, Pars)
}
class(RetentionAtAge) <- 'Retention-at-Age-Model'

