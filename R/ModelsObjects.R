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
      cli::cli_text(paste0("{.strong Model:} {.help MSEtool2::", models[i],"}"))
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


## ---- Length ----

#' Length Models
#'
#' Functions for generating mean length-at-age.
#' @examples
#' LengthModels()
#' @name LengthModels
NULL

#' @describeIn LengthModels Print a list of valid Length-at-Age models
#' @param full Logical. Provide a complete table (TRUE) or just the model names (FALSE)?
#' @param print Logical. Print out the results (TRUE) or just return the data.frame (FALSE)?
#'
#' @return Prints to console and invisible data.frame or model names
#' @export
LengthModels <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass='Length-at-Age-Model', full, print)
}

#' @param Ages A numeric vector of ages. In same units as `K`, and those used in [Ages()] in a
#' [Stock()] object.
#' @param Linf The asymptotic average length. The expected length at infinite age.
#' Units should match those used in [Length()] object.
#' @param K Brody growth rate coefficient in units of yr-1, or whatever time step is
#' set in `Ages`
#' @param t0 Age when the average length was zero. Positive values mean
#' negative length at some positive age; an unlikely reality for most species.
#' Negative lengths will be set to 0. Values << 0 can imply a relatively large
#' size at age-0; again unlikely for many species (life-bearers like sharks may be an exception).
#'
#' @describeIn LengthModels von Bertalanffy growth function
#'
#' @export
vonBert <- function(Ages, Linf, K, t0) {
  Pars <- list(Linf=Structure(Linf, out=c('nsim', 'nTS'), req='nsim'),
               K=Structure(K, out=c('nsim', 'nTS'), req='nsim'),
               t0=Structure(t0, out=c('nsim', 'nTS'), req='nsim'))
  # Check(list(Pars))
  vonBert_(Ages, Pars)
}
class(vonBert) <- 'Length-at-Age-Model'

# TODO convert to C++
vonBert_ <- function(Ages, Pars) {
  Linf <- Pars$Linf
  K <- Pars$K
  t0 <- Pars$t0

  dim_Linf <- dim(Linf)
  dim_K <- dim(K)
  dim_t0 <- dim(t0)

  nsim_Linf <- dim_Linf[1]
  nsim_K <- dim_K[1]
  nsim_t0 <- dim_t0[1]

  nTS_Linf <- dim_Linf[2]
  nTS_K <- dim_K[2]
  nTS_t0 <- dim_t0[2]

  nsim <- max(nsim_Linf, nsim_K, nsim_t0)
  nTS <- max(nTS_Linf, nTS_K, nTS_t0)
  LAA <- array(0, dim=c(nsim, length(Ages), nTS))

  for (s in 1:nsim) {
    for (ts in 1:nTS) {
      tLinf <- Linf[GetIndex(s, nsim_Linf), GetIndex(ts, nTS_Linf)]
      tK <- K[GetIndex(s, nsim_K), GetIndex(ts, nTS_K)]
      tt0 <- t0[GetIndex(s, nsim_t0), GetIndex(ts, nTS_t0)]
      len <- tLinf * (1-exp(-tK*(Ages-tt0)))
      len[len<0] <- 0
      LAA[s,,ts] <- len
    }
  }
  LAA
}


## ---- Weight ----


#' Weight Models
#'
#'  Print a list of valid Weight-at-Age or Weight-at-Length models
#'
#' @param full Logical. Provide a complete table (TRUE) or just the model names (FALSE)?
#' @param print Logical. Print out the results (TRUE) or just return the data.frame (FALSE)?
#'
#' @return Prints to console and invisible data.frame or model names
#' @export
WeightModels <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Weight-at-Age-Model',
                            'Weight-at-Length-Model'), full, print)
}


#'

#' Allometric Weight at Mean Length
#'
#' @param Length Numeric vector of mean length-at-age
#' @param alpha The unit weight in units of [Weight()] used in the [Stock()] object
#' @param beta Exponent, typically around 3
#' @export
WeightatMeanLength <- function(Length, alpha, beta) {
  Pars <- list(a=Structure(alpha, out=c('nsim', 'nTS'), req='nsim'),
               b=Structure(beta, out=c('nsim', 'nTS'), req='nsim'))
  # Check(list(Length, Pars))
  allometric_(Length, Pars)
}
class(WeightatMeanLength) <- 'Weight-at-Age-Model'


#' Allometric Weight at Length
#'
#'
#' @param Length Numeric vector of length classes
#' @param Alpha The unit weight in units of [Weight()] used in the [Stock()] object
#' @param Beta Exponent, typically around 3
#' @export
WeightatLength <- function(Length, Alpha, Beta) {
  Pars <- list(a=Structure(Alpha, out=c('nsim', 'nTS'), req='nsim'),
               b=Structure(Beta, out=c('nsim', 'nTS'), req='nsim'))
  # Check(list(Length, Pars))
  allometric_(Length, Pars)
}
class(WeightatLength) <- 'Weight-at-Length-Model'


allometric_ <- function(Length, Pars) {
  Length <- Structure(Length)
  a <- Pars$a
  b <- Pars$b

  dim_Length <- dim(Length)

  dims <- data.frame(a=dim(a), b=dim(b), Length=dim_Length[c(1,3)])
  nsim <- max(dims[1,])
  nts <- max(dims[2,])

  weight <- array(0, dim=c(nsim, dim_Length[2], nts))
  for (s in 1:nsim) {
    for (ts in 1:nts) {
      weight[s,,ts] <- a[GetIndex(s, dims$a[1]), GetIndex(ts, dims$a[2])] *
        Length[GetIndex(s, dims$Length[1]), , GetIndex(ts, dims$Length[2])]^ b[GetIndex(s, dims$b[1]), GetIndex(ts, dims$b[2])]
    }
  }
  weight
}


#' Allometric Weight at Age
#'
#' @param Ages A numeric vector of age classes
#' @param a The unit weight in units of [Weight()] used in the [Stock()] object
#' @param b Exponent, typically around 3
#'
#' @export
WeightatAge <- function(Ages, a, b) {
  Pars <- list(alpha=Structure(a, out=c('nsim', 'nTS'), req='nsim'),
               beta=Structure(b, out=c('nsim', 'nTS'), req='nsim'))
  # Check(list(Ages, Pars))
  age_weight_(Ages, Pars)
}
class(WeightatAge) <- 'Weight-at-Age-Model'

age_weight_ <- function(Ages, Pars) {
  alpha <- Pars$alpha
  beta <- Pars$beta

  dims <- data.frame(alpha=dim(alpha), beta=dim(beta))
  nsim <- max(dims[1,])
  nts <- max(dims[2,])

  weight <- array(0, dim=c(nsim, length(Ages), nts))
  for (s in 1:nsim) {
    for (ts in 1:nts) {
      weight[s,,ts] <- alpha[GetIndex(s, dims$alpha[1]), GetIndex(ts, dims$alpha[2])] *
        Ages^ beta[GetIndex(s, dims$beta[1]), GetIndex(ts, dims$beta[2])]
    }
  }
  weight
}

## ---- Natural Mortality ----

#' Natural Mortality Models
#'
#' Functions for generating natural mortality-at-age.
#'
#'
#' @return Natural mortality-at-age values
#' @name NaturalMortalityModels
#' @examples
#' NaturalMortalityModels()
NULL

#' @describeIn NaturalMortalityModels Print a list of valid M-at-Age or M-at-Length models
#' @param full Logical. Provide a complete table (TRUE) or just the model names (FALSE)?
#' @param print Logical. Print out the results (TRUE) or just return the data.frame (FALSE)?
#'
#' @return Prints to console and invisible data.frame or model names
#' @export
NaturalMortalityModels <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('NaturalMortality-at-Age-Model',
                            'NaturalMortality-at-Length-Model'), full, print)
}


#' @describeIn NaturalMortalityModels Description
#' @param Ages Number of age classes or vector of age classes
#' @param M Natural mortality rate
#' @export
MortalityatAge <- function(Ages, M) {
  if (length(Ages)>1) {
    nage <- length(Ages)
  } else {
    nage <- Ages
  }

  if (all(dim(M)==1)) {
    return(array(replicate(nage,M), dim=c(1, nage, 1)))
  }

  if (length(dim(M))==3) {
    if (!dim(M)[2] == nage)
      cli::cli_abort('Length of `Age` dimension (dimension 2) for `M` must be equal to `length(Ages)`')
    return(M)
  }

  replicate(nage,M) |>  aperm(c(1,3,2))
}
class(MortalityatAge) <- 'NaturalMortality-at-Age-Model'


# Allometric M-at-Length

# https://pdf.sciencedirectassets.com/271306/1-s2.0-S0165783622X00088/1-s2.0-S0165783622002314/am.pdf

#' @describeIn NaturalMortalityModels See link above ...
#' @param Length Numeric vector of length classes
#' @param M Natural mortality rate at `RefLength`
#' @param RefLength Reference length for `M`
#' @param c Exponent
#' @export
AllometricMortalityatLength <- function(Length, M, RefLength, c) {
  Pars <- list(M=Structure(M, out=c('nsim', 'nTS'), req='nsim'),
               RefLength=Structure(RefLength, out=c('nsim', 'nTS'), req='nsim'),
               c=Structure(c, out=c('nsim', 'nTS'), req='nsim'))

  allometric_mat_length(Length, Pars)

}
class(AllometricMortalityatLength) <- 'NaturalMortality-at-Length-Model'


allometric_mat_length <- function(Length, Pars) {
  Length <- Structure(Length)
  M <- StructurePars(Pars$M)[[1]]
  RefLength <- StructurePars(Pars$RefLength)[[1]]
  c <- StructurePars(Pars$c)[[1]]

  dim_Length <- dim(Length)

  dims <- data.frame(M=dim(M), RefLength=dim(RefLength), c=dim(c), Length=dim_Length[c(1,3)])
  nsim <- max(dims[1,])
  nts <- max(dims[2,])

  naturalmortality <- array(0, dim=c(nsim, dim_Length[2], nts))
  for (s in 1:nsim) {
    for (ts in 1:nts) {
      naturalmortality[s,,ts] <- M[GetIndex(s, dims$M[1]), GetIndex(ts, dims$M[2])] *
        (Length[GetIndex(s, dims$Length[1]), , GetIndex(ts, dims$Length[2])]/
           RefLength[GetIndex(s, dims$RefLength[1]), GetIndex(ts, dims$RefLength[2])])^c[GetIndex(s, dims$c[1]), GetIndex(ts, dims$c[2])]
    }
  }
  naturalmortality
}

## ---- Maturity ----

#' Maturity-at-Length or -Age Models
#'
#' Functions for generating maturity-at-age or -at-length.
#'
#'
#' @return Maturity-at-length or -age values
#' @name MaturityModels
#' @examples
#' MaturityModels()
NULL

#' @describeIn MaturityModels Print a list of maturity-at-Age or maturity-at-Length models
#' @param full Logical. Provide a complete table (TRUE) or just the model names (FALSE)?
#' @param print Logical. Print out the results (TRUE) or just return the data.frame (FALSE)?
#'
#' @return Prints to console and invisible data.frame or model names
#' @export
MaturityModels <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Maturity-at-Age-Model',
                            'Maturity-at-Length-Model'),
               full, print)
}

#' @describeIn MaturityModels Print a list of valid Maturity-at-Length models
#'
#' @export
MaturityModelsLength <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Maturity-at-Length-Model'),
               full, print)
}

#' @describeIn MaturityModels Print a list of valid Maturity-at-Age models
#'
#' @export
MaturityModelsAge <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Maturity-at-Age-Model'),
               full, print)
}

#' @describeIn MaturityModels Logistic maturity-at-length model
#' @param Length A numeric vector of lengths
#' @param L50 Length corresponding with 50% maturity
#' @param L50_95 Interval between `L50` and length at 95% maturity (`L95`)
#' @export
MaturityAtLength <- function(Length, L50, L50_95) {
  Pars <- list(L50=Structure(L50, out=c('nsim', 'nTS'), req='nsim'),
               L50_95=Structure(L50_95, out=c('nsim', 'nTS'), req='nsim'))
  Maturity_at_Length_(Length, Pars)
}
class(MaturityAtLength) <- 'Maturity-at-Length-Model'

Maturity_at_Length_ <- function(Length, Pars) {

  L50 <- Pars$L50
  L50_L95 <- Pars$L50_95

  dim_L50 <- dim(L50)
  dim_L50_L95 <- dim(L50_L95)
  nLength <- length(Length)

  nsim_L50 <- dim_L50[1]
  nTS_L50 <- dim_L50[2]
  nsim_L50_L95 <- dim_L50_L95[1]
  nTS_L50_L95 <- dim_L50_L95[2]

  nsim <- max(nsim_L50, nsim_L50_L95)
  nTS <- max(nTS_L50, nTS_L50_L95)

  MAL <- array(0, dim=c(nsim, nLength, nTS))
  for (s in 1:nsim) {
    for (ts in 1:nTS) {
      MAL[s,,ts] <- Logistic1(Length,
                              L50[GetIndex(s, nsim_L50), GetIndex(ts, nTS_L50)],
                              L50_L95[GetIndex(s, nsim_L50_L95), GetIndex(ts, nTS_L50_L95)]
      )
    }
  }
  MAL
}

# Two-parameter logistic function asymptote at 1
Logistic1 <- function(x, p50, p50_p95, max=1) {
  max/(1 + exp(-log(19) * ((x - p50)/(p50_p95))))
}

#' @describeIn MaturityModels Logistic maturity-at-age model
#' @param Ages A numeric vector of ages
#' @param A50 Age corresponding with 50% maturity
#' @param A50_95 Interval between `A50` and age at 95% maturity (`A95`)
#' @export
MaturityAtAge <- function(Ages, A50, A50_95) {

  Pars <- list(A50=Structure(A50, out=c('nsim', 'nTS'), req='nsim'),
               A50_95=Structure(A50_95, out=c('nsim', 'nTS'), req='nsim'))

  Maturity_at_Age_(Ages, Pars)
}
class(MaturityAtAge) <- 'Maturity-at-Age-Model'

Maturity_at_Age_ <- function(Ages, Pars) {
  A50 <- Pars$A50
  A50_95 <- Pars$A50_95

  dim_A50 <- dim(A50)
  dim_A50_A95 <- dim(A50_95)
  nAge <- length(Ages)

  nsim_A50 <- dim_A50[1]
  nTS_A50 <- dim_A50[2]
  nsim_A50_95 <- dim_A50_A95[1]
  nTS_A50_95 <- dim_A50_A95[2]

  nsim <- max(nsim_A50, nsim_A50_95)
  nTS <- max(nTS_A50, nTS_A50_95)

  MAL <- array(0, dim=c(nsim, nAge, nTS))
  for (s in 1:nsim) {
    for (ts in 1:nTS) {
      MAL[s,,ts] <- Logistic1(Ages,
                              A50[GetIndex(s, nsim_A50), GetIndex(ts, nTS_A50)],
                              A50_95[GetIndex(s, nsim_A50_95), GetIndex(ts, nTS_A50_95)]
      )
    }
  }
  MAL
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


## ---- SRR ----

#' Stock-Recruitment Models
#'
#' Description
#'
#' @param alpha See `Details`
#' @param beta See `Details`
#' @param S spawning output. ... Fecundity
#'
#'
#' @details
#'
#' ## Beverton-Holt
#'
#' ## Ricker
#'
#' ## Hockey Stick
#'
#'
#' Density-independent recruits per unit spawning out (`S`). The
#' slope of the SRR curve near $S=0$.
#'
#' Density-dependent parameter, proportional to both fecundity
#' and density-dependent mortality
#'
#' @name SRRModels
NULL


#' @describeIn SRRModels Print a list of stock-recruit models
#' @param full Logical. Provide a complete table (TRUE) or just the model names (FALSE)?
#' @param print Logical. Print out the results (TRUE) or just return the data.frame (FALSE)?
#'
#' @return Prints to console and invisible data.frame or model names
#' @export
SRRModels <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('SRR-Model'),
               full, print, Independent=c('S', 'S0'))

}

h2alpha <- function(h, phi0, SR=c('BH', 'RK')) {
  SR <- match.arg(SR)
  switch(SR,
         "BH" = 4*h/(1-h)/phi0,
         "RK" = (5*h)^1.25/phi0)
}


alpha2h <- function(alpha, phi0, SR=c('BH', 'RK')) {
  SR <- match.arg(SR)
  switch(SR,
         "BH" = alpha*phi0/(4 + alpha*phi0),
         "RK" = 0.2 * (alpha*phi0)^0.8
  )
}

h_R0_2beta <- function(h, R0, phi0, SR=c('BH', 'RK')) {
  SR <- match.arg(SR)
  switch(SR,
         "BH" = (5*h-1)/(1-h)/phi0/R0,
         "RK" = log((5*h)^1.25)/phi0/R0
  )
}

alpha_beta2R0 <- function(alpha, beta, phi0, SR=c('BH', 'RK')) {

}

#' @describeIn SRRModels Beverton-Holt Stock-Recruitment Model
#' @param S Current total spawning output (often spawning biomass)
#' @param S0 Equilibrium unfished spawning output
#' @param R0 Equilibrium expected recruitment when stock is unfished
#' @param h Steepness of the Beverton-Holt SRR
#'
#' @export
BevertonHolt <- function(S, S0, R0, h) {
  phi0 <- S0/R0
  alpha <- h2alpha(h, phi0, 'BH')
  beta <- h_R0_2beta(h, R0, phi0, 'BH')
  alpha * S / (1+beta*S)
}
class(BevertonHolt) <- 'SRR-Model'


#' @describeIn SRRModels Ricker Stock-Recruitment Model
#' @param RKh Steepness for the Ricker SRR. Argument name indicates it's the
#' Ricker SRR
#' @export
Ricker <- function(S, S0, R0, RKh) {
  phi0 <- S0/R0
  alpha <- h2alpha(RKh, phi0, 'RK')
  beta <- h_R0_2beta(RKh, R0, phi0, 'RK')
  alpha * S * exp(-beta*S)
}
class(Ricker) <- 'SRR-Model'


#' @describeIn SRRModels Hockey Stick Stock-Recruitment Model
#' @param Shinge The hinge-point of the SRR, relative to `S0`
#'
#' @export
HockeyStick <- function(S, S0, R0, Shinge) {
  Shinge <- h*S0
  R0/(Shinge + Shinge) *  (S + Shinge - sqrt((S - Shinge)^2))
}
class(HockeyStick) <- 'SRR-Model'

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
                            'Selectivity-at-Length-Model'),
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

#' @describeIn SelectivityModels Logistic selectivity-at-length model
#' @param Length A numeric vector of lengths
#' @param SL50 Length corresponding with 50% selectivity
#' @param SL50_95 Interval between `SL50` and length at 95% selectivity (`SL95`)
#' @export
SelectivityAtLength <- function(Length, SL50, SL50_95) {
  Pars <- list(SL50=Structure(SL50, out=c('nsim', 'nTS'), req='nsim'),
               SL50_95=Structure(SL50_95, out=c('nsim', 'nTS'), req='nsim'))
  Maturity_at_Length_(Length, Pars)
}
class(SelectivityAtLength) <- 'Selectivity-at-Length-Model'

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
                            'Retention-at-Length-Model'),
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

#' @describeIn RetentionModels Logistic retention-at-length model
#' @param Length A numeric vector of lengths
#' @param L50 Length corresponding with 50% retention
#' @param L50_95 Interval between `RL50` and length at 95% retention (`RL95`)
#' @export
RetentionAtLength <- function(Length, L50, L50_95) {
  Pars <- list(SL50=Structure(L50, out=c('nsim', 'nTS'), req='nsim'),
               SL50_95=Structure(L50_95, out=c('nsim', 'nTS'), req='nsim'))
  Maturity_at_Length_(Length, Pars)
}
class(RetentionAtLength) <- 'Retention-at-Length-Model'

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

