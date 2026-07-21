#' Stock–Recruitment Relationships (SRR)
#'
#' Deterministic stock–recruitment and relative-recruitment models evaluated for a
#' single simulation and a single year.
#'
#' @param S Spawning production (biomass or eggs). Numeric length >= 1
#' @param S0 Unfished equilibrium spawning production. Numeric scalar.
#' @param R0 Unfished equilibrium recruitment. Numeric scalar.
#' @param Pars Named list of parameters required by a specific SRR model.
#' @param h Beverton–Holt steepness. A named argument in `Pars`. 0.2 < h < 1
#' @param hR Ricker steepness. A named argument in `Pars`. 
#' @param Shinge Hockey-stick hinge point **relative to `S0`**. A named argument in `Pars`.0 < Shinge <= 1

#' @param SPR Spawning-per-recruit multiplier. Numeric scalar.
#'
#' @details
#'
#' This family of functions provides:
#'
#' * **Stock–recruitment functions** that return expected recruitment given
#' spawning production.
#' * **Relative-recruitment functions** that return equilibrium recruitment
#' relative to `R0` as a function of spawning-per-recruit (SPR).
#' 
#' ## Model listing
#'
#' `SRRModels()` prints or returns the set of stock–recruitment models
#' available in the package.
#'
#' ## Beverton–Holt
#'
#' * `BevertonHolt()` evaluates expected recruitment:
#' \deqn{R = \frac{\alpha S}{1 + \beta S}}
#'
#' * `BevertonHolt_RelRec()` evaluates equilibrium recruitment relative to unfished
#' recruitment as a function of SPR.
#'
#' ## Ricker
#'
#' * `Ricker()` evaluates expected recruitment:
#' \deqn{R = \alpha S e^{-\beta S}}
#'
#' * `Ricker_RelRec()` evaluates equilibrium relative recruitment as a function
#' of SPR.
#'
#' ## Hockey Stick
#'
#' * `HockeyStick()` evaluates recruitment assuming linear increase up to a hinge
#' point followed by saturation at `R0`:
#' \deqn{R = \begin{cases}
#' \frac{R0}{Shinge} S, & S < Shinge \\
#' R0, & S \ge Shinge
#' \end{cases}}
#'
#' * `HockeyStick_RelRec()` evaluates relative equilibrium recruitment as a
#' function of SPR.
#'
#' @return 
#' For stock–recruitment functions (`BevertonHolt()`, `Ricker()`, `HockeyStick()`):
#' A numeric scalar giving expected recruitment for the specified spawning production.
#' 
#' For relative-recruitment functions (`*_RelRec()`):
#'   A numeric scalar giving equilibrium recruitment relative to unfished recruitment
#'   (`R / R0`) as a function of spawning-per-recruit (SPR).
#'   
#'   For `SRRModels()`:
#'     If `print = TRUE`, prints the available stock–recruitment models to the console
#'     and invisibly returns the result. If `print = FALSE`, returns a character vector
#'     or data frame describing the available models.
#' 
#' @example man-examples/models-srr.R
#' 
#' @seealso [SRR()]
#'
#' @name SRRModels
NULL

# NOTE: these built-in SRR models have C++ equivalents in src/srr_models.cpp
# (called internally via inst/include/calc_recruitment.h); any change here
# should be matched in inst/include/srr_models.h


# ---- Beverton-Holt ----

#' @rdname SRRModels
#' @export
BevertonHolt <- function(S, S0, R0, h) {
  .CheckSArg(S)
  .IsScalarNumeric(S0, 'S0')
  .IsScalarNumeric(R0, 'R0')
  .IsScalarNumeric(h, 'h')
  
  # phi0 <- S0 / R0
  # alpha <- 4 * h / ((1 - h) * phi0)
  # beta <- (5 * h - 1) / ((1 - h) * phi0 * R0)
  # alpha * S / (1 + beta * S)
  
  BevertonHolt_cpp(S, S0, R0, h)
}
class(BevertonHolt) <- "SRR-Model"


#' @rdname SRRModels
#' @export
BevertonHolt_RelRec <- function(Pars, SPR) {
  .CheckParsScalarNumeric(Pars, 'h')
  .IsScalarNumeric(SPR, 'SPR')
  h <- Pars$h
  CR <- 4 * h / (1 - h)
  relrec <- (CR * SPR - 1) / ((CR - 1) * SPR)
  pmax(relrec, 0)
}

# ---- Ricker ----

#' @rdname SRRModels
#' @export
Ricker <- function(S, S0, R0, hR) {
  .CheckSArg(S)
  .IsScalarNumeric(S0, 'S0')
  .IsScalarNumeric(R0, 'R0')
  .IsScalarNumeric(hR, 'hR')
  
  # phi0 <- S0 / R0
  # alpha <- (5 * hR)^1.25 / phi0
  # beta <- log((5 * hR)^1.25) / (phi0 * R0)
  # alpha * S * exp(-beta * S)
  
  Ricker_cpp(S, S0, R0, hR)
}
class(Ricker) <- "SRR-Model"

#' @rdname SRRModels
#' @export
Ricker_RelRec <- function(Pars, SPR) {
  .CheckParsScalarNumeric(Pars, 'hR')
  .IsScalarNumeric(SPR, 'SPR')
  
  hR <- Pars$hR
  CR <- (5 * hR)^1.25
  relrec <- (log(CR * SPR) / log(CR))
  pmax(relrec, 0)
}

# ---- Hockey Stick ----
#' @rdname SRRModels
#' @export
HockeyStick <- function(S, S0, R0, Shinge) {
  .CheckSArg(S)
  .IsScalarNumeric(S0, 'S0')
  .IsScalarNumeric(R0, 'R0')
  .IsScalarNumeric(Shinge, 'Shinge')
  
  if (Shinge <= 0 || Shinge > 1) {
    cli::cli_abort(c("x"="{.val Shinge} must be in (0, 1]",
                     "i"="Currently: {.val {Shinge}}"
    ))
  }
  
  # S_hinge <- S0 * Shinge
  # expR <- (R0/(2*S_hinge)) * ((S+S_hinge) - abs((S-S_hinge)))
  # pmax(expR, 0)
  HockeyStick_cpp(S, S0, R0, Shinge)
}

class(HockeyStick) <- "SRR-Model"

#' @rdname SRRModels
#' @export
HockeyStick_RelRec <- function(Pars, SPR) {
  .CheckParsScalarNumeric(Pars, c("Shinge"))
  .IsScalarNumeric(SPR, "SPR")
  ifelse(SPR >= Pars$Shinge, 1, 0)
}



#' @rdname SRRModels
#' @param full Logical. Provide a complete table (TRUE) or just the model names (FALSE)?
#' @param print Logical. Print out the results (TRUE) or just return the data.frame (FALSE)?
#' 
#' @export
SRRModels <- function(full=TRUE, print=TRUE) {
  .ReturnModels(ModelClass=c('SRR-Model'),
               full, print, Independent=c('S', 'S0', 'R0'))
  
}



.CheckSArg <- function(S) {
  if (!is.numeric(S) || any(is.na(S)) || !length(S)>0) {
    cli::cli_abort("{.val `S`} must be numeric vector", call.=NULL)
  }
}

.IsScalarNumeric <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.val {name}} must be a numeric scalar", call.=NULL)
  }
}

.CheckParsScalarNumeric <- function(Pars, names) {
  for (name in names) {
    if (is.null(Pars[[name]])) {
      cli::cli_abort("{.val {name}} is required", call.=NULL)
    }
    .IsScalarNumeric(Pars[[name]], paste0("Pars$", name))
  }
}
