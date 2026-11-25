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
#' @param zfrac Survival-based SRR: reduction in the log-scale pre-recruit
#'   mortality at very low spawning production, as a fraction of the unfished
#'   value. A named argument in `Pars`. 0 <= zfrac <= 1
#' @param Beta Survival-based SRR: shape of the density-dependent survival
#'   curve. A named argument in `Pars`. Beta > 0. Default 1.
#' @param RecFrac Survival-based SRR: fraction of the recruits from `S` that
#'   enter this stock. 0 < RecFrac <= 1. Default 1. In an operating model it
#'   is calculated from `R0` and is not part of `Pars`.

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
#' ## Parameterisation
#'
#' In [SRR()], `Pars` for `BevertonHolt` and `Ricker` may be given either as
#' steepness (`list(h = ...)` or `list(hR = ...)`, with `R0`) or as
#' `list(alpha = ..., beta = ...)` with `R0 = NULL`. Alpha-beta values are
#' converted to steepness and `R0` during [Populate()] using the first-year
#' unfished spawning production per recruit \eqn{\phi_0}. See [SRRSteepness()]
#' for the conversion.
#'
#' ## Beverton–Holt
#'
#' * `BevertonHolt()` evaluates expected recruitment:
#' \deqn{R = \frac{\alpha S}{1 + \beta S}}
#' where \eqn{\phi_0 = S_0 / R_0},
#' \eqn{\alpha = 4h / ((1 - h)\phi_0)}, and
#' \eqn{\beta = (5h - 1) / ((1 - h)\phi_0 R_0)}.
#'
#' * `BevertonHolt_RelRec()` evaluates equilibrium recruitment relative to unfished
#' recruitment as a function of SPR.
#'
#' ## Ricker
#'
#' * `Ricker()` evaluates expected recruitment:
#' \deqn{R = \alpha S e^{-\beta S}}
#' where \eqn{\alpha = (5h_R)^{1.25} / \phi_0} and
#' \eqn{\beta = \log((5h_R)^{1.25}) / (\phi_0 R_0)}.
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
#' ## SurvivalSRR
#'
#' * `SurvivalSRR()` evaluates the survival-based relationship of Taylor et al.
#' (2013) for low-fecundity species, where `S` is the number of offspring
#' (e.g., pups) and recruitment is offspring times pre-recruit survival:
#' \deqn{R = S e^{z}, \quad z = z_0 + (z_{min} - z_0)\left(1 - (S/S_0)^{\beta}\right)}
#' where \eqn{z_0 = \log(R_0 / S_0) = -\log\phi_0} and
#' \eqn{z_{min} = z_0 (1 - z_{frac})}. Recruitment cannot exceed `S`
#' when \eqn{\phi_0 > 1}. When several stocks recruit from the same
#' spawning production (e.g., the sexes of a two-sex model), each stock
#' receives a fraction `RecFrac` of the recruits, and \eqn{\phi_0} is the
#' spawning production per total recruit: \eqn{R = RecFrac\, S e^{z}} with
#' \eqn{z_0 = \log(R_0 / (RecFrac\, S_0))}, where `R0` is the stock's own
#' unfished recruitment. In an operating model, `RecFrac` is the stock's
#' share of the summed `R0` of all stocks with the same `SPFrom` source(s)
#' and weights.
#'
#' * `SurvivalSRR_RelRec()` evaluates equilibrium relative recruitment as a
#' function of SPR. It requires `Pars$SPR0`, the unfished spawning production
#' per recruit of the stock; \eqn{\phi_0} is `RecFrac * SPR0`:
#' \deqn{R/R_0 = \frac{1}{SPR}\left(1 + \frac{\log SPR}{z_{frac}\log\phi_0}\right)^{1/\beta}}
#' and 0 when the term in brackets is not positive.
#'
#' @references
#' Taylor, I.G., Gertseva, V., Methot, R.D., Maunder, M.N. 2013. A
#' stock-recruitment relationship based on pre-recruit survival, illustrated
#' with application to spiny dogfish shark. Fisheries Research 142: 15-21.
#'
#' @return 
#' For stock–recruitment functions (`BevertonHolt()`, `Ricker()`, `HockeyStick()`, `SurvivalSRR()`):
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
#' @seealso [SRR()], [SRRSteepness()]
#'
#' @name SRRModels
NULL

# NOTE: these built-in SRR models have C++ equivalents in src/srr_models.cpp
# (called internally via inst/include/calc_recruitment.h); any change here
# should be matched in inst/include/srr_functions.h


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

# ---- SurvivalSRR (Taylor et al. 2013) ----
#' @rdname SRRModels
#' @export
SurvivalSRR <- function(S, S0, R0, zfrac, Beta = 1, RecFrac = 1) {
  .CheckSArg(S)
  .IsScalarNumeric(S0, 'S0')
  .IsScalarNumeric(R0, 'R0')
  .IsScalarNumeric(zfrac, 'zfrac')
  .IsScalarNumeric(Beta, 'Beta')
  .IsScalarNumeric(RecFrac, 'RecFrac')
  .CheckSurvivalPars(zfrac, Beta, RecFrac)
  SurvivalSRR_cpp(S, S0, R0, zfrac, Beta, RecFrac)
}
class(SurvivalSRR) <- "SRR-Model"

#' @rdname SRRModels
#' @export
SurvivalSRR_RelRec <- function(Pars, SPR) {
  RecFrac <- Pars$RecFrac %||% 1
  .CheckParsScalarNumeric(Pars, c('zfrac', 'Beta', 'SPR0'))
  .IsScalarNumeric(SPR, 'SPR')
  .IsScalarNumeric(RecFrac, 'Pars$RecFrac')
  .CheckSurvivalPars(Pars$zfrac, Pars$Beta, RecFrac)
  if (SPR <= 0)
    return(0)
  base <- 1 + log(SPR) / (Pars$zfrac * log(RecFrac * Pars$SPR0))
  if (!is.finite(base) || base <= 0)
    return(0)
  base^(1 / Pars$Beta) / SPR
}

.CheckSurvivalPars <- function(zfrac, Beta, RecFrac = 1) {
  if (zfrac < 0 || zfrac > 1)
    cli::cli_abort(c("x" = "{.val zfrac} must be in [0, 1]", "i" = "Currently: {.val {zfrac}}"), call = NULL)
  if (Beta <= 0)
    cli::cli_abort(c("x" = "{.val Beta} must be > 0", "i" = "Currently: {.val {Beta}}"), call = NULL)
  if (RecFrac <= 0 || RecFrac > 1)
    cli::cli_abort(c("x" = "{.val RecFrac} must be in (0, 1]", "i" = "Currently: {.val {RecFrac}}"), call = NULL)
}



# ---- Alpha-Beta ----

#' Convert Between Steepness and Alpha-Beta SRR Parameters
#'
#' Converts `BevertonHolt` and `Ricker` stock-recruitment parameters between
#' the steepness parameterisation (steepness and `R0`) and the alpha-beta
#' parameterisation, given the unfished spawning production per recruit
#' `phi0`.
#'
#' @param alpha Recruits per unit of spawning production at low spawning
#'   production (slope at the origin).
#' @param beta Density-dependence parameter, in units of 1/spawning production.
#' @param h Steepness: `h` for `BevertonHolt`, `hR` for `Ricker`.
#' @param R0 Unfished equilibrium recruitment.
#' @param phi0 Unfished spawning production per recruit, e.g., from [CalcSPR0()].
#' @param Model `"BevertonHolt"` or `"Ricker"`.
#'
#' @details
#' Beverton-Holt: \eqn{R = \alpha S / (1 + \beta S)}, with
#' \eqn{h = \alpha\phi_0 / (4 + \alpha\phi_0)} and
#' \eqn{R_0 = (\alpha\phi_0 - 1) / (\beta\phi_0)}.
#'
#' Ricker: \eqn{R = \alpha S e^{-\beta S}}, with
#' \eqn{h_R = (\alpha\phi_0)^{0.8} / 5} and
#' \eqn{R_0 = \log(\alpha\phi_0) / (\beta\phi_0)}.
#'
#' Both require \eqn{\alpha\phi_0 > 1}; otherwise the stock cannot persist
#' in the absence of fishing.
#'
#' Arguments are vectorised and recycled.
#'
#' @return
#' `SRRSteepness()`: a named list with elements `h` (`hR` for `Ricker`) and `R0`.
#'
#' `SRRAlphaBeta()`: a named list with elements `alpha` and `beta`.
#'
#' @example man-examples/srr-alpha-beta.R
#' @seealso [SRR()], [SRRModels()], [CalcSPR0()]
#' @export
SRRSteepness <- function(alpha, beta, phi0, Model = c("BevertonHolt", "Ricker")) {
  Model <- match.arg(Model)
  CR <- alpha * phi0
  if (any(!is.finite(CR) | CR <= 1) || any(!is.finite(beta) | beta <= 0))
    cli::cli_abort(c("x" = "{.arg alpha} * {.arg phi0} must be > 1 and {.arg beta} must be > 0.",
                     "i" = "Range of {.arg alpha} * {.arg phi0}: {.val {signif(range(CR), 3)}}"))
  if (Model == "BevertonHolt")
    return(list(h = CR / (4 + CR), R0 = (CR - 1) / (beta * phi0)))
  list(hR = CR^0.8 / 5, R0 = log(CR) / (beta * phi0))
}

#' @rdname SRRSteepness
#' @export
SRRAlphaBeta <- function(h, R0, phi0, Model = c("BevertonHolt", "Ricker")) {
  Model <- match.arg(Model)
  if (Model == "BevertonHolt")
    return(list(alpha = 4 * h / ((1 - h) * phi0),
                beta  = (5 * h - 1) / ((1 - h) * phi0 * R0)))
  CR <- (5 * h)^1.25
  list(alpha = CR / phi0, beta = log(CR) / (phi0 * R0))
}

#' @rdname SRRModels
#' @param full Logical. Provide a complete table (TRUE) or just the model names (FALSE)?
#' @param print Logical. Print out the results (TRUE) or just return the data.frame (FALSE)?
#' 
#' @export
SRRModels <- function(full=TRUE, print=TRUE) {
  .ReturnModels(ModelClass=c('SRR-Model'),
               full, print, Independent=c('S', 'S0', 'R0', 'RecFrac'))
  
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
