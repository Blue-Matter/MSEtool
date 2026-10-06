#' Settings for the SPiCT Surplus Production Model
#'
#' Controls for [FitSP()] and [SurplusProduction()] with `Model = 'spict'`,
#' which fit the stochastic surplus production model in continuous time
#' (SPiCT) with the `spict` package. `spict` is not on CRAN; install it with
#' `pak::pkg_install('DTUAqua/spict/spict')`.
#'
#' The model shape, initial depletion, index weighting, and priors are set
#' with the [FitSP()] arguments (`Shape`, `EstShape`, `Depletion`,
#' `EstDepletion`, `IndexSD`, `IndexWeight`, `Priors`), which are converted to
#' the equivalent `spict` inputs:
#'
#' - `EstShape = FALSE` fixes `n` at `Shape`; `EstShape = TRUE` estimates
#'   `n` with a lognormal prior, `Priors$Shape` if given, otherwise with
#'   median `Shape` and CV `ShapeCV`.
#' - `Depletion` is the starting value of the initial depletion
#'   (`logbkfrac`). With `EstDepletion = FALSE` it is held close to
#'   `Depletion` by a lognormal prior with CV `FixedDepletionCV`; with
#'   `EstDepletion = TRUE` it has the prior `Priors$Depletion`, if given.
#' - `IndexWeight` and, with `IndexSD = 'data'`, the relative index `CV`s set
#'   the `stdevfacI` scaling of each index observation's standard deviation.
#'   The standard deviation of each index is still estimated.
#' - `Priors$FMSY` sets a prior on the intrinsic growth rate, `FMSY * n`
#'   (with `n = Shape`), and `Priors$MSY` a prior on `m` (MSY).
#' - `Start` (or the warm start in [SurplusProduction()]) sets the starting
#'   values of `m` and `K`.
#' - Catch observations cannot be zero, so a year (or season) with zero
#'   catch is combined with the preceding catch observation (or the following
#'   one, before the first positive catch) into one observation of the total
#'   catch over the longer interval.
#'
#' A fit is converged when the Hessian of the objective function is positive
#' definite and, for each estimated parameter, the Newton step (the inverse
#' Hessian times the gradient) is below `GradTol`. If the fit does not
#' converge, it is repeated from the default `spict` starting values (when
#' the first fit used `Start`) and then with the starting value of `K` at
#' each multiple of the maximum catch in `RestartK`, and the converged fit
#' with the lowest objective function value is used.
#'
#' @param dteuler Positive number. Euler time step (years) of the model.
#'   Default `1`. With sub-annual catch (`SeasonalCatch = TRUE`), the time
#'   step is at most the catch interval.
#' @param SeasonalCatch Logical. Fit to the seasonal catch of seasonal
#'   `Data` (`Data@@Seasons > 1`) rather than the annual total? Default
#'   `FALSE`.
#' @param Robust Logical. Use robust (mixture) observation error for the
#'   catch and indices (`robflagc`, `robflagi`)? Default `FALSE`.
#' @param Priors Named list of `spict` priors (e.g. `logalpha`, `logbeta`),
#'   each `c(mean, sd, use)` on the log scale. These are applied after, and
#'   replace, any priors set from the [FitSP()] arguments.
#' @param FixedDepletionCV Positive number. CV of the prior on the initial
#'   depletion when `EstDepletion = FALSE`. Default `0.01`.
#' @param stabilise Logical. Use the `spict` vague stabilising priors?
#'   Default `TRUE`.
#' @param optimiser Character. `'nlminb'` (default) or `'optim'`.
#' @param optimiser.control List of optimiser controls. Default
#'   `list(iter.max = 500, eval.max = 1000)`.
#' @param GradTol Positive number. Convergence tolerance on the Newton step;
#'   see Details. Default `1e-3`.
#' @param RestartK Numeric vector. Starting values of `K`, as multiples of the
#'   maximum catch, used for additional fits when a fit does not converge.
#'   `NULL` for no additional fits. Default `c(10, 2)`.
#' @param SDReport Character. When to run `TMB::sdreport()`, which is needed
#'   for the standard errors: `'auto'` (default; when `Uncertainty = TRUE` in
#'   [FitSP()], or when [SurplusProduction()] uses `FractileB`, `FractileF`,
#'   or `Diagnostics = 'full'`), `'always'`, or `'never'`.
#' @param getReportCovariance Logical. Passed to `TMB::sdreport()`. Default
#'   `FALSE`.
#' @param WarmStart Logical. In [SurplusProduction()], start each fit from
#'   the estimates of the previous management cycle? Default `TRUE`.
#' @param Quiet Logical. Suppress `spict` messages and warnings, recording
#'   warnings in the fit's `Log`? Default `TRUE`.
#' @param Inp Named list of any other `spict` `inp` elements. Applied last,
#'   so these replace any values set by `FitSP()` or the other arguments.
#'
#' @return A list of class `spictcontrol`.
#' @references Pedersen, M. W. and Berg, C. W. 2017. A stochastic surplus
#'   production model in continuous time. Fish and Fisheries 18: 226-243.
#' @seealso [FitSP()], [SurplusProduction()], [SPControl()]
#' @export
SpictControl <- function(dteuler             = 1,
                         SeasonalCatch       = FALSE,
                         Robust              = FALSE,
                         Priors              = list(),
                         FixedDepletionCV    = 0.01,
                         stabilise           = TRUE,
                         optimiser           = c('nlminb', 'optim'),
                         optimiser.control   = list(iter.max = 500, eval.max = 1000),
                         GradTol             = 1e-3,
                         RestartK            = c(10, 2),
                         SDReport            = c('auto', 'always', 'never'),
                         getReportCovariance = FALSE,
                         WarmStart           = TRUE,
                         Quiet               = TRUE,
                         Inp                 = list()) {
  optimiser <- match.arg(optimiser, c('nlminb', 'optim'))
  SDReport  <- match.arg(SDReport, c('auto', 'always', 'never'))
  if (!is.list(Priors) || !is.list(Inp))
    cli::cli_abort("{.arg Priors} and {.arg Inp} must be lists.")
  if (!(dteuler > 0))
    cli::cli_abort("{.arg dteuler} must be positive.")
  structure(
    list(dteuler = dteuler, SeasonalCatch = SeasonalCatch, Robust = Robust, Priors = Priors,
         FixedDepletionCV = FixedDepletionCV, stabilise = stabilise, optimiser = optimiser,
         optimiser.control = optimiser.control, GradTol = GradTol, RestartK = RestartK,
         SDReport = SDReport,
         getReportCovariance = getReportCovariance, WarmStart = WarmStart, Quiet = Quiet,
         Inp = Inp),
    class = 'spictcontrol'
  )
}

.SpictInp <- function(Prep, Shape, EstShape, Depletion, EstDepletion, IndexSD, Priors,
                      Control, Start) {
  inp <- if (Control$SeasonalCatch && !is.null(Prep$SubCatch)) {
    .SpictCatch(Prep$SubCatch$Catch, Prep$SubCatch$Time, Prep$SubCatch$dt)
  } else {
    .SpictCatch(Prep$Catch, Prep$Years, 1)
  }

  SDLog    <- sqrt(log(1 + Prep$CV^2))
  inp$obsI <- inp$timeI <- inp$stdevfacI <- list()
  for (i in seq_len(ncol(Prep$Index))) {
    ok <- is.finite(Prep$Index[, i]) & Prep$Index[, i] > 0
    if (!any(ok)) next
    k <- length(inp$obsI) + 1
    inp$obsI[[k]]  <- unname(Prep$Index[ok, i])
    inp$timeI[[k]] <- Prep$Years[ok] + Prep$Timing[i]
    Fac <- rep(1 / sqrt(Prep$Weight[i]), sum(ok))
    if (IndexSD == 'data') {
      s   <- SDLog[ok, i]
      Fac <- Fac * s / mean(s)
    }
    inp$stdevfacI[[k]] <- Fac
  }
  if (!length(inp$obsI))
    cli::cli_abort("No index observations in the fitted years.")

  inp$dteuler             <- min(Control$dteuler, inp$dtc)
  inp$timepredi           <- max(Prep$Years) + 1
  inp$stabilise           <- as.numeric(Control$stabilise)
  inp$optimiser           <- Control$optimiser
  inp$optimiser.control   <- Control$optimiser.control
  inp$getReportCovariance <- Control$getReportCovariance
  if (Control$Robust) {
    inp$robflagc <- 1
    inp$robflagi <- 1
  }

  SDFromCV <- function(CV) sqrt(log(1 + CV^2))
  inp$ini    <- list(logbkfrac = log(Depletion))
  inp$priors <- list()
  inp$phases <- list()
  if (EstShape) {
    inp$priors$logn <- c(log(Priors$Shape[1]), SDFromCV(Priors$Shape[2]), 1)
    inp$ini$logn    <- log(Priors$Shape[1])
  } else {
    inp$ini$logn    <- log(Shape)
    inp$phases$logn <- -1
    inp$priors$logn <- c(log(Shape), 1, 0)
  }
  if (!EstDepletion) {
    inp$priors$logbkfrac <- c(log(Depletion), SDFromCV(Control$FixedDepletionCV), 1)
  } else if (!is.null(Priors$Depletion)) {
    inp$priors$logbkfrac <- c(log(Priors$Depletion[1]), SDFromCV(Priors$Depletion[2]), 1)
  }
  if (!is.null(Priors$FMSY))
    inp$priors$logr <- c(log(Priors$FMSY[1] * Shape), SDFromCV(Priors$FMSY[2]), 1)
  if (!is.null(Priors$MSY))
    inp$priors$logm <- c(log(Priors$MSY[1]), SDFromCV(Priors$MSY[2]), 1)

  if (length(Start) && all(c('FMSY', 'MSY') %in% names(Start))) {
    n            <- if ('Shape' %in% names(Start) && EstShape) Start[['Shape']] else exp(inp$ini$logn)
    BMSYK        <- if (abs(n - 1) < 1e-3) exp(-1) else n^(1 / (1 - n))
    inp$ini$logm <- log(Start[['MSY']])
    inp$ini$logK <- log(Start[['MSY']] / (Start[['FMSY']] * BMSYK))
  }

  for (nm in names(Control$Priors))
    inp$priors[[nm]] <- Control$Priors[[nm]]
  for (nm in names(Control$Inp))
    inp[[nm]] <- Control$Inp[[nm]]
  inp
}

# zero-catch intervals join the preceding positive catch (the following one if leading)
.SpictCatch <- function(Catch, Time, dt) {
  Pos <- is.finite(Catch) & Catch > 0
  if (!any(Pos))
    cli::cli_abort("No positive catch in the fitted years.")
  Group <- pmax(cumsum(Pos), 1)
  list(obsC  = as.numeric(tapply(ifelse(Pos, Catch, 0), Group, sum)),
       timeC = as.numeric(tapply(Time, Group, min)),
       dtc   = as.numeric(tapply(rep(dt, length(Time)), Group, sum)))
}

.FitSPspict <- function(Prep, Shape, EstShape, Depletion, EstDepletion, IndexSD, Priors,
                        Control, Start, Uncertainty) {
  CheckPackage('spict', pkg.path = "pak::pkg_install('DTUAqua/spict/spict')")
  StartTime <- proc.time()[[3]]
  Log       <- Prep$Log

  inp <- tryCatch(.SpictInp(Prep, Shape, EstShape, Depletion, EstDepletion, IndexSD, Priors,
                            Control, Start),
                  error = function(e) conditionMessage(e))
  if (is.character(inp))
    return(.SPFailedFit(Prep, 'spict', inp, Log, StartTime))

  DoSD     <- Control$SDReport == 'always' || (Control$SDReport == 'auto' && Uncertainty)
  HasStart <- length(Start) > 0 && all(c('FMSY', 'MSY') %in% names(Start))
  Base     <- if (HasStart) {
    .SpictInp(Prep, Shape, EstShape, Depletion, EstDepletion, IndexSD, Priors, Control, NULL)
  } else {
    inp
  }
  Starts <- c(if (HasStart) list(inp), list(Base),
              lapply(Control$RestartK, \(k) {
                x <- Base
                x$ini$logK <- log(k * max(x$obsC))
                x
              }))

  Best <- NULL
  for (x in Starts) {
    x$do.sd.report <- DoSD
    Out <- .SpictFitOnce(x, Prep, Log, StartTime, Control)
    if (is.null(Best) || (Out$Converged && !Best$Converged) ||
        (Out$Converged == Best$Converged && isTRUE(Out$NLL < Best$NLL)))
      Best <- Out
    if (Best$Converged) break
  }
  Best
}

.SpictFitOnce <- function(inp, Prep, Log, StartTime, Control) {
  Warnings <- character()
  Fit      <- NULL
  if (Control$Quiet) {
    withCallingHandlers(
      utils::capture.output(Fit <- tryCatch(spict::fit.spict(inp), error = function(e) e)),
      warning = function(w) {
        Warnings <<- c(Warnings, conditionMessage(w))
        invokeRestart('muffleWarning')
      },
      message = function(m) invokeRestart('muffleMessage'))
  } else {
    Fit <- tryCatch(spict::fit.spict(inp), error = function(e) e)
  }
  Log <- c(Log, unique(Warnings))
  if (inherits(Fit, 'error') || is.null(Fit$opt))
    return(.SPFailedFit(Prep, 'spict',
                        paste('spict fit failed:', if (inherits(Fit, 'error')) conditionMessage(Fit) else 'no optimisation result'),
                        Log, StartTime))

  .SpictExtract(Fit, Prep, inp, Log, StartTime, Control)
}

.SpictExtract <- function(Fit, Prep, inp, Log, StartTime, Control) {
  Par  <- Fit$obj$env$last.par.best
  Get  <- function(nm) {
    v <- unname(Par[names(Par) == nm])
    if (length(v)) v else unname(Fit$inp$ini[[nm]])
  }
  m     <- exp(Get('logm'))[1]
  K     <- exp(Get('logK'))[1]
  n     <- exp(Get('logn'))[1]
  BMSYK <- if (abs(n - 1) < 1e-3) exp(-1) else n^(1 / (1 - n))
  BMSY  <- K * BMSYK
  FMSY  <- m / BMSY

  Time    <- Fit$inp$time
  BSeries <- exp(Get('logB'))
  FSeries <- exp(Get('logF'))
  Years   <- Prep$Years
  At <- function(t) BSeries[which.min(abs(Time - t))]
  B <- vapply(c(Years, max(Years) + 1), At, numeric(1))
  F <- vapply(Years, \(y) {
    ok <- Time >= y - 1e-8 & Time < y + 1 - 1e-8
    if (any(ok)) mean(FSeries[ok]) else NA_real_
  }, numeric(1))

  Step <- tryCatch({
    g <- Fit$obj$gr(Fit$opt$par)
    H <- stats::optimHess(Fit$opt$par, Fit$obj$fn, Fit$obj$gr)
    .SPNewtonStep(drop(g), (H + t(H)) / 2)
  }, error = function(e) Inf)
  Finite    <- all(is.finite(c(m, K, n, B, F)))
  Converged <- Finite && all(is.finite(Step)) && max(abs(Step)) < Control$GradTol
  Message   <- if (Converged) 'Converged' else if (!Finite) 'Non-finite estimates' else
    if (!all(is.finite(Step))) 'Hessian not positive definite' else
      sprintf('Estimated distance to the optimum %.2g exceeds GradTol', max(abs(Step)))

  SE <- c(logB_BMSY = NA_real_, logFMSY = NA_real_, logF_FMSY = NA_real_, logMSY = NA_real_)
  if (isTRUE(inp$do.sd.report)) {
    if (!is.null(Fit$sderr) && Fit$sderr != 0) {
      Log <- c(Log, 'spict sdreport failed; standard errors not available.')
    } else {
      Sd <- function(nm, t = NULL) {
        v <- try(spict::get.par(nm, Fit), silent = TRUE)
        if (inherits(v, 'try-error')) return(NA_real_)
        v <- matrix(v, ncol = 5)
        if (is.null(t) || nrow(v) == 1) return(v[1, 4])
        v[which.min(abs(Time[seq_len(nrow(v))] - t)), 4]
      }
      SE <- c(logB_BMSY = Sd('logBBmsy', max(Years) + 1), logFMSY = Sd('logFmsyd'),
              logF_FMSY = Sd('logFFmsy', max(Years)), logMSY = Sd('logMSYd'))
    }
  }

  q     <- exp(Get('logq'))
  Sigma <- exp(Get('logsdi'))
  nIdx  <- ncol(Prep$Index)
  Used  <- which(colSums(is.finite(Prep$Index) & Prep$Index > 0) > 0)
  qOut  <- SigOut <- rep(NA_real_, nIdx)
  qOut[Used]   <- rep_len(q, length(Used))
  SigOut[Used] <- rep_len(Sigma, length(Used))

  par <- c(FMSY = FMSY, MSY = m, Depletion = B[1] / K, Shape = n)
  Out <- .NewSPFit(Model = 'spict', Converged = Converged, Message = Message, par = par,
                   Est = c('FMSY', 'MSY', 'Depletion', if (Fit$inp$phases$logn > 0) 'Shape'),
                   Prep = Prep, B = B, F = F, CatchPred = rep(NA_real_, length(Years)), K = K,
                   q = qOut, Sigma = SigOut, SE = SE, NLL = Fit$opt$objective, Log = Log,
                   StartTime = StartTime, Control = Control)
  Out$Spict <- Fit
  Out
}
