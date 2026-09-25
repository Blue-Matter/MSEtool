#' Fit a Surplus Production Model to Catch and Index Data
#'
#' Fits a Pella-Tomlinson surplus production model to the catch and one or
#' more relative abundance indices in a [data-class] object, and returns
#' estimates of stock status and MSY-based reference points. The fitted model
#' is used by the [SurplusProduction()] management procedure, and `FitSP()`
#' can also be used directly on real or simulated `Data` objects.
#'
#' ## Model
#'
#' Biomass follows
#' \deqn{\frac{dB}{dt} = \gamma \, MSY \left[\frac{B}{K} - \left(\frac{B}{K}\right)^n\right] - F B,
#'   \qquad \gamma = \frac{n^{n/(n-1)}}{n-1},}{dB/dt = gamma MSY [B/K - (B/K)^n] - F B,  gamma = n^(n/(n-1)) / (n-1),}
#' where `n` is the shape parameter (`Shape`; `2` is the Schaefer model, and
#' `n = 1` the Fox model, used when `Shape` is within `FoxTol` of `1`; see
#' [SPControl()]). The estimated parameters are `FMSY` and `MSY`, and
#' optionally the initial depletion (`B/K` at the start of the first fitted
#' year, `Depletion`) and `Shape`. Carrying capacity is
#' `K = MSY / (FMSY * BMSY/K)` with `BMSY/K = n^(1/(1-n))`.
#'
#' The model is deterministic (no process error) and the catch is assumed
#' known. The fishing mortality in each year is solved so that the predicted
#' catch equals the observed catch. Each year is integrated in `nSubStep`
#' sub-steps (see [SPControl()]).
#'
#' Each index is assumed proportional to biomass at the index's `Timing`
#' within the year, with lognormal observation error. The catchability of
#' each index is calculated analytically at its maximum likelihood value.
#' The observation standard deviation is either estimated for each index
#' (`IndexSD = 'estimate'`, with lower bound `MinSD`, see [SPControl()]) or
#' calculated from the index `CV` in `Data` (`IndexSD = 'data'`,
#' `sd = sqrt(log(1 + CV^2))`). The negative log-likelihood of each index is
#' multiplied by its `IndexWeight`.
#'
#' For seasonal data (`Data@@Seasons > 1`), `Data` is first aggregated to
#' calendar years with [AnnualData()]. The timing of an aggregated index is
#' the mean timing, as a fraction of the calendar year, of the seasons in
#' `IndexSeasons`.
#'
#' ## Models
#'
#' - `Model = 'internal'` (default): the model above, fitted by maximum
#'   likelihood with [stats::nlminb()]. Numerical settings are set with
#'   [SPControl()].
#' - `Model = 'spict'`: the stochastic surplus production model in
#'   continuous time (SPiCT; Pedersen and Berg 2017), fitted with the `spict`
#'   package, which must be installed (see [SpictControl()]). SPiCT estimates
#'   process error and observation error in the catch as well as the
#'   indices. The `FitSP()` arguments are converted to `spict` inputs as
#'   described in [SpictControl()].
#'
#' @param Data A [data-class] object.
#' @param Indices,IndexSource,IndexFreq,IndexWeight,IndexSeasons Index
#'   selection, as in [IndexRate()]. `IndexWeight` is normalised to a mean of
#'   `1`.
#' @param IndexUnits Character vector of index `Units` to use: one or both of
#'   `'Biomass'` and `'Number'`. Indices with other units (including
#'   `'Recruitment'`) are excluded. Indices with no units are assumed to be
#'   in biomass.
#' @param CatchType Character. The catch the model is fitted to:
#'   `'Removals'` (landings plus discards, default) or `'Landings'`.
#' @param FitYears `NULL` (default) fits from the first year with catch data
#'   to the last data year. Otherwise a single number (fit to that many most
#'   recent years) or a vector of consecutive years. Missing catch after the
#'   first fitted year is set to `0`.
#' @param Model Character. The surplus production model: `'internal'`
#'   (default) or `'spict'`; see Models.
#' @param Shape Positive number. Shape parameter `n` (fixed value, or the
#'   median of its prior when `EstShape = TRUE`). Default `2` (Schaefer).
#' @param EstShape Logical. Estimate `Shape`? Default `FALSE`.
#' @param ShapeCV Positive number. CV of the lognormal prior on `Shape` when
#'   `EstShape = TRUE` and `Priors$Shape` is not given. Default `0.5`.
#' @param Depletion Positive number. Biomass in the first fitted year
#'   relative to `K` (fixed value, or starting value when
#'   `EstDepletion = TRUE`). Default `1`.
#' @param EstDepletion Logical. Estimate `Depletion`? Default `FALSE`.
#' @param IndexSD Character. `'estimate'` (default) or `'data'`; see Model.
#' @param Priors Named list of lognormal priors, each `c(median, CV)`, on any
#'   of `FMSY`, `MSY` (in catch units), `Depletion`, and `Shape`. A prior on a
#'   fixed parameter is ignored.
#' @param Control An [SPControl()] (`Model = 'internal'`) or
#'   [SpictControl()] (`Model = 'spict'`) list. `NULL` (default) uses the
#'   defaults of the selected model.
#' @param Start `NULL` (default), or a named list or vector with starting
#'   values for the estimated parameters (`FMSY`, `MSY`, `Depletion`,
#'   `Shape`), e.g. the `par` of a previous fit. Used only if it includes both
#'   `FMSY` and `MSY`; otherwise the fit starts from a grid search (see
#'   [SPControl()]).
#' @param Uncertainty Logical. Calculate the standard errors in `SE`?
#'   Default `FALSE`. Overridden by `Hessian` in [SPControl()] or `SDReport`
#'   in [SpictControl()] when these are not `'auto'`.
#'
#' @return An object of class `spfit`, a list with elements:
#'   - `Model`, `Converged` (logical), `Message`
#'   - `par`: named vector of `FMSY`, `MSY`, `Depletion`, `Shape`
#'   - `Est`: names of the estimated parameters
#'   - `Years`: fitted years
#'   - `B` and `B_BMSY` (length `length(Years) + 1`: at the start of each
#'     fitted year and after the last), and `F`, `F_FMSY`, and `CatchPred`
#'     (one per fitted year; `CatchPred` is `NA` for `Model = 'spict'`)
#'   - `BMSY`, `FMSY`, `MSY`, `K`, `Shape`, `Depletion`
#'   - `q`, `Sigma`: catchability and observation standard deviation of each
#'     index
#'   - `Terminal`: named vector of `B_BMSY` (after the last fitted year) and
#'     `F_FMSY` (in the last fitted year)
#'   - `SE`: named vector of standard errors of `logB_BMSY` (as in
#'     `Terminal`), `logFMSY`, `logF_FMSY` (as in `Terminal`), and `logMSY`
#'     (`NA` unless calculated)
#'   - `NLL` (objective function value), `Time` (seconds), `Log` (character
#'     vector of assumptions and warnings)
#'   - `Control`, `Prep` (the prepared data), and, for `Model = 'spict'`,
#'     `Spict` (the `spict` fit)
#'
#'   If the model cannot be fitted, `Converged` is `FALSE`, `Message` gives
#'   the reason, and the estimates are `NA` or absent.
#'
#' @references Pedersen, M. W. and Berg, C. W. 2017. A stochastic surplus
#'   production model in continuous time. Fish and Fisheries 18: 226-243.
#'
#' @examples
#' \dontrun{
#' Hist <- Simulate(SingleStockOM)
#' Fit  <- FitSP(Hist@@Data[[1]][[1]])
#' Fit$Terminal
#' }
#'
#' @seealso [SurplusProduction()], [SPControl()], [SpictControl()],
#'   [AnnualData()]
#' @export
FitSP <- function(Data,
                  Indices      = NULL,
                  IndexSource  = 'Survey',
                  IndexFreq    = NULL,
                  IndexWeight  = NULL,
                  IndexSeasons = NULL,
                  IndexUnits   = c('Biomass', 'Number'),
                  CatchType    = c('Removals', 'Landings'),
                  FitYears     = NULL,
                  Model       = c('internal', 'spict'),
                  Shape        = 2,
                  EstShape     = FALSE,
                  ShapeCV      = 0.5,
                  Depletion    = 1,
                  EstDepletion = FALSE,
                  IndexSD      = c('estimate', 'data'),
                  Priors       = list(),
                  Control      = NULL,
                  Start        = NULL,
                  Uncertainty  = FALSE) {

  Model    <- match.arg(Model, c('internal', 'spict'))
  IndexSD   <- match.arg(IndexSD, c('estimate', 'data'))
  CatchType <- match.arg(CatchType, c('Removals', 'Landings'))

  Prep <- .SPPrepData(Data, Indices, IndexSource, IndexFreq, IndexWeight, IndexSeasons,
                      IndexUnits, CatchType, FitYears)
  .FitSPPrepped(Prep, Model, Shape, EstShape, ShapeCV, Depletion, EstDepletion,
                IndexSD, Priors, Control, Start, Uncertainty)
}

#' Numerical Settings for the Internal Surplus Production Model
#'
#' Controls for [FitSP()] and [SurplusProduction()] with
#' `Model = 'internal'`.
#'
#' @param nSubStep Positive integer. Sub-steps per year used to integrate the
#'   biomass dynamics. Default `4`.
#' @param nItF Positive integer. Maximum Newton iterations used to solve the
#'   annual fishing mortality that produces the observed catch. Default `5`.
#' @param Fmax Positive number. Maximum annual fishing mortality. Default `3`.
#' @param FPenalty Non-negative number. Weight of the penalty on the squared
#'   log difference between observed and predicted catch, added to the
#'   negative log-likelihood. The difference is non-zero when the catch cannot
#'   be taken with `F <= Fmax`. Default `1000`.
#' @param FoxTol Positive number. `Shape` within `FoxTol` of `1` uses the Fox
#'   model. Default `1e-3`.
#' @param CatchScale Character. `'mean'` (default) rescales catch to a mean of
#'   `1` during fitting; `'none'` fits on the original scale.
#' @param WarmStart Logical. In [SurplusProduction()], start each fit from
#'   the estimates of the previous management cycle? Default `TRUE`.
#' @param nGrid Positive integer. Number of `FMSY` and `MSY` values in the
#'   grid of starting values (`nGrid x nGrid` cells), used when there is no
#'   `Start` (or warm start) or the fit from it does not converge. The first
#'   fit starts from the cell with the lowest negative log-likelihood.
#'   Default `12`.
#' @param GridFMSY Numeric vector, length 2. Range of `FMSY` in the grid.
#'   Default `c(0.02, 1)`.
#' @param GridMSY Numeric vector, length 2. Range of `MSY` in the grid, as
#'   multiples of the mean catch. Default `c(0.2, 5)`.
#' @param nRestart Non-negative integer. Maximum number of additional fits,
#'   started from the next-best grid cells, when the fit from the best cell
#'   does not converge. Default `2`.
#' @param MinSD Positive number. Lower bound on an estimated index
#'   observation standard deviation (`IndexSD = 'estimate'`). Default `0.05`.
#' @param GradTol Positive number. A fit is converged when, for each
#'   estimated (log-scale) parameter, the gradient divided by the curvature of
#'   the negative log-likelihood (the estimated distance to the optimum) is
#'   below `GradTol`, and the curvature is positive. Default `1e-3`.
#' @param BoundTol Non-negative number. A fit with a log-scale parameter
#'   within `BoundTol` of a bound is not converged. Default `0.01`.
#' @param Bounds Named list of `c(lower, upper)` bounds on the estimated
#'   `FMSY`, `MSY` (as multiples of the mean catch), `Depletion`, and `Shape`.
#'   Elements not given keep their defaults: `FMSY = c(0.005, 3)`,
#'   `MSY = c(0.01, 100)`, `Depletion = c(0.05, 1.5)`, `Shape = c(0.2, 10)`.
#' @param nlminb List of `control` settings passed to [stats::nlminb()].
#' @param HessianStep Positive number. Finite-difference step (on the log
#'   scale of the parameters) used to calculate the Hessian; steps 10 and 100
#'   times smaller are tried if the Hessian is not positive definite. Default
#'   `1e-5`.
#' @param Hessian Character. When to calculate the Hessian used for the
#'   standard errors: `'auto'` (default; when `Uncertainty = TRUE` in
#'   [FitSP()], or when [SurplusProduction()] uses `FractileB`, `FractileF`,
#'   or `Diagnostics = 'full'`), `'always'`, or `'never'`.
#'
#' @return A list of class `spcontrol`.
#' @seealso [FitSP()], [SurplusProduction()], [SpictControl()]
#' @export
SPControl <- function(nSubStep   = 4,
                      nItF       = 5,
                      Fmax       = 3,
                      FPenalty   = 1e3,
                      FoxTol     = 1e-3,
                      CatchScale = c('mean', 'none'),
                      WarmStart  = TRUE,
                      nGrid      = 12,
                      GridFMSY   = c(0.02, 1),
                      GridMSY    = c(0.2, 5),
                      nRestart   = 2,
                      MinSD      = 0.05,
                      GradTol    = 1e-3,
                      BoundTol   = 0.01,
                      Bounds     = list(FMSY = c(0.005, 3), MSY = c(0.01, 100),
                                        Depletion = c(0.05, 1.5), Shape = c(0.2, 10)),
                      nlminb     = list(iter.max = 500, eval.max = 1000),
                      HessianStep = 1e-5,
                      Hessian    = c('auto', 'always', 'never')) {

  CatchScale <- match.arg(CatchScale, c('mean', 'none'))
  Hessian    <- match.arg(Hessian, c('auto', 'always', 'never'))

  for (nm in c('nSubStep', 'nItF', 'nGrid'))
    if (!is.numeric(get(nm)) || length(get(nm)) != 1 || get(nm) < 1)
      cli::cli_abort("{.arg {nm}} must be a single positive integer.")

  DefaultBounds <- list(FMSY = c(0.005, 3), MSY = c(0.01, 100),
                        Depletion = c(0.05, 1.5), Shape = c(0.2, 10))
  Bad <- setdiff(names(Bounds), names(DefaultBounds))
  if (length(Bad))
    cli::cli_abort("Unknown {.arg Bounds} element{?s}: {.val {Bad}}.")
  Bounds <- utils::modifyList(DefaultBounds, Bounds)
  for (nm in names(Bounds))
    if (length(Bounds[[nm]]) != 2 || any(Bounds[[nm]] <= 0) || Bounds[[nm]][1] >= Bounds[[nm]][2])
      cli::cli_abort("{.arg Bounds${nm}} must be two increasing positive numbers.")

  structure(
    list(nSubStep = as.integer(nSubStep), nItF = as.integer(nItF), Fmax = Fmax,
         FPenalty = FPenalty, FoxTol = FoxTol, CatchScale = CatchScale,
         WarmStart = WarmStart, nGrid = as.integer(nGrid), GridFMSY = GridFMSY,
         GridMSY = GridMSY, nRestart = as.integer(nRestart), MinSD = MinSD,
         GradTol = GradTol, BoundTol = BoundTol, Bounds = Bounds, nlminb = nlminb,
         HessianStep = HessianStep, Hessian = Hessian),
    class = 'spcontrol'
  )
}

#' Prepare Annual Catch and Index Data for a Surplus Production Model
#'
#' @inheritParams FitSP
#' @return A list with `Years` (fitted years), `Catch` (fitted catch),
#'   `Landings`, `Discards` (annual totals over all years), `AllYears`,
#'   `Index`, `CV` (`[nFitYear x nIndex]`), `Timing`, `Weight`, `Name`,
#'   `Units`, `YearLH`, `LastDataYear`, and `Log` (character vector of
#'   assumptions).
#' @keywords internal
.SPPrepData <- function(Data, Indices, IndexSource, IndexFreq, IndexWeight, IndexSeasons,
                        IndexUnits, CatchType, FitYears) {
  .CheckClass(Data, 'data', 'Data')
  IndexSource <- match.arg(IndexSource, c('Survey', 'CPUE'), several.ok = TRUE)
  IndexUnits  <- match.arg(IndexUnits, c('Biomass', 'Number'), several.ok = TRUE)
  Log         <- character()

  Seasons <- max(1L, as.integer(Data@Seasons %||NA% 1))
  Annual  <- AnnualData(Data, .IndexSeasonsBySource(Data, IndexSource, Indices, IndexSeasons))
  Years   <- Annual@Years

  if (is.null(Annual@Landings@Value))
    cli::cli_abort("No landings data in {.code Data@Landings}.")
  CatchUnits <- Annual@Landings@Units
  if (length(CatchUnits) && any(.CatchUnitType(CatchUnits) == 'Number'))
    cli::cli_abort("A surplus production model needs catch in biomass; {.code Data@Landings@Units} is {.val {CatchUnits}}.")

  RowTotal <- function(x) {
    if (is.null(x)) return(rep(NA_real_, length(Years)))
    Tot <- rowSums(x, na.rm = TRUE)
    Tot[rowSums(!is.na(x)) == 0] <- NA
    Tot
  }
  Landings <- RowTotal(Annual@Landings@Value)
  Discards <- RowTotal(Annual@Discards@Value)
  Catch    <- if (CatchType == 'Landings') Landings else {
    Tot <- ifelse(is.na(Landings), 0, Landings) + ifelse(is.na(Discards), 0, Discards)
    Tot[is.na(Landings) & is.na(Discards)] <- NA
    Tot
  }

  Selected <- .SelectIndices(Annual, IndexSource, Indices)
  nSel     <- ncol(Selected$Value)
  if (is.null(IndexFreq))   IndexFreq   <- rep(1, nSel)
  if (is.null(IndexWeight)) IndexWeight <- rep(1, nSel)
  if (length(IndexFreq) != nSel || length(IndexWeight) != nSel)
    cli::cli_abort("{.arg IndexFreq} and {.arg IndexWeight} must have one value per selected index ({.val {nSel}}).")

  IndexMat <- t(.ApplyIndexFrequency(t(Selected$Value), IndexFreq, Data@YearLH, max(Years), Years))
  Keep     <- IndexFreq > 0

  Units    <- Selected$Units[Keep]
  UnitType <- ifelse(is.na(Units), 'Biomass',
                     ifelse(tolower(Units) %in% tolower(.count_units) | tolower(Units) == 'number', 'Number',
                            ifelse(tolower(Units) == 'recruitment', 'Recruitment', 'Biomass')))
  UseUnits <- UnitType %in% IndexUnits
  if (any(UnitType == 'Number' & UseUnits))
    Log <- c(Log, "Indices in numbers are treated as proportional to biomass.")

  Timing <- Selected$Timing[Keep]
  if (Seasons > 1) {
    SeasonsUsed <- if (is.list(IndexSeasons)) IndexSeasons[Keep] else rep(list(IndexSeasons), sum(Keep))
    Timing      <- vapply(seq_along(Timing), \(i) {
      s <- SeasonsUsed[[i]]
      if (is.null(s)) s <- seq_len(Seasons)
      mean((s - 1 + Timing[i]) / Seasons)
    }, numeric(1))
  }
  Timing <- pmin(pmax(Timing, 0), 1)

  CVMat  <- Selected$CV[, Keep, drop = FALSE]
  Name   <- Selected$Name[Keep]
  Weight <- IndexWeight[Keep]

  IndexMat <- IndexMat[, UseUnits, drop = FALSE]
  CVMat    <- CVMat[, UseUnits, drop = FALSE]
  Timing   <- Timing[UseUnits]
  Name     <- Name[UseUnits]
  Units    <- Units[UseUnits]
  Weight   <- Weight[UseUnits]
  if (!length(Weight))
    cli::cli_abort("No selected index has units in {.arg IndexUnits} ({.val {IndexUnits}}).")
  if (any(Weight < 0) || !any(Weight > 0))
    cli::cli_abort("{.arg IndexWeight} must be non-negative with at least one positive value.")
  Weight <- Weight / mean(Weight)

  FitRows <- seq_along(Years)
  if (!is.null(FitYears)) {
    FitRows <- if (length(FitYears) == 1) {
      seq(max(1, length(Years) - FitYears + 1), length(Years))
    } else {
      which(Years %in% FitYears)
    }
  }
  FirstCatch <- which(!is.na(Catch[FitRows]))[1]
  if (is.na(FirstCatch))
    cli::cli_abort("No catch data in the fitted years.")
  FitRows <- FitRows[FirstCatch:length(FitRows)]
  if (any(diff(FitRows) != 1))
    cli::cli_abort("{.arg FitYears} must be consecutive years.")
  FitCatch <- Catch[FitRows]
  if (anyNA(FitCatch)) {
    Log <- c(Log, sprintf("Missing catch in %s set to 0.",
                          paste(Years[FitRows][is.na(FitCatch)], collapse = ', ')))
    FitCatch[is.na(FitCatch)] <- 0
  }

  IndexMat <- IndexMat[FitRows, , drop = FALSE]
  CVMat    <- CVMat[FitRows, , drop = FALSE]
  dimnames(IndexMat) <- dimnames(CVMat) <- list(Year = Years[FitRows], Index = Name)

  SubCatch <- NULL
  if (Seasons > 1) {
    Sub <- .SPSubAnnualCatch(Data, CatchType, Years[FitRows])
    if (!is.null(Sub)) SubCatch <- c(Sub, list(dt = 1 / Seasons))
  }

  list(Years = Years[FitRows], Catch = FitCatch, AllYears = Years, SubCatch = SubCatch,
       Landings = Landings, Discards = Discards, CatchAll = Catch,
       Index = IndexMat, CV = CVMat, Timing = Timing, Weight = Weight,
       Name = Name, Units = Units, YearLH = Data@YearLH,
       LastDataYear = max(Years), CatchUnits = CatchUnits, Log = Log)
}

.SPSubAnnualCatch <- function(Data, CatchType, FitYears) {
  Total <- function(x) {
    if (is.null(x)) return(NULL)
    Tot <- rowSums(x, na.rm = TRUE)
    Tot[rowSums(!is.na(x)) == 0] <- NA
    Tot
  }
  L <- Total(Data@Landings@Value)
  if (is.null(L)) return(NULL)
  Catch <- L
  if (CatchType == 'Removals') {
    D <- Total(Data@Discards@Value)
    if (!is.null(D)) Catch <- ifelse(is.na(L), 0, L) + ifelse(is.na(D), 0, D)
  }
  Keep <- .CalendarYear(Data@Years) %in% FitYears & !is.na(Catch)
  if (!any(Keep)) return(NULL)
  list(Time = Data@Years[Keep], Catch = unname(Catch[Keep]))
}

.FitSPPrepped <- function(Prep, Model, Shape, EstShape, ShapeCV, Depletion, EstDepletion,
                          IndexSD, Priors, Control, Start, Uncertainty) {
  .CheckSPPriors(Priors)
  if (EstShape && is.null(Priors$Shape))
    Priors$Shape <- c(Shape, ShapeCV)

  if (Model == 'spict') {
    if (is.null(Control)) Control <- SpictControl()
    if (!inherits(Control, 'spictcontrol'))
      cli::cli_abort("{.arg Control} must be made with {.fn SpictControl} when {.code Model = 'spict'}.")
    return(.FitSPspict(Prep, Shape, EstShape, Depletion, EstDepletion, IndexSD, Priors,
                       Control, Start, Uncertainty))
  }

  if (is.null(Control)) Control <- SPControl()
  if (!inherits(Control, 'spcontrol'))
    cli::cli_abort("{.arg Control} must be made with {.fn SPControl} when {.code Model = 'internal'}.")
  .FitSPInternal(Prep, Shape, EstShape, Depletion, EstDepletion, IndexSD, Priors,
                 Control, Start, Uncertainty)
}

.CheckSPPriors <- function(Priors) {
  if (!is.list(Priors))
    cli::cli_abort("{.arg Priors} must be a named list.")
  Bad <- setdiff(names(Priors), c('FMSY', 'MSY', 'Depletion', 'Shape'))
  if (length(Bad))
    cli::cli_abort(c("Unknown {.arg Priors} element{?s}: {.val {Bad}}.",
                     "i" = "Priors can be set on {.val {c('FMSY', 'MSY', 'Depletion', 'Shape')}}."))
  for (nm in names(Priors))
    if (length(Priors[[nm]]) != 2 || any(!is.finite(Priors[[nm]])) || any(Priors[[nm]] <= 0))
      cli::cli_abort("{.arg Priors${nm}} must be {.code c(median, CV)} with both positive.")
  invisible(TRUE)
}

.SPIndexSD <- function(Prep, IndexSD) {
  nIndex <- ncol(Prep$Index)
  if (IndexSD == 'estimate')
    return(list(EstSD = rep(TRUE, nIndex), SD = matrix(NA_real_, nrow(Prep$Index), nIndex)))
  Obs       <- !is.na(Prep$Index) & Prep$Index > 0
  MissingCV <- Obs & !(Prep$CV > 0)
  MissingCV[is.na(MissingCV)] <- TRUE
  if (any(MissingCV & Obs))
    cli::cli_abort(c(
      "{.code IndexSD = 'data'} needs a positive index {.field CV} for every index observation.",
      "x" = "Missing for {.val {Prep$Name[colSums(MissingCV & Obs) > 0]}}."
    ))
  list(EstSD = rep(FALSE, nIndex), SD = sqrt(log(1 + Prep$CV^2)))
}

.FitSPInternal <- function(Prep, Shape, EstShape, Depletion, EstDepletion, IndexSD, Priors,
                           Control, Start, Uncertainty) {
  StartTime <- proc.time()[[3]]

  MeanCatch <- mean(Prep$Catch)
  if (!(MeanCatch > 0))
    cli::cli_abort("Mean catch in the fitted years must be positive.")
  Scale  <- if (Control$CatchScale == 'mean') MeanCatch else 1
  CatchS <- Prep$Catch / Scale
  SDInfo <- .SPIndexSD(Prep, IndexSD)
  nObs   <- colSums(!is.na(Prep$Index) & Prep$Index > 0)

  Base <- c(FMSY = 0.2, MSY = MeanCatch, Depletion = Depletion, Shape = Shape)
  if (!is.null(Start)) {
    Start <- unlist(Start)[intersect(names(unlist(Start)), names(Base))]
    Start <- Start[is.finite(Start) & Start > 0]
  }
  Est <- c('FMSY', 'MSY', if (EstDepletion) 'Depletion', if (EstShape) 'Shape')

  B     <- Control$Bounds
  Lower <- log(c(FMSY = B$FMSY[1], MSY = B$MSY[1] * MeanCatch, Depletion = B$Depletion[1], Shape = B$Shape[1]))
  Upper <- log(c(FMSY = B$FMSY[2], MSY = B$MSY[2] * MeanCatch, Depletion = B$Depletion[2], Shape = B$Shape[2]))

  Natural <- function(theta) {
    p <- Base
    p[Est] <- exp(theta)
    p
  }

  PriorNLL <- function(p) {
    Out <- 0
    for (nm in intersect(names(Priors), Est)) {
      sdlog <- sqrt(log(1 + Priors[[nm]][2]^2))
      Out   <- Out + (log(p[[nm]]) - log(Priors[[nm]][1]))^2 / (2 * sdlog^2)
    }
    Out
  }

  Model <- function(p, Report = FALSE) {
    SPModel_cpp(c(p[['FMSY']], p[['MSY']] / Scale, p[['Depletion']], p[['Shape']]),
                CatchS, Prep$Index, SDInfo$SD, Prep$Timing, Prep$Weight, SDInfo$EstSD,
                Control$nSubStep, Control$nItF, Control$Fmax, Control$FPenalty,
                Control$FoxTol, Control$MinSD, Report)
  }

  Objective <- function(theta) {
    p   <- Natural(theta)
    Out <- Model(p)$NLL + PriorNLL(p)
    if (!is.finite(Out)) 1e10 else Out
  }

  Log <- Prep$Log
  if (!sum(nObs))
    return(.SPFailedFit(Prep, 'internal', 'No index observations in the fitted years.', Log,
                        StartTime))

  StartList <- list()
  if (length(Start) && all(c('FMSY', 'MSY') %in% names(Start))) {
    s <- Base
    s[names(Start)] <- Start
    StartList[[1]] <- pmin(pmax(log(s[Est]), Lower[Est] + 1e-6), Upper[Est] - 1e-6)
  }
  Best <- NULL
  Better <- function(Fit, Best) {
    is.null(Best) || (Fit$Converged && !Best$Converged) ||
      (Fit$Converged == Best$Converged && Fit$objective < Best$objective)
  }
  for (Theta in StartList) {
    Fit <- .SPOptimise(Theta, Objective, Lower[Est], Upper[Est], Control)
    if (Better(Fit, Best)) Best <- Fit
  }
  if (is.null(Best) || !Best$Converged) {
    GridStarts <- .SPGridStarts(Objective, Est, Base, MeanCatch, Control, Lower, Upper)
    for (Theta in GridStarts[seq_len(min(length(GridStarts), 1 + Control$nRestart))]) {
      Fit <- .SPOptimise(Theta, Objective, Lower[Est], Upper[Est], Control)
      if (Better(Fit, Best)) Best <- Fit
      if (Best$Converged) break
    }
  }

  p   <- Natural(Best$par)
  Rep <- Model(p, Report = TRUE)

  WantSE <- Control$Hessian == 'always' || (Control$Hessian == 'auto' && Uncertainty)
  SE     <- c(logB_BMSY = NA_real_, logFMSY = NA_real_, logF_FMSY = NA_real_, logMSY = NA_real_)
  if (WantSE) {
    Derived <- function(theta) {
      pp   <- Natural(theta)
      R    <- Model(pp, Report = TRUE)
      BMSY <- pp[['MSY']] / pp[['FMSY']] / Scale
      c(logB_BMSY = log(utils::tail(R$B, 1) / BMSY), logFMSY = log(pp[['FMSY']]),
        logF_FMSY = log(utils::tail(R$F, 1) / pp[['FMSY']]), logMSY = log(pp[['MSY']]))
    }
    SE <- .SPDeltaSE(Best$par, Objective, Derived, SE, Control$HessianStep)
    if (anyNA(SE)) Log <- c(Log, "Hessian not positive definite; standard errors not available.")
  }

  .NewSPFit(Model = 'internal', Converged = Best$Converged, Message = Best$Message,
            par = p, Est = Est, Prep = Prep, B = Rep$B * Scale, F = Rep$F,
            CatchPred = Rep$CatchPred * Scale, K = Rep$K * Scale, q = Rep$q / Scale,
            Sigma = Rep$Sigma, SE = SE, NLL = Best$objective, Log = Log,
            StartTime = StartTime, Control = Control)
}

.SPGridStarts <- function(Objective, Est, Base, MeanCatch, Control, Lower, Upper) {
  FGrid <- exp(seq(log(Control$GridFMSY[1]), log(Control$GridFMSY[2]), length.out = Control$nGrid))
  MGrid <- exp(seq(log(Control$GridMSY[1] * MeanCatch), log(Control$GridMSY[2] * MeanCatch),
                   length.out = Control$nGrid))
  Cells <- expand.grid(FMSY = FGrid, MSY = MGrid)
  Theta <- lapply(seq_len(nrow(Cells)), \(i) {
    s <- Base
    s[c('FMSY', 'MSY')] <- c(Cells$FMSY[i], Cells$MSY[i])
    pmin(pmax(log(s[Est]), Lower[Est] + 1e-6), Upper[Est] - 1e-6)
  })
  Value <- vapply(Theta, Objective, numeric(1))
  Theta[order(Value)]
}

.SPOptimise <- function(Start, Objective, Lower, Upper, Control) {
  Opt <- try(stats::nlminb(Start, Objective, lower = Lower, upper = Upper,
                           control = Control$nlminb), silent = TRUE)
  if (inherits(Opt, 'try-error'))
    return(list(par = Start, objective = Objective(Start), Converged = FALSE,
                Message = conditionMessage(attr(Opt, 'condition'))))

  Step      <- .SPNewtonStep(Opt$par, Opt$objective, Objective)
  AtBound   <- any(Opt$par - Lower < Control$BoundTol | Upper - Opt$par < Control$BoundTol)
  Converged <- Opt$convergence == 0 && all(is.finite(Step)) &&
    max(abs(Step)) < Control$GradTol && !AtBound && Opt$objective < 1e10
  Message <- if (Converged) 'Converged' else if (Opt$convergence != 0) Opt$message else
    if (AtBound) 'Estimate at a parameter bound' else
      sprintf('Estimated distance to the optimum %.2g exceeds GradTol', max(abs(Step)))
  list(par = Opt$par, objective = Opt$objective, Converged = Converged, Message = Message)
}

.SPNewtonStep <- function(theta, f0, fn, h = 1e-4) {
  vapply(seq_along(theta), \(j) {
    Up <- Dn <- theta
    Up[j] <- Up[j] + h
    Dn[j] <- Dn[j] - h
    fUp  <- fn(Up)
    fDn  <- fn(Dn)
    Curv <- (fUp - 2 * f0 + fDn) / h^2
    if (!is.finite(Curv) || Curv <= 0) return(Inf)
    ((fUp - fDn) / (2 * h)) / Curv
  }, numeric(1))
}

.SPDeltaSE <- function(theta, Objective, Derived, Default, Step = 1e-5) {
  V <- NULL
  for (h in Step * 10^(0:-2)) {
    H <- try(stats::optimHess(theta, Objective, control = list(ndeps = rep(h, length(theta)))),
             silent = TRUE)
    if (inherits(H, 'try-error') || any(!is.finite(H))) next
    H <- (H + t(H)) / 2
    if (all(eigen(H, symmetric = TRUE, only.values = TRUE)$values > 0)) {
      V <- solve(H)
      break
    }
  }
  if (is.null(V)) return(Default)
  h <- Step
  J <- vapply(seq_along(theta), \(j) {
    Up <- Dn <- theta
    Up[j] <- Up[j] + h
    Dn[j] <- Dn[j] - h
    (Derived(Up) - Derived(Dn)) / (2 * h)
  }, numeric(length(Default)))
  J  <- matrix(J, nrow = length(Default))
  SE <- sqrt(pmax(diag(J %*% V %*% t(J)), 0))
  stats::setNames(SE, names(Default))
}

.NewSPFit <- function(Model, Converged, Message, par, Est, Prep, B, F, CatchPred, K, q,
                      Sigma, SE, NLL, Log, StartTime, Control) {
  BMSY  <- par[['MSY']] / par[['FMSY']]
  Years <- Prep$Years
  names(B) <- c(Years, max(Years) + 1)
  names(F) <- names(CatchPred) <- Years
  names(q) <- names(Sigma) <- Prep$Name
  structure(
    list(Model = Model, Converged = Converged, Message = Message,
         par = par, Est = Est, Years = Years,
         B = B, F = F, CatchPred = CatchPred,
         B_BMSY = B / BMSY, F_FMSY = F / par[['FMSY']],
         BMSY = BMSY, FMSY = par[['FMSY']], MSY = par[['MSY']], K = K,
         Shape = par[['Shape']], Depletion = par[['Depletion']],
         q = q, Sigma = Sigma,
         Terminal = c(B_BMSY = unname(utils::tail(B, 1)) / BMSY,
                      F_FMSY = unname(utils::tail(F, 1)) / par[['FMSY']]),
         SE = SE, NLL = NLL, Time = proc.time()[[3]] - StartTime,
         Log = Log, Control = Control, Prep = Prep),
    class = 'spfit'
  )
}

.SPFailedFit <- function(Prep, Model, Message, Log, StartTime) {
  structure(
    list(Model = Model, Converged = FALSE, Message = Message,
         par = c(FMSY = NA_real_, MSY = NA_real_, Depletion = NA_real_, Shape = NA_real_),
         Years = Prep$Years, Terminal = c(B_BMSY = NA_real_, F_FMSY = NA_real_),
         SE = c(logB_BMSY = NA_real_, logFMSY = NA_real_, logF_FMSY = NA_real_, logMSY = NA_real_),
         NLL = NA_real_, Time = proc.time()[[3]] - StartTime, Log = Log, Prep = Prep),
    class = 'spfit'
  )
}

#' @export
print.spfit <- function(x, ...) {
  cli::cli_text("Surplus production fit ({x$Model}): {if (x$Converged) 'converged' else x$Message}")
  if (!is.null(x$B)) {
    Out <- c(MSY = x$MSY, FMSY = x$FMSY, BMSY = x$BMSY, K = x$K, Shape = x$Shape,
             Depletion = x$Depletion, x$Terminal)
    print(signif(Out, 4))
  }
  invisible(x)
}

#' Project a Fitted Surplus Production Model
#'
#' @param Fit An `spfit` object.
#' @param Value Numeric vector, one per projection year: the catch
#'   (`Type = 0`) or fishing mortality (`Type = 1`).
#' @param Type Integer vector the same length as `Value`.
#' @param B0 Biomass at the start of the first projection year. Default is
#'   the biomass after the last fitted year.
#' @return A list with `B` (length `length(Value) + 1`), `F`, and `Catch`.
#' @keywords internal
.ProjectSP <- function(Fit, Value, Type, B0 = utils::tail(Fit$B, 1)) {
  if (!length(Value))
    return(list(B = B0, F = numeric(0), Catch = numeric(0)))
  Ctl <- Fit$Control
  if (!inherits(Ctl, 'spcontrol')) Ctl <- SPControl()
  SPProject_cpp(c(Fit$FMSY, Fit$MSY, Fit$Depletion, Fit$Shape), unname(B0),
                as.numeric(Value), as.integer(Type), Ctl$nSubStep, Ctl$nItF,
                Ctl$Fmax, Ctl$FoxTol)
}
