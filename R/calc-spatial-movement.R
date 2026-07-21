#' Fit a Movement Matrix to Target Spatial Parameters
#'
#' Fits a row-stochastic movement matrix for a single simulation, age class,
#' and year by numerical optimisation, so that the asymptotic (equilibrium)
#' unfished distribution and the diagonal (staying probability) simultaneously
#' reproduce the targets in `Spatial@UnfishedDist` and `Spatial@ProbStaying`.
#' Dispatches to `.FitMovement2Area()` for two-area models and
#' `.FitMovementMultiArea()` for three or more areas.
#'
#' This function is called internally by [Populate()] for every combination of
#' simulation, age, and year. It can also be called directly to inspect or
#' debug the fitted movement matrix for a specific slice of a [spatial-class]
#' object.
#'
#' @param Spatial A populated [spatial-class] object whose `UnfishedDist`,
#'   `ProbStaying`, and (for multi-area models) `FracOther` slots have been
#'   expanded to named arrays.
#' @param sim `integer(1)`. Simulation index. Clamped to the `Sim` dimension
#'   of each slot if that dimension has length 1. Default `1`.
#' @param age `integer(1)`. Age index (position in the `Age` dimension).
#'   Clamped to the `Age` dimension if length 1. Default `1`.
#' @param year `integer(1)`. Year index (position in the `Year` dimension).
#'   Clamped to the `Year` dimension if length 1. Default `1`.
#'
#' @return A numeric matrix of dimensions `nArea × nArea`. Each row gives the
#'   movement probabilities from that area to all areas (including itself),
#'   and rows sum to exactly 1.
#'
#' @seealso
#' - [Spatial()] for the constructor and full spatial specification.
#' - `.FitMovement2Area()` for the two-area optimisation.
#' - `.FitMovementMultiArea()` for the multi-area optimisation.
#' - [Populate()] which calls this function internally.
#'
#' @export
FitMovement <-  function(Spatial, sim=1, age=1, year=1) {
  nArea <- dim(Spatial@Movement)[2]
  if (nArea==2) {
    movement <- .FitMovement2Area(Spatial,
                                   sim,
                                   age,
                                   year)
  } else {
    movement <- .FitMovementMultiArea(Spatial,
                                       sim,
                                       age,
                                       year)
  }
  movement
}

#' Fit a Two-Area Movement Matrix
#'
#' Fits a `2 × 2` row-stochastic movement matrix for a single simulation, age,
#' and year by minimising a least-squares objective in logit space. The
#' objective penalises deviations of the fitted staying probability from
#' `ProbStaying` and deviations of the implied area distribution from the
#' target, both in log space.
#'
#' @param Spatial A [spatial-class] object with populated `UnfishedDist` and
#'   `ProbStaying` slots (named `Sim × Area × Age × Year` arrays).
#' @param sim `integer(1)`. Simulation index. Default `1`.
#' @param age `integer(1)`. Age index. Default `1`.
#' @param year `integer(1)`. Year index. Default `1`.
#'
#' @details
#' Optimisation is performed with [stats::optim()] using the `"L-BFGS-B"`
#' method. The two free parameters are the logit-transformed diagonal entries
#' (staying probabilities for Area 1 and Area 2).
#'
#' When `UnfishedDist` has more than one age class and `age` is not the last
#' age class (plus group), a one-step objective is used: the penalty is on
#' the deviation of `UnfishedDist[age] %*% M` from `UnfishedDist[age + 1]`.
#' This ensures the fitted matrix correctly maps the current age's equilibrium
#' distribution to the next age's in a single time step, consistent with how
#' the movement matrix is applied in the population dynamics.
#'
#' For the plus group (`age == nAge`) and for age-invariant movement
#' (`nAge == 1`), the objective falls back to penalising the deviation of the
#' asymptotic (stationary) distribution from `UnfishedDist[age]`, since
#' fish in the plus group apply the same movement matrix repeatedly.
#'
#' Dimension indices are clamped to the size of each slot's array, so a slot
#' with a singleton dimension is treated as constant across that index.
#'
#' @return A `2 × 2` numeric matrix with rows summing to 1.
#'
#' @seealso [FitMovement()], `.FitMovementMultiArea()`, `.MarkovFrac()`,
#'   `.CalcAsymDist2Area()`, `.SolveMovement2Area()`
#'
#' @keywords internal
.FitMovement2Area <- function(Spatial, sim=1, age=1, year=1) {

  PS_dim <- dim(Spatial@ProbStaying)
  PS_sim <- min(sim, PS_dim[1])
  PS_age <- min(age, PS_dim[3])
  PS_year <- min(year, PS_dim[4])

  UD_dim  <- dim(Spatial@UnfishedDist)
  UD_sim  <- min(sim, UD_dim[1])
  UD_age  <- min(age, UD_dim[3])
  UD_year <- min(year, UD_dim[4])
  nAgeUD  <- UD_dim[3]

  UnfishedDist <- Spatial@UnfishedDist[UD_sim, , UD_age, UD_year]

  # One-step objective for non-plus-group ages with age-varying distribution
  UnfishedDistNext <- if (nAgeUD > 1L && age < nAgeUD) {
    Spatial@UnfishedDist[UD_sim, , age + 1L, UD_year]
  } else {
    NULL
  }

  optMovement <- stats::optim(
    logit(rep(Spatial@ProbStaying[PS_sim, 1, PS_age, PS_year], 2)),
    .SolveMovement2Area,
    UnfishedDist     = UnfishedDist,
    ProbStaying      = Spatial@ProbStaying[PS_sim, 1, PS_age, PS_year],
    UnfishedDistNext = UnfishedDistNext,
    method = "L-BFGS-B"
  )

  .MarkovFrac(LogitProbs=optMovement$par)
}

#' Objective Function for Two-Area Movement Optimisation
#'
#' Computes the least-squares objective used by `.FitMovement2Area()`.
#' Penalties are applied in log space to deviations of the fitted staying
#' probability from `ProbStaying` and of the implied area distribution from the
#' target.
#'
#' @param LogitProbs `numeric(2)`. Logit-transformed staying probabilities for
#'   Area 1 and Area 2. The off-diagonal entries are set to
#'   `1 - ilogit(LogitProbs)`.
#' @param UnfishedDist `numeric(2)`. Target unfished distribution across areas
#'   for the current age class.
#' @param ProbStaying `numeric(1)`. Target probability of remaining in Area 1.
#' @param UnfishedDistNext `numeric(2)` or `NULL`. Target unfished distribution
#'   for the **next** age class. When supplied, a one-step penalty is used:
#'   `UnfishedDist %*% M` should equal `UnfishedDistNext`. When `NULL`
#'   (plus-group or age-invariant movement), the asymptotic distribution of `M`
#'   is compared to `UnfishedDist` instead.
#'
#' @return `numeric(1)`. The value of the objective (to be minimised).
#'
#' @seealso `.FitMovement2Area()`, `.CalcAsymDist2Area()`
#'
#' @keywords internal
.SolveMovement2Area <- function(LogitProbs,
                                 UnfishedDist,
                                 ProbStaying,
                                 UnfishedDistNext = NULL) {
  Movement <- matrix(0, 2, 2)
  diag(Movement) <- ilogit(LogitProbs)
  Movement[1, 2] <- 1 - Movement[1, 1]
  Movement[2, 1] <- 1 - Movement[2, 2]

  if (!is.null(UnfishedDistNext)) {
    # One-step objective: UnfishedDist %*% M should equal UnfishedDistNext
    Projected <- as.vector(UnfishedDist %*% Movement)
    dist_penalty <- sum((log(Projected) - log(UnfishedDistNext))^2)
  } else {
    # Asymptotic objective for plus-group / age-invariant movement
    Distribution <- .CalcAsymDist2Area(Movement)
    dist_penalty <- (log(UnfishedDist[1]) - log(Distribution[1]))^2
  }

  NLL <- (log(Movement[1, 1]) - log(ProbStaying))^2 + dist_penalty
  NLL
}

#' Construct a Row-Stochastic Movement Matrix from Logit Probabilities
#'
#' Converts a vector of logit-transformed staying probabilities into a
#' row-stochastic movement matrix. When `FracOther` is supplied (multi-area
#' case), the off-diagonal mass for each row is distributed proportionally
#' across destination areas according to `FracOther`. When `FracOther` is
#' `NULL` (two-area case), a `2 × 2` matrix is constructed with the remaining
#' probability evenly split.
#'
#' @param LogitProbs `numeric`. Logit-transformed staying probabilities, one
#'   per area. Back-transformed via [ilogit()] to give the diagonal entries.
#' @param FracOther `matrix` or `NULL`. Square matrix of relative movement
#'   probabilities among areas. Diagonal elements should be `NA` and are
#'   ignored. When `NULL` (default), a two-area matrix is constructed.
#' @param tol `numeric(1)`. Tolerance for the row-sum check. Default `1e-10`.
#'
#' @return A square numeric matrix with `length(LogitProbs)` rows and columns.
#'   Rows sum to 1 (verified to within `tol`).
#'
#' @seealso `.FitMovement2Area()`, `.FitMovementMultiArea()`
#'
#' @keywords internal
.MarkovFrac <- function(LogitProbs, FracOther=NULL, tol = 1e-10){
  probs <- ilogit(LogitProbs)
  left <- 1-probs
  
  if (!is.null(FracOther)) {
    diag(FracOther) <- NA
    mov <- FracOther/apply(FracOther,1,sum,na.rm=T)*left
  } else {
    mov <- matrix(left, 2,2)
  }
  diag(mov) <- probs
  rs <- rowSums(mov, na.rm = FALSE)
  if (any(abs(rs - 1) > tol)) {
    cli::cli_abort('Movement matrix does not sum to 1 across areas')
  }
  mov
}

#' Fit a Multi-Area Movement Matrix
#'
#' Fits an `nArea × nArea` row-stochastic movement matrix for a single
#' simulation, age, and year by minimising a penalised negative log-likelihood
#' objective. The objective simultaneously penalises deviations of the implied
#' asymptotic distribution from `UnfishedDist` (log scale, penalty `CVDist`)
#' and deviations of the diagonal (staying probabilities) from `ProbStaying`
#' (logit scale, penalty `CVStay`). See `.SolveMovementMultiArea()` for the
#' objective details.
#'
#' @param Spatial A [spatial-class] object with populated `UnfishedDist`,
#'   `ProbStaying`, and `FracOther` slots (named arrays).
#' @param sim `integer(1)`. Simulation index. Default `1`.
#' @param age `integer(1)`. Age index. Default `1`.
#' @param year `integer(1)`. Year index. Default `1`.
#'
#' @details
#' Optimisation uses [stats::nlminb()] initialised at `rep(0, nArea)` in
#' logit space (corresponding to staying probabilities of 0.5). Dimension
#' indices for each slot are clamped to the slot's array size, so singleton
#' dimensions are treated as constant.
#'
#' The relative movement structure among areas is fixed by `FracOther` and
#' passed to `.MarkovFrac()` to construct the full matrix at each iteration.
#'
#' @return An `nArea × nArea` numeric matrix with rows summing to 1.
#'
#' @seealso [FitMovement()], `.FitMovement2Area()`, `.SolveMovementMultiArea()`,
#'   `.MarkovFrac()`, [CalcAsymDist()]
#'
#' @keywords internal
.FitMovementMultiArea <- function(Spatial, sim=1, age=1, year=1) {

  PS_dim <- dim(Spatial@ProbStaying)
  PS_sim <- min(sim, PS_dim[1])
  PS_age <- min(age, PS_dim[3])
  PS_year <- min(year, PS_dim[4])

  UD_dim  <- dim(Spatial@UnfishedDist)
  UD_sim  <- min(sim, UD_dim[1])
  UD_age  <- min(age, UD_dim[3])
  UD_year <- min(year, UD_dim[4])
  nAgeUD  <- UD_dim[3]

  FO_dim  <- dim(Spatial@FracOther)
  FO_sim  <- min(sim, FO_dim[1])
  FO_age  <- min(age, FO_dim[4])
  FO_year <- min(year, FO_dim[5])

  nArea <- UD_dim[2]

  UnfishedDist <- Spatial@UnfishedDist[UD_sim, , UD_age, UD_year]

  # One-step objective for non-plus-group ages with age-varying distribution
  UnfishedDistNext <- if (nAgeUD > 1L && age < nAgeUD) {
    Spatial@UnfishedDist[UD_sim, , age + 1L, UD_year]
  } else {
    NULL
  }

  optMovement <- stats::nlminb(
    rep(0, nArea),
    .SolveMovementMultiArea,
    UnfishedDist     = UnfishedDist,
    ProbStaying      = Spatial@ProbStaying[PS_sim, , PS_age, PS_year],
    FracOther        = Spatial@FracOther[FO_sim, , , FO_age, FO_year],
    UnfishedDistNext = UnfishedDistNext,
    CVDist           = Spatial@CVDist,
    CVStay           = Spatial@CVStay,
    control = list(iter.max = 5e3, eval.max = 1e4)
  )

  .MarkovFrac(LogitProbs = optMovement$par,
             FracOther  = Spatial@FracOther[FO_sim, , , FO_age, FO_year])
}

#' Objective Function for Multi-Area Movement Optimisation
#'
#' Computes the penalised negative log-likelihood used by
#' `.FitMovementMultiArea()`. Two normal penalties are applied: one on the
#' log-scale deviation of the implied asymptotic distribution from
#' `UnfishedDist` (controlled by `CVDist`), and one on the logit-scale
#' deviation of the staying probabilities from `ProbStaying` (controlled by
#' `CVStay`).
#'
#' @param LogitProbs `numeric(nArea)`. Logit-transformed staying probabilities,
#'   one per area. The full movement matrix is constructed via `.MarkovFrac()`.
#' @param UnfishedDist `numeric(nArea)`. Target unfished distribution across
#'   areas. Must sum to 1.
#' @param ProbStaying `numeric(nArea)`. Target staying probabilities, one per
#'   area.
#' @param FracOther `matrix`. Square matrix of relative off-diagonal movement
#'   probabilities. Diagonal elements must be `NA`.
#' @param CVDist `numeric(1)`. Standard deviation of the log-scale normal
#'   penalty on the distribution. Larger values allow greater deviation from
#'   the target. Default `0.1`.
#' @param CVStay `numeric(1)`. Standard deviation of the logit-scale normal
#'   penalty on the staying probabilities. Larger values allow greater
#'   deviation from `ProbStaying`. Default `1`.
#' @param UnfishedDistNext `numeric(nArea)` or `NULL`. Target unfished
#'   distribution for the **next** age class. When supplied, a one-step penalty
#'   replaces the asymptotic-distribution penalty: `UnfishedDist %*% M` should
#'   equal `UnfishedDistNext`. When `NULL` (plus-group or age-invariant
#'   movement), the asymptotic distribution of `M` is penalised against
#'   `UnfishedDist` instead.
#'
#' @details
#' When `UnfishedDistNext` is `NULL`, the distribution objective is:
#'
#' \deqn{
#'   -\sum_i \log \mathcal{N}\!\left(\log \pi_i \mid \log u_i,\,
#'   \sigma_{\text{dist}}^2\right)
#' }
#'
#' where \eqn{\pi_i} is the implied asymptotic distribution. When
#' `UnfishedDistNext` is supplied, it is replaced by:
#'
#' \deqn{
#'   -\sum_i \log \mathcal{N}\!\left(\log(\boldsymbol{u}_a \mathbf{M})_i
#'   \mid \log u_{a+1,i},\, \sigma_{\text{dist}}^2\right)
#' }
#'
#' In both cases the staying-probability penalty is:
#'
#' \deqn{
#'   -\sum_i \log \mathcal{N}\!\left(\text{logit}(m_{ii}) \mid
#'   \text{logit}(p_i),\, \sigma_{\text{stay}}^2\right)
#' }
#'
#' @return `numeric(1)`. The value of the objective (to be minimised).
#'
#' @seealso `.FitMovementMultiArea()`, `.MarkovFrac()`, [CalcAsymDist()]
#'
#' @keywords internal
.SolveMovementMultiArea <- function(LogitProbs,
                                     UnfishedDist,
                                     ProbStaying,
                                     FracOther,
                                     UnfishedDistNext = NULL,
                                     CVDist = 0.1,
                                     CVStay = 1) {
  Movement <- .MarkovFrac(LogitProbs, FracOther)

  if (!is.null(UnfishedDistNext)) {
    # One-step objective: UnfishedDist %*% M should equal UnfishedDistNext
    Projected <- as.vector(UnfishedDist %*% Movement)
    nll_dist <- dnorm(log(Projected), log(UnfishedDistNext), CVDist, TRUE)
  } else {
    Distribution <- CalcAsymDist(Movement)
    nll_dist <- dnorm(log(Distribution), log(UnfishedDist), CVDist, TRUE)
  }

  nll_stay <- dnorm(LogitProbs, logit(ProbStaying), CVStay, TRUE)
  -sum(c(nll_dist, nll_stay))
}
