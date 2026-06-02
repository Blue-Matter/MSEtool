#' Fit a Movement Matrix to Target Spatial Parameters
#'
#' Fits a row-stochastic movement matrix for a single simulation, age class,
#' and year by numerical optimisation, so that the asymptotic (equilibrium)
#' unfished distribution and the diagonal (staying probability) simultaneously
#' reproduce the targets in `Spatial@UnfishedDist` and `Spatial@ProbStaying`.
#' Dispatches to [FitMovement_2_Area()] for two-area models and
#' [FitMovement_Multi_Area()] for three or more areas.
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
#' - [FitMovement_2_Area()] for the two-area optimisation.
#' - [FitMovement_Multi_Area()] for the multi-area optimisation.
#' - [Populate()] which calls this function internally.
#'
#' @export
FitMovement <-  function(Spatial, sim=1, age=1, year=1) {
  nArea <- dim(Spatial@Movement)[2]
  if (nArea==2) {
    movement <- FitMovement_2_Area(Spatial,
                                   sim,
                                   age,
                                   year)
  } else {
    movement <- FitMovement_Multi_Area(Spatial,
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
#' objective penalises deviations of the optimised diagonal (staying
#' probability) from `ProbStaying` and deviations of the implied asymptotic
#' distribution from `UnfishedDist`, both in log space.
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
#' (staying probabilities for Area 1 and Area 2). The off-diagonal entries are
#' derived as `1 - diagonal`. The objective is:
#'
#' \deqn{
#'   \bigl(\log m_{11} - \log p_{\text{stay}}\bigr)^2 +
#'   \bigl(\log \pi_1 - \log u_1\bigr)^2
#' }
#'
#' where \eqn{m_{11}} is the fitted staying probability in Area 1,
#' \eqn{p_{\text{stay}}} is the target from `ProbStaying`,
#' \eqn{\pi_1} is the asymptotic distribution in Area 1 (computed by
#' [CalcAsymDist_2Area()]), and \eqn{u_1} is the target from `UnfishedDist`.
#'
#' Dimension indices are clamped to the size of each slot's array, so a slot
#' with a singleton dimension is treated as constant across that index.
#'
#' @return A `2 × 2` numeric matrix with rows summing to 1.
#'
#' @seealso [FitMovement()], [FitMovement_Multi_Area()], [MarkovFrac()],
#'   [CalcAsymDist_2Area()]
#'
#' @keywords internal
FitMovement_2_Area <- function(Spatial, sim=1, age=1, year=1) {
  
  PS_dim <- dim(Spatial@ProbStaying)
  PS_sim <- min(sim, PS_dim[1])
  PS_age <- min(age, PS_dim[3])
  PS_year <- min(year, PS_dim[4])
  
  UD_dim <- dim(Spatial@UnfishedDist)
  UD_sim <- min(sim, UD_dim[1])
  UD_age <- min(age, UD_dim[3])
  UD_year <- min(year, UD_dim[4])
  
  optMovement <- stats::optim(logit(rep(Spatial@ProbStaying[PS_sim, 1, PS_age, PS_year], 2)),
                              SolveMovement_2_Area,
                              UnfishedDist = Spatial@UnfishedDist[UD_sim,1,UD_age,UD_year],
                              ProbStaying= Spatial@ProbStaying[PS_sim, 1, PS_age, PS_year],
                              method = "L-BFGS-B")
  
  MarkovFrac(LogitProbs=optMovement$par)
}

#' Objective Function for Two-Area Movement Optimisation
#'
#' Computes the least-squares objective used by [FitMovement_2_Area()].
#' Penalties are applied in log space to deviations of the fitted staying
#' probability from `ProbStaying` and of the implied asymptotic area
#' distribution from `UnfishedDist`.
#'
#' @param LogitProbs `numeric(2)`. Logit-transformed staying probabilities for
#'   Area 1 and Area 2. The off-diagonal entries are set to
#'   `1 - ilogit(LogitProbs)`.
#' @param UnfishedDist `numeric(1)`. Target unfished fraction in Area 1.
#' @param ProbStaying `numeric(1)`. Target probability of remaining in Area 1.
#'
#' @return `numeric(1)`. The value of the objective (to be minimised).
#'
#' @seealso [FitMovement_2_Area()], [CalcAsymDist_2Area()]
#'
#' @keywords internal
SolveMovement_2_Area <- function(LogitProbs,
                               UnfishedDist,
                               ProbStaying) {
  Movement <- matrix(0, 2,2)
  diag(Movement) <- ilogit(LogitProbs)
  Movement[1,2] <- 1- Movement[1,1]
  Movement[2,1] <- 1- Movement[2,2]
  
  Distribution <- CalcAsymDist_2Area(Movement)
  
  NLL <- (log(Movement[1,1]) - log(ProbStaying))^2 +
    (log(UnfishedDist) - log(Distribution[1]))^2
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
#' @seealso [FitMovement_2_Area()], [FitMovement_Multi_Area()]
#'
#' @keywords internal
MarkovFrac <- function(LogitProbs, FracOther=NULL, tol = 1e-10){
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
#' (logit scale, penalty `CVStay`). See [SolveMovement_Multi_Area()] for the
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
#' passed to [MarkovFrac()] to construct the full matrix at each iteration.
#'
#' @return An `nArea × nArea` numeric matrix with rows summing to 1.
#'
#' @seealso [FitMovement()], [FitMovement_2_Area()], [SolveMovement_Multi_Area()],
#'   [MarkovFrac()], [CalcAsymDist()]
#'
#' @keywords internal
FitMovement_Multi_Area <- function(Spatial, sim=1, age=1, year=1) {
  
  PS_dim <- dim(Spatial@ProbStaying)
  PS_sim <- min(sim, PS_dim[1])
  PS_age <- min(age, PS_dim[3])
  PS_year <- min(year, PS_dim[4])
  
  UD_dim <- dim(Spatial@UnfishedDist)
  UD_sim <- min(sim, UD_dim[1])
  UD_age <- min(age, UD_dim[3])
  UD_year <- min(year, UD_dim[4])
  
  FO_dim <- dim(Spatial@FracOther)
  FO_sim <- min(sim, FO_dim[1])
  FO_age <- min(age, FO_dim[4])
  FO_year <- min(year, FO_dim[5])
  
  nArea <- UD_dim[2]
  
  optMovement <- stats::nlminb(rep(0, nArea),
                               SolveMovement_Multi_Area,
                               UnfishedDist = Spatial@UnfishedDist[UD_sim, , UD_age, UD_year],
                               ProbStaying = Spatial@ProbStaying[PS_sim,, PS_age, PS_year],
                               FracOther = Spatial@FracOther[FO_sim,,, FO_age, FO_year],
                               CVDist = Spatial@CVDist,
                               CVStay=Spatial@CVStay,
                               control = list(iter.max = 5e3, eval.max = 1e4))
  
  MarkovFrac(LogitProbs=optMovement$par,
             FracOther = Spatial@FracOther[FO_sim,,, FO_age, FO_year])
  
}

#' Objective Function for Multi-Area Movement Optimisation
#'
#' Computes the penalised negative log-likelihood used by
#' [FitMovement_Multi_Area()]. Two normal penalties are applied: one on the
#' log-scale deviation of the implied asymptotic distribution from
#' `UnfishedDist` (controlled by `CVDist`), and one on the logit-scale
#' deviation of the staying probabilities from `ProbStaying` (controlled by
#' `CVStay`).
#'
#' @param LogitProbs `numeric(nArea)`. Logit-transformed staying probabilities,
#'   one per area. The full movement matrix is constructed via [MarkovFrac()].
#' @param UnfishedDist `numeric(nArea)`. Target unfished distribution across
#'   areas. Must sum to 1.
#' @param ProbStaying `numeric(nArea)`. Target staying probabilities, one per
#'   area.
#' @param FracOther `matrix`. Square matrix of relative off-diagonal movement
#'   probabilities. Diagonal elements must be `NA`.
#' @param CVDist `numeric(1)`. Standard deviation of the log-scale normal
#'   penalty on the distribution. Larger values allow greater deviation from
#'   `UnfishedDist`. Default `0.1`.
#' @param CVStay `numeric(1)`. Standard deviation of the logit-scale normal
#'   penalty on the staying probabilities. Larger values allow greater
#'   deviation from `ProbStaying`. Default `1`.
#'
#' @details
#' The objective is:
#'
#' \deqn{
#'   -\sum_i \log \mathcal{N}\!\left(\log \pi_i \mid \log u_i,\,
#'   \sigma_{\text{dist}}^2\right)
#'   -\sum_i \log \mathcal{N}\!\left(\text{logit}(m_{ii}) \mid
#'   \text{logit}(p_i),\, \sigma_{\text{stay}}^2\right)
#' }
#'
#' where \eqn{\pi_i} is the implied asymptotic distribution (from
#' [CalcAsymDist()]), \eqn{u_i} is `UnfishedDist[i]`, \eqn{m_{ii}} is the
#' fitted staying probability in area \eqn{i}, \eqn{p_i} is `ProbStaying[i]`,
#' \eqn{\sigma_{\text{dist}}} is `CVDist`, and \eqn{\sigma_{\text{stay}}} is
#' `CVStay`.
#'
#' @return `numeric(1)`. The value of the objective (to be minimised).
#'
#' @seealso [FitMovement_Multi_Area()], [MarkovFrac()], [CalcAsymDist()]
#'
#' @keywords internal
SolveMovement_Multi_Area <- function(LogitProbs, 
                                     UnfishedDist, 
                                     ProbStaying, 
                                     FracOther, 
                                     CVDist=0.1, 
                                     CVStay=1) {
  Movement <- MarkovFrac(LogitProbs, FracOther)
  Distribution <- CalcAsymDist(Movement)
  nll_dist <- dnorm(log(Distribution), log(UnfishedDist), CVDist, TRUE)
  nll_stay <- dnorm(LogitProbs, logit(ProbStaying), CVStay, TRUE)
  -sum(c(nll_dist, nll_stay))
}
