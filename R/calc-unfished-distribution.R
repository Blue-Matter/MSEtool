#' Calculate the Unfished Spatial Distribution
#'
#' Computes the unfished spatial distribution for each simulation, age class,
#' and time step from `Spatial@Movement`. The result is stored in
#' `Spatial@UnfishedDist`.
#'
#' For a single-area model, all values are set to 1. For multi-area models the
#' distribution is the equilibrium reached when the movement matrices of a
#' calendar year are repeated indefinitely, following each cohort through the
#' time steps of the year. Movement is applied as in the population dynamics:
#' fish of age \eqn{a} in time step \eqn{t} are distributed as
#' \deqn{\boldsymbol{\pi}_{a,t} = \boldsymbol{\pi}_{a-1,t-1} \mathbf{M}_{a,t}}
#' where \eqn{t-1} is the previous time step within the repeated annual cycle.
#'
#' - **Age-invariant movement** (`Movement` Age dimension = 1): all ages share
#'   the periodic stationary distribution of the annual cycle of movement
#'   matrices. For annual models this is the stationary distribution of the
#'   movement matrix ([CalcAsymDist()]).
#'
#' - **Age-varying movement** (`Movement` Age dimension > 1): recruits take the
#'   age-0 distribution in `Spatial@UnfishedDist[, , 1, ]` when it is populated,
#'   otherwise the periodic stationary distribution of the age-0 movement
#'   matrices. Older ages follow the cohort recursion above. The plus group
#'   combines fish entering from the previous age class with fish remaining in
#'   the plus group. When `Number` and `Survival` are supplied, these are
#'   weighted by their unfished numbers; otherwise the plus group takes the
#'   periodic stationary distribution of its own movement matrices.
#'
#' The distribution is calculated for each calendar year in the `Year`
#' dimension of `Movement` (or of `Number`, when supplied), with all time
#' steps of that year.
#'
#' @param Spatial A [Spatial()] object with a populated `Movement` slot. The
#'   `Movement` array must have dimensions `Sim × FromArea × ToArea × Age × Year`.
#' @param Ages An [ages-class] object supplying age class labels. If `NULL`
#'   (default), [DefaultAges()] is used.
#' @param Years Numeric vector of time steps. If `NULL` (default),
#'   [DefaultYears()] is used.
#' @param Number Optional unfished numbers-at-age, `Sim × Age × Year`, summed
#'   over areas.
#' @param Survival Optional unfished survival-at-age over one time step,
#'   `Sim × Age × Year`.
#'
#' @return `Spatial` with `Spatial@UnfishedDist` populated as a named
#'   `Sim × Area × Age × Year` array.
#'
#' @seealso [CalcAsymDist()], [FitMovement()], [Spatial()]
#' @export
CalcUnfishedDist <- function(Spatial, Ages=NULL, Years=NULL, Number=NULL, Survival=NULL) {
  Ages  <- DefaultAges(Ages)
  Years <- DefaultYears(Years)

  Mov <- Spatial@Movement
  dims <- dim(Mov)
  if (is.null(dims))
    return(Spatial)

  nSim  <- dims[1]
  nArea <- dims[2]
  nAge  <- dims[4]
  MovYears <- as.numeric(dimnames(Mov)[['Year']] %||% Years[seq_len(dims[5])])
  AgeClasses <- Ages@Classes[seq_len(nAge)]

  if (nArea == 1L) {
    Spatial@UnfishedDist <- array(1, c(nSim, 1, nAge, length(MovYears)),
                                  list(Sim = seq_len(nSim), Area = 1,
                                       Age = AgeClasses, Year = MovYears))
    return(Spatial)
  }

  Exact <- nAge > 1L && !is.null(Number) && !is.null(Survival)

  UD <- Spatial@UnfishedDist
  HasRec <- nAge > 1L && is.array(UD) && length(dim(UD)) == 4L && dim(UD)[2] == nArea

  CalYears <- floor(MovYears + 1e-8)
  if (HasRec)
    CalYears <- c(CalYears, floor(as.numeric(dimnames(UD)[['Year']]) + 1e-8))
  if (Exact)
    CalYears <- c(CalYears, floor(as.numeric(dimnames(Number)[['Year']]) + 1e-8))
  CalYears <- sort(unique(CalYears))

  YearCal <- floor(Years + 1e-8)
  StepsPerYear <- table(YearCal)
  nStep <- max(StepsPerYear)
  RefSteps <- Years[YearCal == as.numeric(names(StepsPerYear)[which.max(StepsPerYear)])]
  Cycles <- lapply(CalYears, \(cy) {
    Steps <- Years[YearCal == cy]
    if (length(Steps) != nStep)
      Steps <- cy + RefSteps %% 1
    Steps
  })
  AllSteps <- unlist(Cycles)

  AtSteps <- function(x, Steps) {
    x <- ExtendSims(x, nSim) |> ExtendYears(AllSteps, backfill = TRUE)
    Yd <- match('Year', names(dimnames(x)))
    Ind <- match(round(Steps, 6), round(as.numeric(dimnames(x)[[Yd]]), 6))
    abind::asub(x, Ind, Yd, drop = FALSE)
  }

  PlusGroup <- isTRUE(Ages@PlusGroup)
  Out <- array(NA_real_, c(nSim, nArea, nAge, length(AllSteps)),
               list(Sim = seq_len(nSim), Area = seq_len(nArea),
                    Age = AgeClasses, Year = AllSteps))

  Prev <- NULL
  for (i in seq_along(CalYears)) {
    Steps <- Cycles[[i]]
    Inputs <- list(Mov = unname(AtSteps(Mov, Steps)),
                   Rec = if (HasRec) unname(AtSteps(UD, Steps)[, , 1, , drop = FALSE]),
                   N   = if (Exact) unname(AtSteps(Number, Steps)),
                   S   = if (Exact) unname(AtSteps(Survival, Steps)))
    Cols <- (i - 1L) * nStep + seq_len(nStep)
    if (identical(Inputs, Prev)) {
      Out[, , , Cols] <- Out[, , , Cols - nStep]
      next
    }
    Prev <- Inputs
    for (s in seq_len(nSim)) {
      Out[s, , , Cols] <- .UnfishedDistCycle(
        Mov = Inputs$Mov[s, , , , , drop = TRUE] |> array(c(nArea, nArea, nAge, nStep)),
        Rec = if (HasRec) matrix(Inputs$Rec[s, , 1, ], nArea, nStep),
        N   = if (Exact) matrix(Inputs$N[s, , ], nAge, nStep),
        S   = if (Exact) matrix(Inputs$S[s, , ], nAge, nStep),
        PlusGroup = PlusGroup)
    }
  }

  # one step per year when constant within each year and the year starts on a whole year
  First <- (seq_along(CalYears) - 1L) * nStep + 1L
  if (nStep > 1L && all(abs(AllSteps[First] %% 1) < 1e-8) &&
      isTRUE(all.equal(Out, Out[, , , rep(First, each = nStep), drop = FALSE],
                       check.attributes = FALSE, tolerance = 1e-12)))
    Out <- Out[, , , First, drop = FALSE]

  Spatial@UnfishedDist <- Out
  Spatial
}

# Equilibrium distribution over one repeated annual cycle for one sim; returns Area x Age x Step
.UnfishedDistCycle <- function(Mov, Rec = NULL, N = NULL, S = NULL, PlusGroup = TRUE) {
  nArea <- dim(Mov)[1]
  nAge  <- dim(Mov)[3]
  nStep <- dim(Mov)[4]
  Prev  <- c(nStep, seq_len(nStep - 1L))
  M <- \(a, k) matrix(Mov[, , a, k], nArea, nArea)
  Out <- array(NA_real_, c(nArea, nAge, nStep))

  # periodic stationary distribution of age a's movement matrices
  Periodic <- function(a) {
    P <- diag(nArea)
    for (k in seq_len(nStep)) P <- P %*% M(a, k)
    p <- CalcAsymDist(P)
    vapply(seq_len(nStep), \(k) {
      p <<- as.vector(p %*% M(a, k))
      p / sum(p)
    }, numeric(nArea))
  }

  if (nAge == 1L) {
    Out[, 1, ] <- Periodic(1)
    return(Out)
  }

  Out[, 1, ] <- if (is.null(Rec)) Periodic(1) else Rec
  LastChain <- if (PlusGroup) nAge - 1L else nAge
  for (a in seq_len(LastChain)[-1]) {
    for (k in seq_len(nStep)) {
      p <- as.vector(Out[, a - 1L, Prev[k]] %*% M(a, k))
      Out[, a, k] <- p / sum(p)
    }
  }
  if (!PlusGroup)
    return(Out)

  Fallback <- Periodic(nAge)
  if (is.null(N) || is.null(S)) {
    Out[, nAge, ] <- Fallback
    return(Out)
  }

  # plus-group numbers by area are linear over the cycle: n_end = n_start C + d
  C <- diag(nArea)
  d <- rep(0, nArea)
  Inflow <- matrix(0, nArea, nStep)
  for (k in seq_len(nStep)) {
    Mk <- M(nAge, k)
    Inflow[, k] <- as.vector((N[nAge - 1L, Prev[k]] * S[nAge - 1L, Prev[k]] *
                                Out[, nAge - 1L, Prev[k]]) %*% Mk)
    Step <- S[nAge, Prev[k]] * Mk
    C <- C %*% Step
    d <- as.vector(d %*% Step) + Inflow[, k]
  }
  nEnd <- tryCatch(as.vector(d %*% solve(diag(nArea) - C)), error = \(e) NULL)
  if (is.null(nEnd) || any(!is.finite(nEnd))) {
    Out[, nAge, ] <- Fallback
    return(Out)
  }
  n <- nEnd
  for (k in seq_len(nStep)) {
    n <- as.vector((S[nAge, Prev[k]] * n) %*% M(nAge, k)) + Inflow[, k]
    Out[, nAge, k] <- if (sum(n) > 0) n / sum(n) else Fallback[, k]
  }
  Out
}
