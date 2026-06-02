#' Internal Observation Error Helper Functions
#'
#' A family of internal functions used by [PopulateEffortObs()],
#' [PopulateCatchObs()], [PopulateIndexObs()], and [PopulateCompObs()] to
#' expand, validate, and generate observation error arrays across simulation
#' replicates and years.
#'
#' @param CV `numeric`. A CV value to expand. See [PopulateObsCV()].
#' @param object An S4 obs sub-object with `@Error`, `@CV`, and `@Bias` slots.
#'   See [PopulateObsError()] and [PopulateObsBias()].
#' @param nSim Integer. Number of simulation replicates.
#' @param Years Integer vector. Calendar years (historical and projection
#'   combined) over which the error array spans.
#' @param Ref `numeric`. A reference level value. See [PopulateObsRef()].
#' @param AC `numeric`. A lag-1 autocorrelation coefficient or vector. See
#'   [PopulateObsAC()].
#' @param Error `numeric` array `[nSim x nYear]`. A lognormal error array to
#'   which AR(1) autocorrelation is applied. See [ApplyObsAC()].
#' @param x `numeric`. A scalar, length-2 uniform bound, or named matrix to
#'   expand to `[nSim x nYear]`. See [PopulateObsScalar()].
#' @param label Character scalar. Name of the parameter being expanded, used
#'   in error messages. See [PopulateObsScalar()].
#' @param Shift `numeric`. A per-bin log-concentration offset. See
#'   [PopulateObsShift()].
#' @param Bins Numeric vector. Composition bin values (ages or length
#'   midpoints). See [PopulateObsShift()].
#' @param BinName Character scalar. Name of the bin dimension
#'   (e.g. `"Age"` or `"Size"`). See [PopulateObsShift()].
#'
#' @details
#'
#' ## PopulateObsCV
#'
#' Expands a CV value to a named `[nSim]` vector following the standard
#' `StructurePars` + `ExtendSims` convention. If `CV` already has dimnames
#' (i.e. it has been previously populated), it is trimmed to `nSim` and
#' returned unchanged.
#'
#' ## PopulateObsError
#'
#' Generates or validates a lognormal error array of dimension
#' `[nSim x nYear]`.
#'
#' If `object@Error` is empty and `object@CV` is set, draws `nSim × nYear`
#' lognormal values with mean 1 and the specified CV, returning a named
#' `[nSim x nYear]` array.
#'
#' If `object@Error` is already provided, it is validated against the expected
#' dimensions. A pre-supplied array with more rows than `nSim` is silently
#' trimmed; mismatches in either dimension after trimming raise an error.
#' Dimension names are applied to the validated array before returning.
#'
#' ## PopulateObsBias
#'
#' Expands a `Bias` value to a named `[nSim]` multiplicative array on the
#' natural scale. Accepted inputs:
#'
#' - Empty or `NULL`: defaults silently to 1 (no bias).
#' - Vector of length `nSim`: wrapped in a named array directly.
#' - Vector longer than `nSim`: trimmed silently to `nSim`.
#' - Scalar or length-2 vector: treated as a CV from which `nSim` lognormal
#'   values are drawn.
#'
#' All supplied values must be strictly positive.
#'
#' ## PopulateObsRef
#'
#' Generates or returns a named `[nSim]` reference level array. If `Ref` has
#' dimnames it is assumed already populated and trimmed to `nSim`. If
#' `length(Ref) == nSim` and `nSim != 2` (to avoid ambiguity with a length-2
#' uniform bound), it is returned as-is wrapped in a named array. Otherwise
#' `Ref` is treated as a CV and a lognormal draw of `nSim` values is taken.
#'
#' ## PopulateObsAC
#'
#' Expands a lag-1 autocorrelation coefficient to a named `[nSim]` array.
#' Accepted inputs:
#'
#' - Empty or `NULL`: defaults silently to 0 (no autocorrelation).
#' - Scalar: constant value applied to all simulations.
#' - Length-2 vector `c(lower, upper)` (when `nSim != 2`): bounds of a
#'   Uniform distribution from which `nSim` values are drawn.
#' - Vector of length `nSim`: used directly.
#' - Vector longer than `nSim`: trimmed silently.
#'
#' All values are validated to lie in `[-1, 1]`.
#'
#' ## ApplyObsAC
#'
#' Applies per-simulation lag-1 AR(1) autocorrelation to a
#' `[nSim x nYear]` lognormal error array using a variance-preserving
#' formulation. Let \eqn{\varepsilon_t = \log(\mathrm{Error}_t)} be the
#' raw log-errors. The AR(1) series is constructed as:
#'
#' \deqn{\varepsilon_t^* = \rho \, \varepsilon_{t-1}^* +
#'   \sqrt{1 - \rho^2} \, \varepsilon_t}
#'
#' where \eqn{\rho} is the per-simulation autocorrelation coefficient. This
#' preserves the marginal variance \eqn{\mathrm{Var}(\varepsilon_t^*) =
#' \mathrm{Var}(\varepsilon_t)} for all values of \eqn{\rho}. The AR(1)
#' series is exponentiated back to the natural scale before returning. If all
#' AC values are zero the input array is returned unchanged.
#'
#' ## PopulateObsScalar
#'
#' Expands a scalar, length-2 uniform bound, or named matrix to a
#' `[nSim x nYear]` array, following the standard change-point convention
#' used throughout the operating model. Accepted inputs:
#'
#' - Named array with correct dimensions: trimmed to `nSim` and returned.
#' - Length-2 vector `c(lower, upper)`: `nSim` values drawn from
#'   `Uniform(lower, upper)`, held constant across years.
#' - Scalar or change-point matrix: expanded via `StructurePars` +
#'   `ExtendSims` + `ExtendYears`.
#'
#' ## PopulateObsShift
#'
#' Expands a `Shift` value to a named `[nSim x nYear x nBin]` array following
#' the same change-point convention, with the third dimension named by
#' `BinName` and labelled with `Bins`. Accepted inputs:
#'
#' - Full `[nSim x nYear x nBin]` array: trimmed to `nSim` if oversupplied.
#' - Scalar: broadcast to all simulations, years, and bins.
#' - Vector of length `nBin`: bin-specific offset, constant across simulations
#'   and years.
#' - Named matrix or partial array with change-point years: expanded via
#'   `Extend()` then broadcast across bins.
#'
#' @return
#' - `PopulateObsCV()`: named `[nSim]` numeric vector, or the original
#'   zero-length value if `CV` was empty.
#' - `PopulateObsError()`: named `[nSim x nYear]` numeric array.
#' - `PopulateObsBias()`: named `[nSim]` numeric array of positive
#'   multiplicative bias values.
#' - `PopulateObsRef()`: named `[nSim]` numeric array, or the original
#'   zero-length value if `Ref` was empty.
#' - `PopulateObsAC()`: named `[nSim]` numeric array with values in `[-1, 1]`.
#' - `ApplyObsAC()`: named `[nSim x nYear]` numeric array with AR(1)
#'   autocorrelation applied on the log scale.
#' - `PopulateObsScalar()`: named `[nSim x nYear]` numeric array.
#' - `PopulateObsShift()`: named `[nSim x nYear x nBin]` numeric array.
#'
#' @seealso
#' [PopulateEffortObs()], [PopulateCatchObs()], [PopulateIndexObs()],
#' [PopulateCompObs()]
#'
#' @name populate-obs-helpers
#' @keywords internal
NULL


#' @rdname populate-obs-helpers
#' @export
PopulateObsCV <- function(CV, nSim) {
  if (!length(CV))
    return(CV)
  
  if (!is.null(dimnames(CV))) 
    return(CV)
  
  StructurePars(list(CV), nSim)[[1]] |>
    ExtendSims(nSim) |>
    DropDimension("Year", warn = FALSE)
}

#' @rdname populate-obs-helpers
#' @export
PopulateObsError <- function(object, nSim, Years) {
  nTS <- length(Years)
  
  if (length(object@Error) < 1) {
    if (is.null(object@CV))
      return(object@Error)
    
    Error <- array(
      rlnorm(
        nTS * nSim,
        mconv(1, rep(object@CV, nTS)),
        sdconv(1, rep(object@CV, nTS))
      ),
      dim      = c(nSim, nTS),
      dimnames = list(Sim = seq_len(nSim), Year = Years)
    )
    return(Error)
  }
  
  if (!inherits(object@Error, "array"))
    cli::cli_abort(
      c("x" = "`{class(object)}@Error` must be an array.",
        "i" = "Expected dimensions `[{nSim} x {nTS}]`.")
    )
  
  nr <- nrow(object@Error)
  nc <- ncol(object@Error)
  
  if (nr > nSim) {
    object@Error <- object@Error[seq_len(nSim), , drop = FALSE]
    nr <- nSim
  }
  
  chk_sim  <- nr != nSim
  chk_year <- nc != nTS
  
  if (chk_sim && chk_year)
    cli::cli_abort(
      c("x" = "`{class(object)}@Error` has incompatible dimensions.",
        "i" = "Expected `[{nSim} x {nTS}]`; got `[{nr} x {nc}]`.")
    )
  
  if (chk_sim)
    cli::cli_abort(
      c("x" = "`{class(object)}@Error` has wrong number of rows.",
        "i" = "Expected {nSim} simulation rows; got {nr}.")
    )
  
  if (chk_year)
    cli::cli_abort(
      c("x" = "`{class(object)}@Error` has wrong number of columns.",
        "i" = "Expected {nTS} year columns; got {nc}.")
    )
  
  dimnames(object@Error) <- list(Sim = seq_len(nSim), Year = Years)
  object@Error
}

#' @rdname populate-obs-helpers
#' @export
PopulateObsBias <- function(object, nSim) {
  cls <- class(object)
  
  if (length(object@Bias) < 1)
    return(array(1, dim = nSim, dimnames = list(Sim = seq_len(nSim))))
  
  if (any(object@Bias <= 0))
    cli::cli_abort(
      c("x" = "`{cls}@Bias` must contain positive values.",
        "i" = "`Bias` is a multiplicative scalar on the natural scale.")
    )
  
  if (length(object@Bias) > nSim)
    object@Bias <- object@Bias[seq_len(nSim)]
  
  if (length(object@Bias) == nSim)
    return(array(
      object@Bias,
      dim      = nSim,
      dimnames = list(Sim = seq_len(nSim))
    ))
  
  array(
    rlnorm(nSim, mconv(1, object@Bias), sdconv(1, object@Bias)),
    dim      = nSim,
    dimnames = list(Sim = seq_len(nSim))
  )
}

#' @rdname populate-obs-helpers
#' @export
PopulateObsRef <- function(Ref, nSim) {
  if (!length(Ref))
    return(Ref)
  
  if (!is.null(dimnames(Ref)))
    return(Ref[seq_len(nSim)])
  
  if (nSim != 2 && length(Ref) == nSim)
    return(Ref)
  
  CV <- StructurePars(list(Ref), nSim)[[1]] |>
    ExtendSims(nSim) |>
    DropDimension("Year", warn = FALSE)
  
  array(
    rlnorm(nSim, mconv(1, CV), sdconv(1, CV)),
    dim      = nSim,
    dimnames = list(Sim = seq_len(nSim))
  )
}

#' @rdname populate-obs-helpers
#' @export
PopulateObsAC <- function(AC, nSim) {
  if (!length(AC))
    return(array(0, dim = nSim, dimnames = list(Sim = seq_len(nSim))))
  
  if (!is.null(dimnames(AC))) {
    if (any(AC < -1) || any(AC > 1))
      cli::cli_abort(
        c("x" = "`AC` values must be in [-1, 1].",
          "i" = "`AC = 0` gives independent errors; `AC = 1` gives a random walk.")
      )
    return(AC)
  }
  
  if (length(AC) == 2 && nSim != 2) {
    if (AC[1] < -1 || AC[2] > 1)
      cli::cli_abort(
        c("x" = "`AC` bounds must be within [-1, 1].",
          "i" = "Supplied bounds: [{AC[1]}, {AC[2]}].")
      )
    vals <- runif(nSim, min = AC[1], max = AC[2])
    return(array(vals, dim = nSim, dimnames = list(Sim = seq_len(nSim))))
  }
  
  if (length(AC) > nSim)
    AC <- AC[seq_len(nSim)]
  
  if (length(AC) == 1)
    AC <- rep(AC, nSim)
  
  if (any(AC < -1) || any(AC > 1))
    cli::cli_abort(
      c("x" = "`AC` values must be in [-1, 1].",
        "i" = "`AC = 0` gives independent errors; `AC = 1` gives a random walk.")
    )
  
  array(AC, dim = nSim, dimnames = list(Sim = seq_len(nSim)))
}

#' @rdname populate-obs-helpers
#' @export
ApplyObsAC <- function(Error, AC) {
  if (!length(Error) || all(AC == 0))
    return(Error)
  
  nSim  <- nrow(Error)
  nYear <- ncol(Error)
  dns   <- dimnames(Error)
  
  log_err <- log(Error)
  
  for (s in seq_len(nSim)) {
    ac_sim <- min(s, dim(AC)[1])
    rho <- AC[ac_sim]
    if (rho == 0) next
    scale <- sqrt(1 - rho^2)
    for (t in 2:nYear) {
      log_err[s, t] <- rho * log_err[s, t - 1] + scale * log_err[s, t]
    }
  }
  
  Error <- exp(log_err)
  dimnames(Error) <- dns
  Error
}

#' @rdname populate-obs-helpers
#' @export
PopulateObsScalar <- function(x, nSim, Years, label = "value") {
  nYear <- length(Years)
  
  if (!is.null(dimnames(x))) {
    dd <- dim(x)
    
    if (length(dd) == 2) {
      
      dimnames(x) <- list(Sim = seq_len(dd[1]), Year = Years[seq_len(dd[2])])
      return(x)
    }
    cli::cli_abort(
      c("x" = "`{label}` array has incompatible dimensions.",
        "i" = "Expected `[{nSim} x {nYear}]`; got `[{nrow(x)} x {ncol(x)}]`.")
    )
  }
  
  if (length(x) == 2) {
    vals <- runif(nSim, min = x[1], max = x[2])
    return(array(
      rep(vals, nYear),
      dim      = c(nSim, nYear),
      dimnames = list(Sim = seq_len(nSim), Year = Years)
    ))
  }
  
  expanded <- StructurePars(list(x), nSim)[[1]] |>
    ExtendSims(nSim) |>
    ExtendYears(Years)
  
  dimnames(expanded) <- list(Sim = seq_len(nSim), Year = Years)
  expanded
}

#' @rdname populate-obs-helpers
#' @importFrom rlang :=
#' @export
PopulateObsShift <- function(Shift, nSim, Years, Bins, BinName = "Bin") {
  nYear <- length(Years)
  nBin  <- length(Bins)
  
  if (is.array(Shift) && length(dim(Shift)) == 3) {
    if (dim(Shift)[1] >= nSim &&
        dim(Shift)[2] == nYear &&
        dim(Shift)[3] == nBin) {
      Shift <- Shift[seq_len(nSim), , , drop = FALSE]
      dimnames(Shift) <- list(
        Sim        = seq_len(nSim),
        Year       = Years,
        !!BinName := Bins
      )
      return(Shift)
    }
    cli::cli_abort(
      c("x" = "`Shift` array has incompatible dimensions.",
        "i" = "Expected `[{nSim} x {nYear} x {nBin}]`.")
    )
  }
  
  if (length(Shift) == 1) {
    return(array(
      Shift,
      dim      = c(nSim, nYear, nBin),
      dimnames = list(Sim = seq_len(nSim), Year = Years, !!BinName := Bins)
    ))
  }
  
  if (is.vector(Shift) && length(Shift) == nBin) {
    return(array(
      rep(Shift, each = nSim * nYear),
      dim      = c(nSim, nYear, nBin),
      dimnames = list(Sim = seq_len(nSim), Year = Years, !!BinName := Bins)
    ))
  }
  
  expanded <- Extend(
    Shift,
    nSim  = nSim,
    Years = Years[1]
  )
  
  if (length(dim(expanded)) == 2) {
    expanded <- array(
      rep(as.vector(expanded), nBin),
      dim      = c(nSim, nYear, nBin),
      dimnames = list(Sim = seq_len(nSim), Year = Years, !!BinName := Bins)
    )
  } else {
    dimnames(expanded) <- list(
      Sim        = seq_len(nSim),
      Year       = Years,
      !!BinName := Bins
    )
  }
  
  expanded
}