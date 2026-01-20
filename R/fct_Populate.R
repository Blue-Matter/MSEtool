#' Populate Operating Model Components
#'
#' Populate operating model objects and their component classes by generating 
#' stochastic values, filling derived slots, and checking object structure
#'  and contents.
#'
#' @details
#' `Populate()` is an S4 generic used to initialize and expand operating
#' model objects and their components across simulation replicates, years,
#' seasons, and spatial areas.
#'
#' Methods are defined for [OM()], stock-level biological components
#' (e.g. [Length()], [Weight()], [NaturalMortality()], [Maturity()],
#' [Fecundity()], [SRR()], [Spatial()]), fleet components
#' (e.g. [Fleet()], [Effort()], [Catchability()], [Selectivity()],
#' [Retention()], [DiscardMortality()]), and observation models.
#'
#' Population typically involves generating stochastic values, filling derived
#' slots (e.g., `MeanAtAge` from `Model` and `Pars`), and checking internal
#' consistency and validity of the objets.
#'
#' @param object An object to be populated. The class of `object` determines
#'   which method is dispatched.
#'
#' @param nYear Number of historical years.
#' @param pYear Number of projection years.
#' @param CurrentYear Character or numeric. Calendar year of last historical year.
#' @param Years Numeric vector of years to populate.
#' @param HistYears Numeric vector of historical years.
#' @param ProjYears Numeric vector of projection years.
#'
#' @param nSim Number of simulations
#' @param Seasons Number of seasons per year.
#' @param nArea Number of spatial areas.
#'
#' @param Ages An [Ages()] object.
#' @param Length A [Length()] object.
#' @param Weight A [Weight()] object.
#' @param Maturity A [Maturity()] object.
#' @param RelativeSize `nSim` x `nArea` matrix of the relative area size
#'
#' @param ALK Logical; whether to populate age–length keys.
#' @param AWK Logical; whether to populate age–weight keys.
#' @param ASK Logical; whether to populate age–size keys.
#'
#' @param CalcAtLength Logical; whether to calculate at-length from at-age arrays
#' @param Stock A [Stock()] object used to populate [Fleet()] objects.
#'
#' @param seed Integer random seed used for gnerating stochastic values.
#' @param silent Logical; if `TRUE`, suppress messages.
#' @param force Logical; if `TRUE`, force re-population even if the object
#'   digest is unchanged.
#' @param ... Additional arguments passed to class-specific population methods.
#'
#' @return
#' An object of the same class as \code{object}, populated with simulation- and
#' time-specific values.
#'
#' @section Methods:
#' \describe{
#'   \item{`Populate(om)`}{Populate an [OM()] object}
#'   \item{`Populate(stock)`}{Populate a [Stock()] object}
#'   \item{`Populate(length)`}{Populate a [Length()] object}
#'   \item{`Populate(weight)`}{Populate a [Weight()] object}
#'   \item{`Populate(naturalmortality)`}{Populate a [NaturalMortality()] object}
#'   \item{`Populate(maturity)`}{Populate a [Maturity()] object}
#'   \item{`Populate(fecundity)`}{Populate a [Fecundity()] object}
#'   \item{`Populate(srr)`}{Populate an [SRR()] object}
#'   \item{`Populate(spatial)`}{Populate a [Spatial()] object}
#'   \item{`Populate(fleet)`}{Populate a [Fleet()] object}
#'   \item{`Populate(effort)`}{Populate an [Effort()] object}
#'   \item{`Populate(catchability)`}{Populate a [Catchability()] object}
#'   \item{`Populate(selectivity)`}{Populate a [Selectivity()] object}
#'   \item{`Populate(retention)`}{Populate a [Retention()] object}
#'   \item{`Populate(discardmortality)`}{Populate a [DiscardMortality()] object}
#' }
#'
#' @seealso
#' [PopulateOM()], [PopulateStock()], [PopulateFleet()]
#'
#' @name Populate
#' @rdname Populate
#' @export
setGeneric("Populate", function(object, ...) {
  standardGeneric("Populate")
})

## ---- OM ----

#' @name Populate
#' @export
setMethod("Populate", "om", function(object, silent = FALSE, ...) {
  PopulateOM(object, silent)
})


## ---- Stock ----
#' @name Populate
#' @param ALK ...
#' @param AWK ...
#' @export
setMethod("Populate", "stock", function(object,
                                        nYear,
                                        pYear,
                                        CurrentYear = NULL,
                                        nSim = 5,
                                        Seasons = 1,
                                        ALK = TRUE,
                                        AWK = TRUE,
                                        seed = NULL,
                                        silent = FALSE, ...) {
  PopulateStock(object, nYear, pYear, CurrentYear, nSim, Seasons, ALK, AWK, seed, silent)
})

### ---- Length ----
#' @name Populate
#' @param ALK ...
#' @param AWK ...
#' @export
setMethod("Populate", "length", function(object,
                                         Ages = NULL,
                                         Years = NULL,
                                         nSim = 5,
                                         ASK = TRUE,
                                         seed = NULL,
                                         silent = FALSE,
                                         ...) {
  PopulateLength(
    object,
    Ages,
    Years,
    nSim,
    ASK,
    seed,
    silent
  )
})

### ---- Weight ----
#' @name Populate
#' @export
setMethod("Populate", "weight", function(object,
                                         Ages = NULL,
                                         Length = NULL,
                                         Years = NULL,
                                         nSim = 5,
                                         ASK = FALSE,
                                         seed = NULL,
                                         silent = FALSE,
                                         CalcAtLength = FALSE,
                                         ...) {
  PopulateWeight(
    object,
    Ages,
    Length,
    Years,
    nSim,
    ASK,
    seed,
    silent,
    CalcAtLength
  )
})

### ---- NaturalMortality ----

#' @name Populate
#' @export
setMethod("Populate", "naturalmortality", function(object,
                                                   Ages = NULL,
                                                   Length = NULL,
                                                   Years = NULL,
                                                   nSim = 5,
                                                   seed = NULL,
                                                   silent = FALSE,
                                                   CalcAtLength = FALSE,
                                                   ...) {
  PopulateNaturalMortality(
    object,
    Ages,
    Length,
    Years,
    nSim,
    seed,
    silent,
    CalcAtLength
  )
})

### ---- Maturity ----
#' @name Populate
#' @export
setMethod("Populate", "maturity", function(object,
                                           Ages = NULL,
                                           Length = NULL,
                                           Weight = NULL,
                                           Years = NULL,
                                           nSim = 5,
                                           seed = NULL,
                                           silent = FALSE,
                                           CalcAtLength = FALSE,
                                           ...) {
  PopulateMaturity(
    object,
    Ages,
    Length,
    Weight,
    Years,
    nSim,
    seed,
    silent,
    CalcAtLength
  )
})

### --- Fecundity ----
#' @name Populate
#' @export
setMethod("Populate", "fecundity", function(object,
                                            Ages = NULL,
                                            Length = NULL,
                                            Weight = NULL,
                                            Maturity = NULL,
                                            Years = NULL,
                                            nSim = 5,
                                            seed = NULL,
                                            silent = FALSE,
                                            CalcAtLength = FALSE,
                                            ...) {
  PopulateFecundity(
    object,
    Ages,
    Length,
    Weight,
    Maturity,
    Years,
    nSim,
    seed,
    silent,
    CalcAtLength
  )
})

### --- SRR ----
#' @name Populate
#' @export
setMethod("Populate", "srr", function(object,
                                      Ages = NULL,
                                      CurrentYear = NULL,
                                      Years = NULL,
                                      nSim = 5,
                                      seed = NULL,
                                      silent = FALSE,
                                      ...) {
  PopulateSRR(
    object,
    Ages,
    CurrentYear,
    Years,
    nSim,
    seed,
    silent
  )
})

### ---- Spatial ----
#' @name Populate
#' @export
setMethod("Populate", "spatial", function(object,
                                          Ages = NULL,
                                          Years = NULL,
                                          nSim = 5,
                                          seed = NULL,
                                          silent = FALSE,
                                          ...) {
  PopulateSpatial(
    object,
    Ages,
    Years,
    nSim,
    seed,
    silent
  )
})


### --- Depletion ----
#' @name Populate
#' @export
setMethod("Populate", "depletion", function(object,
                                      nSim = 5,
                                      seed = NULL,
                                      silent = FALSE,
                                      ...) {
  PopulateDepletion(
    object,
    nSim,
    seed,
    silent
  )
})

# Fleet ----
#' @name Populate
#' @export
setMethod("Populate", "fleet", function(object,
                                        Stock,
                                        seed = NULL,
                                        silent = FALSE,
                                        ...) {
  PopulateFleet(
    Fleet = object,
    Stock,
    seed,
    silent
  )
})

### ---- Effort ----
#' @name Populate
#' @export
setMethod("Populate", "effort", function(object,
                                         HistYears = NULL,
                                         nArea = 1,
                                         nSim = 5,
                                         seed = NULL,
                                         ...) {
  PopulateEffort(
    Effort = object,
    HistYears,
    nArea,
    nSim,
    seed
  )
})


### ---- Catchability ----
#' @name Populate
#' @export
setMethod("Populate", "catchability", function(object,
                                               RelativeSize = NULL,
                                               nSim = 5,
                                               HistYears = NULL,
                                               ProjYears = NULL,
                                               seed = NULL,
                                               silent = FALSE,
                                               ...) {
  PopulateCatchability(
    Fleet = object,
    RelativeSize,
    nSim,
    HistYears,
    ProjYears,
    seed,
    silent
  )
})

### ---- Selectivity ----
#' @name Populate
#' @export
setMethod("Populate", "selectivity", function(object,
                                              Ages,
                                              Length = NULL,
                                              Weight = NULL,
                                              nSim = 5,
                                              Years = NULL,
                                              nArea = 1,
                                              CalcAtLength = TRUE,
                                              seed = NULL,
                                              silent = FALSE,
                                              ...) {
  PopulateSelectivity(
    Selectivity = object,
    Ages,
    Length,
    Weight,
    nSim,
    Years,
    nArea,
    CalcAtLength,
    seed,
    silent = silent
  )
})

### ---- Retention ----
#' @name Populate
#' @export
setMethod("Populate", "retention", function(object,
                                            Ages,
                                            Length = NULL,
                                            Weight = NULL,
                                            nSim = 5,
                                            Years,
                                            nArea = 1,
                                            CalcAtLength = TRUE,
                                            seed = NULL,
                                            silent = FALSE,
                                            ...) {
  PopulateRetention(
    Retention = object,
    Ages,
    Length,
    Weight,
    nSim,
    Years,
    nArea,
    CalcAtLength,
    seed,
    silent = silent
  )
})

### ---- DiscardMortality ----
#' @name Populate
#' @export
setMethod("Populate", "discardmortality", function(object,
                                                   Ages,
                                                   Length = NULL,
                                                   nSim = 5,
                                                   Years,
                                                   nArea,
                                                   CalcAtLength = TRUE,
                                                   seed = NULL,
                                                   silent = FALSE,
                                                   ...) {
  PopulateDiscardMortality(
    DiscardMortality = object,
    Ages,
    Length,
    nSim,
    Years,
    nArea,
    CalcAtLength,
    seed,
    silent = silent
  )
})



getACF <- function(Value) {
  acf(Value, plot = FALSE)$acf[2, 1, 1]
}

PopulateIndexObs <- function(Index, nSim, HistYears, ProjYears) {
  Index@CV <- PopulateObsCV(Index@CV, nSim)
  Index@Error <- PopulateObsError(Index, nSim, c(HistYears, ProjYears))
  Index@Beta # TODO - currently not implemented
  Index@Ref <- PopulateObsRef(Index@Ref, nSim)

  if (length(Index@Years) < 1) {
    Index@Years <- HistYears
  }

  # TODO implement AC if specified
  if (!is.null(Index@AC)) {}

  Index@Selectivity
  Index@Type
  Index@q
  Index
}

PopulateEffortObs <- function(Effort, nSim, HistYears, ProjYears) {
  if (EmptyObject(Effort)) {
    return(Effort)
  }
  Effort@CV <- PopulateObsCV(Effort@CV, nSim)
  Effort@Error <- PopulateObsError(Effort, nSim, c(HistYears, ProjYears))
  Effort@Bias <- PopulateObsBias(Effort, nSim)

  # if (length(Effort@Years)<1)
  #   Effort@Years <- Years
  Effort
}

PopulateCatchObs <- function(Catch, nSim, HistYears, ProjYears) {
  if (EmptyObject(Catch)) {
    return(Catch)
  }

  Catch@CV <- PopulateObsCV(Catch@CV, nSim)
  Catch@Error <- PopulateObsError(Catch, nSim, c(HistYears, ProjYears))
  Catch@Bias <- PopulateObsBias(Catch, nSim)
  Catch@Ref <- PopulateObsRef(Catch@Ref, nSim)

  if (length(Catch@Years) < 1) {
    Catch@Years <- HistYears
  }

  if (!Catch@Type %in% c("Removals", "Landings")) {
    cli::cli_abort(message = "Valid values for `Obs@Catch@Type` are: {.val {c('Removals', 'Landings')}} ")
  }

  Catch
}

PopulateObsRef <- function(Ref, nSim) {
  if (!length(Ref)) 
    return(Ref)
  
  if (!is.null(dimnames(Ref)))  
    return(Ref[1:nSim])
  
  if (nSim != 2 && length(Ref) == nSim) 
    return(Ref)
  

  CV <- StructurePars(list(Ref), nSim)[[1]] |>
    ExtendSims(nSim) |>
    DropDimension("Year", FALSE)

  Error <- array(
    rlnorm(
      nSim,
      mconv(1, CV),
      sdconv(1, CV)
    ),
    nSim
  )

  dimnames(Error) <- list(Sim = 1:nSim)
  Error
}

PopulateObsCV <- function(CV, nSim) {
  if (!length(CV)) return(CV)
  if (!is.null(dimnames(CV))) return(CV[seq_len(nSim)])
  
  StructurePars(list(CV), nSim)[[1]] |>
    ExtendSims(nSim) |>
    DropDimension("Year", warn = FALSE)
}


PopulateObsError <- function(object, nSim, Years) {
  nTS <- length(Years)
  if (length(object@Error) < 1) {
    if (is.null(object@CV)) {
      return(object@Error)
    }
    Error <- array(
      rlnorm(
        nTS * nSim,
        mconv(1, rep(object@CV, nTS)),
        sdconv(1, rep(object@CV, nTS))
      ),
      c(nSim, nTS)
    )

    dimnames(Error) <- list(
      Sim = 1:nSim,
      Year = Years
    )
    object@Error <- Error
  } else {
    if (!inherits(object@Error, "array")) {
      cli::cli_abort("`object@Error` must be an array with `nSim` rows and `nTS` columns")
    }

    chk1 <- nrow(object@Error) != nSim

    if (chk1 & nrow(object@Error) > nSim) {
      object@Error <- object@Error[1:nSim, ]
      chk1 <- !chk1
    }


    chk2 <- ncol(object@Error) != length(Years)
    if (chk1 & chk2) {
      cli::cli_abort("`object@Error` must be an array with `nSim` rows and `nTS` columns")
    }
    if (chk1 & !chk2) {
      cli::cli_abort("`object@Error` must be an array with `nSim` rows")
    }

    if (!chk1 & chk2) {
      cli::cli_abort("`object@Error` must be an array with `nTS` columns")
    }

    dimnames(object@Error) <- list(
      Sim = 1:nSim,
      Year = Years
    )
  }
  object@Error
}

PopulateObsBias <- function(object, nSim) {
  if (length(object@Bias) < 1) {
    object@Bias <- array(1, dim = nSim, dimnames = list(Sim = 1:nSim))
    return(object@Bias)
  }

  if (any(object@Bias <= 0)) {
    cli::cli_abort("`Bias` must be positive values")
  }

  if (length(object@Bias) != nSim) {
    object@Bias <- array(rlnorm(nSim, mconv(1, object@Bias), sdconv(1, object@Bias)),
      dim = nSim,
      dimnames = list(Sim = 1:nSim)
    )
    return(object@Bias)
  }

  if (length(object@Bias) > nSim) {
    object@Bias <- object@Bias[1:nSim]
  }

  if (length(object@Bias) == nSim) {
    object@Bias <- array(object@Bias, dim = nSim, dimnames = list(Sim = 1:nSim))
    return(object@Bias)
  }
  cli::cli_abort("`Catch@Bias` must be length 1, 2, or `nSim`")
  object@Bias
}
