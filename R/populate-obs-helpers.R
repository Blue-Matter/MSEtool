
# slots set before Populate fills defaults; conditioning keeps these for projection years
.RecordUserSupplied <- function(object, slots) {
  if (!is.null(object@Misc$UserSupplied)) return(object)
  object@Misc$UserSupplied <- slots[vapply(slots, \(sl) length(slot(object, sl)) > 0, logical(1))]
  object
}

.UserSupplied <- function(object, slot) slot %in% object@Misc$UserSupplied

# [nSim x nYear] projection-year values of a populated [Sim x Year] array
.ProjYearValues <- function(x, nSim, ProjYears) {
  x <- .ArraySubsetYear(x, ProjYears)
  if (length(dim(x)) != 2) x <- matrix(x, nrow = 1)
  x <- x[pmin(seq_len(nSim), nrow(x)), pmin(seq_along(ProjYears), ncol(x)), drop = FALSE]
  dimnames(x) <- list(Sim = seq_len(nSim), Year = ProjYears)
  x
}

# per-sim value of a populated [Sim] array
.SimValues <- function(x, nSim) as.numeric(x)[pmin(seq_len(nSim), length(x))]

.PopulateObsCV <- function(CV, nSim) {
  if (!length(CV))
    return(CV)
  
  if (!is.null(dimnames(CV))) 
    return(CV)
  
  .StructurePars(list(CV), nSim)[[1]] |>
    ExtendSims(nSim) |>
    DropDimension("Year", warn = FALSE)
}

.PopulateObsBeta <- function(Beta, nSim) {
  if (!is.null(Beta) && !length(Beta))
    Beta <- NULL
  if (is.null(Beta)) return(Beta)
  
  .StructurePars(list(Beta), nSim)[[1]] |>
    ExtendSims(nSim) |>
    DropDimension("Year", warn = FALSE)
  
  
}

.PopulateObsError <- function(object, nSim, Years) {
  nTS <- length(Years)
  
  if (length(object@Error) < 1) {
    if (is.null(object@CV))
      return(object@Error)
    
    CV <- rep_len(as.numeric(object@CV), nSim)
    Error <- array(
      .SimDraws(\(i, n) rlnorm(n, mconv(1, CV[i]), sdconv(1, CV[i])), nSim, nTS),
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

  if (nc > 1 && nc < nTS) {
    existing_years <- suppressWarnings(as.numeric(dimnames(object@Error)$Year))
    if (!anyNA(existing_years) && all(existing_years %in% Years)) {
      object@Error <- ExtendYears(object@Error, Years = Years)
      nc <- ncol(object@Error)
    }
  }

  chk_sim  <- nr != nSim && nr != 1
  chk_year <- nc != nTS  && nc != 1
  
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
  
  dimnames(object@Error) <- list(Sim = seq_len(nr), Year = Years[seq_len(nc)])
  object@Error
}

 
.PopulateObsBias <- function(object, nSim) {
  cls <- class(object)
  
  if (length(object@Bias) < 1)
    return(array(1, dim = nSim, dimnames = list(Sim = seq_len(nSim))))
  
  if (any(object@Bias < 0))
    cli::cli_abort(
      c("x" = "`{cls}@Bias` must contain positive values.",
        "i" = "`Bias` is a multiplicative scalar on the natural scale.")
    )
  
  if (length(object@Bias) == 2 && nSim != 2)
    return(array(
      .SimRunif(nSim, min(object@Bias), max(object@Bias)),
      dim      = nSim,
      dimnames = list(Sim = seq_len(nSim))
    ))

  if (length(object@Bias) > nSim)
    object@Bias <- object@Bias[seq_len(nSim)]

  if (length(object@Bias) == nSim)
    return(array(
      object@Bias,
      dim      = nSim,
      dimnames = list(Sim = seq_len(nSim))
    ))

  array(
    .SimRlnorm(nSim, mconv(1, object@Bias), sdconv(1, object@Bias)),
    dim      = nSim,
    dimnames = list(Sim = seq_len(nSim))
  )
}


 
.PopulateObsRef <- function(Ref, nSim) {
  if (!length(Ref))
    return(Ref)
  
  if (!is.null(dimnames(Ref)))
    return(Ref[seq_len(nSim)])
  
  if (nSim != 2 && length(Ref) == nSim)
    return(Ref)
  
  CV <- .StructurePars(list(Ref), nSim)[[1]] |>
    ExtendSims(nSim) |>
    DropDimension("Year", warn = FALSE)
  
  array(
    .SimRlnorm(nSim, mconv(1, CV), sdconv(1, CV)),
    dim      = nSim,
    dimnames = list(Sim = seq_len(nSim))
  )
}

.PopulateObsqInc <- function(qInc, nSim) {
  if (!length(qInc) || all(qInc == 0))
    return(NULL)

  if (!is.null(dimnames(qInc)))
    return(qInc[seq_len(min(nSim, length(qInc)))])

  qInc <- .StructurePar(qInc, nSim = nSim)[, 1] |> unname()
  array(rep_len(qInc, nSim), dim = nSim, dimnames = list(Sim = seq_len(nSim)))
}

# compounds per calendar year: Elapsed is years since the last historical time step
.qIncMultiplier <- function(qInc, Elapsed) {
  (1 + qInc / 100)^Elapsed
}


.CheckObsAC <- function(AC) {
  if (any(abs(AC) >= 1, na.rm = TRUE))
    cli::cli_abort(
      c("x" = "`AC` values must be within (-1, 1), exclusive.",
        "i" = "`AC = 0` gives independent errors; an AR(1) process with `abs(AC) >= 1` has no stationary distribution.")
    )
  invisible(AC)
}

.PopulateObsAC <- function(AC, nSim) {
  if (!length(AC))
    return(array(0, dim = nSim, dimnames = list(Sim = seq_len(nSim))))
  
  if (!is.null(dimnames(AC)))
    return(.CheckObsAC(AC))
  
  if (length(AC) == 2 && nSim != 2) {
    if (AC[1] <= -1 || AC[2] >= 1)
      cli::cli_abort(
        c("x" = "`AC` bounds must be within (-1, 1), exclusive.",
          "i" = "Supplied bounds: [{AC[1]}, {AC[2]}].")
      )
    vals <- .SimRunif(nSim, AC[1], AC[2])
    return(array(vals, dim = nSim, dimnames = list(Sim = seq_len(nSim))))
  }
  
  if (length(AC) > nSim)
    AC <- AC[seq_len(nSim)]
  
  if (length(AC) == 1)
    AC <- rep(AC, nSim)
  
  .CheckObsAC(AC)
  array(AC, dim = nSim, dimnames = list(Sim = seq_len(nSim)))
}

# AR(1) on the zero-mean log errors, so E[Error] = 1 and the log-scale SD is kept for any AC
.ApplyObsAC <- function(Error, AC, CV) {
  if (!length(Error) || all(AC == 0))
    return(Error)
  
  nSim <- nrow(Error)
  .CheckObsAC(AC)
  Mean <- exp(mconv(1, rep_len(as.numeric(CV), nSim)))
  ApplyAC(Error / Mean, AC = .SimValues(AC, nSim), LastError = rep(NA_real_, nSim)) * Mean
}


# Year labels already on the calendar of `Years`, e.g. ReduceDims() output
.HasPopulatedYears <- function(x, Years) {
  yr <- dimnames(x)$Year
  !is.null(yr) && all(as.numeric(yr) %in% Years) && min(as.numeric(yr)) == min(Years)
}

# consecutive series from Years[1] covering a full year, so every season has a value to repeat
.RepeatsSeasons <- function(x, Years) {
  yr <- as.numeric(dimnames(x)$Year)
  n  <- length(yr)
  n < length(Years) && all(yr == Years[seq_len(n)]) &&
    n >= sum(floor(Years) == floor(Years[1]))
}

# step-fill inverts ReduceDims(); `pattern` repeats the seasonal cycle of raw user input instead
.FillYearsByName <- function(x, nSim, Years, pattern = FALSE) {
  pattern <- pattern && .RepeatsSeasons(x, Years)
  x <- ExtendSims(x, nSim) |> ExtendYears(Years, maintain_seasonal_pattern = pattern)
  abind::asub(x, list(as.character(Years)), match("Year", names(dimnames(x))), drop = FALSE)
}

.PopulateObsScalar <- function(x, nSim, Years, label = "value", pattern = FALSE) {
  nYear <- length(Years)
  
  if (!is.null(dimnames(x))) {
    dd <- dim(x)
    
    if (length(dd) == 2) {
      if (identical(names(dimnames(x)), c("Sim", "Year")) && .HasPopulatedYears(x, Years))
        return(.FillYearsByName(x, nSim, Years, pattern))
      dimnames(x) <- list(Sim = seq_len(dd[1]), Year = Years[seq_len(dd[2])])
      return(x)
    }
    cli::cli_abort(
      c("x" = "`{label}` array has incompatible dimensions.",
        "i" = "Expected `[{nSim} x {nYear}]`; got `[{nrow(x)} x {ncol(x)}]`.")
    )
  }
  
  if (length(x) == 1) {
    vals <- x
    return(array(
      rep(vals, nYear),
      dim      = c(nSim, nYear),
      dimnames = list(Sim = seq_len(nSim), Year = Years)
    ))
  }
  
  if (length(x) == 2) {
    vals <- .SimRunif(nSim, x[1], x[2])
    return(array(
      rep(vals, nYear),
      dim      = c(nSim, nYear),
      dimnames = list(Sim = seq_len(nSim), Year = Years)
    ))
  }
  
  expanded <- .StructurePars(list(x), nSim)[[1]] |>
    ExtendSims(nSim) |>
    ExtendYears(Years)
  
  dimnames(expanded) <- list(Sim = seq_len(nSim), Year = Years)
  expanded
}

 
.PopulateObsShift <- function(Shift, nSim, Years, Bins, BinName = "Bin", pattern = FALSE) {
  nYear <- length(Years)
  nBin  <- length(Bins)

  # `BinName` is dynamic, so name the third element after building the list
  DimNames <- function() {
    dn <- list(Sim = seq_len(nSim), Year = Years, Bins)
    names(dn)[3] <- BinName
    dn
  }

  if (is.array(Shift) && length(dim(Shift)) == 3) {
    dn <- names(dimnames(Shift))
    if (all(c("Sim", "Year") %in% dn) && .HasPopulatedYears(Shift, Years)) {
      ind   <- match(c("Sim", "Year"), dn)
      Shift <- .Aperm(Shift, c(ind, setdiff(1:3, ind)))
      if (dim(Shift)[3] == nBin) {
        Shift <- .FillYearsByName(Shift, nSim, Years, pattern)
        return(array(Shift[seq_len(nSim), , , drop = FALSE],
                     dim = c(nSim, nYear, nBin), dimnames = DimNames()))
      }
    }
    if (dim(Shift)[1] >= nSim &&
        dim(Shift)[2] == nYear &&
        dim(Shift)[3] == nBin) {
      Shift <- Shift[seq_len(nSim), , , drop = FALSE]
      dimnames(Shift) <- DimNames()
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
      dimnames = DimNames()
    ))
  }

  if (is.vector(Shift) && length(Shift) == nBin) {
    return(array(
      rep(Shift, each = nSim * nYear),
      dim      = c(nSim, nYear, nBin),
      dimnames = DimNames()
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
      dimnames = DimNames()
    )
  } else {
    dimnames(expanded) <- DimNames()
  }
  
  expanded
}
