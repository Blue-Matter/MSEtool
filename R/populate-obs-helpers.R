
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
    rlnorm(nSim, mconv(1, CV), sdconv(1, CV)),
    dim      = nSim,
    dimnames = list(Sim = seq_len(nSim))
  )
}

 
.PopulateObsAC <- function(AC, nSim) {
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

 
.ApplyObsAC <- function(Error, AC) {
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

 
.PopulateObsScalar <- function(x, nSim, Years, label = "value") {
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
  
  if (length(x) == 1) {
    vals <- x
    return(array(
      rep(vals, nYear),
      dim      = c(nSim, nYear),
      dimnames = list(Sim = seq_len(nSim), Year = Years)
    ))
  }
  
  if (length(x) == 2) {
    vals <- runif(nSim, min = x[1], max = x[2])
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

 
.PopulateObsShift <- function(Shift, nSim, Years, Bins, BinName = "Bin") {
  nYear <- length(Years)
  nBin  <- length(Bins)

  # `BinName` is dynamic, so name the third element after building the list
  DimNames <- function() {
    dn <- list(Sim = seq_len(nSim), Year = Years, Bins)
    names(dn)[3] <- BinName
    dn
  }

  if (is.array(Shift) && length(dim(Shift)) == 3) {
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
