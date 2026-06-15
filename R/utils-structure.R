#' Structure and Extend a `Pars` List 
#'
#' Each element is converted to a named `Sim × Year` (or `Sim × Year × Area`) array via
#' [StructurePars_()], a random walk is applied to any parameter paired with
#' an `*SD` entry (unless `nArea` is supplied), and all arrays are extended to
#' consistent dimensions via [ExtendPars()].
#'
#' @param Pars Named list of parameter values. Each element may be a scalar,
#'   a length-2 uniform bounds vector, an `nSim`-length vector, or an
#'   existing array. See [StructurePars_()] for full conversion rules.
#' @param nSim Integer. Number of simulations. Required unless all elements
#'   of `Pars` are already arrays.
#' @param Years Numeric vector of years used to label the `Year` dimension.
#' @param nArea Integer or `NULL`. Number of spatial areas. If non-`NULL`,
#'   arrays gain a third `Area` dimension and random walks are suppressed.
#'
#' @return A named list of arrays, each with dimensions `Sim × Year` or
#'   `Sim × Year × Area`, with consistent extents across all elements.
#' @keywords internal
StructurePars <- function(Pars, nSim=NULL, Years=NULL, nArea=NULL) {
  if (!length(Pars))
    return(Pars)
  
  Pars <- purrr::map(Pars, \(Par)
                     StructurePars_(Par, nSim, Years, nArea)
  )
  
  if (is.null(nArea))
    Pars <- ApplyRandomWalk(Pars, Years)
  
  ExtendPars(Pars)
}

#' Convert a Single Parameter Value to a Structured Array
#'
#' Converts one element of a `Pars` list to a named `Sim × Year` (or
#' `Sim × Year × Area`) array according to the following rules:
#'
#' - If already an array, dimension names are assigned via
#'   [NameParDimensions()] if missing.
#' - If length 2 (and `nSim != 2`), treated as uniform bounds and sampled
#'   via [stats::runif()]. When `nSim == 1` the midpoint is used.
#' - Otherwise, values are recycled or truncated to `nSim` and wrapped in a
#'   1-year array.
#'
#' @param Par A scalar, length-2 bounds vector, numeric vector of length
#'   `nSim`, or an existing array.
#' @param nSim Integer. Number of simulations.
#' @param Years Numeric vector of year labels for the `Year` dimension.
#' @param nArea Integer or `NULL`. If non-`NULL`, output gains an `Area`
#'   dimension.
#'
#' @return A named array with dimensions `Sim × Year` or
#'   `Sim × Year × Area`.
#' @keywords internal
StructurePars_ <- function(Par, nSim=NULL, Years=NULL, nArea=NULL) {
  
  if (inherits(Par, 'array'))
    return(NameParDimensions(Par, nSim, Years, nArea))
  
  # Length-2 vector: sample from uniform distribution
  if (length(Par) == 2 && nSim != 2) {
    Par <- sort(Par)
    if (is.null(nSim))
      cli::cli_abort(c(
        '`nSim` required to generate stochastic values.',
        'i' = 'Provide number of simulations to the `nSim` argument.'
      ))
    val <- if (nSim == 1) mean(Par) else stats::runif(nSim, Par[1], Par[2])
    if (is.null(nArea)) {
      Par <- array(val, dim=c(length(val), 1))
    } else {
      Par <- array(val, dim=c(length(val), 1, 1))  
    }
    
    return(NameParDimensions(Par, nSim, Years, nArea))
  }
  
  # Vector longer than nSim: truncate
  if (length(Par) > nSim)
    Par <- Par[seq_len(nSim)]
  
  Par <- if (is.null(nArea)) {
    array(Par, dim=c(length(Par), 1))
  } else {
    array(Par, dim=c(length(Par), 1, 1))
  }
  NameParDimensions(Par, nSim, Years, nArea)
}

#' Apply a Log-Normal Random Walk to Time-Varying Parameters
#'
#' Scans `Pars` for entries whose name ends in `"SD"` (case-insensitive).
#' For each such entry, applies a mean-preserving log-normal random walk to
#' the matching base parameter (identified by stripping the trailing `"SD"`),
#' then removes the SD entry from the list.
#'
#' @param Pars Named list of structured parameter arrays (output of
#'   [StructurePars_()]).
#'
#' @return `Pars` with random walks applied to matched parameters and all
#'   `*SD` entries removed.
#' @keywords internal
ApplyRandomWalk <- function(Pars, Years) {
  detect_sd <- which(substrRight(tolower(names(Pars)), 2) == 'sd')
  if (!length(detect_sd))
    return(Pars)
  
  for (i in detect_sd) {
   
    nm_sd   <- names(Pars)[i]
    nm_par  <- strsplit(nm_sd, split="(?<=.)(?=.{2}$)", perl=TRUE)[[1]][1]
    par_ind <- match(nm_par, names(Pars))
    
    dnames <- dimnames(Pars[[par_ind]])
    nSim   <- length(dnames[['Sim']])
    
    Pars[[par_ind]] <- RandomWalk(
      targ   = Pars[[par_ind]],
      targsd = Pars[[i]],
      nSim   = nSim,
      Years  = Years
    )
    Pars[[i]] <- NA
  }
  
  # Remove spent SD entries (now NA)
  all_na <- which(vapply(Pars, \(x) all(is.na(x)), logical(1)))
  Pars[all_na] <- NULL
  Pars
}


#' Extend All Parameter Arrays to Consistent Dimensions
#'
#' Determines the union of `Sim`, `Year`, and optionally `Area` dimension
#' values across all elements of `Pars`, then calls [Extend()] on each to
#' ensure consistent extents across the list.
#'
#' @param Pars Named list of structured parameter arrays.
#'
#' @return `Pars` with all arrays extended to consistent dimensions.
#' @keywords internal
ExtendPars <- function(Pars) {
  dnames <- unique_dimname_values(Pars)
  
  nSim  <- max(as.numeric(dnames[[1]]))
  Years <- as.numeric(dnames[[2]])
  Areas <- if (length(dnames) == 3) as.numeric(dnames[[3]]) else NULL
  
  purrr::map(Pars, \(par) Extend(par, 
                                 nSim = nSim, 
                                 AgeClasses = NULL, 
                                 Years = Years, 
                                 Areas = Areas))
}

#' Assign Dimension Names to an Unnamed Parameter Array
#'
#' Assigns `Sim`, `Year`, and optionally `Area` dimension names to `Par` if
#' it does not already have them. Aborts if the `Year` dimension length is
#' greater than 1 but not equal to `length(Years)`.
#'
#' @param Par An array with 2 or 3 dimensions and no existing `dimnames`.
#' @param nSim Integer. Total number of simulations, used to label the `Sim`
#'   dimension.
#' @param Years Numeric vector of year labels.
#' @param nArea Integer or `NULL`. If non-`NULL`, used to label the `Area`
#'   dimension.
#'
#' @return `Par` with `dimnames` assigned.
#' @keywords internal
NameParDimensions <- function(Par, nSim=NULL, Years=NULL, nArea=NULL) {
  if (!is.null(dimnames(Par)))
    return(Par)
  
  dd <- dim(Par)
  
  # if (dd[2] > 1 && dd[2] != length(Years))
  #   cli::cli_abort(
  #     '`Year` dimension length must be 1 or equal to `length(Years)`.'
  #   )
  
  dimnames(Par) <- if (length(dd) < 3) {
    list(Sim  = seq_len(nSim)[seq_len(dd[1])],
         Year = Years[seq_len(dd[2])])
  } else {
    list(Sim  = seq_len(nSim)[seq_len(dd[1])],
         Year = Years[seq_len(dd[2])],
         Area = seq_len(nArea)[seq_len(dd[3])])
  }
  Par
}


#' Generate a Mean-Preserving Log-Normal Random Walk
#'
#' Applies a log-normal random walk to `targ`, scaled so the mean across
#' years is preserved within each simulation. Random deviates are
#' parameterised with a log-space mean of `-0.5 * targsd^2` to achieve
#' approximate mean-preservation on the natural scale.
#'
#' @param targ Numeric. Target value(s) to perturb, recycled to
#'   `nSim × nTS`.
#' @param targsd Numeric. Log-scale standard deviation of the random walk.
#' @param nSim Integer. Number of simulations.
#' @param Years Numeric vector of years, used to label the output.
#'
#' @return A `nSim × nTS` numeric array with `Sim` and `Year` dimnames.
#' @keywords internal
RandomWalk <- function(targ, targsd, nSim, Years) {
  nTS    <- length(Years)
  targ   <- matrix(targ, nSim, nTS)
  mutemp <- -0.5 * targsd^2
  temp   <- array(
    exp(rnorm(nSim * nTS, mutemp, targsd)),
    dim      = c(nSim, nTS),
    dimnames = list(Sim=seq_len(nSim), Year=Years)
  )
  row_means <- if (nSim > 1) apply(temp, 1, mean) else mean(temp)
  targ * temp / row_means
}


#' Extract Unique Dimname Values Across a List of Arrays
#'
#' Returns a list of unique dimension label vectors, one per dimension,
#' computed across all arrays in `ParsList`. Assumes all arrays have the
#' same rank.
#'
#' @param ParsList A list of arrays all having the same number of dimensions.
#'
#' @return A list of length `ndim`, where each element is a character vector
#'   of unique labels for that dimension across all arrays in `ParsList`.
#' @keywords internal
unique_dimname_values <- function(ParsList) {
  nD <- length(dim(ParsList[[1]]))
  lapply(seq_len(nD), function(d) {
    unique(unlist(lapply(ParsList, function(x) {
      dn <- dimnames(x)
      if (is.null(dn)) character(0) else dn[[d]]
    }), use.names=FALSE))
  })
}


#' Extract the Rightmost `n` Characters of a String
#'
#' @param x Character vector.
#' @param n Integer. Number of characters to extract from the right.
#'
#' @return Character vector of the same length as `x`.
#' @keywords internal
substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

#' Reshape a Value into a Standard Simulation Array
#'
#' Coerces `value` into an array with dimensions specified by `out`, setting
#' the size of the `req` dimension to `length(value)` and all others to 1.
#' If `value` is already an array of the correct rank it is returned
#' unchanged. Aborts if `value` has more dimensions than `out`.
#'
#' @param value A scalar, vector, or array.
#' @param out Character vector of dimension names in order. Default
#'   `c('nSim', 'nage', 'nTS')`.
#' @param req Character scalar. The dimension name whose size should equal
#'   `length(value)` when `value` is not already an array. Default `'nage'`.
#'
#' @return An array with `length(out)` dimensions.
#' @keywords internal
Structure <- function(value, out=c('nSim', 'nage', 'nTS'), req='nage') {
  if (is.null(value))
    return(NULL)
  
  array_str <- data.frame(name=out, size=1)
  
  if (!is.array(value)) {
    array_str$size[match(req, array_str$name)] <- length(value)
    return(array(value, dim=array_str$size))
  }
  
  dim_value <- dim(value)
  
  if (length(dim_value) == length(out))
    return(value)
  
  if (length(dim_value) > length(out))
    cli::cli_abort('`length(dim(value)) > length(out)`')
  
  i <- seq_along(dim_value)
  array_str$size[i] <- dim_value[i]
  array(value, dim=array_str$size)
}

#' Structure a CV-at-Age Value into a Standard Array
#'
#' Convenience wrapper around [Structure()] for CV-at-age values. Handles
#' three input forms:
#'
#' - Scalar: passed directly to [Structure()].
#' - Length-2 vector: treated as uniform bounds, first converted via
#'   [StructurePars_()] before structuring.
#' - Array or longer vector: passed directly to [Structure()].
#'
#' @param CVatAge Scalar, length-2 bounds vector, or array of CV-at-age
#'   values.
#' @param nSim Integer. Number of simulations. Only used when `CVatAge` is a
#'   length-2 bounds vector.
#'
#' @return An array with dimensions `nSim × nage × nTS`.
#' @keywords internal
StructureCV <- function(CVatAge, nSim) {
  if (is.null(dim(CVatAge))) {
    if (length(CVatAge) == 1)
      return(Structure(CVatAge))
    if (length(CVatAge) == 2)
      return(Structure(StructurePars_(CVatAge, nSim)))
  }
  Structure(CVatAge)
}

