
#' Validate the class of an object
#'
#' Checks whether a given object inherits from one or more specified classes.
#' Throws an informative error if the object does not match any of the allowed classes.
#'
#' @param object The object to check.
#' @param class Character vector of allowed class names. Default is `"om"`.
#' @param name Name of the object, used in error messages. Default is `"OM"`.
#' @param type Character string describing the type of check (e.g., `"Argument"`). Default is `"Argument"`.
#'
#' The function tests whether `object` inherits from any class listed in `class`.
#'
#' @return
#' Invisibly returns the input `object` if it passes the class check.
#'
#' @keywords internal
.CheckClass <- function(object, class='om', name='OM', type='Argument') {

  checkClass <- sapply(class, function(i) inherits(object, i))
  if (all(!checkClass)) {
    cli::cli_abort(c('{type} {.var {name}} must be class {.cls {class}}',
                     "x" = "You've supplied an object of class {.cls {class(object)}}"), call=NULL)
  }
  invisible(object)
}


.CheckPopulated <- function(value, component, label = NULL, hint = NULL) {
  if (!EmptyObject(value))
    return(invisible(value))

  if (is.null(hint))
    hint <- cli::format_inline("Check the {.var {component}} input parameters and re-run.")

  prefix <- if (is.null(label)) "" else paste0(label, ": ")
  cli::cli_abort(c(
    "x" = paste0(prefix, cli::format_inline("{.var {component}} is required but did not populate.")),
    "i" = hint
  ), call = NULL)
}


.RequireArray <- function(object, arraySlot, component, label = NULL,
                          optional = FALSE, hint = NULL) {
  if (optional && EmptyObject(object))
    return(invisible(object))

  if (is.null(hint)) {
    hint <- cli::format_inline(
      "Provide {.var Pars} for {.var {component}}, or set {.var {arraySlot}} directly"
    )
    if (optional)
      hint <- paste0(hint, cli::format_inline(", or leave {.var {component}} fully unset"))
    hint <- paste0(hint, ", then re-run.")
  }

  .CheckPopulated(slot(object, arraySlot), component, label, hint)
  invisible(object)
}


.SafePopulate <- function(fn, component, label = NULL) {
  tryCatch(fn(), error = function(e) {
    prefix <- if (is.null(label)) "" else paste0(label, ": ")

    if (inherits(e, "rlang_error")) {
      header <- unname(rlang::cnd_header(e))
      body   <- unname(rlang::cnd_body(e))
    } else {
      header <- conditionMessage(e)
      body   <- character(0)
    }
    msg <- c("x" = paste0(prefix, cli::format_inline("{.var {component}} failed to populate.")))
    if (length(header)) msg <- c(msg, setNames(header, rep("i", length(header))))
    if (length(body))   msg <- c(msg, setNames(body, rep("i", length(body))))
    cli::cli_abort(msg, call = NULL)
  })
}


.CheckDependency <- function(object, arraySlot, dependency, component) {
  .CheckPopulated(slot(object, arraySlot), dependency,
    hint = cli::format_inline(
      "{.var {component}} requires a populated {.var {dependency}} — provide {.var Pars} for {.var {dependency}}, or set its {.var {arraySlot}} directly, then re-run."
    ))
}


.CheckRequiredObject <- function(object, class, argName=NULL) {
  if (methods::is(object, class))
    return(invisible(NULL))
  
  if (is.null(argName))
    argName <- .FirstUp(class)
  
  obj <- paste0('MSEtool::', argName)
  cli::cli_abort(c(
    "{.arg {argName}} must be a {.help {obj}} object.",
    "i" = "Provide a {.cls {class}} object to the {.arg {argName}} argument."
  ))
}


.CheckCatchFrac <- function(OM) {
  StockNames <- StockNames(OM)
  if (length(OM@CatchFrac)<1) {
    OM@CatchFrac <- MakeNamedList(StockNames)
  }
  
  nStock <- nStock(OM)
  nFleet <- nFleet(OM)
  nSim <- nSim(OM)
  
  names(OM@CatchFrac) <- StockNames
  
  if (nFleet==1)
    return(OM)
  
  if (length(OM@CatchFrac)!= nStock)
    cli::cli_abort('`OM@CatchFrac` must be a list length 0 or length `nStock(OM)` ')
  
  for (st in 1:nStock) {
    CatchFracFleet <- OM@CatchFrac[[st]]

    if (is.null(CatchFracFleet))
      next

    dd <- dim(CatchFracFleet)
    if (dd[1] != nSim && dd[1] != 1)
      cli::cli_abort('`OM@CatchFrac` must be a list length `nStock(OM)` with a `nSim` by `nFleet` matrix  for each stock')

    if (dd[2]!=nFleet)
      cli::cli_abort('`OM@CatchFrac` must be a list length `nStock(OM)` with a `nSim` by `nFleet` matrix  for each stock')

    if (any(CatchFracFleet<0) || any(!is.finite(CatchFracFleet)))
      cli::cli_abort('Values in `OM@CatchFrac` must be positive')

    rsum <- rowSums(CatchFracFleet)
    tol  <- sqrt(.Machine$double.eps)
    if (any(abs(rsum - 1) > tol))
      cli::cli_abort(
        c('Values in `OM@CatchFrac` must sum to 1 across rows',
          'i' = 'Max deviation: {.val {max(abs(rsum - 1))}}')
      )

    dimnames(CatchFracFleet) <- list("Sim"=1:dd[1],
                                     "Fleet"=FleetNames(OM))

    OM@CatchFrac[[st]] <- CatchFracFleet
  }
  OM
}


#' Check and Standardise Selectivity-at-Age to a Maximum of 1
#'
#' Checks whether the maximum selectivity-at-age value equals 1 for each
#' simulation, year, and (optionally) area. Where the maximum is below 0.99
#' and non-zero, the slice is rescaled so its maximum equals 1. Returns
#' `MeanAtAge` unchanged if all slices already have a maximum >= 0.99.
#'
#' Selectivity-at-age not peaking at 1 means that apical fishing mortality
#' does not correspond with F-at-age, which can produce unexpected behaviour
#' in downstream calculations. If `alert = TRUE`, an assumption is recorded
#' identifying the affected simulations, years, and areas.
#'
#' @param object A `Selectivity` object with `MeanAtAge` slot a 
#' n umeric array of selectivity-at-age values. Dimensions
#'   `Sim × Age × Year` or `Sim × Age × Year × Area`.
#'
#' @return `MeanAtAge` with each affected slice rescaled so its maximum
#'   equals 1, with original `dimnames` preserved.
#' @keywords internal
.CheckSelectivityMaximum <- function(object) {
  MeanAtAge <- object@MeanAtAge
  dnames <- dimnames(MeanAtAge)
  byArea <- !is.null(dnames[['Area']])
  
  margin    <- if (byArea) c('Sim', 'Year', 'Area') else c('Sim', 'Year')
  MaxValues <- round(apply(MeanAtAge, margin, max), 3)
  
  ind <- MaxValues < 0.99 & MaxValues != 0
  if (!any(ind))
    return(object)
  
  
  object <- .CaptureLog(object,
             string =
               cli::format_inline("Selectivity-at-Age does not reach a maximum of 1. \\
                                  F-at-Age will not correspond with apical F."),
             name = '.CheckSelectivityMaximum',
             type = 'assumption')

  object <- .CaptureLog(object,
                       string =
                         cli::format_inline("Standardizing to a maximum of 1. Check the selectivity schedule in the OM."),
                       type = 'assumption'
  )
  
    trunc_vec <- list("vec-trunc"=5)
    sims <- which(apply(ind, 'Sim',  any)) |> cli::cli_vec(trunc_vec)
    yrs  <- which(apply(ind, 'Year', any)) |> cli::cli_vec(trunc_vec)
    
    if (byArea) {
      areas <- which(apply(ind, 'Area', any)) |> cli::cli_vec(trunc_vec)
      
      object <- .CaptureLog(object,
                           string =
                             cli::format_inline('Simulations: {.val {sims}}; Years: {.val {yrs}}; Areas: {.val {areas}}'),
                           type = 'assumption'
      )

    } else {
      object <- .CaptureLog(object,
                           string =
                             cli::format_inline('Simulations: {.val {sims}}; Years: {.val {yrs}}'),
                           type = 'assumption'
      )

    }
  
  
  # Normalise each Sim x Year (x Area) slice so max == 1.
  # apply() moves the margin dims to position 1, so .Aperm() restores
  # Age back to dimension 2.
  normalise_slice <- function(x) x / max(x, na.rm=TRUE)
  
  MeanAtAge <- if (byArea) {
    .Aperm(
      apply(MeanAtAge, c('Sim', 'Year', 'Area'), normalise_slice),
      c(2, 1, 3, 4)
    )
  } else {
    .Aperm(
      apply(MeanAtAge, c('Sim', 'Year'), normalise_slice),
      c(2, 1, 3)
    )
  }
  
  dimnames(MeanAtAge) <- dnames
  object@MeanAtAge <- MeanAtAge
  object
}


#' Check and simplify spatial structure of a named array
#'
#' If `array` lacks an `Area` dimension, has only one area, or is identical
#' across all areas, the `Area` dimension is dropped via [DropDimension()].
#' Aborts if the array varies across areas, as spatial structure is not
#' currently supported.
#'
#' @param array A named array, optionally with an `Area` dimension.
#' @param name `character()` or `NULL`. Name of the array used in the error
#'   message. Defaults to `NULL`.
#'
#' @return `array` with the `Area` dimension dropped, or aborts if the array
#'   varies across areas.
#'
#' @keywords internal
.CheckSpatial <- function(array, name = NULL) {
  dn <- dimnames(array)
  areas <- dn[["Area"]]
  
  if (is.null(areas))
    return(array)
  if (length(areas) == 1)
    return(DropDimension(array, "Area"))
  
  area_dim <- which(names(dn) == "Area")
  
  if (.IdenticalAcrossAreas(array, area_dim))
    return(DropDimension(array, "Area", warn=FALSE))
  
  cli::cli_abort(c(
    "i" = "{.val {name}} varies over spatial areas",
    "x" = "This function currently does not support spatial structure"
  ))
}

#' Check if an array is identical across a given dimension
#'
#' @param array A named array.
#' @param area_dim `integer(1)`. Index of the dimension to check across.
#'
#' @return `logical`. `TRUE` if all slices along `area_dim` are identical.
#'
#' @keywords internal
.IdenticalAcrossAreas <- function(array, area_dim) {
  perm <- c(area_dim, seq_along(dim(array))[-area_dim])
  mat <- matrix(.Aperm(array, perm), nrow = dim(array)[area_dim])
  all(apply(mat[-1, , drop = FALSE], 1, identical, mat[1, ]))
}
