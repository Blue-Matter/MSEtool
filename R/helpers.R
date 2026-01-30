#' General Helper Functions
#' 
#' @name helpers
#' @export
nSim <- function(x) {
  if (isS4(x)) {
    slots <- slotNames(x)
    if ('nSim' %in% slots)
      return(x@nSim)
    return(x@OM@nSim)
  }

  if (is.list(x)) {
    return(purrr::map(x, nSim) |> unlist())
  }

  dnames <- dimnames(x)
  if (!is.null(dnames))
    return(length(dnames[['Sim']]))
}


#' @rdname helpers
#' @export
nArea <- function(x, st=1) {
  if (inherits(x, 'hist')) {
   x <- x@OM 
  }
    
  if (inherits(x, 'om')) {
    stock <- x@Stock
    if (is.list(stock)) {
      stock <- stock[[st]]
    }
    dd <- dim(stock@Spatial@UnfishedDist)
    d1 <- length(stock@Spatial@UnfishedDist)
  } else {
    stock <- x
    dd <- dim(stock@Spatial@UnfishedDist)
    d1 <- length(stock@Spatial@UnfishedDist)
  }

  if (length(dd)<1) {
    if (length(d1)>0)
      return(d1)
    return(1)
  }

  nms <- names(dimnames(stock@Spatial@UnfishedDist))
  dd[which(nms=='Area')]

}

#' @rdname helpers
#' @export
nAge <- function(x, st=NULL) {
  if (inherits(x, 'hist')) {
    x <- x@OM 
  }
  
  if (inherits(x, 'mse')) {
    x <- x@OM 
  }
  
  if (inherits(x, 'om')) {
    stock <- x@Stock
    if (is.list(stock)) {
      if (!is.null(st)) {
        return(
          length(stock[[st]]@Ages@Classes)
               )
      } else {
        return(lapply(stock, nAge))
      }
      
    }
  }
  if (inherits(x, 'stock')) {
    return(length(x@Ages@Classes))
  }
  
}

#' @rdname helpers
#' @export
nStock <- function(object) {
  
  CheckClass(object, c('om', 'hist', 'mse'), 'object')
  
  if (inherits(object,'om')) {
    return(length(object@Stock))
  }
  
  if (inherits(object,'hist')) {
    return(length(object@OM@Stock))
  }
  
  if (inherits(object,'mse')) {
    return(length(object@OM@Stock))
  }
  
}

#' @rdname helpers
#' @export
nFleet <- function(object) {
  CheckClass(object, c('om', 'hist', 'mse'), 'object')
  
  if (inherits(object,'om')) {
    fleet <- object@Fleet
    if (is.null(fleet))
      return(0)
    if (inherits(fleet, 'fleet'))
      return(1)
    if (is.list(fleet[[1]]))
      return(length(fleet[[1]]))
    if(isS4(fleet[[1]])) {
      dd <- dim(object@Fleet[[1]]@Selectivity@MeanAtAge)
      return(dd[3])
    }
  }
  
  return(
    dim(object@LandingsAtAge[[1]])[[4]]
  )
  
}

# Much quicker than apply
sumOverDim <- function(x, dimName) {
  
  if (!is.array(x)) 
    cli::cli_abort("`x` must be an array", .internal = TRUE)
  
  dn <- dimnames(x)
  dims <- dim(x)
  nd <- length(dims)
  
  # Determine which dimension to sum over
  sumDim <- if (!is.null(dn) && dimName %in% names(dn)) {
    match(dimName, names(dn))
  } else {
    nd  # last dimension if not named
  }
  
  if (dims[sumDim] < 2) {
    return(DropDimension(x, dimName))
    
  }
  
  # Move sumDim to last dimension
  perm <- c(setdiff(seq_len(nd), sumDim), sumDim)
  x_perm <- aperm(x, perm)
  
  new_dims <- dims[perm]
  x_mat <- matrix(x_perm, ncol = new_dims[length(new_dims)])
  
  summed <- rowSums(x_mat)
  
  # Rebuild array without the summed dimension
  out_dims <- new_dims[-length(new_dims)]
  if (length(out_dims) == 0) out_dims <- 1
  out <- array(summed, dim = out_dims)
  
  # Restore dimnames
  if (!is.null(dn)) {
    dn_new <- dn[-sumDim]
    if (length(dn_new) > 0) dimnames(out) <- dn_new
  }
  
  out
}


#' Sum over named array dimensions
#'
#' Convenience wrappers for summing an array over a specific named dimension.
#' These functions provide fast, explicit alternatives to `apply()` when
#' working with multi-dimensional arrays that use named dimensions.
#'
#'
#' If the requested dimension name is present in `names(dimnames(x))`, that
#' dimension is summed out. If the dimension is not named (or `x` has no
#' dimension names), the **last dimension** of the array is summed by default.
#' 
#' ## Exported functions
#' * `SumOverAge()` – sum over the `"Age"` dimension
#' * `SumOverArea()` – sum over the `"Area"` dimension
#' * `SumOverFleet()` – sum over the `"Fleet"` dimension
#' * `SumOverStock()` – sum over the `"Stock"` dimension
#' * `SumOverYear()` – sum over the `"Year"` dimension
#'
#' @param x An array. If the requested dimension is named, it will be summed
#'   out; otherwise the last dimension is used as a fallback.
#'
#' @return
#' An array with the specified dimension removed. Dimension names are preserved
#' where possible.
#'
#'
#' @rdname SumOverDim
#' @export
SumOverAge <- function(x) {
  sumOverDim(x, dimName = "Age")
}

#' @rdname SumOverDim
#' @export
SumOverArea <- function(x) {
  sumOverDim(x, dimName = "Area")
}

#' @rdname SumOverDim
#' @export
SumOverFleet <- function(x) {
  sumOverDim(x, dimName = "Fleet")
}

#' @rdname SumOverDim
#' @export
SumOverStock <- function(x) {
  sumOverDim(x, dimName = "Stock")
}

#' @rdname SumOverDim
#' @export
SumOverYear <- function(x) {
  sumOverDim(x, dimName = "Year")
}





# ----- Non-exported ----

DefaultAges <- function(Ages = NULL) {
  if (!is.null(Ages)) {
    return(Ages)
  }
  Ages(MaxAge = 10)
}

DefaultYears <- function(Years = NULL) {
  if (!is.null(Years)) {
    return(Years)
  }
  
  CurrentYear <- format(Sys.Date(), "%Y") |>
    as.numeric()
  
  seq(1950, CurrentYear + 5)
}



#' Copy Slots Between S4 Objects
#'
#' Internal utility to copy specified slots from one S4 object to another.
#' The input and output objects must share the named slots.
#'
#' @param S4in An S4 object providing source slot values.
#' @param S4out An S4 object receiving copied slot values.
#' @param slots A character vector of slot names to copy.
#'
#' @return The modified `S4out` object.
#'
#' @keywords internal
CopySlots <- function(S4in, S4out, slots, ignore='Misc') {
  
  if (!isS4(S4in)) {
    cli::cli_abort("`S4in` must be an S4 object.")
  }
  if (!isS4(S4out)) {
    cli::cli_abort("`S4out` must be an S4 object.")
  }
  if (!is.character(slots)) {
    cli::cli_abort("`slots` must be a character vector.")
  }
  if (length(slots) == 0L) {
    return(S4out)
  }
  
  in_slots  <- slotNames(S4in)
  out_slots <- slotNames(S4out)
  
  missing_in  <- setdiff(slots, in_slots)
  missing_out <- setdiff(slots, out_slots)
  
  if (length(missing_in)) {
    cli::cli_abort(
      c("x" = "Some slots are missing from `S4in`:",
        "*" = paste(missing_in, collapse = ", ")
      )
    )
  }
  
  if (length(missing_out)) {
    cli::cli_abort(
      c("x"= "Some slots are missing from `S4out`:",
        "*" = paste(missing_out, collapse = ", ")
      )
    )
  }
  
  for (sl in slots) {
    slot(S4out, sl) <- slot(S4in, sl)
  }
  
  S4out
}


#' Linear interpolation of y at specified x values
#'
#' Computes linear interpolation of `y` at levels `xlev` based on input 
#' vectors `x` and `y`. Optionally prepends a zero-zero intercept and/or 
#' restricts interpolation to ascending x values.
#'
#' @param x Numeric vector of x values.
#' @param y Numeric vector of y values, same length as `x`.
#' @param xlev Numeric value or vector at which to interpolate `y`.
#' @param ascending Logical; if `TRUE`, only considers x values up to the 
#'   maximum in `x` for interpolation.
#' @param zeroint Logical; if `TRUE`, prepends a zero-zero intercept to `x` 
#'   and `y`.
#'
#' @details
#' Uses `stats::approx()` with `rule = 2` (constant extrapolation beyond range) 
#' and `ties = "ordered"`. If `xlev` lies outside the interpolated range, a 
#' warning is issued.
#'
#' @return
#' Numeric vector of interpolated y values corresponding to `xlev`.
#'
#' @author T. Carruthers
#' @keywords internal
LinInterp <- function(x, y, xlev, ascending = FALSE, zeroint = FALSE) {
  
  if (!is.numeric(x) || !is.numeric(y)) {
    cli::cli_abort("`x` and `y` must be numeric vectors.")
  }
  
  if (length(x) != length(y)) {
    cli::cli_abort("`x` and `y` must have the same length.")
  }
  
  # Prepend zero-zero if requested
  if (zeroint) {
    x <- c(0, x)
    y <- c(0, y)
  }
  
  # Restrict to ascending sequence if requested
  if (ascending) {
    idx <- 1:which.max(x)
    x_out <- x[idx]
    y_out <- y[idx]
  } else {
    x_out <- x
    y_out <- y
  }
  
  # Warn if xlev is outside the range
  if (any(xlev < min(x_out))) {
    cli::cli_warn(c("Some `xlev` values are less than min(x).",
                    '`xlev: {.val {xlev}}',
                    '`min(x): {.val {min(x)}}')
                  )
  }
  if (any(xlev > max(x_out))) {
    cli::cli_warn(c("Some `xlev` values are greater than max(x).",
                    '`xlev: {.val {xlev}}',
                    '`max(x): {.val {max(x)}}')
    )
  }
  
  stats::approx(x = x_out, y = y_out, xout = xlev, rule = 2, 
                ties = "ordered")$y
}


#' Print S4 Slot Sizes
#'
#' Iteratively inspects all slots of an S4 object and prints the
#' memory size of each slot in MB or GB. Recursively checks nested S4 objects.
#'
#' @param object An S4 object to inspect.
#' @param unit Character; "MB" or "GB", default is "MB".
#' @param indent Character; used internally for recursive indentation.
#' @param recursive Logical; if TRUE, recursively prints nested S4 slots.
#'
#' @return Invisibly returns a named list of slot sizes.
#'
#' @keywords internal
S4_SlotSizes <- function(object, unit = "MB", indent = "", recursive = TRUE) {
  if (!isS4(object)) {
    cli::cli_abort("`object` must be an S4 object")
  }
  
  slots <- slotNames(object)
  slot_sizes <- list()
  
  for (s in slots) {
    val <- slot(object, s)
    size_bytes <- as.numeric(object.size(val))
    size_val <- switch(
      toupper(unit),
      "GB" = size_bytes / 1024^3,
      "MB" = size_bytes / 1024^2,
      cli::cli_abort("`unit` must be 'MB' or 'GB'")
    )
    
    slot_sizes[[s]] <- size_val
    cat(indent, sprintf("%s (class: %s): %.3f %s\n", s, class(val)[1], size_val, toupper(unit)))
    
    # Recursive call for nested S4 objects
    if (recursive && isS4(val)) {
      slot_sizes[[s]] <- S4_SlotSizes(val, unit = unit, indent = paste0(indent, "  "), recursive = TRUE)
    }
  }
  
  invisible(slot_sizes)
}


#' Copy first element along `Sim` dimension to all other elements
#'
#' @param x An n-dimensional array with the first dimension named `Sim`.
#'  Or a list of such arrays 
#'
#' @return An array of the same dimensions as `x` with the first element
#'   in the first dimension repeated along that dimension.
#' @keywords internal
CopyFirstSim <- function(x) {
  
  if (is.list(x)) {
    for (i in seq_along(x)) {
      x[[i]] <- Recall(x[[i]])
    }
    return(x)  
  }
  
  if (!is.array(x)) cli::cli_abort("x must be an array", .internal=TRUE)
  
  dims <- dim(x)
  dn <- dimnames(x)
  
  if (dims[1] < 2) return(x)
  
  if (!"Sim" %in% names(dn)) cli::cli_abort("x must have named `Sim` dimension", .internal=TRUE)
  
  first_slice <- abind::asub(x, idx = 1, dims = 1, drop = FALSE)
  new_dims <- dims
  new_dims[1] <- dims[1]
  x <- array(rep(first_slice, each = dims[1]), dim = dims)
  
  dimnames(x) <- dn
  x
}





#' Get last non-NA residual per simulation
#'
#' @param LogResiduals Numeric array, dimensions nSim x nYear
#' @return Numeric vector of length nSim, each element is the most recent non-NA residual
#' @keywords internal
LastResidual <- function(LogResiduals) {
  apply(LogResiduals, 1, function(x) {
    if (all(is.na(x))) return(NA_real_)
    x[max(which(!is.na(x)))]
  })
}