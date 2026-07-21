#' Add Auto-Correlation to a Vector of Values
#'
#' Applies an AR(1) auto-correlation structure to a vector of independent
#' values, conditioning on a provided last observed value. The transformation
#' scales each value to preserve the marginal variance under the AR(1) process.
#'
#' @param values `numeric` vector. Independent values to be transformed with
#'   auto-correlation (e.g., log recruitment deviations).
#' @param ac `numeric` scalar. Auto-correlation coefficient, typically in
#'   \[-1, 1\].
#' @param last_value `numeric` scalar. The last observed value from the
#'   preceding time period, used to condition the first element of `values`.
#
#'
#' The AR(1) recursion applied is:
#'
#' \deqn{x_t = \rho \cdot x_{t-1} + \epsilon_t \cdot \sqrt{1 - \rho^2}}
#'
#' where \eqn{\rho} is `ac` and \eqn{\epsilon_t} are the input `values`. The
#' \eqn{\sqrt{1 - \rho^2}} scaling ensures the marginal variance of \eqn{x_t}
#' equals the variance of \eqn{\epsilon_t}.
#'
#' @return A `numeric` vector of the same length as `values` with AR(1)
#'   auto-correlation applied.
#'   
#' @seealso [GenMultiStockRecDevs()]
.AddAutoCorrelation <- function(values, ac, last_value) {
  n_fill <- length(values)
  values[1] <- ac * last_value + values[1] * sqrt(1 - ac^2)
  for (i in seq_len(n_fill)[-1]) {
    values[i] <- ac * values[i - 1] + values[i] * sqrt(1 - ac^2)
  }
  values
}


#' Add dimension names to a `MeanAtAge` array
#'
#' Validates and assigns `Sim`, `Age`, `Year`, and optionally `Area` dimnames
#' to the `MeanAtAge` slot of an object. If `MeanAtAge` is a plain numeric
#' vector, it is first expanded to a 3D array. `Year` dimnames are preserved
#' if already present.
#'
#' @param object An S4 object with a `MeanAtAge` slot.
#' @param Ages An S4 object with a `Classes` slot giving the age classes.
#' @param Years A vector of year values.
#' @param name A string naming the object, used in error messages.
#'
#' @return `object` with named dimensions on `MeanAtAge`.
#' @keywords internal
.AddAtAgeDimnames <- function(object, Ages, Years, name = class(object)) {
  
  if (!isS4(object) || !.hasSlot(object, "MeanAtAge"))
    cli::cli_abort(c(
      "x" = "`object` must be an S4 object with a `MeanAtAge` slot.",
      "i" = "Got an object of class {.cls {class(object)}}."
    ))
  
  if (length(object@MeanAtAge) == 0)
    return(object)
  
  if (missing(Ages) || !isS4(Ages) || !.hasSlot(Ages, "Classes"))
    cli::cli_abort(c(
      "x" = "`Ages` must be an S4 object with a `Classes` slot.",
      "i" = "Provide a valid `Ages` object."
    ))
  
  if (length(Ages@Classes) == 0)
    cli::cli_abort(c(
      "x" = "`Ages@Classes` is empty.",
      "i" = "Populate `Ages@Classes` before calling this function."
    ))
  
  if (missing(Years) || length(Years) == 0)
    cli::cli_abort(c(
      "x" = "`Years` is missing or empty.",
      "i" = "Provide a non-empty vector of year values."
    ))
  
  AgeClasses <- Ages@Classes
  nAge <- length(AgeClasses)
  dd <- dim(object@MeanAtAge)
  
  if (is.null(dd)) {
    if (length(object@MeanAtAge) != nAge)
      cli::cli_abort(c(
        "x" = "If `{name}@MeanAtAge` is a numeric vector it must be length `nAge`.",
        "i" = "`length({name}@MeanAtAge)` = {.val {length(object@MeanAtAge)}}",
        "i" = "`nAge` = {.val {nAge}}"
      ))
    object@MeanAtAge <- array(object@MeanAtAge, dim = c(1, nAge, 1),
                              dimnames = list(Sim = 1, Age = AgeClasses, Year = min(Years)))
    return(object)
  }
  
  if (dd[2] != nAge)
    cli::cli_abort(c(
      "x" = "The second dimension of `{name}@MeanAtAge` must be length `nAge`.",
      "i" = "`dim({name}@MeanAtAge)` = {.val {dd}}",
      "i" = "`nAge` = {.val {nAge}}"
    ))
  
  if (length(dd) >= 3 && dd[3] > length(Years))
    cli::cli_abort(c(
      "x" = "`{name}@MeanAtAge` has more year dimensions than `Years`.",
      "i" = "`dim({name}@MeanAtAge)[3]` = {.val {dd[3]}}",
      "i" = "`length(Years)` = {.val {length(Years)}}"
    ))
  
  existing_year_names <- dimnames(object@MeanAtAge)$Year
  
  object@MeanAtAge <- switch(as.character(length(dd)),
                             `2` = {
                               out <- object@MeanAtAge
                               dimnames(out) <- list(Sim = seq_len(dd[1]), Age = AgeClasses)
                               AddDimension(out, 'Year', val = min(Years))
                             },
                             `3` = {
                               out <- object@MeanAtAge
                               dimnames(out) <- list(
                                 Sim  = seq_len(dd[1]),
                                 Age  = AgeClasses,
                                 Year = if (!is.null(existing_year_names)) existing_year_names else Years[seq_len(dd[3])]
                               )
                               out
                             },
                             `4` = {
                               out <- object@MeanAtAge
                               dimnames(out) <- list(
                                 Sim  = seq_len(dd[1]),
                                 Age  = AgeClasses,
                                 Year = if (!is.null(existing_year_names)) existing_year_names else Years[seq_len(dd[3])],
                                 Area = seq_len(dd[4])
                               )
                               out
                             },
                             cli::cli_abort(c(
                               "x" = "`{name}@MeanAtAge` must be a vector or 2D, 3D, or 4D array.",
                               "i" = "`length(dim({name}@MeanAtAge))` = {.val {length(dd)}}"
                             ))
  )
  
  object
}


# Shared implementation behind .AddAtLengthDimnames/.AddAtWeightDimnames:
# validates and assigns `Sim`, `Class`, and optionally `Year`/`Area`
# dimnames to the given size-based slot (`MeanAtLength` or `MeanAtWeight`).
.AddAtSizeDimnames <- function(object, slotName, Years, name = class(object)) {

  if (!isS4(object) || !.hasSlot(object, slotName))
    cli::cli_abort(c(
      "x" = "`object` must be an S4 object with a `{slotName}` slot.",
      "i" = "Got an object of class {.cls {class(object)}}."
    ))

  Value <- slot(object, slotName)
  if (length(Value) == 0 || is.null(Value)) return(object)

  if (!.hasSlot(object, "Classes") || length(object@Classes) == 0)
    cli::cli_abort(c(
      "x" = "`{name}@Classes` is missing or empty.",
      "i" = "Populate `{name}@Classes` before calling this function."
    ))

  if (missing(Years) || length(Years) == 0)
    cli::cli_abort(c(
      "x" = "`Years` is missing or empty.",
      "i" = "Provide a non-empty vector of year values."
    ))

  if (!is.numeric(Value))
    cli::cli_abort(c(
      "x" = "`{name}@{slotName}` must be a numeric array.",
      "i" = "Got an object of class {.cls {class(Value)}}."
    ))

  Classes <- object@Classes
  dd <- dim(Value)

  if (is.null(dd) || !length(dd) %in% 2:4)
    cli::cli_abort(c(
      "x" = "`{name}@{slotName}` must be a 2D, 3D, or 4D array.",
      "i" = "`dim({name}@{slotName})` = {.val {dd}}"
    ))

  if (dd[2] != length(Classes))
    cli::cli_abort(c(
      "x" = "The second dimension of `{name}@{slotName}` must match `length({name}@Classes)`.",
      "i" = "`dim({name}@{slotName})[2]` = {.val {dd[2]}}",
      "i" = "`length({name}@Classes)` = {.val {length(Classes)}}"
    ))

  if (length(dd) >= 3 && dd[3] > length(Years))
    cli::cli_abort(c(
      "x" = "`{name}@{slotName}` has more year dimensions than `Years`.",
      "i" = "`dim({name}@{slotName})[3]` = {.val {dd[3]}}",
      "i" = "`length(Years)` = {.val {length(Years)}}"
    ))

  existing_year_names <- dimnames(Value)$Year

  slot(object, slotName) <- switch(as.character(length(dd)),
    `2` = {
      out <- Value
      dimnames(out) <- list(Sim = seq_len(dd[1]), Class = Classes)
      out
    },
    `3` = {
      out <- Value
      dimnames(out) <- list(
        Sim   = seq_len(dd[1]),
        Class = Classes,
        Year  = if (!is.null(existing_year_names)) existing_year_names else Years[seq_len(dd[3])]
      )
      out
    },
    `4` = {
      out <- Value
      dimnames(out) <- list(
        Sim   = seq_len(dd[1]),
        Class = Classes,
        Year  = if (!is.null(existing_year_names)) existing_year_names else Years[seq_len(dd[3])],
        Area  = seq_len(dd[4])
      )
      out
    }
  )

  object
}

#' Add dimension names to a `MeanAtLength` array
#'
#' Validates and assigns `Sim`, `Class`, and optionally `Year` and `Area`
#' dimnames to the `MeanAtLength` slot of an object. Returns `object`
#' unchanged if `MeanAtLength` is `NULL` or dimnames are already present.
#'
#' @param object An S4 object with `MeanAtLength` and `Classes` slots.
#' @param Years A vector of year values.
#' @param name A string naming the object, used in error messages.
#'
#' @return `object` with named dimensions on `MeanAtLength`.
#' @keywords internal
.AddAtLengthDimnames <- function(object, Years, name = class(object)) {
  .AddAtSizeDimnames(object, "MeanAtLength", Years, name)
}

#' Add dimension names to a `MeanAtWeight` array
#'
#' Validates and assigns `Sim`, `Class`, and optionally `Year` and `Area`
#' dimnames to the `MeanAtWeight` slot of an object. Returns `object`
#' unchanged if `MeanAtWeight` is `NULL` or dimnames are already present.
#'
#' @param object An S4 object with `MeanAtWeight` and `Classes` slots.
#' @param Years A vector of year values.
#' @param name A string naming the object, used in error messages.
#'
#' @return `object` with named dimensions on `MeanAtWeight`.
#' @keywords internal
.AddAtWeightDimnames <- function(object, Years, name = class(object)) {
  .AddAtSizeDimnames(object, "MeanAtWeight", Years, name)
}


# Shared implementation behind .AddYearDimnames/.AddFleetDimnames: recurses
# into S4/list objects, and for any array with a `dimname` dimension whose
# values are NA/missing/empty, fills them from `values`.
.AddDimnames <- function(object, dimname, values) {

  if (isS4(object)) {
    slots <- slotNames(object)

    for (s in slots) {
      val <- slot(object, s)

      if (!is.null(val)) {
        slot(object, s) <- Recall(val, dimname, values)
      }
    }

    return(object)
  }

  if (is.list(object)) {
    out <- object
    for (i in seq_along(out)) {
      el <- object[[i]]
      if (!is.null(el)) {
        out[[i]] <- Recall(el, dimname, values)
      }
    }
    return(out)
  }

  if (is.array(object)) {
    dnames <- dimnames(object)
    if (!is.null(dnames) && dimname %in% names(dnames)) {
      existing <- dnames[[dimname]]
      if (is.null(existing) || any(is.na(existing)) || any(nchar(existing) < 1)) {
        dimnames(object)[[dimname]] <- values[seq_along(dimnames(object)[[dimname]])]
      }
    }
    return(object)
  }
  object
}

#' Recursively assign year dimnames to arrays within an object
#'
#' Traverses S4 objects, lists, and arrays recursively. For any array with a
#' `"Year"` dimension whose dimnames contain `NA`s, replaces those dimnames
#' with values from `Years`. Leaves all other objects unchanged.
#'
#' @param object An S4 object, list, array, or atomic vector.
#' @param Years Numeric vector of year values to assign to `"Year"` dimnames.
#' @return `object` with `"Year"` dimnames populated wherever they were `NA`.
#' @keywords internal
.AddYearDimnames <- function(object, Years) {
  .AddDimnames(object, "Year", Years)
}

#' Recursively assign fleet dimnames to arrays within an object
#'
#' Traverses S4 objects, lists, and arrays recursively. For any array with a
#' `"Fleet"` dimension whose dimnames contain `NA`s or are missing, replaces those dimnames
#' with values from `FleetNames`. Leaves all other objects unchanged.
#'
#' @param object An S4 object, list, array, or atomic vector.
#' @param FleetNames Character vector of names to assign to `"Fleet"` dimension.
#' @return `object` with `"Fleet"` dimnames populated wherever they were `NA`.
#' @keywords internal
.AddFleetDimnames <- function(object, FleetNames) {
  .AddDimnames(object, "Fleet", FleetNames)
}



.AddSimNumber <- function(Hist) {
  DataSimList <- Hist@Data
  DataSimList <- purrr::imap(DataSimList, \(DataStock, i)
              purrr::map(DataStock, \(Data) {
                Data@Misc$Sim  <- as.numeric(i)
                Data  
              })
  )
  Hist@Data <- DataSimList
  Hist
}
