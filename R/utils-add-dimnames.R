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
AddAtAgeDimnames <- function(object, Ages, Years, name = class(object)) {
  
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
AddAtLengthDimnames <- function(object, Years, name = class(object)) {
  
  if (!isS4(object) || !.hasSlot(object, "MeanAtLength"))
    cli::cli_abort(c(
      "x" = "`object` must be an S4 object with a `MeanAtLength` slot.",
      "i" = "Got an object of class {.cls {class(object)}}."
    ))
  
  if (length(object@MeanAtLength) == 0)
    return(object)
  
  if (is.null(object@MeanAtLength)) return(object)
  
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
  
  if (!is.numeric(object@MeanAtLength))
    cli::cli_abort(c(
      "x" = "`{name}@MeanAtLength` must be a numeric array.",
      "i" = "Got an object of class {.cls {class(object@MeanAtLength)}}."
    ))
  
  Classes <- object@Classes
  dd <- dim(object@MeanAtLength)
  
  if (is.null(dd) || !length(dd) %in% 2:4)
    cli::cli_abort(c(
      "x" = "`{name}@MeanAtLength` must be a 2D, 3D, or 4D array.",
      "i" = "`dim({name}@MeanAtLength)` = {.val {dd}}"
    ))
  
  if (dd[2] != length(Classes))
    cli::cli_abort(c(
      "x" = "The second dimension of `{name}@MeanAtLength` must match `length({name}@Classes)`.",
      "i" = "`dim({name}@MeanAtLength)[2]` = {.val {dd[2]}}",
      "i" = "`length({name}@Classes)` = {.val {length(Classes)}}"
    ))
  
  if (length(dd) >= 3 && dd[3] > length(Years))
    cli::cli_abort(c(
      "x" = "`{name}@MeanAtLength` has more year dimensions than `Years`.",
      "i" = "`dim({name}@MeanAtLength)[3]` = {.val {dd[3]}}",
      "i" = "`length(Years)` = {.val {length(Years)}}"
    ))
  
  existing_year_names <- dimnames(object@MeanAtLength)$Year
  
  object@MeanAtLength <- switch(as.character(length(dd)),
                                `2` = {
                                  out <- object@MeanAtLength
                                  dimnames(out) <- list(Sim = seq_len(dd[1]), Class = Classes)
                                  out
                                },
                                `3` = {
                                  out <- object@MeanAtLength
                                  dimnames(out) <- list(
                                    Sim   = seq_len(dd[1]),
                                    Class = Classes,
                                    Year  = if (!is.null(existing_year_names)) existing_year_names else Years[seq_len(dd[3])]
                                  )
                                  out
                                },
                                `4` = {
                                  out <- object@MeanAtLength
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
AddAtWeightDimnames <- function(object, Years, name = class(object)) {
  
  if (!isS4(object) || !.hasSlot(object, "MeanAtWeight"))
    cli::cli_abort(c(
      "x" = "`object` must be an S4 object with a `MeanAtWeight` slot.",
      "i" = "Got an object of class {.cls {class(object)}}."
    ))
  
  if (length(object@MeanAtWeight) == 0)
    return(object)
  
  if (is.null(object@MeanAtWeight)) return(object)
  
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
  
  if (!is.numeric(object@MeanAtWeight))
    cli::cli_abort(c(
      "x" = "`{name}@MeanAtWeight` must be a numeric array.",
      "i" = "Got an object of class {.cls {class(object@MeanAtWeight)}}."
    ))
  
  Classes <- object@Classes
  dd <- dim(object@MeanAtWeight)
  
  if (is.null(dd) || !length(dd) %in% 2:4)
    cli::cli_abort(c(
      "x" = "`{name}@MeanAtWeight` must be a 2D, 3D, or 4D array.",
      "i" = "`dim({name}@MeanAtWeight)` = {.val {dd}}"
    ))
  
  if (dd[2] != length(Classes))
    cli::cli_abort(c(
      "x" = "The second dimension of `{name}@MeanAtWeight` must match `length({name}@Classes)`.",
      "i" = "`dim({name}@MeanAtWeight)[2]` = {.val {dd[2]}}",
      "i" = "`length({name}@Classes)` = {.val {length(Classes)}}"
    ))
  
  if (length(dd) >= 3 && dd[3] > length(Years))
    cli::cli_abort(c(
      "x" = "`{name}@MeanAtWeight` has more year dimensions than `Years`.",
      "i" = "`dim({name}@MeanAtWeight)[3]` = {.val {dd[3]}}",
      "i" = "`length(Years)` = {.val {length(Years)}}"
    ))
  
  existing_year_names <- dimnames(object@MeanAtWeight)$Year
  
  object@MeanAtWeight <- switch(as.character(length(dd)),
                                `2` = {
                                  out <- object@MeanAtWeight
                                  dimnames(out) <- list(Sim = seq_len(dd[1]), Class = Classes)
                                  out
                                },
                                `3` = {
                                  out <- object@MeanAtWeight
                                  dimnames(out) <- list(
                                    Sim   = seq_len(dd[1]),
                                    Class = Classes,
                                    Year  = if (!is.null(existing_year_names)) existing_year_names else Years[seq_len(dd[3])]
                                  )
                                  out
                                },
                                `4` = {
                                  out <- object@MeanAtWeight
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