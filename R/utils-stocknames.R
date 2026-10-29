#' @param object An [om-class], [hist-class], [mse-class], [stock-class],
#'   [fleet-class], or [data-class] object, or a list of these objects.
#' @rdname Dimensions
#' @export
StockNames <- function(object) {
  if (inherits(object, c("stock", "data")))
    return(object@Name)

  if (!is.list(object) && !inherits(object, c("om", "hist", "mse")))
    return(NULL)

  stocks <- .StockList(object)
  nms <- names(stocks)
  if (!is.null(nms))
    return(nms)

  purrr::map_chr(stocks, Name)
}

#' @rdname Dimensions
#' @param IncSurvey Logical. Include names of survey (non-fishing) fleets? Default: FALSE
#' @export
FleetNames <- function(object, IncSurvey = FALSE) {

  if (inherits(object, "data"))
    object <- list(object)

  if (.IsDataList(object)) {
    slots <- c("Landings", "Discards", "Effort", "LandingsAtAge", "DiscardsAtAge",
               "LandingsAtSize", "DiscardsAtSize")
    if (IncSurvey)
      slots <- c(slots, "CPUE", "Survey")
    fleetnames <- purrr::map(object, \(d) purrr::map(slots, \(sl) .DataFleetNames(methods::slot(d, sl))))
    return(as.character(unique(unlist(fleetnames))))
  }

  if (!is.list(object))
    .CheckClass(object, c("om", "hist", "mse", "fleet", "data"), "object")

  fleets <- .FleetList(object)
  fleetnames <- names(fleets)
  if (is.null(fleetnames))
    fleetnames <- purrr::map_chr(fleets, \(fl) if (length(fl@Name)) fl@Name[1] else NA_character_)
  fleetnames <- as.character(fleetnames)

  if (inherits(object, c("hist", "mse")))
    object <- object@OM

  if (IncSurvey && inherits(object, "om") && !is.null(object@Data)) {
    DataList <- if (inherits(object@Data, "data")) list(object@Data) else object@Data
    surveynames <- lapply(DataList, \(d) c(d@Survey@Name, d@CPUE@Name)) |> unlist()
    fleetnames <- unique(c(fleetnames, surveynames))
  }
  fleetnames
}

.DataFleetNames <- function(DataSlot) {
  if (!is.null(DataSlot@Name))
    return(DataSlot@Name)
  Value <- DataSlot@Value
  if (is.null(Value))
    return(NULL)
  dimnames(Value)[["Fleet"]] %||% paste("Fleet", seq_len(NCOL(Value)))
}
