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

#' @rdname Dimensions
#' @export
IndexNames <- function(x, Type = c("CPUE", "Survey")) {
  Type <- match.arg(Type, several.ok = TRUE)

  if (inherits(x, "data"))
    x <- list(x)
  if (.IsDataList(x))
    return(purrr::map(x, \(d) purrr::map(Type, \(type) .DataFleetNames(methods::slot(d, type)))) |>
             unlist() |> unique() |> as.character())

  .CheckClass(x, c("om", "hist", "mse", "data"), "x")
  Simulated <- inherits(x, c("hist", "mse"))
  if (Simulated)
    x <- x@OM

  DataList <- x@Data
  if (inherits(DataList, "data"))
    DataList <- list(DataList)
  Names <- if (length(DataList)) IndexNames(DataList, Type) else character(0)

  if (Simulated) {
    ObsNames <- purrr::map(x@Obs, \(ObsList) {
      HasIndex <- purrr::map_lgl(ObsList, \(obs) any(purrr::map_lgl(Type, \(type) !is.null(methods::slot(obs, type)@Error))))
      names(ObsList)[HasIndex]
    })
    Names <- c(Names, unlist(ObsNames))
  }
  unique(as.character(Names))
}
