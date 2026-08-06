#' Check Obs Objects in an Operating Model
#'
#' Checks that all [Obs()] objects are present and non-empty for each
#' stock-fleet combination in the `OM` object. Issues warnings for any missing
#' or empty [Obs()] objects. No data will be generated for stock-fleet
#' combinations without a populated [Obs()] object.
#'
#' @param OM An operating model object containing an `@Obs` slot.
#'
#' @return Logical; `TRUE` if all [Obs()] objects are present and non-empty,
#'   `FALSE` if any are missing or empty.
#' @keywords internal
.CheckObs <- function(OM, Proj=FALSE) {

  .CheckClass(OM, c('om', 'hist'))
  
  if (inherits(OM,'hist')) {
    Obs <- OM@OM@Obs
  } else {
    Obs <- OM@Obs
  }
  if (is.null(Obs)) {
    OM <- .CaptureLog(OM,
               string =
                 cli::format_inline("No {.help MSEtool::Obs} object has been provided in the `OM` object.
       No Data will be generated."),
               name = '.GenerateHistoricalData')

    return(OM)
  }

  FleetNms <- FleetNames(OM)

  MissingSel <- purrr::map(names(Obs), function(st) {
    purrr::map(setdiff(names(Obs[[st]]), FleetNms), function(idx) {
      obs_idx <- Obs[[st]][[idx]]
      purrr::map_chr(c('CPUE', 'Survey'), function(type) {
        IndexObs <- slot(obs_idx, type)
        if (EmptyObject(IndexObs) || !is.null(IndexObs@Selectivity)) return(NA_character_)
        cli::format_inline("Stock: {.val {st}}, Index: {.val {idx}} ({type})")
      })
    }) |> unlist()
  }) |> unlist()
  MissingSel <- MissingSel[!is.na(MissingSel)]

  if (length(MissingSel))
    cli::cli_abort(c(
      "x" = "The following {cli::qty(length(MissingSel))} {.cls indicesobs} object{?s} {?is/are} configured (non-empty) but {.field Selectivity} is not set:",
      stats::setNames(MissingSel, rep('*', length(MissingSel))),
      "i" = "Set e.g. {.code OM@Obs[[Stock]][[Index]]@Survey@Selectivity <- 'Biomass'} (full selection) before running {.fn Simulate}/{.fn Project}."
    ))

  Empty <- purrr::map(Obs, \(stock)
                      purrr::map(stock, EmptyObject)
  )
  
  if (!any(unlist(Empty)))
    return(OM)
  
  OM <- .CaptureLog(OM,
                   string = cli::format_inline("No {.help MSEtool::Obs} object found for the following: "),
                   name = '.GenerateHistoricalData')
            
  for (i in seq_along(Empty)) {
    for (j in seq_along(Empty[[i]])) {
      if (Empty[[i]][[j]]) {
        OM <- .CaptureLog(OM,
                         string = cli::format_inline("Stock: {.val {names(Empty)[i]}} and Fleet: {.val {names(Empty[[i]])[j]}}.")
                         )
      }
    }
  }
  
  OM <- .CaptureLog(OM,
                   string = cli::format_inline("No Data will be generated for these stocks/fleets.")
  )
  
  OM
}
