# Clashes with Slick
# 
# #' Check an object for errors or issues
# #'
# #' Checks S4 objects to check for warnings and errors
# #'
# #' @param object An object of class: [Ages()], ...
# #' @param ... Additional arguments
# #' @return Prints messages to the console
# #' @export
# #' @examples
# #' ages <- Ages()
# #' Check(ages)
# #'
# setGeneric('Check', function(object, ...)
#   standardGeneric('Check')
# )
# 
# ## ---- Stock ----
# setMethod("Check", signature(object = "stock"), function(object, ...) {
#   # cli::cli_alert_info('`Check` not complete for Class {.val {class(object)}}')
#   ll <- CheckList(object)
#   if (ll@empty)
#     return(ll)
#   if (length(ll@errors)<1)
#     ll@complete <- TRUE
#   ll
#   
# })
# 
# # Stock Objects ----
# 
# ## ---- Ages ----
# #' @rdname Check
# #' @export
# setMethod("Check", signature(object = "ages"), function(object, ...) {
#   ll <- CheckList(object)
#   if (ll@empty)
#     return(ll)
# 
#   if (length(object@MaxAge)>1)
#     ll@errors <- append(ll@errors, list(MaxAge='`MaxAge` must be length 1'))
# 
#   if (max(object@MaxAge)<1)
#     ll@errors <- append(ll@errors, list(MaxAge='`MaxAge` must be greater than 0'))
# 
#   if (max(object@MaxAge)<5 & max(object@MaxAge)>0)
#     ll@warnings <- append(ll@warnings, list(MaxAge='Suggest switching to smaller time step for `MaxAge` <5'))
# 
#    if (!object@Units %in% ValidUnits(object))
#      ll@errors <- append(ll@errors,
#                          list(Units=paste0(object@Units, " is not valid for `Units`. See `ValidUnits('Ages')`"))
#                          )
# 
#   if (length(ll@errors)<1 & length(ll@warnings)<1)
#     ll@complete <- TRUE
#   ll
# 
# })
# 
# ## ---- Length ----
# #' @rdname Check
# #' @export
# setMethod("Check", signature(object = "length"), function(object, ...) {
#   ll <- CheckList(object)
#   if (ll@empty)
#     return(ll)
# 
#   # cli::cli_alert_info('`Check` not complete for Class {.val {class(object)}}')
# 
# 
#   if (length(ll@errors)<1)
#     ll@complete <- TRUE
#   ll
# 
# })
# 
# ## ---- Weight ----
# 
# #' @rdname Check
# #' @export
# setMethod("Check", signature(object = "weight"), function(object, ...) {
#   ll <- CheckList(object)
#   if (ll@empty)
#     return(ll)
# 
#   # cli::cli_alert_info('`Check` not complete for Class {.val {class(object)}}')
# 
# 
# 
#   if (length(ll@errors)<1)
#     ll@complete <- TRUE
#   ll
# 
# })
# 
# ## ---- NaturalMortality ----
# 
# #' @rdname Check
# #' @export
# setMethod("Check", signature(object = "naturalmortality"), function(object, ...) {
#   ll <- CheckList(object)
#   if (ll@empty)
#     return(ll)
# 
#   # cli::cli_alert_info('`Check` not complete for Class {.val {class(object)}}')
# 
# 
# 
#   if (length(ll@errors)<1)
#     ll@complete <- TRUE
#   ll
# 
# })
# 
# ## ---- Maturity ----
# 
# #' @rdname Check
# #' @export
# setMethod("Check", signature(object = "maturity"), function(object, ...) {
#   ll <- CheckList(object)
#   if (ll@empty)
#     return(ll)
# 
#   # cli::cli_alert_info('`Check` not complete for Class {.val {class(object)}}')
# 
# 
# 
#   if (length(ll@errors)<1)
#     ll@complete <- TRUE
#   ll
# 
# })
# 
# 
# ## ---- Fecundity ----
# 
# #' @rdname Check
# #' @export
# setMethod("Check", signature(object = "fecundity"), function(object, ...) {
#   ll <- CheckList(object)
#   if (ll@empty)
#     return(ll)
# 
#   # cli::cli_alert_info('`Check` not complete for Class {.val {class(object)}}')
# 
# 
# 
#   if (length(ll@errors)<1)
#     ll@complete <- TRUE
#   ll
# 
# })
# 
# ## ---- SRR ----
# 
# #' @rdname Check
# #' @export
# setMethod("Check", signature(object = "srr"), function(object, ...) {
#   ll <- CheckList(object)
#   if (ll@empty)
#     return(ll)
# 
#   # cli::cli_alert_info('`Check` not complete for Class {.val {class(object)}}')
# 
# 
# 
#   if (length(ll@errors)<1)
#     ll@complete <- TRUE
#   ll
# 
# })
# 
# ## ---- Spatial ----
# 
# #' @rdname Check
# #' @export
# setMethod("Check", signature(object = "spatial"), function(object, ...) {
#   ll <- CheckList(object)
#   if (ll@empty)
#     return(ll)
# 
#   if (length(ll@errors)<1)
#     ll@complete <- TRUE
#   ll
# 
# })
# 
# ## ---- Depletion ----
# 
# #' @rdname Check
# #' @export
# setMethod("Check", signature(object = "depletion"), function(object, ...) {
#   ll <- CheckList(object)
#   if (ll@empty)
#     return(ll)
# # 
#   # cli::cli_alert_info('`Check` not complete for Class {.val {class(object)}}')
# 
# 
# 
#   if (length(ll@errors)<1)
#     ll@complete <- TRUE
#   ll
# 
# })
# 
# # ---- Fleet Object ----
# 
# 
# setMethod("Check", signature(object = "fleet"), function(object, ...) {
#   # cli::cli_alert_info('`Check` not complete for Class {.val {class(object)}}')
#   ll <- CheckList(object)
#   if (ll@empty)
#     return(ll)
#   if (length(ll@errors)<1)
#     ll@complete <- TRUE
#   ll
# })
# 
# # Fleet Objects ----
# 
# ## ---- FishingMortality ----
# 
# #' @rdname Check
# #' @export
# setMethod("Check", signature(object = "fishingmortality"), function(object, ...) {
#   ll <- CheckList(object)
#   if (ll@empty)
#     return(ll)
# 
#   # cli::cli_alert_info('`Check` not complete for Class {.val {class(object)}}')
# 
# 
# 
#   if (length(ll@errors)<1)
#     ll@complete <- TRUE
#   ll
# 
# })
# 
# ## ---- DiscardMortality ----
# 
# #' @rdname Check
# #' @export
# setMethod("Check", signature(object = "discardmortality"), function(object, ...) {
#   ll <- CheckList(object)
#   if (ll@empty)
#     return(ll)
# 
#   # cli::cli_alert_info('`Check` not complete for Class {.val {class(object)}}')
# 
# 
# 
#   if (length(ll@errors)<1)
#     ll@complete <- TRUE
#   ll
# 
# })
# 
# 
# ## ---- Effort ----
# 
# #' @rdname Check
# #' @export
# setMethod("Check", signature(object = "effort"), function(object, ...) {
#   ll <- CheckList(object)
#   if (ll@empty)
#     return(ll)
# 
#   # cli::cli_alert_info('`Check` not complete for Class {.val {class(object)}}')
# 
# 
# 
#   if (length(ll@errors)<1)
#     ll@complete <- TRUE
#   ll
# 
# })
# 
# ## ---- Selectivity ----
# 
# #' @rdname Check
# #' @export
# setMethod("Check", signature(object = "selectivity"), function(object, ...) {
#   ll <- CheckList(object)
#   if (ll@empty)
#     return(ll)
# 
#   # cli::cli_alert_info('`Check` not complete for Class {.val {class(object)}}')
# 
# 
# 
#   if (length(ll@errors)<1)
#     ll@complete <- TRUE
#   ll
# 
# })
# 
# ## ---- Retention ----
# 
# #' @rdname Check
# #' @export
# setMethod("Check", signature(object = "retention"), function(object, ...) {
#   ll <- CheckList(object)
#   if (ll@empty)
#     return(ll)
# 
#   # cli::cli_alert_info('`Check` not complete for Class {.val {class(object)}}')
# 
# 
# 
#   if (length(ll@errors)<1)
#     ll@complete <- TRUE
#   ll
# 
# })
# 
# 
# ## ---- Distribution ----
# 
# #' @rdname Check
# #' @export
# setMethod("Check", signature(object = "distribution"), function(object, ...) {
#   ll <- CheckList(object)
#   if (ll@empty)
#     return(ll)
# 
#   # cli::cli_alert_info('`Check` not complete for Class {.val {class(object)}}')
# 
# 
# 
#   if (length(ll@errors)<1)
#     ll@complete <- TRUE
#   ll
# 
# })
# 
# 
# 
# # ---- OM Object ----
# setMethod("Check", signature(object = "om"), function(object, ...) {
#   
#   OM <- object
#   
#   ll <- CheckList(OM)
#   if (ll@empty)
#     return(ll)
#   
#   if (methods::is(OM@Stock, 'list'))
#     class(OM@Stock) <- 'StockList'
# 
#   if (methods::is(OM@Stock, 'stock')) {
#     nStocks <- 1
#   } else if (methods::is(OM@Stock, 'StockList')) {
#     nStocks <- length(OM@Stock)
#   } else {
#     cli::cli_abort('`Stock` must be a {.fun {"Stock"}} object or a list of {.fun {"Stock"}} objects')
#   }
# 
#   if (methods::is(OM@Fleet, 'list'))
#     class(OM@Fleet) <- 'StockFleetList'
# 
#   if (methods::is(OM@Fleet, 'fleet')) {
#     nFleets <- 1
#   } else if (methods::is(OM@Fleet, 'StockFleetList')) {
#     nFleets <- length(OM@Fleet[[1]])
#   } else {
#     cli::cli_abort('`Fleet` must be a {.fun {"Fleet"}} object or a nested list of {.fun {"Fleet"}} objects for each {.fun {"Stock"}} object')
#   }
#   
#   
#   
#   if (length(ll@errors)<1)
#     ll@complete <- TRUE
#   ll
# })
# 
# 
# 
# ## SexPars ----
# setMethod("Check", signature(object = "sexpars"), function(object, ...) {
#   # cli::cli_alert_info('`Check` not complete for Class {.val {class(object)}}')
#   ll <- CheckList(object)
#   if (ll@empty)
#     return(ll)
#   
#   if (length(ll@errors)<1)
#     ll@complete <- TRUE
#   ll
# })
# 
# 
# 
# 
# 
# CheckTimeUnitsStock <- function(stock) {
#   out <- list()
#   time_units <- NULL
#   if (!EmptyObject(stock@Ages)) {
#     time_units <- stock@Ages@Units
#   }
#   if (!EmptyObject(stock@NaturalMortality) & !is.null(stock@NaturalMortality@Units) & !is.null(time_units)) {
# 
#     if (stock@NaturalMortality@Units != time_units)
#       out[['Time Units']] <- "`Ages@Units` must match `NaturalMortality@Units`"
#   }
#   out
# }
# 
# 
# 
# CheckStock <- function(stock) {
#   stock <- Update(stock)
# 
#   ll <- CheckList()
#   ll@object <-  paste0(class(stock), ' Object: ', stock@Name)
#   ll@empty <- isNewObject(stock)
# 
#   # check for errors
# 
#   # check time units are all the same
#   ll@errors <- append(ll@errors, CheckTimeUnitsStock(stock))
# 
#   ll
# 
# }
# 
# 

CheckPars <- function(Pars) {
  if (length(Pars)<1)
    return(Pars)

  if (any(nchar(names(Pars))==0)) {
    cli::cli_abort('`Pars` must be a named list (or an empty list or NULL)')
  }
  Pars
}

CheckModel <- function(object) {
  fun <- paste0(firstup(class(object)), 'Models')
  nms <-  names(object@Pars)
  cli::cli_abort(c(
    'No model found for this object class {.val {class(object)}} with parameters named: {.val { nms }}.',
    'i'='See {.fun { fun}} or set `Pars` to NULL or `Model` to a R function with arguments corresponding with those in `Pars`.')
  )
}

