#' @describeIn show Print a [OM] object
setMethod("show", "om", function(object) {
  
  cli::cli_h2("An {.help MSEtool::OM} Object")
  
  .show_slot(object, 'Name')
  
  cli::cli_text("")
  
  .show_slot(object, 'Agency')
  .show_slot(object, 'Author')
  .show_slot(object, 'Email')
  .show_slot(object, 'Region')
  
  cli::cli_text("")
  
  .show_slot(object, 'nSim')
  .show_slot(object, 'nYear')
  .show_slot(object, 'pYear')
  .show_slot(object, 'CurrentYear')
  .show_slot(object, 'Seasons')

  cli::cli_text("")
  
  cli::cli_text("Stocks: {.val {StockNames(object)}}")
  cli::cli_text("Fleets: {.val {FleetNames(object)}}")
  
})