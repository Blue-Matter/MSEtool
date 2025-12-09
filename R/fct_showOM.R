#' @describeIn show Print a [OM] object
setMethod("show", "om", function(object) {
  
  cli::cli_par()
  cli::cli_h2("An {.help MSEtool::OM} Object")
  
  cli::cli_h3('{.code Name}')
  cli::cli_text("{.val {object@Name}}")
  
  cli::cli_h3('{.code nSim}')
  cli::cli_text("{.val {object@nSim}}")
  
  cli::cli_h3('{.code CurrentYear}')
  cli::cli_text("{.val {object@CurrentYear}}")
  
  cli::cli_h3('{.code nYear}')
  cli::cli_text("{.val {object@nYear}}")
  
  cli::cli_h3('{.code pYear}')
  cli::cli_text("{.val {object@pYear}}")
  
  # cli::cli_h3('{.code TimeUnits}')
  # cli::cli_text("{.val {object@TimeUnits}}")
  
  
  cli::cli_h3('Number of Stocks')
  cli::cli_text("{.val {nStock(object)}}")
  
  cli::cli_h3('Number of Fleets')
  cli::cli_text("{.val {nFleet(object)}}")
  
  cli::cli_end()
  
  # print(Check(object))
})