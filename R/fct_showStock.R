#' @rdname show
setMethod('show', 'stock', function(object) {
  
  cli::cli_par()
  cli::cli_h2("A {.help MSEtool::Stock} Object")
  
  
  
  cli::cli_h3('{.code Name}')
  cli::cli_text('{.val { object@Name}}')
  
  cli::cli_h3('{.code CommonName}')
  cli::cli_text('{.val { object@CommonName}}')
  
  cli::cli_h3('{.code Species}')
  cli::cli_text('{.val { object@Species}}')
  
  slots <- c('Ages', 
             'Length', 
             'Weight',
             'NaturalMortality',
             'Maturity',
             'Fecundity',
             'SRR',
             'Spatial',
             'Depletion'
  )
  
  for (sl in slots) {
    cli::cli_h3('{.code {sl}}')
    print_slot(object, sl)
  }
  
  cli::cli_end()
})