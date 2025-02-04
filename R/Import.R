Import <- function(dir=NULL,
                   nSim=48,
                   pYears=50, ...) {
  list.files(dir)
  
}

# Import SS3 
# TODO add messages
ImportSS <- function(SSdir,     
                     nSim=48,
                     pYears=50, 
                     Name = "Imported SS3 Model",
                     ...) {
  
  if(!requireNamespace("r4ss", quietly = TRUE)) 
    cli::cli_abort(c('The `r4ss` package is required to use this function.',
                     'i'="It is recommended to install the Github version with `pak::pkg_install('r4ss/r4ss')`"
    ))

  dots <- list(dir = SSdir, ...)
  if(!any(names(dots) == "covar")) dots$covar <- FALSE
  if(!any(names(dots) == "forecast")) dots$forecast <- FALSE
  #if(!any(names(dots) == "ncols")) dots$ncols <- 1e3
  if(!any(names(dots) == "printstats")) dots$printstats <- FALSE
  if(!any(names(dots) == "verbose")) dots$verbose <- FALSE
  if(!any(names(dots) == "warn")) dots$warn <- FALSE

  replist <- try(do.call(r4ss::SS_output, dots), silent = TRUE)
  
  if(is.character(replist)) 
    cli::cli_abort(c("`r4ss::SS_output` function returned an error.", 
                     'x' = replist), call. = FALSE)
  
  OM <- OM(Name=Name)
  
}
  