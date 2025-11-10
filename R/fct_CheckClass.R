CheckClass <- function(object, class='om', name='OM', type='Argument') {
  
  checkClass <- sapply(class, function(i) inherits(object, i))
  if (all(!checkClass)) {
    cli::cli_abort(c('{type} {.var {name}} must be class {.cls {class}}',
                     "x" = "You've supplied an object of class {.cls {class(object)}}"), call=NULL)
  }
  invisible(object)
}