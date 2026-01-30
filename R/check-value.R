CheckInteger <- function(value) {
  if (value%%1!=0)
    cli::cli_abort(c('x'='`value` must be an integer'), call=NULL)
}
