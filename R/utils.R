# change messages to blue text instead of default red

message <- function(...) {
  if (requireNamespace("crayon", quietly = TRUE)) {
    x <- paste(...)
    return(base::message(crayon::blue(paste(base::strwrap(x), collapes="\n"))))
  } else {
    return(base::message(...))
  }
}


tiny <- 1e-15  # define tiny variable
