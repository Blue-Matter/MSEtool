# change messages to blue text instead of default red

message <- function(...) {
  if (requireNamespace('usethis', quietly = TRUE)) {
    # first try use usethis messages
    x <- paste(...)
    usethis::ui_done(x)
  } else if (requireNamespace("crayon", quietly = TRUE)) {
    # otherwise crayon
    x <- paste(...)
    return(base::message(crayon::blue(paste(base::strwrap(x), collapes="\n"))))
  } else {
    return(base::message(...))
  }
}

message_oops <- function(...) {
  if (requireNamespace('usethis', quietly = TRUE)) {
    # first try use usethis messages
    x <- paste(...)
    usethis::ui_oops(x)
  } else if (requireNamespace("crayon", quietly = TRUE)) {
    # otherwise crayon
    x <- paste(...)
    return(base::message(crayon::red(paste(base::strwrap(x), collapes="\n"))))
  } else {
    return(base::message(...))
  }
}

message_warn <- function(...) {
  if (requireNamespace('usethis', quietly = TRUE)) {
    # first try use usethis messages
    x <- paste(...)
    usethis::ui_warn(x)
  } else if (requireNamespace("crayon", quietly = TRUE)) {
    # otherwise crayon
    x <- paste(...)
    return(base::message(crayon::red(paste(base::strwrap(x), collapes="\n"))))
  } else {
    return(base::message(...))
  }
}

message_info <- function(...) {
  if (requireNamespace('usethis', quietly = TRUE)) {
    # first try use usethis messages
    x <- paste(...)
    usethis::ui_info(x)
  } else if (requireNamespace("crayon", quietly = TRUE)) {
    # otherwise crayon
    x <- paste(...)
    return(base::message(crayon::green(paste(base::strwrap(x), collapes="\n"))))
  } else {
    return(base::message(...))
  }
}


tiny <- 1e-15  # define tiny variable
