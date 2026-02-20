#' Check that management procedures are of class `mp`
#'
#' Verifies that all supplied management procedure (MP) names correspond
#' to functions of class `mp`. Throws an error if any MP does not meet this requirement.
#'
#' @param MPs Character vector of names of management procedure functions to check.
#'
#' The function retrieves each function by name using `get()`, checks its class,
#' and ensures that all are of class `mp`. If any function is not of class `mp`,
#' the function will abort with a clear error message.
#'
#' @return
#' Invisibly returns `NULL`. The function is used for validation and does not modify inputs.
#'
#' @keywords internal
CheckMPClass <- function(MPs) {
  CheckClass(MPs, 'character', 'MPs')
  
  MPFunctions <- purrr::map(MPs, get)
  names(MPFunctions) <- MPs
  MPClass <- purrr::map(MPFunctions, class) |> unlist()
  if (any(MPClass != 'mp')) 
    cli::cli_abort("Currently only MPs of class `mp` are supported", call=NULL)
  
  invisible(NULL)
}

#' Detect helper functions called by an MP
#'
#' Scans the body of a management procedure (`MP`) and identifies names of
#' functions that are called within it
#'
#' @param MP A function of class `mp`.
#'
#' @return
#' A character vector of unique function names referenced in the body of `MP`.
#'
#' @keywords internal
DetectCalledFunctions <- function(MP, MSEtool_funs) {
  
  skip <- c("{", "<-", "=", "(", "[", "[[",
            "if", "for", "while", "repeat", "return")
  
  find_functions <- function(expr) {
    if (!is.call(expr)) return(NULL)
    
    fun <- expr[[1]]
    
    out <- character()
    
    if (is.symbol(fun)) {
      fname <- as.character(fun)
      
      if (!fname %in% skip &&
          exists(fname, mode = "function", inherits = TRUE) &&
          !fname %in% MSEtool_funs) {
        out <- fname
      }
    }
    
    c(out, unlist(lapply(as.list(expr)[-1], find_functions)))
  }
  
  unique(find_functions(body(MP)))
}

#' Make an MP self-contained by capturing helper functions
#'
#' Creates a new function environment for a management procedure (`MP`) and
#' stores any detected helper functions within that environment so the MP
#' can be safely returned and executed independently of the caller's scope.
#'
#' @param MP A function of class `mp`.
#'
#' @return
#' The same MP function with an updated environment containing its helpers.
#'
#' @keywords internal
MakeSelfContained <- function(MP) {
  CheckClass(MP, 'mp', 'MP')
  
  # Create a new environment for the function
  env <- new.env(parent = baseenv())  
  
  # Get all exported functions from MSEtool
  MSEtool_funs <- ls(getNamespace("MSEtool"), all.names = TRUE)
  MSEtool_funs <- MSEtool_funs[sapply(MSEtool_funs, function(x) is.function(get(x, envir = asNamespace("MSEtool"))))]
  
  # Get internal helper functions
  helpers <- DetectCalledFunctions(MP, MSEtool_funs=MSEtool_funs)
  if (length(helpers)) {
    helpers <- helpers[sapply(helpers, function(x) 
      exists(x, envir = parent.frame()) && 
      !isNamespace(environment(get(x))))]
  }
 
  
  # Add helper functions
  for (hname in helpers) {
    env[[hname]] <- get(hname, envir = parent.frame())
  }
  if (!is.null(helpers)) {
    for (hname in helpers) {
      env[[hname]] <- get(hname, envir = parent.frame())  
    }
  }
  
  # Add MSEtool exported functions 
  for (fname in MSEtool_funs) {
    fun <- try( get(fname, envir = asNamespace("MSEtool")), silent=TRUE)
    if (!inherits(fun,'try-error')) {
      env[[fname]] <- fun  
    }
    
  }
  
  environment(MP) <- env
  MP
}



