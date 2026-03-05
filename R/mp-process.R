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
#' Recursively scans the body of an MP function and any detected helper
#' functions, returning the names of all non-MSEtool, non-base user-defined
#' functions reachable from the MP. Recurses into helper bodies to catch
#' second-order dependencies.
#'
#' @param MP A function of class `"mp"`.
#' @param MSEtool_funs Character vector of function names exported by MSEtool,
#'   used to exclude them from the result.
#' @param visited Character vector of already-visited function names, used
#'   internally to prevent infinite recursion.
#' @return Character vector of unique helper function names.
#' @keywords internal
DetectCalledFunctions <- function(MP, MSEtool_funs, visited = character()) {
  
  skip <- c("{", "<-", "=", "(", "[", "[[",
            "if", "for", "while", "repeat", "return")
  
  find_calls <- function(expr) {
    if (!is.call(expr)) return(NULL)
    fun <- expr[[1]]
    out <- character()
    if (is.symbol(fun)) {
      fname <- as.character(fun)
      if (!fname %in% skip &&
          !fname %in% MSEtool_funs &&
          exists(fname, mode = "function", inherits = TRUE)) {
        out <- fname
      }
    }
    c(out, unlist(lapply(as.list(expr)[-1], find_calls)))
  }
  
  direct <- unique(find_calls(body(MP)))
  
  # Recurse into helper bodies to catch second-order dependencies
  new_helpers <- setdiff(direct, c(MSEtool_funs, visited))
  visited     <- union(visited, new_helpers)
  
  deeper <- unlist(lapply(new_helpers, function(fname) {
    fun <- tryCatch(get(fname, mode = "function", inherits = TRUE),
                    error = function(e) NULL)
    if (is.null(fun) || isNamespace(environment(fun))) return(NULL)
    DetectCalledFunctions(fun, MSEtool_funs, visited)
  }))
  
  unique(c(direct, deeper))
}

#' Make an MP self-contained by capturing helper functions
#'
#' Creates a new function environment for an MP, parented to the MSEtool
#' namespace so that package functions (including `methods::new()`,
#' `utils::tail()`, and all MSEtool internals) are findable by normal lexical
#' scoping without being copied. Only user-defined helper functions from the
#' calling environment are captured explicitly.
#'
#' @param MP A function of class `"mp"`.
#' @return The same MP with an updated self-contained environment.
#' @keywords internal
MakeSelfContained <- function(MP) {
  CheckClass(MP, 'mp', 'MP')
  
  # Create a new environment for the function
  env <- new.env(parent = asNamespace("MSEtool"))
  mp_env <- environment(MP)
  
  # Get all exported functions from MSEtool
  MSEtool_funs <- vapply(
    ls(getNamespace("MSEtool"), all.names = TRUE),
    function(x) is.function(get(x, envir = asNamespace("MSEtool"))),
    logical(1)
  )
  MSEtool_funs <- names(MSEtool_funs)[MSEtool_funs]
  

  # Get internal helper functions
  helpers <- DetectCalledFunctions(MP, MSEtool_funs=MSEtool_funs)
  
  helpers <- Filter(function(fname) {
    exists(fname, envir = mp_env, inherits = TRUE) &&
      !isNamespace(environment(get(fname, envir = mp_env, inherits = TRUE)))
  }, helpers)
  
  for (fname in helpers) {
    env[[fname]] <- get(fname, envir = mp_env, inherits = TRUE)
  }
 
  environment(MP) <- env
  MP
}



