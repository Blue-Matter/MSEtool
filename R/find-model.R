#' Find All Model Functions for a Given Class
#'
#' Returns the names of all exported functions in the `MSEtool` package whose
#' class matches `ModelClass`.
#'
#' @param ModelClass Character. The class name to match against (e.g.
#'   `"NaturalMortalityModel"`).
#'
#' @return Character vector of matching function names.
#' @keywords internal
.FindModels <- function(ModelClass) {
  objects_name <- ls.str("package:MSEtool", mode = "function")
  objects <- lapply(objects_name, get, envir=asNamespace('MSEtool'))
  objects_class <- lapply(objects, class)
  objects_class <- unlist(lapply(objects_class, '[[', 1))
  ind <- which(objects_class%in%ModelClass)
  objects_name[ind]
}

#' Identify the Model Function for an Object
#'
#' Attempts to identify the model function associated with `object` by matching
#' the names of `object@Pars` against the formal arguments of candidate model
#' functions returned by the relevant `*Models()` function. If `object@Model`
#' is already a function it is returned directly. Returns `NULL` if `Pars` is
#' empty or contains `NA`.
#'
#' @param object An S4 object with slots `Model` and `Pars`.
#' @param ignore Character vector of formal argument names to exclude from
#'   matching. Defaults to common auxiliary arguments such as `"Ages"`,
#'   `"Length"`, `"Weight"`, etc.
#' @param doCheck Logical. If `TRUE` (default), calls `.CheckModel()` and
#'   throws an informative error when no matching model is found.
#'
#' @return A character string naming the matched model function, a function
#'   object if `object@Model` is already a function, or `NULL` if `Pars` is
#'   empty or `NA`.
#' @keywords internal
.FindModel <- function(object, 
                      ignore=c('Ages', 'MeanLength', 'Length', 'Weight', 'nage',
                               'AtAge', 'MaxLen', 'S0', 'S', 'R0'),
                      doCheck=TRUE) {

  
  if (inherits(object@Model,'function'))
    return(object@Model)

  if (any(is.na(object@Pars)))
    return(NULL)

  if (length(object@Pars)<1)
    return(NULL)

  .CheckPars(object@Pars)

  # if (inherits(object@Model,'character'))
  #   return(object@Model)

  cl <- class(object)
  slots <- c(slotNames('stock'), slotNames('fleet'))
  ind <- match(cl, tolower(slots))

  fun <- get(paste0(slots[ind], 'Models'))
  models <- fun(FALSE, FALSE)

  matching_parameters <- rep(TRUE, length(models))

  ParNames <- names(object@Pars)
  ind <- which(tolower(ParNames) |> .SubstrRight(2) == 'sd')
  if (length(ind)>0)
    ParNames <- ParNames[-ind]
  
  for (i in seq_along(matching_parameters)) {
    formals <- formals(get(models[i]))
    formal_names <- NA
    for (j in seq_along(formals)) {
      formal_names[j] <- names(formals[j])
    }
    formal_names <- formal_names[!formal_names%in% ignore]
    if (!(all(ParNames %in% formal_names)
        & all(formal_names %in% ParNames)))
      matching_parameters[i] <- FALSE
  }
  model <- models[matching_parameters]

  if (doCheck && length(model)<1) {
    .CheckModel(object)
  }

  model
}


#' Validate a `Pars` List
#'
#' Checks that `Pars` is a named list. Throws an informative error if any
#' element has an empty name. Returns `Pars` unchanged if valid or empty.
#'
#' @param Pars A named list of model parameters.
#'
#' @return `Pars`, unchanged.
#' @keywords internal
.CheckPars <- function(Pars) {
  if (length(Pars)<1)
    return(Pars)

  if (is.null(names(Pars)) || any(nchar(names(Pars))==0)) {
    cli::cli_abort('`Pars` must be a named list (or an empty list or NULL)')
  }
  Pars
}

#' Throw an Informative Error When No Model is Found
#'
#' Called by `.FindModel()` when parameter names in `object@Pars` do not match
#' any candidate model. Constructs an error message directing the user to the
#' relevant `*Models()` function or advising them to set `Pars = NULL` or
#' supply a custom function to `Model`.
#'
#' @param object An S4 object with slots `Model` and `Pars`.
#' 
#' @importFrom rlang caller_call
#' @return Does not return. Always throws an error via [cli::cli_abort()].
#' 
#' @keywords internal
.CheckModel <- function(object) {
  if (inherits(object, "srr")) {
    fun <- paste0(.FirstUp(class(object),3), 'Models')
  } else {
    fun <- paste0(.FirstUp(class(object)), 'Models')  
  }
  
  nms <-  names(object@Pars)
  cli::cli_abort(c(
    'No model found for this object class {.val {class(object)}} with parameters named: {.val { nms }}.',
    'i'='See {.fun { fun}} or set `Pars` to NULL or `Model` to a R function with arguments corresponding with those in `Pars`.'), call=rlang::caller_call(n=2)
  )
}
