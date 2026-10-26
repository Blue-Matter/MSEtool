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
  ns <- asNamespace('MSEtool')
  objects_name <- sort(getNamespaceExports(ns))
  objects <- lapply(objects_name, get, envir=ns)
  objects_class <- vapply(objects, \(x) class(x)[1], character(1))
  ind <- which(vapply(objects, is.function, logical(1)) & objects_class %in% ModelClass)
  objects_name[ind]
}

#' Identify the Model Function for an Object
#'
#' Attempts to identify the model function associated with `object` by matching
#' the names of `object@Pars` against the formal arguments of candidate model
#' functions returned by the relevant `*Models()` function. Formals with a
#' default value are optional: a model matches when every supplied parameter is
#' a formal and every formal without a default is supplied. If several models
#' match, the one with the fewest omitted optional formals is chosen; a tie is
#' an error. If `object@Model`
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
  ParNames <- ParNames[!.IsCVPar(ParNames)]
  
  nOmitted <- rep(NA_integer_, length(models))
  for (i in seq_along(models)) {
    fm <- .ModelFormals(models[i], ignore)
    if (all(ParNames %in% fm$Names) && all(fm$Required %in% ParNames)) {
      matching_parameters[i] <- TRUE
      nOmitted[i] <- length(setdiff(fm$Names, ParNames))
    } else {
      matching_parameters[i] <- FALSE
    }
  }
  # prefer the model needing the fewest defaulted optional parameters
  if (any(matching_parameters))
    matching_parameters <- matching_parameters & nOmitted == min(nOmitted, na.rm = TRUE)
  model <- models[matching_parameters]

  if (length(model) > 1)
    cli::cli_abort(c(
      'Parameters {.val {names(object@Pars)}} match more than one model for class {.val {class(object)}}: {.val {model}}.',
      'i' = 'Supply the optional parameters that identify the model, or set `Model` explicitly.'),
      call = NULL)

  if (doCheck && length(model)<1) {
    .CheckModel(object)
  }

  model
}


# formal names of a model (excluding `ignore`) and those without a default
.ModelFormals <- function(model, ignore = character()) {
  fm <- formals(if (is.function(model)) model else get(model))
  fm <- fm[!names(fm) %in% c(ignore, '...')]
  required <- vapply(fm, \(x) is.symbol(x) && !nzchar(as.character(x)), logical(1))
  list(Names = names(fm), Required = names(fm)[required], Defaults = fm[!required])
}

#' Add Default Values for Omitted Optional Model Parameters
#'
#' Appends the model's default value for every optional formal argument of the
#' matched model that is missing from `object@Pars`.
#'
#' @param object An S4 object with slots `Model` and `Pars`.
#' @param ignore Character vector of formal argument names to exclude.
#'
#' @return The `Pars` list with defaults added.
#' @keywords internal
.AddDefaultPars <- function(object,
                            ignore = c('Ages', 'MeanLength', 'Length', 'Weight', 'nage',
                                       'AtAge', 'MaxLen', 'S0', 'S', 'R0')) {
  Pars <- object@Pars
  if (inherits(object@Model, 'function') || length(Pars) < 1 || any(is.na(Pars)))
    return(Pars)
  model <- .FindModel(object, ignore = ignore, doCheck = FALSE)
  if (length(model) != 1) return(Pars)
  Defaults <- .ModelFormals(model, ignore)$Defaults
  for (nm in setdiff(names(Defaults), names(Pars)))
    Pars[[nm]] <- eval(Defaults[[nm]])
  Pars
}

# model class, inferring the size basis of a custom function from its arguments
.SizeModelClass <- function(Model) {
  cl <- .GetModelClass(Model)
  if (!identical(cl, 'function')) return(cl)
  args <- names(formals(Model))
  if ('Length' %in% args) return('function-at-Length')
  if ('Weight' %in% args) return('function-at-Weight')
  if ('Ages' %in% args) return('function-at-Age')
  cl
}

# `<name>_CV` and `CV_<name>` entries of Pars report the uncertainty of a parameter
.IsCVPar <- function(Names) grepl("(_CV$|^CV_)", Names)

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
