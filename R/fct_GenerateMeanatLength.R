GenerateMeanatGeneric <- function(Model, Pars, ...) {
  fun_args <- names(formals(Model))
  fun <- get(Model)
  arg_ind <- match(names(Pars), fun_args)
  val_ind <- 1:max(min(arg_ind - 1), 1)
  dots <- list(...)
  
  ParsList <- list()
  for (i in seq_along(val_ind)) {
    ParsList[[fun_args[[val_ind[i]]]]] <- dots[[fun_args[[i]]]]
  }
  
  AreaDimension <- purrr::map(Pars, \(Par) {
    dimnames(Par)[["Area"]]
  }) |> unlist()
  
  if (is.null(AreaDimension)) {
    for (i in seq_along(arg_ind)) {
      ParsList[[fun_args[[arg_ind[i]]]]] <- Pars[[i]]
    }
    return(do.call(fun, ParsList))
  }
  
  nArea <- AreaDimension |>
    as.numeric() |>
    max()
  AreaValues <- list()
  for (area in 1:nArea) {
    for (i in seq_along(Pars)) {
      ParsList[[fun_args[[arg_ind[i]]]]] <- abind::adrop(Pars[[i]][, , area, drop = FALSE], 3)
    }
    AreaValues[[area]] <- do.call(fun, ParsList)
  }
  abind::abind(AreaValues, along=4)
  
}



#' Generate `MeanAtLength` Values
#'
#' @param Model Either the name of a built-in model (character) or a valid R function
#' @param Pars A `list` of named parameters for `Model`
#' @param Length A numeric vector of length classes
#'
#' @export
GenerateMeanatLength <- function(Model, Pars, Length) {
  if (inherits(Model, "function")) {
    return(ApplyCustomAtLengthModel(Model, Pars, Length))
  }
  GenerateMeanGeneric(Model, Pars, Length=Length)
}

#' Generate `MeanAtLength` Values
#'
#' @param Model Either the name of a built-in model (character) or a valid R function
#' @param Pars A `list` of named parameters for `Model`
#' @param Length A numeric vector of length classes
#'
#' @export
GenerateMeanatLength <- function(Model, Pars, Length) {
  if (inherits(Model, "function")) {
    return(ApplyCustomAtLengthModel(Model, Pars, Length))
  }
  GenerateMeanatGeneric(Model, Pars, Length=Length)
}


#' @rdname GenerateMeanatLength
#' @param Weight A numeric vector of weight classes
#'
#' @export
#' 
GenerateMeanatWeight <- function(Model, Pars, Weight) {
  
  if (inherits(Model, 'function')) {
    stop("R functions not currently supported for MeanAtWeight")
    # return(ApplyCustomAtWeightModel(Model, Pars, Weight))
  }
  GenerateMeanatGeneric(Model, Pars, Weight=Weight)
}

#' @rdname GenerateMeanatLength
#' @param Ages A numeric vector of age classes
#'
#' @export
#' 
GenerateMeanAtAge <- function(Model, Pars, Ages) {
  
  if (inherits(Model, 'function')) {
    return(ApplyCustomAtAgeModel(Model, Pars, Ages))
  }

  GenerateMeanatGeneric(Model, Pars, Ages=Ages)
}
