GenerateMeanatGeneric <- function(Model, Pars, ...) {
  dots <- list(...)
  
  if (length(dots)!=1) {
    cli::cli_abort("dots must be length 1", .internal=TRUE)
  }
  
  fun <- get(Model)
  arg <- dots[[1]]
  
  stop("Need to update to handle `arg` as numeric vector and nD array")
  
  
  ArgNames <- names(dots)
  L <- list(dots[[1]])
  names(L) <- ArgNames
  
  if (is.array(dots[[1]])) {
    isArr <- TRUE
    dn <- names(dimnames(out)) 
    sim_index <- which(dn == "Sim")
    year_index <- which(dn == "Year")
    dd <- dim(dots[[1]])
    dd[dd[-c(sim_index, year_index)]]
    
    
   dimnames(dots[[1]])
    
  } else {
    isArr <- FALSE
    ArgLength <- length(dots[[1]])
  }
  
  

  

  
 
  dim_out <- c(ArgLength, dim(arr))

  out <- array(NA, dim=dim_out, dimnames = c(L, dimnames(arr)))
  dn <- names(dimnames(out))
  sim_index <- which(dn == "Sim")
  new_order <- c(sim_index, setdiff(seq_along(dn), sim_index))
  out <- aperm(out, new_order)
  
  if (length(dim_out)==3) {
    # no area
    for (sim in seq_len(dim_out[2])) {
      for (year in seq_len(dim_out[3])) {
        
        args <- c(dots, lapply(Pars, function(p) {
          p[sim, year]
          })
          )
        out[sim, , year] <- do.call(fun, args)
        
      }
    }
    return(out)
  } 
  
  if (length(dim_out)==4) {
    # by area
    for (sim in seq_len(dim_out[2])) {
      for (year in seq_len(dim_out[3])) {
        for (area in seq_len(dim_out[4])) {
          args <- c(dots, lapply(Pars, function(p) {
            p[sim, year,area]
          })
          )
          out[sim, , year,area] <- do.call(fun, args)
          
        }
      }
    }
    return(out)
  }
  
  cli::cli_abort(c("x"="Pars must be sim x year or sim x year x area"),)

  

  
  #              
  #              
  # AreaDimension <- purrr::map(Pars, \(Par) {
  #   dimnames(Par)[["Area"]]
  # }) |> unlist()
  # 
  # if (is.null(AreaDimension)) {
  #   
  #   
  #                          
  #   
  #   
  #   
  #   for (i in seq_along(arg_ind)) {
  #     ParsList[[fun_args[[arg_ind[i]]]]] <- Pars[[i]]
  #   }
  #   
  #   # 
  #  
  #     NA,
  #     dim = c(length(Ages), length(sims), length(years)),
  #     dimnames = list(Age = Ages, Sim = sims, Year = years)
  #   )
  #   
  #   
  #   
  #   return(do.call(fun, ParsList))
  # }
  # 
  # nArea <- AreaDimension |>
  #   as.numeric() |>
  #   max()
  # AreaValues <- list()
  # for (area in 1:nArea) {
  #   for (i in seq_along(Pars)) {
  #     ParsList[[fun_args[[arg_ind[i]]]]] <- abind::adrop(Pars[[i]][, , area, drop = FALSE], 3)
  #   }
  #   AreaValues[[area]] <- do.call(fun, ParsList)
  # }
  # abind::abind(AreaValues, along=4)
  
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
