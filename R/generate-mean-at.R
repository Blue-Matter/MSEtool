#' Generate Mean-at-Age, Length, or Weight Arrays
#'
#' These functions generate simulated "mean at" arrays using a model and parameters. 
#'
#' @param Model A character string naming a built-in model or a valid R function.
#' @param Pars A named `list` of parameters for the model.
#' @param Ages Numeric vector of age classes (for `GenMeanAtAge`).
#' @param Length Numeric vector of length classes (for `GenMeanAtLength`).
#' @param Weight Numeric vector of weight classes (for `GenMeanAtWeight`).
#'
#' @return A numeric array with dimensions: `Sim`, `Age/Class`, `Year`, and 
#' sometimes `Area`.
#'
#' @example man-examples/generate-mean.R
#' 
#' @seealso [LengthModels()], [WeightModels()], [NaturalMortalityModels()],
#'  [MaturityModels], [FecundityModels()], [SelectivityModels()],
#'   [RetentionModels()]
#' @name GenMeanAtLength
#' @export
GenMeanAtAge <- function(Model, Pars, Ages) {
  MeanAtAge <- GenerateMeanatGeneric(Model, Pars, Ages=Ages)
  dn <- dimnames(MeanAtAge)
  names(dn)[names(dn) == "Ages"] <- "Age"
  dimnames(MeanAtAge) <- dn
  MeanAtAge
}

#' @rdname GenMeanAtLength
#' @export
GenMeanAtLength <- function(Model, Pars, Length) {
  MeanAtLength <- GenerateMeanatGeneric(Model, Pars, Length=Length)
  dn <- dimnames(MeanAtLength)
  names(dn)[names(dn) == "Length"] <- "Class"
  dimnames(MeanAtLength) <- dn
  MeanAtLength
}

#' @rdname GenMeanAtLength
#' @export
GenMeanAtWeight <- function(Model, Pars, Weight) {
  MeanAtWeight <- GenerateMeanatGeneric(Model, Pars, Weight=Weight)
  dn <- dimnames(MeanAtWeight)
  names(dn)[names(dn) == "Weight"] <- "Class"
  dimnames(MeanAtWeight) <- dn
  MeanAtWeight
}


GenerateMeanatGeneric <- function(Model, Pars, nSim = 5, Years=NULL,  ...) {
  
  dots <- list(...)
  if (length(dots) != 1) {
    cli::cli_abort("dots must be length 1", .internal = TRUE)
  }
  
  # Convert Pars to named arrays if needed
  Pars <- StructurePars(Pars=Pars, nSim=nSim, Years=DefaultYears(Years))
  
  # Determine function
  fun <- if (is.function(Model)) Model else get(Model)
  
  arg_name <- names(dots)
  arg <- dots[[1]]
  
  # Convert non-array input to array
  if (!is.array(arg)) {
    par_array <- Pars[[1]]
    dim_out <- c(length(arg), dim(par_array))
    L <- list(arg); names(L) <- arg_name
    arg_array <- array(arg, dim = dim_out, dimnames = c(L, dimnames(par_array)))
    arg_array <- reorderdims(arg_array)
    dim_out <- dim(arg_array)
    array_out <- array(NA, dim = dim_out, dimnames = dimnames(arg_array))
  } else {
    # Extend arrays to common dimensions
    nsim   <- max(c(dimnames(arg)$Sim, dimnames(Pars[[1]])$Sim) |> as.numeric(), na.rm = TRUE)
    years  <- sort(unique(c(dimnames(arg)$Year, dimnames(Pars[[1]])$Year) |> as.numeric()))
    areas  <- sort(unique(c(dimnames(arg)$Area, dimnames(Pars[[1]])$Area) |> as.numeric()))
    
    nsim   <- if (length(nsim)) nsim else NULL
    years  <- if (length(years)) years else NULL
    areas  <- if (length(areas)) areas else NULL
    
    Pars <- purrr::map(Pars, \(par) Extend(par, nsim, NULL, years, areas))
    arg_array <- Extend(arg, nsim, NULL, years, areas)
    dim_out <- dim(arg_array)
    array_out <- array(NA, dim = dim_out, dimnames = dimnames(arg_array))
  }
  
  # no area dimension
  if (length(dim_out) == 3) {
    for (sim in seq_len(dim_out[1])) {
      for (year in seq_len(dim_out[3])) {
        arg_list <- list(arg_array[sim, , year])
        names(arg_list) <- arg_name
        args <- c(arg_list, lapply(Pars, function(p) {
          dd <- dim(p)
          p_sim <- min(sim, dd[1])
          p_year <- min(year, dd[2])
          if (length(dd)==3) {
            return(p[p_sim, p_year,])
          } 
          p[p_sim, p_year]
          
        })
        )
        array_out[sim, , year] <- do.call(fun, args)
      }
    }
    return(array_out)
  }
  
  # has area dimension
  if (length(dim_out) == 4) {
    for (sim in seq_len(dim_out[1])) {
      for (year in seq_len(dim_out[3])) {
        for (area in seq_len(dim_out[4])) {
          arg_list <- list(arg_array[sim, , year, area]); names(arg_list) <- arg_name
          args <- c(arg_list, lapply(Pars, function(p) p[sim, year, area]))
          array_out[sim, , year, area] <- do.call(fun, args)
        }
      }
    }
    return(array_out)
  }
  
  cli::cli_abort(c("x" = "Only works for 2D (sim x year) or 3D (sim x year x area) Par arrays"), 
                 .internal=TRUE)
}


reorderdims <- function(array) {
  dn <- names(dimnames(array))
  sim_index <- which(dn == "Sim")
  new_order <- c(sim_index, setdiff(seq_along(dn), sim_index))
  aperm(array, new_order)
}