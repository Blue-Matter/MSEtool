#' Extend an array
#'
#' Extends an array with named dimensions to include all simulations, ages, and areas
#'  and any missing years,
#'
#'
#' @param array An [array()] with named dimensions including at least one of
#' `Sim`, `Year`,  `Age`, or `Area`. Alternatively, an [S4] or a [list()] object, in which
#' case the function will loop over all slots or elements respectively and extend
#' any named arrays. Any other objects will be returned unchanged.
#' @param nSim The total number of simulations. Integer (or NULL to skip)
#' @param AgeClasses A numeric vector of age classes (or NULL to skip)
#' @param Years A numeric vector of `Year` values (or NULL to skip)
#' @param Areas A numeric vector with values 1:`nArea` (or NULL to skip)
#' @param default Default value for forward filled year values. Default is the same
#' value as the most recent year
#' @param debug Logical. Print debug messages?
#'
#' @details
#' ## ExtendSims
#'
#' A `Sim` dimension in an array must have either length `1` or length `nSim`.
#' If the latter, `ExtendSims` returns the array unchanged. If the former, the names of the
#' `Sim` dimension is extended to length `nSim` with the values of simulation 1 replicated for
#' all simulations.
#'
#' ## ExtendAges
#' An `Age` dimension in an array must have either length `1` or length `nAge` for a given stock.
#' If the latter, `ExtendAges` returns the array unchanged. If the former, the names of the
#' `Age` dimension is extended to length `nAge` with the values of the first age class replicated for
#' all age classes.
#'
#' ## ExtendYears
#' A `Year` dimension can be any length and include any years in `Years(OM)`.
#' If the names of the `Year` dimension includes all values in `Years`, the array
#' will be returned unchanged. Otherwise, the array will be extended to include
#' all missing values in `Years`.
#' 
#' ## ExtendAreas
#' An `Area` dimension in an array must have either length `1` or length `nArea` for a given OM
#' If the latter, `ExtendAreas` returns the array unchanged. If the former, the names of the
#' `Area` dimension is extended to length `nArea` with the values of the first area replicated for
#' all other areas.
#'
#' Values will be back filled or forward filled as necessary to include all `Years`.
#' If there is seasonality (decimal years), the filled values will match those from the
#' most recent season.
#'
#' @example man-examples/Extend.R
#'
#' @export
Extend <- function(array,
                   nSim = NULL,
                   AgeClasses = NULL,
                   Years = NULL,
                   Areas = NULL,
                   default = NULL,
                   debug = FALSE) {
  if (debug) {
    print(class(array))
  }
  
  # Recall if not an `array` class object
  if (isS4(array)) {
    if (inherits(array, "data")) {
      return(array)
    }
    slots <- slotNames(array)

    for (sl in slots) {
      if (debug) {
        print(sl)
      }
      slot(array, sl) <- Recall(slot(array, sl), nSim, AgeClasses, Years, Areas, default, debug)
    }
    return(array)
  }

  if (is.list(array)) {
    if (length(array)) {
      for (i in 1:length(array)) {
        temp <- Recall(array[[i]], nSim, AgeClasses, Years, Areas, default, debug)
        if (!is.null(temp)) {
          array[[i]] <- temp
        }
      }
      return(array)
    }
  }

  array |>
    ExtendSims(nSim) |>
    ExtendAges(AgeClasses) |>
    ExtendYears(Years, default) |>
    ExtendAreas(Areas)
}

#' @rdname Extend
#' @export
#'
ExtendSims <- function(array, nSim = NULL) {
  if (is.null(nSim)) {
    return(array)
  }

  if (length(nSim) != 1) {
    cli::cli_abort("`nSim` must be an integer or numeric value of length 1")
  }
  # Recall if not an `array` class object
  if (isS4(array)) {
    if (inherits(array, "data")) {
      return(array)
    }
    slots <- slotNames(array)

    for (sl in slots) {
      if (debug) {
        print(sl)
      }
      slot(array, sl) <- Recall(slot(array, sl), nSim)
    }
    return(array)
  }

  if (is.list(array)) {
    if (length(array)) {
      for (i in 1:length(array)) {
        temp <- Recall(array[[i]], nSim)
        if (!is.null(temp)) {
          array[[i]] <- temp
        }
      }
      return(array)
    }
  }

  d <- dim(array)
  dn <- dimnames(array)

  if (is.null(dn) || !"Sim" %in% names(dn)) {
    return(array)
  }

  sim_dim <- which(names(dn) == "Sim")

  existing_sims <- as.numeric(dn[[sim_dim]])

  if (length(existing_sims) == nSim) {
    return(array)
  }

  if (length(existing_sims) != 1) {
    cli::cli_abort(c("The `Sim` dimension must be either length `nSim` ({.val {nSim}}) or length 1",
      "x" = "The `Sim` dimension of this array has length {.val {d[sim_dim]}}"
    ))
  }
  
  # replicate Sim = 1 along the Sim dimension
  idx <- lapply(seq_along(d), function(i) {
    if (i == sim_dim) rep(1L, nSim) else seq_len(d[i])
  })
  
  OutArray <- do.call(`[`, c(list(array), idx, list(drop = FALSE)))
  dimnames(OutArray)[[sim_dim]] <- as.character(seq_len(nSim))
  OutArray
}

#' @rdname Extend
#' @export
#'
ExtendAges <- function(array, AgeClasses = NULL) {
  if (is.null(AgeClasses)) {
    return(array)
  }

  # Recall if not an `array` class object
  if (isS4(array)) {
    if (inherits(array, "data")) {
      return(array)
    }
    slots <- slotNames(array)

    for (sl in slots) {
      if (debug) {
        print(sl)
      }
      slot(array, sl) <- Recall(slot(array, sl), AgeClasses)
    }
    return(array)
  }

  if (is.list(array)) {
    if (length(array)) {
      for (i in 1:length(array)) {
        temp <- Recall(array[[i]], AgeClasses)
        if (!is.null(temp)) {
          array[[i]] <- temp
        }
      }
      return(array)
    }
  }

  nAge <- length(AgeClasses)
  d <- dim(array)
  dn <- dimnames(array)

  if (is.null(dn) || !"Age" %in% names(dn)) {
    return(array)
  }

  age_dim <- which(names(dn) == "Age")
  existing_ages <- as.numeric(dn[[age_dim]])

  if (length(existing_ages) == nAge) {
    return(array)
  }

  if (sum(AgeClasses %in% existing_ages) == nAge - 1) {
    # exception for RecDevInit
    return(array)
  }

  if (length(existing_ages) != 1) {
    cli::cli_abort(c("The `Age` dimension must be either length `nAge` ({.val {nAge}}) or length 1",
      "x" = "The `Age` dimension of this array has length {.val {d[age_dim]}}"
    ))
  }

  # replicate Age = 1 along the Age dimension
  idx <- lapply(seq_along(d), function(i) {
    if (i == age_dim) rep(1L, nAge) else seq_len(d[i])
  })
  
  OutArray <- do.call(`[`, c(list(array), idx, list(drop = FALSE)))
  dimnames(OutArray)[[age_dim]] <- as.character(AgeClasses)
  OutArray

}

#' @rdname Extend
#' @export
ExtendYears <- function(array, Years = NULL, default = NULL) {
  if (!is.array(array) | is.null(Years)) {
    return(array)
  }

  d <- dim(array)
  dn <- dimnames(array)

  if (is.null(dn) || !"Year" %in% names(dn)) {
    return(array)
  }

  year_dim <- which(names(dn) == "Year")
  nyear <- d[year_dim]
  existing_years <- as.numeric(dn[[year_dim]])
  fill_years <- unique(Years) |> sort()
  all_years <- c(existing_years, fill_years) |>
    unique() |>
    sort()

  if (all(fill_years %in% existing_years)) {
    return(array)
  }

  # Create output array
  d[[year_dim]] <- length(all_years)
  dn[[year_dim]] <- all_years
  OutArray <- array(NA, dim = d, dimnames = dn)
  abind::afill(OutArray) <- array # add the existing values

  fill_years <- all_years[!all_years %in% existing_years]
  back_years <- fill_years[which(fill_years < min(existing_years))]
  forward_years <- fill_years[which(fill_years > max(existing_years))]
  inside_years <- fill_years[!fill_years %in% back_years & !fill_years %in% forward_years]

  # Seasonal
  isSeasonal <- which(all_years - round(all_years, 0) > 0) |> length()

  if (isSeasonal) {
    return(ExtendYears_seasonal(array, Years, default))
  }

  # Forward fill years from most recent existing year
  if (length(forward_years)) {
    MostRecent <- abind::asub(array, nyear, year_dim, drop = FALSE)
    if (!is.null(default)) {
      MostRecent[] <- default
    }
    d <- dim(MostRecent)
    d[[year_dim]] <- length(forward_years)
    dn[[year_dim]] <- forward_years
    abind::afill(OutArray) <- array(MostRecent, dim = d, dimnames = dn)
  }


  # Back fill years from first existing year
  if (length(back_years)) {
    FirstYear <- abind::asub(array, 1, year_dim, drop = FALSE)
    d <- dim(FirstYear)
    d[[year_dim]] <- length(back_years)
    dn[[year_dim]] <- back_years
    abind::afill(OutArray) <- array(FirstYear, dim = d, dimnames = dn)
  }

  # Fill years within existing years
  if (length(inside_years)) {
    interval_index <- findInterval(inside_years, existing_years)
    interval_index[interval_index == 0] <- 1
    interval_index[interval_index > length(existing_years) - 1] <- length(existing_years) - 1
    TimeBlocks <- split(inside_years, interval_index)

    for (i in seq_along(TimeBlocks)) {
      year_ind <- which(existing_years < min(TimeBlocks[[i]])) |> max()
      FillValue <- abind::asub(array, year_ind, year_dim, drop = FALSE)
      d <- dim(FillValue)
      d[[year_dim]] <- length(TimeBlocks[[i]])
      dn[[year_dim]] <- TimeBlocks[[i]]
      abind::afill(OutArray) <- array(FillValue, dim = d, dimnames = dn)
    }
  }
  OutArray
}

NoSeasonVals <- function(x) {
  all(abs(x - round(x)) < .Machine$double.eps^0.5)
}

ExtendYears_seasonal <- function(array, Years = NULL, default = NULL, tol = 0.01) {
  if (!is.array(array)) {
    cli::cli_abort("`array` must be an array")
  }

  if (is.null(Years)) {
    return(array)
  }

  d <- dim(array)
  dn <- dimnames(array)

  if (is.null(dn) || !"Year" %in% names(dn)) {
    cli::cli_abort("`array` must have a dimension named 'Year'")
  }

  year_dim <- which(names(dn) == "Year")
  nyear <- d[year_dim]
  existing_years <- as.numeric(dn[[year_dim]])
  fill_years <- unique(Years) |> sort()
  all_years <- c(existing_years, fill_years) |>
    unique() |>
    sort()

  if (all(fill_years %in% existing_years)) {
    return(array)
  }

  # Create output array
  d[[year_dim]] <- length(all_years)
  dn[[year_dim]] <- all_years
  OutArray <- array(NA, dim = d, dimnames = dn)
  abind::afill(OutArray) <- array # add the existing values

  fill_years <- all_years[!all_years %in% existing_years]
  back_years <- fill_years[which(fill_years < min(existing_years))]
  forward_years <- fill_years[which(fill_years > max(existing_years))]
  inside_years <- fill_years[!fill_years %in% back_years & !fill_years %in% forward_years]

  season_existing <- existing_years %% 1
  # Forward fill years from most recent existing year
  if (length(forward_years)) {
    season_forward <- (forward_years %% 1) |> unique()
    if (NoSeasonVals(season_existing)) {
      # no seasons in provided values - constant over seasons within years
      most_recent_ind <- nyear
      MostRecent <- abind::asub(array, most_recent_ind, year_dim, drop = FALSE)
      if (!is.null(default)) {
        MostRecent[] <- default
      }
      d <- dim(MostRecent)
      d[[year_dim]] <- length(forward_years)
      dn[[year_dim]] <- forward_years
      abind::afill(OutArray) <- array(MostRecent, dim = d, dimnames = dn)
      
    } else {
      # loop over seasons - match the season
      for (i in seq_along(season_forward)) {
        season_ind <- which(abs(forward_years %% 1 - season_forward[i]) < tol)
        most_recent_ind <- which(abs(season_existing - season_forward[i]) < tol)
        if (length(existing_years) == 1) {
          most_recent_ind <- 1
        }
        
        CheckSeasonExists(most_recent_ind, season_forward[i], existing_years, season_existing)
        most_recent_ind <- max(most_recent_ind)
        
        MostRecent <- abind::asub(array, most_recent_ind, year_dim, drop = FALSE)
        if (!is.null(default)) {
          MostRecent[] <- default
        }
        d <- dim(MostRecent)
        d[[year_dim]] <- length(forward_years[season_ind])
        dn[[year_dim]] <- forward_years[season_ind]
        abind::afill(OutArray) <- array(MostRecent, dim = d, dimnames = dn)
      }
    }
  }

  # Back fill years from first existing year
  if (length(back_years)) {
    season_backward <- (back_years %% 1) |> unique()
    if (NoSeasonVals(season_existing)) {
      # no seasons in provided values - constant over seasons within years
      MostRecent <- abind::asub(array, 1, year_dim, drop = FALSE)
      if (!is.null(default)) {
        MostRecent[] <- default
      }
      d <- dim(MostRecent)
      d[[year_dim]] <- length(forward_years)
      dn[[year_dim]] <- forward_years
      abind::afill(OutArray) <- array(MostRecent, dim = d, dimnames = dn)
      
    } else {
      # loop over seasons - match the season
      for (i in seq_along(season_backward)) {
        season_ind <- which(abs(back_years %% 1 - season_backward[i]) < tol)
        most_recent_ind <- which(abs(season_existing - season_backward[i]) < tol)
        if (length(existing_years) == 1) {
          most_recent_ind <- 1
        }
        CheckSeasonExists(most_recent_ind, season_backward[i], existing_years, season_existing)
        most_recent_ind <- min(most_recent_ind)
        
        MostRecent <- abind::asub(array, most_recent_ind, year_dim, drop = FALSE)
        d <- dim(MostRecent)
        d[[year_dim]] <- length(back_years[season_ind])
        dn[[year_dim]] <- back_years[season_ind]
        abind::afill(OutArray) <- array(MostRecent, dim = d, dimnames = dn)
      }
    }
  }

  # Fill years within existing years
  if (length(inside_years)) {
    interval_index <- findInterval(inside_years, existing_years)
    interval_index[interval_index == 0] <- 1
    interval_index[interval_index > length(existing_years) - 1] <- length(existing_years) - 1
    TimeBlocks <- split(inside_years, interval_index)

    for (i in seq_along(TimeBlocks)) {
      years_block <- TimeBlocks[[i]]
      season_inside <- (years_block %% 1) |> unique()

      if (NoSeasonVals(season_existing)) {
        # no seasons in provided values - constant over seasons within years
        most_recent_ind <- which(existing_years <  min(years_block)) |> max()
        MostRecent <- abind::asub(array, most_recent_ind, year_dim, drop = FALSE)
        if (!is.null(default)) {
          MostRecent[] <- default
        }
        d <- dim(MostRecent)
        d[[year_dim]] <- length(inside_years)
        dn[[year_dim]] <- inside_years
        abind::afill(OutArray) <- array(MostRecent, dim = d, dimnames = dn)
        
      } else {
        for (j in seq_along(season_inside)) {
          season_ind <- which(abs(years_block %% 1 - season_inside[j]) < tol)
          most_recent_ind <- which(abs(season_existing - season_inside[i]) < tol)
          if (length(existing_years) == 1) {
            most_recent_ind <- 1
          }
          CheckSeasonExists(most_recent_ind, season_inside[i], existing_years, season_existing)
          most_recent_ind <- min(most_recent_ind)
          
          MostRecent <- abind::asub(array, most_recent_ind, year_dim, drop = FALSE)
          d <- dim(MostRecent)
          d[[year_dim]] <- length(years_block[season_ind])
          dn[[year_dim]] <- years_block[season_ind]
          abind::afill(OutArray) <- array(MostRecent, dim = d, dimnames = dn)
        }
      }
    }
  }
  OutArray
}

CheckSeasonExists <- function(most_recent_ind, try_season, existing_years, season_existing) {
  if (length(existing_years) == 1) {
    return(NULL)
  }
  if (any(!is.finite(most_recent_ind)) || !length(most_recent_ind)) {
    cli::cli_abort(c(
      "x" = "Could not match Season {.val {try_season}} with an existing season",
      "i" = "Existing Years: {.val {existing_years}}",
      "i" = "Existing Seasons: {.val {season_existing}}"
    ))
  }
}


#' @rdname Extend
#' @export
#'
ExtendAreas <- function(array, Areas = NULL) {
  if (is.null(Areas)) {
    return(array)
  }
  
  # Recall if not an `array` class object
  if (isS4(array)) {
    if (inherits(array, "data")) {
      return(array)
    }
    slots <- slotNames(array)
    
    for (sl in slots) {
      if (debug) {
        print(sl)
      }
      slot(array, sl) <- Recall(slot(array, sl), Areas)
    }
    return(array)
  }
  
  if (is.list(array)) {
    if (length(array)) {
      for (i in 1:length(array)) {
        temp <- Recall(array[[i]], Areas)
        if (!is.null(temp)) {
          array[[i]] <- temp
        }
      }
      return(array)
    }
  }
  
  nArea <- length(Areas)
  d <- dim(array)
  dn <- dimnames(array)
  
  if (is.null(dn) || !"Area" %in% names(dn)) {
    return(array)
  }
  
  area_dim <- which(names(dn) == "Area")
  existing_areas <- as.numeric(dn[[area_dim]])
  
  if (length(existing_areas) == nArea) {
    return(array)
  }
  
  if (length(existing_areas) != 1) {
    cli::cli_abort(c("The `Area` dimension must be either length `Area` ({.val {nArea}}) or length 1",
                     "x" = "The `nArea` dimension of this array has length {.val {d[area_dim]}}"
    ))
  }
  
  # replicate Area = 1 along the Area dimension
  idx <- lapply(seq_along(d), function(i) {
    if (i == area_dim) rep(1L, nArea) else seq_len(d[i])
  })
  
  OutArray <- do.call(`[`, c(list(array), idx, list(drop = FALSE)))
  dimnames(OutArray)[[area_dim]] <- as.character(Areas)
  OutArray
  
}
