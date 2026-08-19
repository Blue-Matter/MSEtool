#' Extend an Array Along Named Dimensions
#'
#' Extends an array with named dimensions to include all simulations, ages,
#' size classes, areas, and any missing years. Can also recurse into 
#' S4 objects and lists.
#'
#' @param array An [array()] with named dimensions including at least one of
#'   `Sim`, `Year`, `Age`, or `Area`. Alternatively, an S4 or [list()] object,
#'   in which case the function recurses over all slots or elements and extends
#'   any named arrays. Any other object is returned unchanged.
#' @param nSim Integer. Total number of simulations, or `NULL` to skip.
#' @param AgeClasses Numeric vector of age classes, or `NULL` to skip.
#' @param Classes Numeric vector of size classes (length or weight bins), or
#'   `NULL` to skip. 
#' @param Years Numeric vector of `Year` values to extend to, or `NULL` to
#'   skip.
#' @param Areas Numeric vector of area indices (`1:nArea`), or `NULL` to skip.
#' @param default Default fill value for forward-filled years. If `NULL`
#'   (default), the value from the most recent existing year is used.
#' @param backfill Logical. If `TRUE`, also back-fill years earlier than the
#'   earliest existing year. Default `FALSE`.
#' @param maintain_seasonal_pattern Logical. Seasonal models only. 
#' If `TRUE` fills in missing year values by matching those from the closest
#' corresponding season (e.g., maintains seasonal recruitment pattern)
#' @param skip_data `logical(1)` Skip any [data-class] objects? Default `TRUE`
#' @param debug Logical. If `TRUE`, prints the class of each object as it is
#'   processed. Default `FALSE`.
#'   
#' @return The input object with dimensions extended as specified. The return
#'   type matches the input type (array, S4, or list).
#'
#' @details
#' 
#' ## ExtendSims
#'
#' The `Sim` dimension must have length `1` or `nSim`. If length `1`, it is
#' replicated to length `nSim` with simulation 1 values copied to all
#' simulations. If already length `nSim`, the array is returned unchanged.
#'
#' ## ExtendAges
#'
#' The `Age` dimension must have length `1` or `nAge`. If length `1`, it is
#' replicated to length `nAge` with the first age class copied to all ages.
#' If already length `nAge`, the array is returned unchanged.
#'
#' ## ExtendClasses
#'
#' The `Class` dimension must have length `1` or `nClass`. If length `1`, it
#' is replicated to length `nClass` with class 1 values copied to all classes.
#' If already length `nClass`, the array is returned unchanged.
#' 
#' ## ExtendYears
#'
#' The `Year` dimension can be any length. If all values in `Years` are already
#' present, the array is returned unchanged. Otherwise missing years are added
#' by forward-filling from the most recent existing year, back-filling from the
#' earliest (if `backfill = TRUE`), or step-filling for years within the
#' existing range. If any year values are non-integer (decimal), seasonal
#' matching is used via `.ExtendYearsSeasonal()`.
#'
#' ## ExtendAreas
#'
#' The `Area` dimension must have length `1` or `nArea`. If length `1`, it is
#' replicated to length `nArea` with area 1 values copied to all areas. If
#' already length `nArea`, the array is returned unchanged.
#'
#' @example man-examples/Extend.R
#' @seealso [ExtendSims()], [ExtendAges()], [ExtendClasses()], 
#' [ExtendYears()], [ExtendAreas()]
#' @export
Extend <- function(array,
                   nSim       = NULL,
                   AgeClasses = NULL,
                   Classes    = NULL,
                   Years      = NULL,
                   Areas      = NULL,
                   default    = NULL,
                   backfill   = FALSE,
                   maintain_seasonal_pattern = TRUE,
                   skip_data  = TRUE,
                   debug      = FALSE) {
  if (debug)
    print(class(array))
  
  if (isS4(array)) {
    if (skip_data)
      if (inherits(array, "data")) return(array)
    for (sl in slotNames(array)) {
      if (debug) print(sl)
      slot(array, sl, check = FALSE) <- Recall(slot(array, sl),
                                nSim       = nSim,
                                AgeClasses = AgeClasses,
                                Classes    = Classes,
                                Years      = Years,
                                Areas      = Areas,
                                default    = default,
                                backfill   = backfill,
                                maintain_seasonal_pattern = maintain_seasonal_pattern,
                                debug      = debug)
    }
    return(array)
  }

  if (is.list(array)) {
    if (length(array)) {
      for (i in seq_along(array)) {
        temp <- Recall(array[[i]],
                       nSim       = nSim,
                       AgeClasses = AgeClasses,
                       Classes    = Classes,
                       Years      = Years,
                       Areas      = Areas,
                       default    = default,
                       backfill   = backfill,
                       maintain_seasonal_pattern = maintain_seasonal_pattern,
                       debug      = debug)
        if (!is.null(temp)) array[[i]] <- temp
      }
    }
    return(array)
  }
  
  array |>
    ExtendSims(nSim) |>
    ExtendAges(AgeClasses) |>
    ExtendClasses(Classes) |>
    ExtendYears(Years                     = Years,
                default                   = default,
                backfill                  = backfill,
                maintain_seasonal_pattern = maintain_seasonal_pattern) |>
    ExtendAreas(Areas)
}


.ExtendDim <- function(array, dimname, target_values, match_mode = c("exact", "min"),
                       coerce_char = TRUE, allow_missing_one = FALSE) {

  match_mode <- if (length(match_mode) == 1L) match_mode else match.arg(match_mode)
  if (is.null(target_values)) return(array)

  if (isS4(array)) {
    if (inherits(array, "data")) return(array)
    for (sl in slotNames(array))
      slot(array, sl, check = FALSE) <- Recall(slot(array, sl), dimname, target_values, match_mode,
                                coerce_char, allow_missing_one)
    return(array)
  }

  if (is.list(array)) {
    if (length(array)) {
      for (i in seq_along(array)) {
        temp <- Recall(array[[i]], dimname, target_values, match_mode,
                       coerce_char, allow_missing_one)
        if (!is.null(temp)) array[[i]] <- temp
      }
    }
    return(array)
  }

  n  <- length(target_values)
  d  <- dim(array)
  dn <- dimnames(array)

  if (is.null(dn) || !dimname %in% names(dn)) return(array)

  target_dim <- which(names(dn) == dimname)
  existing    <- dn[[target_dim]]
  existing_n  <- if (coerce_char) as.numeric(existing) else existing

  already_ok <- if (match_mode == "min") length(existing) >= n else length(existing) == n
  if (already_ok) return(array)

  if (allow_missing_one && sum(target_values %in% existing_n) == n - 1) return(array)

  if (length(existing) != 1)
    cli::cli_abort(c(
      "The `{dimname}` dimension must be length 1 or `n{dimname}` ({.val {n}}).",
      "x" = "Found length {.val {d[target_dim]}}."
    ))

  idx <- lapply(seq_along(d), \(i)
                if (i == target_dim) rep(1L, n) else seq_len(d[i]))

  OutArray <- do.call(`[`, c(list(array), idx, list(drop = FALSE)))
  dimnames(OutArray)[[target_dim]] <- if (coerce_char) as.character(target_values) else target_values
  OutArray
}

#' @rdname Extend
#' @export
#'
ExtendSims <- function(array, nSim = NULL) {
  if (is.null(nSim)) return(array)
  if (length(nSim) != 1)
    cli::cli_abort("`nSim` must be an integer or numeric value of length 1")
  .ExtendDim(array, "Sim", seq_len(nSim), match_mode = "min")
}

#' @rdname Extend
#' @export
#'
ExtendAges <- function(array, AgeClasses = NULL) {
  .ExtendDim(array, "Age", AgeClasses, allow_missing_one = TRUE)
}

#' @rdname Extend
#' @export
ExtendClasses <- function(array, Classes = NULL) {
  .ExtendDim(array, "Class", Classes)
}

#' @rdname Extend
#' @export
ExtendYears <- function(array, Years = NULL, default = NULL,
                        backfill = FALSE, 
                        maintain_seasonal_pattern = TRUE) {
  
  if (!is.array(array) || is.null(Years) || !length(array)) 
    return(array)
  
  d <- dim(array)
  dn <- dimnames(array)

  if (is.null(dn) || !"Year" %in% names(dn)) 
    return(array)
  
  year_dim       <- which(names(dn) == "Year")
  nyear          <- d[year_dim]
  existing_years <- as.numeric(dn[[year_dim]])
  fill_years     <- sort(unique(Years))
  all_years      <- sort(unique(c(existing_years, fill_years)))

  if (all(fill_years %in% existing_years)) 
    return(array)
  
  fill_years    <- all_years[!all_years %in% existing_years]
  back_years    <- fill_years[fill_years < min(existing_years)]
  forward_years <- fill_years[fill_years > max(existing_years)]
  inside_years  <- fill_years[!fill_years %in% back_years & !fill_years %in% forward_years]
  
  if (!backfill) 
    all_years <- all_years[!all_years %in% back_years]
  
  # Create output array
  d[[year_dim]] <- length(all_years)
  dn[[year_dim]] <- all_years
  OutArray <- array(NA, dim = d, dimnames = dn)
  abind::afill(OutArray) <- array # add the existing values
  
  # Seasonal
  if (any(all_years %% 1 != 0))
    return(.ExtendYearsSeasonal(array, Years, default, backfill = backfill,
                                maintain_seasonal_pattern=maintain_seasonal_pattern))
  

  # Forward fill years from most recent existing year
  if (length(forward_years)) {
    MostRecent <- abind::asub(array, nyear, year_dim, drop = FALSE)
    if (!is.null(default)) MostRecent[] <- default
    abind::afill(OutArray) <- .ExtendAlongDim(MostRecent, year_dim, forward_years)
  }
  

  # Back fill years from first existing year
  if (length(back_years) && backfill) {
    FirstYear <- abind::asub(array, 1, year_dim, drop = FALSE)
    if (!is.null(default)) FirstYear[] <- default
    abind::afill(OutArray) <- .ExtendAlongDim(FirstYear, year_dim, back_years)
  }
  
  # Fill years within existing years
  if (length(inside_years)) {
    interval_index <- findInterval(inside_years, existing_years)
    interval_index[interval_index == 0] <- 1
    interval_index[interval_index > length(existing_years) - 1] <- length(existing_years) - 1
    TimeBlocks <- split(inside_years, interval_index)
    
    for (i in seq_along(TimeBlocks)) {
      year_ind  <- max(which(existing_years < min(TimeBlocks[[i]])))
      FillValue <- abind::asub(array, year_ind, year_dim, drop = FALSE)
      abind::afill(OutArray) <- .ExtendAlongDim(FillValue, year_dim, TimeBlocks[[i]])
    }
  }
  OutArray
}

.NoSeasonVals <- function(x) {
  all(abs(x - round(x)) < .Machine$double.eps^0.5)
}

.ExtendYearsSeasonal <- function(array, Years = NULL, default = NULL, backfill = FALSE, 
                                 maintain_seasonal_pattern=maintain_seasonal_pattern, tol = 0.01) {
  
  if (!is.array(array)) 
    cli::cli_abort("`array` must be an array")
  
  if (is.null(Years)) 
    return(array)
  
  d <- dim(array)
  dn <- dimnames(array)

  if (is.null(dn) || !"Year" %in% names(dn)) 
    cli::cli_abort("`array` must have a dimension named 'Year'")
  

  year_dim <- which(names(dn) == "Year")
  nyear <- d[year_dim]
  existing_years <- as.numeric(dn[[year_dim]])
  fill_years <- unique(Years) |> sort()
  all_years <- c(existing_years, fill_years) |>
    unique() |>
    sort()

  if (all(fill_years %in% existing_years)) 
    return(array)
  
  fill_years <- all_years[!all_years %in% existing_years]
  back_years <- fill_years[which(fill_years < min(existing_years))]
  forward_years <- fill_years[which(fill_years > max(existing_years))]
  inside_years <- fill_years[!fill_years %in% back_years & !fill_years %in% forward_years]
  
  if (!backfill) 
    all_years <- all_years[!all_years %in% back_years]
  
  # Create output array
  d[[year_dim]] <- length(all_years)
  dn[[year_dim]] <- all_years
  OutArray <- array(NA, dim = d, dimnames = dn)
  abind::afill(OutArray) <- array # add the existing values
  
  season_existing <- existing_years %% 1
  # Forward fill years from most recent existing year
  if (length(forward_years)) {
    season_forward <- (forward_years %% 1) |> unique()
    if (.NoSeasonVals(season_existing) || !maintain_seasonal_pattern) {
      # no seasons in provided values - constant over seasons within years
      most_recent_ind <- nyear
      MostRecent <- abind::asub(array, most_recent_ind, year_dim, drop = FALSE)
      if (!is.null(default)) {
        MostRecent[] <- default
      }
      
      Extended <- .ExtendAlongDim(
        x = MostRecent,
        along_dim = year_dim,
        new_index = forward_years
      )
      abind::afill(OutArray) <- Extended
      
    } else {
      # loop over seasons - match the season
      for (i in seq_along(season_forward)) {
        season_ind <- which(abs(forward_years %% 1 - season_forward[i]) < tol)
        most_recent_ind <- which(abs(season_existing - season_forward[i]) < tol)
        if (length(existing_years) == 1) {
          most_recent_ind <- 1
        }
        
        .CheckSeasonExists(most_recent_ind, season_forward[i], existing_years, season_existing)
        most_recent_ind <- max(most_recent_ind)
        
        MostRecent <- abind::asub(array, most_recent_ind, year_dim, drop = FALSE)
      
        if (!is.null(default)) {
          MostRecent[] <- default
        }
        
        Extended <- .ExtendAlongDim(
          x = MostRecent,
          along_dim = year_dim,
          new_index = forward_years[season_ind]
        )
        abind::afill(OutArray) <- Extended
        
      }
    }
  }

  # Back fill years from first existing year
  if (length(back_years) && backfill) {
    season_backward <- (back_years %% 1) |> unique()
    if (.NoSeasonVals(season_existing) || maintain_seasonal_pattern) {
      # no seasons in provided values - constant over seasons within years
      MostRecent <- abind::asub(array, 1, year_dim, drop = FALSE)
      if (!is.null(default)) {
        MostRecent[] <- default
      }
      
      Extended <- .ExtendAlongDim(
        x = MostRecent,
        along_dim = year_dim,
        new_index = back_years
      )
      abind::afill(OutArray) <- Extended

      
    } else {
      # loop over seasons - match the season
      for (i in seq_along(season_backward)) {
        season_ind <- which(abs(back_years %% 1 - season_backward[i]) < tol)
        most_recent_ind <- which(abs(season_existing - season_backward[i]) < tol)
        if (length(existing_years) == 1) {
          most_recent_ind <- 1
        }
        .CheckSeasonExists(most_recent_ind, season_backward[i], existing_years, season_existing)
        most_recent_ind <- min(most_recent_ind)
        
        MostRecent <- abind::asub(array, most_recent_ind, year_dim, drop = FALSE)
        
        Extended <- .ExtendAlongDim(
          x = MostRecent,
          along_dim = year_dim,
          new_index = back_years[season_ind]
        )
        abind::afill(OutArray) <- Extended

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

      if (.NoSeasonVals(season_existing) || maintain_seasonal_pattern) {
        # no seasons in provided values - constant over seasons within years
        most_recent_ind <- which(existing_years <  min(years_block)) |> max()
        MostRecent <- abind::asub(array, most_recent_ind, year_dim, drop = FALSE)
        if (!is.null(default)) {
          MostRecent[] <- default
        }
        d <- dim(MostRecent)
        d[[year_dim]] <- length(years_block)
        dn[[year_dim]] <- years_block
        abind::afill(OutArray) <- array(MostRecent, dim = d, dimnames = dn)
        
      } else {
        for (j in seq_along(season_inside)) {
          season_ind <- which(abs(years_block %% 1 - season_inside[j]) < tol)
          most_recent_ind <- which(abs(season_existing - season_inside[i]) < tol)
          if (length(existing_years) == 1) 
            most_recent_ind <- 1
          
          if (!length(most_recent_ind))
            most_recent_ind <- 1
          
          .CheckSeasonExists(most_recent_ind, season_inside[i], existing_years, season_existing)
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

.CheckSeasonExists <- function(most_recent_ind, try_season, existing_years, season_existing) {
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
  .ExtendDim(array, "Area", Areas)
}

.ExtendFleets <- function(array, Fleets = NULL) {
  .ExtendDim(array, "Fleet", Fleets, coerce_char = FALSE)
}

.ExtendStocks <- function(array, Stocks = NULL) {
  .ExtendDim(array, "Stock", Stocks, coerce_char = FALSE)
}




.ExtendAlongDim <- function(x, along_dim, new_index, dimnames_list = dimnames(x)) {
  
  # Permute so target dimension is first
  perm <- seq_along(dim(x))
  perm <- c(along_dim, perm[-along_dim])
  
  x_perm <- .Aperm(x, perm)
  dx_perm <- dim(x_perm)
  
  # Build index list for ND subset
  idx <- vector("list", length(dx_perm))
  idx[[1]] <- rep(seq_len(dx_perm[1]), length(new_index))
  for (i in 2:length(dx_perm)) {
    idx[[i]] <- seq_len(dx_perm[i])
  }
  
  x_rep <- do.call(`[`, c(list(x_perm), idx, list(drop = FALSE)))
  
  # Update dimension
  dim(x_rep)[1] <- length(new_index)
  
  # Permute back
  inv_perm <- order(perm)
  out <- .Aperm(x_rep, inv_perm)
  
  # Fix dimnames
  if (!is.null(dimnames_list)) {
    dimnames_list[[along_dim]] <- new_index
    dimnames(out) <- dimnames_list
  }
  
  out
}
