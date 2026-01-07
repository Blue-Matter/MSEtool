

#' Extend an array to include missing year values
#' 
#' Values are forward- or back-filled 
#' 
#' Accounts for seasonality
#' 
#' @param array An array with named dimensions, one of which is `Year`
#' @param Years A numeric vector of `Year` values
#' @param default Default value for forward projected years
#' 
#' @export
#' 
#' @example man-examples/ExtendYears.R
#' 

ExtendYears <- function(array, Years=NULL, default=NULL) {
  
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
  all_years <- c(existing_years, fill_years) |> unique() |> sort()

  if (all(fill_years %in% existing_years)) {
    return(array)
  }
  
  # Create output array
  d[[year_dim]] <- length(all_years)
  dn[[year_dim]] <- all_years
  OutArray <- array(NA, dim=d, dimnames=dn)
  abind::afill(OutArray) <- array # add the existing values
  
  fill_years <- all_years[!all_years %in% existing_years]
  back_years <- fill_years[which(fill_years < min(existing_years))]
  forward_years <- fill_years[which(fill_years > max(existing_years))]
  inside_years <- fill_years[!fill_years %in% back_years & !fill_years %in% forward_years]
 
  # Seasonal
  isSeasonal <- which(all_years - round(all_years,0)>0) |> length()
  
  if (isSeasonal) {
    return(ExtendYears_seasonal(array, Years, default=NULL))
  }
  
  
  # Forward fill years from most recent existing year
  if (length(forward_years)) {
    MostRecent <- abind::asub(array, nyear, year_dim, drop=FALSE)
    if (!is.null(default)) {
      MostRecent[] <- default
    }
    d <- dim(MostRecent)
    d[[year_dim]] <- length(forward_years)
    dn[[year_dim]] <- forward_years
    abind::afill(OutArray) <- array(MostRecent, dim=d, dimnames=dn)
  }

  
  # Back fill years from first existing year
  if (length(back_years)) {
    FirstYear <- abind::asub(array, 1, year_dim, drop=FALSE)
    d <- dim(FirstYear)
    d[[year_dim]] <- length(back_years)
    dn[[year_dim]] <- back_years
    abind::afill(OutArray) <- array(FirstYear, dim=d, dimnames=dn)
  }
  
  # Fill years within existing years
  if (length(inside_years)) {
    
    interval_index <- findInterval(inside_years, existing_years)
    interval_index[interval_index == 0] <- 1
    interval_index[interval_index > length(existing_years) - 1] <- length(existing_years) - 1
    TimeBlocks <-  split(inside_years, interval_index)
    
    for (i in seq_along(TimeBlocks)) {
      year_ind <- which(existing_years < min(TimeBlocks[[i]])) |> max()
      FillValue <- abind::asub(array, year_ind, year_dim, drop=FALSE)
      d <- dim(FillValue)
      d[[year_dim]] <- length(TimeBlocks[[i]])
      dn[[year_dim]] <- TimeBlocks[[i]]
      abind::afill(OutArray) <- array(FillValue, dim=d, dimnames=dn)
    }
  }
  OutArray
}

ExtendYears_seasonal <- function(array, Years=NULL, default=NULL, tol=0.01) {
  
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
  all_years <- c(existing_years, fill_years) |> unique() |> sort()
  
  if (all(fill_years %in% existing_years)) {
    return(array)
  }
  
  # Create output array
  d[[year_dim]] <- length(all_years)
  dn[[year_dim]] <- all_years
  OutArray <- array(NA, dim=d, dimnames=dn)
  abind::afill(OutArray) <- array # add the existing values
  
  fill_years <- all_years[!all_years %in% existing_years]
  back_years <- fill_years[which(fill_years < min(existing_years))]
  forward_years <- fill_years[which(fill_years > max(existing_years))]
  inside_years <- fill_years[!fill_years %in% back_years & !fill_years %in% forward_years]
  
  season_existing <- existing_years %% 1
  # Forward fill years from most recent existing year
  if (length(forward_years)) {
    season_forward <- (forward_years %% 1) |> unique() 
 
    # loop over seasons - match the season
    for (i in seq_along(season_forward)) {
      season_ind <- which(abs(forward_years %% 1 - season_forward[i]) < tol)
      most_recent_ind <- which(abs(season_existing - season_forward[i]) < tol) 
      CheckSeasonExists(most_recent_ind, season_forward[i], existing_years, season_existing)
      most_recent_ind <- max(most_recent_ind)
      
      MostRecent <- abind::asub(array, most_recent_ind, year_dim, drop=FALSE)
      if (!is.null(default)) {
        MostRecent[] <- default
      }
      d <- dim(MostRecent)
      d[[year_dim]] <- length(forward_years[season_ind])
      dn[[year_dim]] <- forward_years[season_ind]
      abind::afill(OutArray) <- array(MostRecent, dim=d, dimnames=dn)
    }
  }
  
  # Back fill years from first existing year
  if (length(back_years)) {
    season_backward <- (back_years %% 1) |> unique() 
    
    # loop over seasons - match the season
    for (i in seq_along(season_backward)) {
      season_ind <- which(abs(back_years %% 1 - season_backward[i]) < tol)
      most_recent_ind <- which(abs(season_existing - season_backward[i]) < tol) 
      CheckSeasonExists(most_recent_ind, season_backward[i], existing_years, season_existing)
      most_recent_ind <- min(most_recent_ind)
      
      MostRecent <- abind::asub(array, most_recent_ind, year_dim, drop=FALSE)
      d <- dim(MostRecent)
      d[[year_dim]] <- length(back_years[season_ind])
      dn[[year_dim]] <- back_years[season_ind]
      abind::afill(OutArray) <- array(MostRecent, dim=d, dimnames=dn)
    }
  }
  
  # Fill years within existing years
  if (length(inside_years)) {
    
    interval_index <- findInterval(inside_years, existing_years)
    interval_index[interval_index == 0] <- 1
    interval_index[interval_index > length(existing_years) - 1] <- length(existing_years) - 1
    TimeBlocks <-  split(inside_years, interval_index)
    
    for (i in seq_along(TimeBlocks)) {
      years_block <- TimeBlocks[[i]]
      season_inside <- (years_block %% 1) |> unique()
      
      for (j in seq_along(season_inside)) {
        season_ind <- which(abs(years_block %% 1 - season_inside[j]) < tol)
        most_recent_ind <- which(abs(season_existing - season_inside[i]) < tol) 
        CheckSeasonExists(most_recent_ind, season_inside[i], existing_years, season_existing)
        most_recent_ind <- min(most_recent_ind)
        
        MostRecent <- abind::asub(array, most_recent_ind, year_dim, drop=FALSE)
        d <- dim(MostRecent)
        d[[year_dim]] <- length(years_block[season_ind])
        dn[[year_dim]] <- years_block[season_ind]
        abind::afill(OutArray) <- array(MostRecent, dim=d, dimnames=dn)
      }
    }
  }
  OutArray
}

CheckSeasonExists <- function(most_recent_ind, try_season, existing_years, season_existing) {
  if (any(!is.finite(most_recent_ind)) || !length(most_recent_ind)) {
    cli::cli_abort(c("x"="Could not match Season {.val {try_season[i]}} with an existing season",
                     'i'= 'Existing Years: {.val {existing_years}}',
                     'i'= 'Existing Seasons: {.val {season_existing}}'))
    
  }
}




