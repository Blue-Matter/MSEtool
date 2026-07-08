#' Generate Historical Age Composition Data
#'
#' Internal function to generate simulated historical age composition data
#' (landings or discards) when real observations are not available.
#'
#' @param x Integer index of the simulation replicate.
#' @param Data Existing `data` object from `OM@Data`.
#' @param Hist A `hist` class object.
#' @param HistYears Numeric vector of historical years.
#' @param i Integer index of the stock complex.
#' @param stocks Integer vector of stock indices in the complex.
#' @param FleetNames Character vector of fleet names.
#' @param type Character. Either `"LandingsAtAge"` or `"DiscardsAtAge"`.
#'
#' @details
#'
#' ## Early Exit Conditions
#'
#' The function returns `slot(Data, type)` unchanged in two cases:
#'
#' - **Real data exist**: `slot(Data, type)` is already populated
#'   (`!EmptyObject(slot(Data, type))`).
#' - **No observation structure defined**: every fleet's [CompObs()] object
#'   is a default (unconditioned) object, as determined by `isNewObject()`.
#'
#' ## True Composition
#'
#' True catch-at-age is extracted from `Hist@LandingsAtAge` or
#' `Hist@DiscardsAtAge` (selected by `type`) for replicate `x`, summed over
#' areas and stocks within the complex, retaining the fleet and age dimensions.
#' The result is normalised to proportions \eqn{\mathbf{q}} within each year
#' and fleet.
#'
#' ## Composition Simulation Model
#'
#' For each fleet and year where `SampleSize > 0`, the Dirichlet concentration
#' vector is constructed as:
#'
#' \deqn{\alpha_b = \mathrm{ESS} \cdot \Theta \cdot q_b \cdot \exp(\mathrm{Shift}_b)}
#'
#' A Dirichlet-Multinomial sample of size `SampleSize` is then drawn via
#' [rDirichletMultinomial()]. Years where `SampleSize` is zero or `NA` are
#' left as `NA` in the output.
#'
#' The slot defaults are:
#'
#' - `ESS`: falls back to `SampleSize` if `NULL`
#' - `Theta`: defaults to `1` if `NULL`, recovering a near-multinomial draw
#' - `Shift`: defaults to zero for all bins if `NULL`, applying no tilt
#'
#' ## Obs Structure
#'
#' Observation parameters are accessed via:
#'
#' ```r
#' Hist@OM@Obs[[i]][[fl]]@LandingsAtAge  # or @DiscardsAtAge
#' ```
#'
#' where `i` is the stock complex index and `fl` the fleet index. The relevant
#' [CompObs()] slots are:
#'
#' - `@SampleSize[sim, t]`: nominal sample size; output counts sum to this value
#' - `@ESS[sim, t]`: effective sample size scaling the concentration vector
#' - `@Theta[sim, t]`: Dirichlet-Multinomial dispersion parameter
#' - `@Shift[sim, t, bin]`: per-bin log-concentration offset
#'
#' See [obs-class] and [CompObs()] for full slot documentation.
#'
#' @return A [compdata-class] object with:
#'
#' - `@Name`: character vector of fleet names
#' - `@Value`: `[nYear x nFleet x nAge]` array of composition counts
#' - `@Classes`: numeric vector of age classes
#' - `@Units`: `"years"`
#'
#' @seealso [CompObs()], [CompData()], [compdata-class], [obs-class],
#'   [rDirichletMultinomial()], [GenHistData_Catch()]
#' @keywords internal
GenHistData_AgeComp <- function(x, Data, Hist, HistYears, i, stocks, FleetNames,
                                type = c('LandingsAtAge', 'DiscardsAtAge')) {
  
  type <- match.arg(type, c('LandingsAtAge', 'DiscardsAtAge'))
  
  # Return real data unchanged if already populated
  if (!EmptyObject(slot(Data, type)))
    return(slot(Data, type))
  
  AllObs <- lapply(Hist@OM@Obs[[i]], slot, type)
  no_obs <- all(unlist(lapply(AllObs, isNewObject)))
  
  # No Obs specified for any fleet — skip simulation
  if (no_obs)
    return(slot(Data, type))
  
  nTS    <- length(HistYears)
  nFleet <- length(FleetNames)
  
  # Aggregate true catch-at-age over stocks and areas: [nAge x nYear x nFleet]
  CatchAtAge <- purrr::map(slot(Hist, type)[stocks], \(catch_n) {
    sim_x <- min(x, dim(catch_n)[1])
    catch_n[sim_x,,,,,drop=FALSE] |>
      SumOverArea() |>
      DropDimension('Sim')
  })
  
  ageclasses <- purrr::map(CatchAtAge, \(st) as.numeric(dimnames(st)$Age))
 
  if (length(CatchAtAge)>1 && ! all(duplicated(ageclasses)[-1])) {
    CatchAtAge <- align_age_dim(CatchAtAge)
  }
  CatchAtAge <- CatchAtAge |> List2Array('Stock') |> SumOverStock()
  
  CatchAtAge <- SubsetYear(CatchAtAge, HistYears)
  
  AgeClasses <- dimnames(CatchAtAge)$Age |> as.numeric()
  nAge       <- length(AgeClasses)
  
  Value <- array(0,
                 dim      = c(nTS, nFleet, nAge),
                 dimnames = list(Year  = HistYears,
                                 Fleet = FleetNames,
                                 Age   = AgeClasses))
  
  for (fl in seq_len(nFleet)) {
    CompObs <- slot(Hist@OM@Obs[[i]][[fl]], type)
    if (EmptyObject(CompObs) || is.null(CompObs@SampleSize))
      next()
    
    sim_ss     <- min(x, nrow(CompObs@SampleSize))
    SampleSize <- SubsetYear(CompObs@SampleSize, HistYears)[sim_ss, ]
    
    ESS <- if (!is.null(CompObs@ESS)) {
      sim_ess <- min(x, nrow(CompObs@ESS))
      SubsetYear(CompObs@ESS, HistYears)[sim_ess, ]
    } else {
      SampleSize
    }
    
    Theta <- if (!is.null(CompObs@Theta)) {
      sim_th <- min(x, nrow(CompObs@Theta))
      SubsetYear(CompObs@Theta, HistYears)[sim_th, ]
    } else {
      rep(1, nTS)
    }
    
    hasShift <- !is.null(CompObs@Shift)
    if (hasShift) {
      sim_sh <- min(x, dim(CompObs@Shift)[1])
      Shift  <- SubsetYear(CompObs@Shift, HistYears)[sim_sh,,]  # [nYear x nBin]
    }
    
    for (yr in seq_len(nTS)) {
      ss  <- SampleSize[yr]
      ess <- ESS[yr]
      th  <- Theta[yr]
      
      if (is.na(ss) || ss == 0) next()
      
      true_n  <- CatchAtAge[, yr, fl]
      total_n <- sum(true_n, na.rm = TRUE)
      if (is.na(total_n) || total_n == 0) next()
      
      q <- true_n / total_n
      
      shift_b <- if (hasShift) Shift[yr, ] else rep(0, nAge)
      alpha   <- ess * th * q * exp(shift_b)
      
      # Guard against zero/negative alpha
      if (any(is.na(alpha)) || sum(alpha) == 0) next()
      
      Value[yr, fl, ] <- rDirichletMultinomial(n = round(ss), alpha = alpha)
    }
  }
  
  CompData         <- new('compdata')
  CompData@Name    <- FleetNames
  CompData@Value   <- Value
  CompData@Classes <- AgeClasses
  CompData@Units   <- 'years'
  CompData
}

align_age_dim <- function(arr_list, dim_name = "Age") {
  
  all_ages <- sort(unique(as.numeric(unlist(
    lapply(arr_list, function(a) dimnames(a)[[dim_name]])
  ))))
  all_ages_chr <- as.character(all_ages)
  
  lapply(arr_list, function(a) {
    dn <- dimnames(a)
    dim_idx <- which(names(dn) == dim_name)
    new_dim <- dim(a)
    new_dim[dim_idx] <- length(all_ages_chr)
    
    new_dn <- dn
    new_dn[[dim_idx]] <- all_ages_chr
    new_arr <- array(0, dim = new_dim, dimnames = new_dn)
    
    idx <- vector("list", length(dim(a)))
    for (i in seq_along(idx)) idx[[i]] <- TRUE  
    idx[[dim_idx]] <- dn[[dim_idx]] 
    do.call(`[<-`, c(list(new_arr), idx, list(value = a)))
  })
}
