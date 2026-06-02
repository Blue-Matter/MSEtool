#' Calculate Catch-at-Size for Landings and Discards
#'
#' Computes landings-at-size and discards-at-size for each stock and fleet by
#' projecting numbers-at-age through a selectivity-conditioned Age-Size Key.
#' Supports both length- and weight-based size compositions, determined
#' automatically per stock by the availability of an AWK. Under
#' `sel_mode = "length"`, the Age-Size Key is reweighted by size-selectivity
#' within each age class before projection, correctly capturing the effect of
#' length-based selectivity on the size composition of catch. Under
#' `sel_mode = "age"`, the population Age-Size Key is used unchanged.
#'
#' @param Hist A `Hist` object.
#'   
#' @param Years Numeric vector of years to compute catch-at-size for. If
#'   `NULL` (default), all historical years in `Hist` are used.
#'   
#' @param sel_mode Character. One of `"length"` (default) or `"age"`. Controls
#'   whether the Age-Size Key is conditioned on size-selectivity before
#'   projection. See [ConditionAgeSizeKey()] for details.
#'
#' @return `Hist` with `Hist@LandingsAtSize[[stock]][[fleet]]` and
#'   `Hist@DiscardsAtSize[[stock]][[fleet]]` populated for the requested years.
#'   Each array has dimensions `Sim x Class x Year x Area`.
#'
#' @seealso [ConditionAgeSizeKey()]
#' @keywords internal
CalcCatchAtSize <- function(Hist, Years = NULL, sel_mode = c("length", "age")) {
  
  sel_mode  <- match.arg(sel_mode) # hard coded to `length` for now
  
  if (is.null(Years)) Years <- Years(Hist, 'H')
  
  n_stock <- nStock(Hist)
  n_fleet <- nFleet(Hist)
  
  # Determine size type (length or weight) per stock
  # Use AWK if available, otherwise fall back to ALK
  awk_exists <- purrr::map_lgl(Hist@OM@Stock, \(stock) 
                               !is.null(stock@Weight@AWK))
  size_type  <- rep('length', n_stock)
  size_type[awk_exists] <- 'weight'
  
  sel_mode_sf <- purrr::map2(Hist@OM@Fleet, size_type, \(stock_fleet, type) {
    purrr::map(stock_fleet, \(fleet) {
      has_sel_at_size <- if (type == 'length') {
        !is.null(fleet@Selectivity@MeanAtLength)
      } else {
        !is.null(fleet@Selectivity@MeanAtWeight)
      }
      if (!has_sel_at_size) {
        if (sel_mode == 'length')
          # cli::cli_warn("Selectivity-at-size not available for this fleet; 
          #                  defaulting to `sel_mode = 'age'`")
        return('age')
      }
      sel_mode
    })
  })
  
  ask_exists <- purrr::map2_lgl(Hist@OM@Stock, size_type, \(stock, type) {
    if (type == 'length') return(!is.null(stock@Length@ALK))
    !is.null(stock@Weight@AWK)
  }) 
  
  # Extract age-size keys per stock 
  # ALK: Sim x Age x Class x Year  (length bins)
  # AWK: Sim x Age x Class x Year  (weight bins)
  age_size_keys <- purrr::map2(Hist@OM@Stock, size_type, \(stock, type) {
    if (type == 'length') return(stock@Length@ALK)
    stock@Weight@AWK
  }) |> SubsetYear(Years = Years)
  
  # Extract selectivity-at-size per fleet per stock 
  # MeanAtLength / MeanAtWeight: Sim x Class x Year x Area
  sel_size_vectors <- purrr::map2(Hist@OM@Fleet,size_type,  \(stock_fleet, type) {
    purrr::map(stock_fleet, \(fleet) {
      if (type == 'length') return(fleet@Selectivity@MeanAtLength)
      fleet@Selectivity@MeanAtWeight
    })
  }) |> SubsetYear(Years = Years)
  
  length_object_vectors <- purrr::map(Hist@OM@Stock, \(stock) stock@Length)
  
  # Condition age-size keys on size selectivity 
  # conditioned_keys[[stock]][[fleet]]: Sim x Age x Class x Year x Area
  conditioned_keys <- purrr::pmap(
    list(age_size_keys, sel_size_vectors, sel_mode_sf, length_object_vectors),
    \(key, sel_size_stock, sel_mode_stock, length_object) {
      purrr::map2(sel_size_stock, sel_mode_stock, \(selectivity, sm) {
        
        # check classes
        key_class <- as.numeric(dimnames(key)$Class)
        sel_class <- as.numeric(dimnames(selectivity)$Class)
        
        recalc_key <- FALSE
        
        if (length(key_class) != length(sel_class)) {
          recalc_key <- TRUE
        } else if (any(key_class != sel_class)) {
          recalc_key <- TRUE
        }
        
        if (recalc_key) {
          key <- CalcAgeSizeKey(MeanAtAge = length_object@MeanAtAge,
                                CVatAge   =length_object@CVatAge,
                                Classes   = sel_class,
                                TruncSD   = length_object@TruncSD,
                                Dist      = length_object@Dist)
        }
        ConditionAgeSizeKey(key, selectivity, sel_mode = sm)
      })
    })
  
  # Loop over stocks and fleets
  for (st in seq_len(n_stock)) {
    
    if (!ask_exists[st]) next
    
    # F arrays: Sim x Age x Year x Fleet x Area
    f_interact <- Hist@FInteractArea[[st]] |> SubsetYear(Years = Years)
    f_dead     <- Hist@FDeadArea[[st]] |> SubsetYear(Years = Years)
    f_retain   <- Hist@FRetainArea[[st]] |> SubsetYear(Years = Years)
    
    # Natural mortality: Sim x Age x Year
    nat_mort <- Hist@OM@Stock[[st]]@NaturalMortality@MeanAtAge |>
      SubsetYear(Years = Years) |>
      AddDimension('Area')
    
    # Numbers at age: Sim x Age x Year x Area
    naa <- Hist@Number[[st]] |> SubsetYear(Years = Years)
    
    # Derive Z and N_dead per sim, age, year, area
    f_dead_total <- SumOverFleet(f_dead)           # Sim x Age x Year x Area
    Z            <- ArraySum(nat_mort, f_dead_total)
    N_dead       <- ArrayMultiply(naa, 1 - exp(-Z))
    
    for (fl in seq_len(n_fleet)) {
      
      # F slices for this fleet: Sim x Age x Year x Area
      fd_fl <- f_dead[, , , fl, , drop = FALSE] |> DropDimension('Fleet')
      fr_fl <- f_retain[, , , fl, , drop = FALSE] |> DropDimension('Fleet')
      
      # F-ratios (proportion of total mortality attributable to this fleet)
      Z_safe    <- Z
      Z_safe[Z_safe == 0] <- .Machine$double.eps
      
      fr_ratio  <- ArrayDivide(fr_fl, Z_safe)
      fd_ratio <- ArrayDivide(pmax(ArraySubtract(fd_fl, fr_fl), 0), Z_safe)
      
      # Conditioned key for this stock x fleet: Sim x Age x Class x Year x Area
      cond_key <- conditioned_keys[[st]][[fl]]
      
      # Sim x Age x Year x Area — numbers dying from landings/discards per age
      landings_N <- ArrayMultiply(fr_ratio, N_dead)
      discards_N <- ArrayMultiply(fd_ratio, N_dead)
      
      classes <- as.numeric(dimnames(cond_key)$Class)
      
      landings_N_exp <- AddDimension(landings_N, 'Class', pos = 3, val = classes[1])
      discards_N_exp <- AddDimension(discards_N, 'Class', pos = 3, val = classes[1])
      
      LAS <- SumOverAge(ArrayMultiply(cond_key, landings_N_exp))
      DAS <- SumOverAge(ArrayMultiply(cond_key, discards_N_exp))
      
      ArrayFill(Hist@LandingsAtSize[[st]][[fl]]) <- LAS
      ArrayFill(Hist@DiscardsAtSize[[st]][[fl]]) <- DAS
    }
  }
  Hist
}

