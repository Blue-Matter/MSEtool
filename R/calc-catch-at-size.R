
.NeedsCatchAtSize <- function(OM, control) {
  n_stock  <- nStock(OM)
  override <- control$CalcCatchAtSize %||NA% NA

  if (!is.na(override))
    return(rep(override, n_stock))

  needed    <- rep(FALSE, n_stock)
  complexes <- Complexes(OM)

  for (i in seq_along(complexes)) {
    stocks <- complexes[[i]]

    needs_gen <- isTRUE(control$GenerateData) && i <= length(OM@Obs) &&
      any(vapply(c('LandingsAtSize', 'DiscardsAtSize'), \(type) {
        !all(vapply(OM@Obs[[i]], \(o) isNewObject(slot(o, type)), logical(1)))
      }, logical(1)))

    needs_cond <- isTRUE(control$ConditionObs) && i <= length(OM@Data) &&
      any(vapply(c('LandingsAtSize', 'DiscardsAtSize'), \(type) {
        !EmptyObject(slot(OM@Data[[i]], type))
      }, logical(1)))

    needed[stocks] <- needs_gen || needs_cond
  }

  needed
}

.CalcCatchAtSize <- function(Hist, Years = NULL, sel_mode = c("length", "age"),
                             needed = NULL, useCpp = TRUE) {

  sel_mode  <- match.arg(sel_mode) # hard coded to `length` for now

  if (is.null(Years)) Years <- Years(Hist, 'H')

  n_stock <- nStock(Hist)
  n_fleet <- nFleet(Hist)

  if (is.null(needed)) needed <- rep(TRUE, n_stock)
  
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
  }) |> .SubsetYear(Years = Years)
  
  # Extract selectivity-at-size per fleet per stock 
  # MeanAtLength / MeanAtWeight: Sim x Class x Year x Area
  sel_size_vectors <- purrr::map2(Hist@OM@Fleet,size_type,  \(stock_fleet, type) {
    purrr::map(stock_fleet, \(fleet) {
      if (type == 'length') return(fleet@Selectivity@MeanAtLength)
      fleet@Selectivity@MeanAtWeight
    })
  }) |> .SubsetYear(Years = Years)
  
  length_object_vectors <- purrr::map(Hist@OM@Stock, \(stock) stock@Length)

  key_area_defaults <- purrr::map2(age_size_keys, sel_size_vectors, \(key, sel_size_stock) {
    if (length(sel_size_stock)) {
      area_vals <- as.numeric(dimnames(sel_size_stock[[1]])$Area)
      ExtendAreas(AddDimension(key, 'Area'), Areas = area_vals)
    } else {
      AddDimension(key, 'Area')
    }
  })

  # Loop over stocks and fleets
  for (st in seq_len(n_stock)) {
    
    if (!ask_exists[st] || !needed[st]) next
    
    # F arrays: Sim x Age x Year x Fleet x Area
    f_interact <- Hist@FInteractArea[[st]] |> .SubsetYear(Years = Years)
    f_dead     <- Hist@FDeadArea[[st]] |> .SubsetYear(Years = Years)
    f_retain   <- Hist@FRetainArea[[st]] |> .SubsetYear(Years = Years)
    
    # Natural mortality: Sim x Age x Year
    nat_mort <- Hist@OM@Stock[[st]]@NaturalMortality@MeanAtAge |>
      .SubsetYear(Years = Years) |>
      AddDimension('Area')
    
    # Numbers at age: Sim x Age x Year x Area
    naa <- Hist@Number[[st]] |> .SubsetYear(Years = Years)
    
    # Derive Z and N_dead per sim, age, year, area
    f_dead_total <- SumOverFleet(f_dead)           # Sim x Age x Year x Area
    Z            <- ArraySum(nat_mort, f_dead_total)
    N_dead       <- ArrayMultiply(naa, 1 - exp(-Z))

    # F-ratios (proportion of total mortality attributable to this fleet)
    Z_safe <- Z
    Z_safe[Z_safe == 0] <- .Machine$double.eps

    key_area_default  <- key_area_defaults[[st]]
    length_object      <- length_object_vectors[[st]]
    recalced_key_cache <- list() # keyed by sel_class signature, shared across fleets

    for (fl in seq_len(n_fleet)) {

      # F slices for this fleet: Sim x Age x Year x Area
      fd_fl <- f_dead[, , , fl, , drop = FALSE] |> DropDimension('Fleet')
      fr_fl <- f_retain[, , , fl, , drop = FALSE] |> DropDimension('Fleet')

      fr_ratio  <- ArrayDivide(fr_fl, Z_safe)
      fd_ratio <- ArrayDivide(pmax(ArraySubtract(fd_fl, fr_fl), 0), Z_safe)

      # Sim x Age x Year x Area — numbers dying from landings/discards per age
      landings_N <- ArrayMultiply(fr_ratio, N_dead)
      discards_N <- ArrayMultiply(fd_ratio, N_dead)

      selectivity <- sel_size_vectors[[st]][[fl]]
      sm          <- sel_mode_sf[[st]][[fl]]

      # Age-Size Key for this fleet: recalculate only if the fleet's
      # selectivity is defined on different size classes to the stock's key
      key_class <- as.numeric(dimnames(key_area_default)$Class)
      sel_class <- as.numeric(dimnames(selectivity)$Class)

      recalc_key <- length(key_class) != length(sel_class) ||
        any(key_class != sel_class)

      key_area <- if (!recalc_key) {
        key_area_default
      } else {
        cache_id <- paste(sel_class, collapse = ",")
        if (is.null(recalced_key_cache[[cache_id]])) {
          recalced <- CalcAgeSizeKey(MeanAtAge = length_object@MeanAtAge,
                                     CVatAge   = length_object@CVatAge,
                                     Classes   = sel_class,
                                     TruncSD   = length_object@TruncSD,
                                     Dist      = length_object@Dist) |>
            .SubsetYear(Years = Years)
          area_vals <- as.numeric(dimnames(key_area_default)$Area)
          recalced_key_cache[[cache_id]] <- ExtendAreas(AddDimension(recalced, 'Area'),
                                                        Areas = area_vals)
        }
        recalced_key_cache[[cache_id]]
      }

      res <- .CalcCatchAtSizeFleet(key_area, selectivity, sel_mode = sm,
                                   landings_N = landings_N, discards_N = discards_N,
                                   useCpp = useCpp)

      ArrayFill(Hist@LandingsAtSize[[st]][[fl]]) <- res$LAS
      ArrayFill(Hist@DiscardsAtSize[[st]][[fl]]) <- res$DAS
    }
  }
  Hist
}
