
# TODO
# - Combine Data
# - Combine Obs

#' Combine Multiple Fleets into a Single Fleet
#'
#' Combines several fleets into a new aggregated fleet within an Operating
#' Model, preserving the overall fishery dynamics that would result from the
#' disaggregated fleets.
#'
#' @param OM An [OM()] object.
#' @param FleetList A named list of character vectors. Each element names the
#'   fleets to merge, and the element's name becomes the name of the new
#'   combined fleet. Multiple entries combine multiple sets of fleets
#'   simultaneously.
#' @param silent `logical(1)`. If `TRUE`, suppresses informational console
#'   messages. Defaults to `FALSE`.
#'
#' @return An updated [OM()] object in which each set of fleets named in
#'   `FleetList` has been replaced by a single aggregated fleet across all
#'   stocks.
#'
#' For each entry in `FleetList`, the first named fleet is replaced in-place
#' by the combined fleet and the remaining fleets are dropped. Combination
#' preserves aggregate fishing mortality, selectivity, retention, discard
#' mortality, and weight-at-age. S
#' 
#' ## Combination equations
#'
#' Let \eqn{F^{apical}_f} be the apical fishing mortality for fleet \eqn{f},
#' and \eqn{s_f(a)}, \eqn{r_f(a)}, \eqn{d_f(a)} be selectivity, retention,
#' and discard mortality at age \eqn{a}.
#'
#' **Apical F and effort**
#' \deqn{F^{apical}_{combined} = \sum_f F^{apical}_f}
#' Effort is recovered as \eqn{F^{apical}_{combined} / q}, where \eqn{q} is
#' taken from the first fleet. 
#'
#' **F-at-age (interaction)**
#' \deqn{F_{combined}(a) = \sum_f F^{apical}_f \cdot s_f(a)}
#'
#' **Selectivity**
#' \deqn{s_{combined}(a) = F_{combined}(a) \,/\, \max_a F_{combined}(a)}
#'
#' **Retention**
#' \deqn{r_{combined}(a) = \frac{\sum_f F^{apical}_f \cdot s_f(a) \cdot r_f(a)}{F_{combined}(a)}}
#'
#' **Discard mortality** (age-specific F-weighted average on the
#' instantaneous-rate scale)
#' \deqn{m_{combined}(a) = 1 - \exp\!\left(
#'   -\frac{\sum_f F_{combined,f}(a)\cdot(-\log(1-d_f(a)))}{F_{combined}(a)}
#' \right)}
#'
#' **WeightFleetSelected** (age-specific interaction-F-weighted average)
#' \deqn{W^{sel}_{combined}(a) = \frac{\sum_f F_{combined,f}(a)\cdot W^{sel}_f(a)}{F_{combined}(a)}}
#'
#' **WeightFleetRetained** (age-specific retained-F-weighted average)
#' \deqn{W^{ret}_{combined}(a) = \frac{\sum_f F^{retain}_{combined,f}(a)\cdot W^{ret}_f(a)}{F^{retain}_{combined}(a)}}
#'
#'
#' @export
CombineFleets <- function(OM, FleetList, silent = FALSE) {
  
  .CheckClass(OM)
  .ValidateFleetList(OM, FleetList)
  
  OM <- Populate(OM, silent = TRUE)
  
  FleetIndList <- purrr::map(FleetList, \(Fleets)
                             .ResolveFleetIndices(OM, Fleets)
  )
  
  if (!silent)
    cli::cli_alert_info("Combining fleets into aggregated fleet(s):")
  
  for (i in seq_along(FleetList)) {
    Name <- names(FleetList)[i]
    FleetInds <- FleetIndList[[i]]
    replaceInd <- FleetInds[1]
    
    if (!silent)
      cli::cli_li("{.val {FleetList[[i]]}} \u2192 new fleet: {.val {Name}}")
    
    for (st in seq_len(nStock(OM))) {
      OM@Fleet[[st]][[replaceInd]] <- .CombineFleetsStock(OM, st, Name, FleetInds)
      names(OM@Fleet[[st]])[replaceInd] <- Name
    }
    
    # Combine Allocation 
    OM@Allocation <- purrr::map(OM@Allocation, \(allocate) {
      purrr::imap(FleetIndList, \(fleet_ind, idx)
                  allocate[,fleet_ind, drop=FALSE] |> SumOverFleet()
      ) |> List2Array()
    })
  }
  
  # Combine stock targeting
  OM <- .CombineFleetsTargeting(OM, FleetList, FleetIndList, silent)
  
  # Combine Data
  OM <- .CombineFleetsData(OM, FleetList, silent)
  
  # Combine Obs 
  OM <- .CombineFleetsObs(OM, FleetList, silent)
  
  # EFactor 
  stock_names <- StockNames(OM)
  fleet_names <- FleetNames(OM)
  n_fleet <- length(fleet_names)
  OM@EFactor <-  MakeNamedList(stock_names, 
                               array(1, dim=c(1, n_fleet),
                                     dimnames = list(
                                       Sim = 1,
                                       Fleet = fleet_names
                                     ))
  )
  
  # Drop the source fleets (all but the first index per group)
  drop_names <- purrr::map(FleetList, \(f) f[-1]) |> unlist()
  for (st in seq_len(nStock(OM))) {
    OM@Fleet[[st]][drop_names] <- NULL
  }
  
  OM
}


.CombineFleetsDataCpue <- function(OM, FleetList, type=c('CPUE', 'Survey'), 
                                    silent=FALSE) {
  
  type <- match.arg(type)
  for (st in seq_along(OM@Data)) {
    data <- slot(OM@Data[[st]], type)
    
    if (is.null(data@Value)) next
    
    drop_ind <- integer(0)
    
    for (fl in seq_along(FleetList)) {
      combine_fleets <- FleetList[[fl]]
      
      ind <- match(combine_fleets, data@Name)
      if (!length(ind) || all(is.na(ind))) next
      
      data@Value[,ind[1]] <- .WeightedMeanByCv(
        values=data@Value[,ind, drop=FALSE],
        cvs=data@CV[,ind, drop=FALSE]
        )

      colnames(data@Value)[ind[1]] <- names(FleetList)[fl]
      data@Name[ind[1]] <- names(FleetList)[fl]
      drop_ind <- c(drop_ind, ind[-1])
    }

    # drop fleet columns
    if (length(drop_ind)) {
      drop_ind   <- sort(unique(drop_ind))
      data@Value <- data@Value[,-drop_ind, drop=FALSE]
      data@Name  <- data@Name[-drop_ind]
      if (!is.null(data@CV))
        data@CV  <- data@CV[,-drop_ind, drop=FALSE]
    }
    
    slot(OM@Data[[st]], type) <- data
  }
  
  OM 
}

.WeightedMeanByCv <- function(values, cvs) {
  weights <- 1/cvs
  out <- rowSums(values * weights, na.rm = TRUE) / rowSums(weights, na.rm = TRUE)
  out[!is.finite(out)] <- NA
  out
}


.CombineFleetsDataCatch <- function(OM,
                                     FleetList, 
                                     type=c('Landings', 'Discards'),
                                     silent=FALSE) {
  
  type <- match.arg(type)
  
  for (st in seq_along(OM@Data)) {
    data <- slot(OM@Data[[st]], type)
    
    if (is.null(data@Value)) next
    
    for (fl in seq_along(FleetList)) {
      combine_fleets <- FleetList[[fl]]
  
      ind <- match(combine_fleets, data@Name)
      if (!length(ind) || any(is.na(ind))) next
      
      all_units <- data@Units[ind]
      unique_units <- unique(all_units)
      if (length(unique_units)>1) 
        cli::cli_abort(c('x'='{.val {type}}: Units must be the same for all combined fleets',
                         'i'='Units for Fleets {.val {combine_fleets}}: {.val {all_units}}'))
      
      data@Value[,ind[1]] <- rowSums(data@Value[,ind, drop=FALSE], na.rm=TRUE)
      data@Value[,ind[-1]][] <- 1E-15
      colnames(data@Value)[ind[1]] <- names(FleetList)[fl]
      data@Name[ind[1]] <- names(FleetList)[fl]
    }
    # drop fleet columns
    drop_ind <- which(colMeans(data@Value) <= 1E-15)
    if (length(drop_ind)) {
      data@Value <- data@Value[,-drop_ind, drop=FALSE]
      data@Units <- data@Units[-drop_ind]
      data@Name <- data@Name[-drop_ind]
      if (!is.null(data@CV))
        data@CV  <- data@CV[,-drop_ind, drop=FALSE]
    }
    
    
    slot(OM@Data[[st]], type) <- data
  }
  OM
}

.CombineFleetsData <- function(OM, FleetList, silent=FALSE) {
  if (!length(OM@Data)) return(OM)
  
  # Effort TODO
  
  OM <- .CombineFleetsDataCatch(OM, FleetList, type = 'Landings', silent = silent)
  
  OM <- .CombineFleetsDataCatch(OM, FleetList, type = 'Discards', silent = silent)

  OM <- .CombineFleetsDataCpue(OM, FleetList, type='CPUE', silent = silent)
  
  OM <- .CombineFleetsDataCpue(OM, FleetList, type='Survey', silent = silent)

  OM
}

.CombineFleetsObs <- function(OM, FleetList, silent=FALSE) {
  
  for (st in seq_along(OM@Obs)) {
    obs_list <- OM@Obs[[st]]
    
    for (fl in seq_along(FleetList)) {
      new_name <- names(FleetList)[fl]
      combine_fleets <- FleetList[[fl]]
      
      ind <- match(combine_fleets, names(obs_list))
      names(obs_list)[ind[1]] <- new_name
      obs_list[ind[-1]] <- NULL
    }
    OM@Obs[[st]] <- obs_list
  }
  OM
}

#' Validate that all fleets in a FleetList exist in the OM
#'
#' @param OM An [OM()] object.
#' @param FleetList A named list of character vectors, each naming one or more
#'   fleets to combine.
#'
#' @return Called for its side effects; aborts with an informative error if any
#'   fleet name in `FleetList` is not found in `FleetNames(OM)`.
#'
#' @keywords internal
.ValidateFleetList <- function(OM, FleetList) {
  if (!is.list(FleetList))
    cli::cli_abort("`FleetList` must be a list")
  
  if (is.null(names(FleetList)))
    cli::cli_abort("`FleetList` must be a named list")
  
  fleetnames <- FleetNames(OM)
  all_supplied <- as.character(unlist(FleetList))
  missing <- !all_supplied %in% fleetnames
  
  if (any(missing)) {
    cli::cli_abort(c(
      "x" = "Names in `FleetList` do not match `FleetNames(OM)`",
      "i" = "Invalid fleet(s): {.val {all_supplied[missing]}}"
    ))
  }
}

#' Resolve fleet names to integer indices
#'
#' @param OM An [OM()] object.
#' @param Fleets A character vector of fleet names, or an integer vector of
#'   fleet indices.
#'
#' @return An integer vector of fleet indices corresponding to `Fleets`.
#'
#' @keywords internal
.ResolveFleetIndices <- function(OM, Fleets) {
  fleetnames <- FleetNames(OM)
  
  FleetInds <- if (is.character(Fleets)) {
    match(Fleets, fleetnames)
  } else {
    as.integer(Fleets)
  }
  
  if (any(is.na(FleetInds)) || !all(FleetInds %in% seq_along(fleetnames))) {
    cli::cli_abort(c(
      "x" = "Invalid `Fleets` supplied.",
      "i" = "Fleets: {.val {Fleets}}",
      "i" = "Existing fleets: {.val {fleetnames}}"
    ))
  }
  
  FleetInds
}

#' Standardize an F-at-age array so the maximum across ages equals 1
#'
#' Divides each age slice by the maximum F across ages, preserving the shape
#' of selectivity across simulations, years, and areas.
#'
#' @param Farray A numeric array with a named `"Age"` dimension.
#'
#' @return An array of the same dimensions as `Farray`, scaled so the maximum
#'   value across the Age dimension is 1.
#'
#' @keywords internal
.StandardizeF <- function(Farray) {
  nms     <- names(dimnames(Farray))
  age_ind <- which(nms == "Age")
  maxF    <- apply(Farray, nms[-age_ind], max) |>
    AddDimension("Age", pos = age_ind)
  ArrayDivide(Farray, maxF)
}

#' Combine multiple fleets into a single aggregated fleet for one stock
#'
#' Computes effort, catchability, selectivity, retention, discard mortality,
#' and weight-fleet for the combined fleet, preserving the aggregate fishery
#' dynamics of the individual fleets.
#'
#' @param OM An [OM()] object (already populated via [Populate()]).
#' @param st Integer. Stock index.
#' @param Name Character. Name for the new combined fleet.
#' @param FleetInds Integer vector. Indices of the fleets to combine.
#'
#' @return A [Fleet()] object representing the aggregated fleet.
#'
#' @keywords internal
.CombineFleetsStock <- function(OM, st, Name, FleetInds) {
  
  FleetList <- OM@Fleet[[st]][FleetInds]
  NewFleet  <- Fleet(Name = Name)
  
  apicalF_list <- purrr::map(FleetList, \(fleet)
                             ArrayMultiply(fleet@Effort@Effort, fleet@Catchability@Efficiency)
  )
  totalApicalF <- Reduce(`+`, apicalF_list)
  
  Efficiency <- FleetList[[1]]@Catchability@Efficiency
  Effort(NewFleet)       <- Effort(Effort = ArrayDivide(totalApicalF, Efficiency))
  Catchability(NewFleet) <- Catchability(Efficiency = Efficiency)
  
  FInteract_list <- purrr::map2(apicalF_list, FleetList, \(apicalF, fleet) {
    apicalF_expanded <- apicalF |>
      AddDimension("Age",  pos = 2) |>
      AddDimension("Area", pos = 4)
    ArrayMultiply(apicalF_expanded, fleet@Selectivity@MeanAtAge)
  })
  FInteract <- Reduce(`+`, FInteract_list)   # aggregate F-at-age
  
  # TODO - at length
  
  Selectivity(NewFleet) <- Selectivity(
    MeanAtAge    = .StandardizeF(FInteract))

  
  FRetain_list <- purrr::map2(FInteract_list, FleetList, \(Fint, fleet)
                              ArrayMultiply(Fint, fleet@Retention@MeanAtAge)
  )
  FRetain <- Reduce(`+`, FRetain_list)
  Retention(NewFleet) <- Retention(MeanAtAge = ArrayDivide(FRetain, FInteract))
  
  discZ_list <- purrr::map2(FInteract_list, FleetList, \(Fint, fleet) {
    discZ <- -log(1 - fleet@DiscardMortality@MeanAtAge)
    discZ[!is.finite(discZ)] <- Inf
    ArrayMultiply(Fint, discZ)
  })
  discZ_combined <- ArrayDivide(Reduce(`+`, discZ_list), FInteract)
  DiscardMortality(NewFleet) <- DiscardMortality(
    MeanAtAge = 1 - exp(-discZ_combined)
  )
  
  # WeightFleetSelected: interaction-F-weighted average (selectivity-only,
  # not retention-weighted, so weighted by total interaction F FInteract).
  WFSel_list <- purrr::map2(FInteract_list, FleetList, \(Fint, fleet)
                            ArrayMultiply(Fint, AddDimension(WeightFleetSelected(fleet),'Area'))
  )
  WeightFleetSelected(NewFleet) <- ArrayDivide(Reduce(`+`, WFSel_list), FInteract) |>
    DropDimension('Area')

  # WeightFleetRetained: retained-F-weighted average (weighted by FRetain,
  # not FInteract, since it represents the retained/landed catch specifically).
  WFRet_list <- purrr::map2(FRetain_list, FleetList, \(Fret, fleet)
                            ArrayMultiply(Fret, AddDimension(WeightFleetRetained(fleet),'Area'))
  )
  WeightFleetRetained(NewFleet) <- ArrayDivide(Reduce(`+`, WFRet_list), FRetain) |>
    DropDimension('Area')

  NewFleet
}


#' Combine stocktargeting slots after fleet aggregation
#'
#' Updates `OM@StockTargeting` so that the combined fleet's targeting
#' parameters reflect an apical-F-weighted average of the source fleets'
#' parameters. 
#'
#' The combined targeting deviation for stock `s` at time `t` is:
#'
#' \deqn{\tau_{s,\text{comb},t} =
#'   \frac{\sum_f F^{apical}_{s,f,t} \cdot \tau_{s,f,t}}{\sum_f F^{apical}_{s,f,t}}}
#'
#' and analogously for `Mean` and `Covariance`.
#'
#' If `OM@StockTargeting` is uninitialised (all `NA`), the function returns
#' `OM` unchanged.
#'
#' @param OM An [OM()] object (already populated and with fleet slots updated).
#' @param FleetList A named list of character vectors as passed to [CombineFleets()].
#' @param FleetIndList A named list of integer vectors; the resolved fleet
#'   indices corresponding to `FleetList`.
#' @param silent `logical(1)`. Suppresses messages when `TRUE`.
#'
#' @return Updated `OM` object with `@StockTargeting` reflecting the combined
#'   fleet structure.
#'
#' @keywords internal
.CombineFleetsTargeting <- function(OM, FleetList, FleetIndList, silent = FALSE) {
  
  ST <- OM@StockTargeting
  if (all(is.na(ST@Targeting))) return(OM)
  
  nStock <- nStock(OM)
  
  for (i in seq_along(FleetList)) {
    replaceInd <- FleetIndList[[i]][1]
    FleetInds  <- FleetIndList[[i]]
    
    # Apical F per fleet, per stock: [nSim, nStock, nYear]
    apicalF_list <- purrr::map(FleetInds, \(fl) {
      slices <- purrr::map(seq_len(nStock), \(st) {
        fleet <- OM@Fleet[[st]][[fl]]
        apF   <- ArrayMultiply(fleet@Effort@Effort, fleet@Catchability@Efficiency)
        AddDimension(apF, "Stock", pos = 2)             
      })
      abind::abind(slices, along = 2, use.dnns = TRUE)                  
    })
    
    totalApicalF <- Reduce(`+`, apicalF_list)           
    
    targeting_combined <- Reduce(`+`,
                                 purrr::map2(apicalF_list, FleetInds, \(apF, fl) {
                                   tau <- ST@Targeting[, , fl, , drop=FALSE] |> DropDimension('Fleet')
                                   ArrayMultiply(tau, apF)
                                 })
    )
    ST@Targeting[, , replaceInd, ] <- ArrayDivide(targeting_combined, totalApicalF)
    
    
    meanF_list <- purrr::map(apicalF_list, \(apF) {
      # apply over Year dim (dim 3), keep [nSim, nStock]
      apply(apF, c(1, 2), mean)   # [nSim, nStock] - check orientation
    })
    # apply(X, c(1,2), mean) on [nSim, nStock, nYear] returns [nSim, nStock] correctly
    totalMeanF <- Reduce(`+`, meanF_list)               # [nSim, nStock]
    
    mean_combined <- Reduce(`+`,
                            purrr::map2(meanF_list, FleetInds, \(mF, fl) {
                              mu <- ST@Mean[, , fl, drop = FALSE]             # [nSim, nStock, 1] - preserve dims
                              mu <- drop(mu)                                  # [nSim, nStock]
                              mu * mF                                         # element-wise, both [nSim, nStock]
                            })
    )
    ST@Mean[, , replaceInd] <- ArrayDivide(mean_combined, totalMeanF)
    
    make_outer_weight <- function(mF) {
      nSim_  <- nrow(mF)
      w <- array(0,
                 dim      = c(nSim_, nStock, nStock),
                 dimnames = list(Sim     = rownames(mF),
                                 Stock_i = dimnames(ST@Covariance)[[2]],
                                 Stock_j = dimnames(ST@Covariance)[[3]]))
      # vectorised: outer product per sim via sweep
      for (sim in seq_len(nSim_))
        w[sim, , ] <- outer(mF[sim, ], mF[sim, ], \(a, b) sqrt(a * b))
      w
    }
    
    meanF_list_cov  <- purrr::map(meanF_list, make_outer_weight)
    totalCovWeight  <- Reduce(`+`, meanF_list_cov)      # [nSim, nStock_i, nStock_j]
    
    cov_combined <- Reduce(`+`,
                           purrr::map2(meanF_list_cov, FleetInds, \(w, fl) {
                             cov_fl <- ST@Covariance[, , , fl, drop = FALSE] |> DropDimension('Fleet') 
                             names(dimnames(cov_fl)) <- c('Sim', 'Stock_i', 'Stock_j')
                             cov_fl * w
                           })
    )
    ST@Covariance[, , , replaceInd] <- ArrayDivide(cov_combined, totalCovWeight)
  }
  
  # Drop source fleet indices (all but first per group), highest index first
  # to avoid index shifting
  drop_inds <- purrr::map(FleetIndList, \(inds) inds[-1]) |>
    unlist() |> sort(decreasing = TRUE)
  
  for (ind in drop_inds) {
    ST@Targeting  <- ST@Targeting[,  , -ind,  , drop = FALSE]
    ST@Mean       <- ST@Mean[,       , -ind,    drop = FALSE]
    ST@Covariance <- ST@Covariance[, , , -ind,  drop = FALSE]
  }
  
  OM@StockTargeting <- ST
  OM
}
