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
#'   simultaneously. Defaults to `NULL`, which combines every fleet in `OM`
#'   into one, named `"Combined"`.
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
#' **F-at-age (interaction)**
#' \deqn{F_{combined}(a) = \sum_f F^{apical}_f \cdot s_f(a)}
#'
#' **Apical F and effort**
#' \deqn{F^{apical}_{combined} = \max_a F_{combined}(a)}
#' the peak of the summed age curve. 
#' \eqn{q} for the combined fleet is taken from the first
#' fleet, and effort is recovered as \eqn{F^{apical}_{combined} / q}.
#'
#' Because effort has no stock dimension (one effort series
#' drives every stock a fleet interacts with) this reconstruction can only
#' define effort for the first stock processed. For every other stock,
#' effort is reused from that first stock's reconstruction, and the
#' difference is instead absorbed into that stock's own \eqn{q}, so
#' \eqn{q_{combined,stock} = F^{apical}_{combined,stock} / \text{Effort}_{ref}}
#' still reproduces that stock's true apical F exactly.
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
#' ## Composition data
#'
#' `LandingsAtAge`/`DiscardsAtAge` counts are summed directly across the
#' combined fleets, since age classes are shared by every fleet.
#' `LandingsAtSize`/`DiscardsAtSize` counts are only summed when every fleet
#' in a group shares the same size-class bins (within floating-point
#' tolerance); if bins differ, that composition is not well-defined for the
#' combined fleet, so it is dropped and an assumption is recorded in the
#' relevant stock's `Data` object (see [Log()]).
#'
#'
#' @export
CombineFleets <- function(OM, FleetList = NULL, silent = FALSE) {

  .CheckClass(OM)
  if (is.null(FleetList))
    FleetList <- list(Combined = FleetNames(OM))
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
    
    RefEffort <- NULL
    for (st in seq_len(nStock(OM))) {
      Combined <- .CombineFleetsStock(OM, st, Name, FleetInds, RefEffort = RefEffort)
      OM@Fleet[[st]][[replaceInd]] <- Combined$Fleet
      names(OM@Fleet[[st]])[replaceInd] <- Name
      if (is.null(RefEffort)) RefEffort <- Combined$Effort
    }
  }

  OM@FleetAllocation <- purrr::map(OM@FleetAllocation, \(allocate) {
    purrr::imap(FleetIndList, \(fleet_ind, idx)
                allocate[,fleet_ind, drop=FALSE] |> SumOverFleet()
    ) |> List2Array()
  })

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

    drop_ind <- integer(0)

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
      colnames(data@Value)[ind[1]] <- names(FleetList)[fl]
      data@Name[ind[1]] <- names(FleetList)[fl]
      drop_ind <- c(drop_ind, ind[-1])
    }
    # drop fleet columns
    if (length(drop_ind)) {
      drop_ind   <- sort(unique(drop_ind))
      data@Value <- data@Value[,-drop_ind, drop=FALSE]
      data@Units <- data@Units[-drop_ind]
      data@Name  <- data@Name[-drop_ind]
      if (!is.null(data@CV))
        data@CV  <- data@CV[,-drop_ind, drop=FALSE]
    }

    slot(OM@Data[[st]], type) <- data
  }
  OM
}

.CombineFleetsDataCompAge <- function(OM, FleetList, type=c('LandingsAtAge', 'DiscardsAtAge'),
                                       silent=FALSE) {

  type <- match.arg(type)

  for (st in seq_along(OM@Data)) {
    data <- slot(OM@Data[[st]], type)

    if (is.null(data@Value)) next

    drop_ind <- integer(0)

    for (fl in seq_along(FleetList)) {
      combine_fleets <- FleetList[[fl]]

      ind <- match(combine_fleets, data@Name)
      if (!length(ind) || any(is.na(ind))) next

      data@Value[,ind[1],] <- apply(data@Value[,ind,, drop=FALSE], c(1,3), sum, na.rm=TRUE)

      dimnames(data@Value)$Fleet[ind[1]] <- names(FleetList)[fl]
      data@Name[ind[1]] <- names(FleetList)[fl]
      drop_ind <- c(drop_ind, ind[-1])
    }
    # drop fleet columns
    if (length(drop_ind)) {
      drop_ind   <- sort(unique(drop_ind))
      data@Value <- data@Value[,-drop_ind,, drop=FALSE]
      data@Name  <- data@Name[-drop_ind]
    }

    slot(OM@Data[[st]], type) <- data
  }
  OM
}

.CombineFleetsDataCompSize <- function(OM, FleetList, type=c('LandingsAtSize', 'DiscardsAtSize'),
                                        silent=FALSE) {

  type <- match.arg(type)

  for (st in seq_along(OM@Data)) {
    data <- slot(OM@Data[[st]], type)

    if (is.null(data@Value)) next

    drop_ind <- integer(0)

    for (fl in seq_along(FleetList)) {
      combine_fleets <- FleetList[[fl]]
      new_name <- names(FleetList)[fl]

      ind <- match(combine_fleets, data@Name)
      if (!length(ind) || any(is.na(ind))) next

      classes_ind <- purrr::map(ind, \(i) .CompdataClasses(data, i))
      same_bins   <- length(classes_ind) < 2 ||
        all(purrr::map_lgl(classes_ind[-1], \(cl) isTRUE(all.equal(cl, classes_ind[[1]]))))

      if (same_bins) {
        nC <- length(classes_ind[[1]])
        data@Value[,ind[1], seq_len(nC)] <- apply(
          data@Value[,ind, seq_len(nC), drop=FALSE], c(1,3), sum, na.rm=TRUE
        )
        dimnames(data@Value)$Fleet[ind[1]] <- new_name
        data@Name[ind[1]] <- new_name
        if (is.list(data@Classes))
          names(data@Classes)[ind[1]] <- new_name
        drop_ind <- c(drop_ind, ind[-1])
      } else {
        if (!silent)
          cli::cli_alert_info(
            "{.val {type}}: fleets {.val {combine_fleets}} use different size-class bins - dropping size composition for combined fleet {.val {new_name}}"
          )
        OM@Data[[st]] <- .CaptureLog(
          OM@Data[[st]],
          string = cli::format_inline(
            "Fleets {.val {combine_fleets}} use different size-class bins and could not be combined; {.val {type}} was dropped for the new fleet {.val {new_name}}."
          ),
          name = "CombineFleets",
          type = "assumption"
        )
        # drop all fleets in this group - no combined column is created
        drop_ind <- c(drop_ind, ind)
      }
    }
    # drop fleet columns
    if (length(drop_ind)) {
      drop_ind     <- sort(unique(drop_ind))
      data@Value   <- data@Value[,-drop_ind,, drop=FALSE]
      data@Name    <- data@Name[-drop_ind]
      if (is.list(data@Classes))
        data@Classes <- data@Classes[-drop_ind]
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

  OM <- .CombineFleetsDataCompAge(OM, FleetList, type='LandingsAtAge', silent = silent)

  OM <- .CombineFleetsDataCompAge(OM, FleetList, type='DiscardsAtAge', silent = silent)

  OM <- .CombineFleetsDataCompSize(OM, FleetList, type='LandingsAtSize', silent = silent)

  OM <- .CombineFleetsDataCompSize(OM, FleetList, type='DiscardsAtSize', silent = silent)

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

  .ValidateFleetListUnits(OM, FleetList)
}


.ValidateFleetListUnits <- function(OM, FleetList) {
  if (!length(OM@Data)) return(invisible(NULL))

  for (type in c('Landings', 'Discards')) {
    for (st in seq_along(OM@Data)) {
      data <- slot(OM@Data[[st]], type)
      if (is.null(data@Value) || is.null(data@Units)) next

      for (i in seq_along(FleetList)) {
        ind <- match(FleetList[[i]], data@Name)
        ind <- ind[!is.na(ind)]
        if (length(ind) < 2) next

        units <- data@Units[ind]
        if (length(unique(units)) > 1)
          .AbortMixedUnits(type, names(FleetList)[i], data@Name[ind], units)
      }
    }
  }
  invisible(NULL)
}

.AbortMixedUnits <- function(type, group_name, fleet_names, units) {
  by_unit <- split(fleet_names, units)

  group_lines <- purrr::imap_chr(by_unit, \(fls, u) {
    n <- length(fls)
    cli::format_inline("{.val {u}} ({n} fleet{if (n == 1) '' else 's'}): {.val {fls}}")
  })

  vec_code <- \(x) paste0('c(', paste0('"', x, '"', collapse = ', '), ')')
  suggestion_args <- purrr::imap_chr(by_unit, \(fls, u) paste0(make.names(u), " = ", vec_code(fls)))
  suggestion <- paste0("CombineFleets(OM, FleetList = list(", paste(suggestion_args, collapse = ", "), "))")

  bullets <- c(
    "x" = "Cannot combine fleets recorded in different {.val {type}} units.",
    "i" = "Group {.val {group_name}} mixes units - group fleets by matching units instead:"
  )
  names(group_lines) <- rep("*", length(group_lines))
  bullets <- c(bullets, group_lines, "i" = "e.g. {.code {suggestion}}")

  cli::cli_abort(bullets)
}

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

.StandardizeF <- function(Farray) {
  nms     <- names(dimnames(Farray))
  age_ind <- which(nms == "Age")
  maxF    <- apply(Farray, nms[-age_ind], max) |>
    AddDimension("Age", pos = age_ind)
  ArrayDivide(Farray, maxF)
}


.CombineFleetsStock <- function(OM, st, Name, FleetInds, RefEffort = NULL) {

  FleetList <- OM@Fleet[[st]][FleetInds]
  NewFleet  <- Fleet(Name = Name)

  apicalF_list <- purrr::map(FleetList, \(fleet)
                             ArrayMultiply(fleet@Effort@Effort, fleet@Catchability@Efficiency)
  )

  Efficiency <- FleetList[[1]]@Catchability@Efficiency
  HistYears  <- as.numeric(dimnames(FleetList[[1]]@Effort@Effort)$Year)

  FInteract_list <- purrr::map2(apicalF_list, FleetList, \(apicalF, fleet) {
    apicalF_expanded <- apicalF |>
      AddDimension("Age",  pos = 2) |>
      AddDimension("Area", pos = 4)
    ArrayMultiply(apicalF_expanded, fleet@Selectivity@MeanAtAge)
  })
  FInteract <- Reduce(ArraySum, FInteract_list)   # aggregate F-at-age

  nms         <- names(dimnames(FInteract))
  keepDims    <- which(nms %in% c('Sim', 'Year'))
  trueApicalF <- apply(FInteract, keepDims, max) |>
    .ArraySubsetYear(HistYears)
  Efficiency  <- .ArraySubsetYear(Efficiency, HistYears)

  if (is.null(RefEffort)) {
    EffortOut <- ArrayDivide(trueApicalF, Efficiency)
  } else {
    EffortOut  <- RefEffort
    Efficiency <- ArrayDivide(trueApicalF, RefEffort)
  }

  Effort(NewFleet)       <- Effort(Effort = EffortOut)
  Catchability(NewFleet) <- Catchability(Efficiency = Efficiency)

  # TODO - at length

  Selectivity(NewFleet) <- Selectivity(
    MeanAtAge    = .StandardizeF(FInteract))

  
  FRetain_list <- purrr::map2(FInteract_list, FleetList, \(Fint, fleet)
                              ArrayMultiply(Fint, fleet@Retention@MeanAtAge)
  )
  FRetain <- Reduce(ArraySum, FRetain_list)
  Retention(NewFleet) <- Retention(MeanAtAge = ArrayDivide(FRetain, FInteract))
  
  discZ_list <- purrr::map2(FInteract_list, FleetList, \(Fint, fleet) {
    discZ <- -log(1 - fleet@DiscardMortality@MeanAtAge)
    discZ[!is.finite(discZ)] <- Inf
    ArrayMultiply(Fint, discZ)
  })
  discZ_combined <- ArrayDivide(Reduce(ArraySum, discZ_list), FInteract)
  DiscardMortality(NewFleet) <- DiscardMortality(
    MeanAtAge = 1 - exp(-discZ_combined)
  )
  
  WFSel_list <- purrr::map2(FInteract_list, FleetList, \(Fint, fleet)
                            ArrayMultiply(Fint, AddDimension(WeightFleetSelected(fleet),'Area'))
  )
  WeightFleetSelected(NewFleet) <- ArrayDivide(Reduce(ArraySum, WFSel_list), FInteract) |>
    DropDimension('Area')

  WFRet_list <- purrr::map2(FRetain_list, FleetList, \(Fret, fleet)
                            ArrayMultiply(Fret, AddDimension(WeightFleetRetained(fleet),'Area'))
  )
  WeightFleetRetained(NewFleet) <- ArrayDivide(Reduce(ArraySum, WFRet_list), FRetain) |>
    DropDimension('Area')

  list(Fleet = NewFleet, Effort = EffortOut)
}


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
        ArrayMultiply(fleet@Effort@Effort, fleet@Catchability@Efficiency)
      })
      List2Array(slices, name = "Stock", pos = 2)
    })

    totalApicalF <- Reduce(ArraySum, apicalF_list)

    targeting_combined <- Reduce(ArraySum,
                                 purrr::map2(apicalF_list, FleetInds, \(apF, fl) {
                                   tau <- ST@Targeting[, , fl, , drop=FALSE] |> DropDimension('Fleet')
                                   ArrayMultiply(tau, apF)
                                 })
    )
    ST@Targeting[, , replaceInd, ] <- ArrayDivide(targeting_combined, totalApicalF)


    meanF_list <- purrr::map(apicalF_list, \(apF) {
      apply(apF, c(1, 2), mean)   
    })
    totalMeanF <- Reduce(ArraySum, meanF_list)      

    mean_combined <- Reduce(ArraySum,
                            purrr::map2(meanF_list, FleetInds, \(mF, fl) {
                              mu <- ST@Mean[, , fl, drop = FALSE]          
                              mu <- DropDimension(mu, 'Fleet')             
                              ArrayMultiply(mu, mF)                        
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
      for (sim in seq_len(nSim_))
        w[sim, , ] <- outer(mF[sim, ], mF[sim, ], \(a, b) sqrt(a * b))
      w
    }
    
    meanF_list_cov  <- purrr::map(meanF_list, make_outer_weight)
    totalCovWeight  <- Reduce(ArraySum, meanF_list_cov)      # [nSim, nStock_i, nStock_j]

    cov_combined <- Reduce(ArraySum,
                           purrr::map2(meanF_list_cov, FleetInds, \(w, fl) {
                             cov_fl <- ST@Covariance[, , , fl, drop = FALSE] |> DropDimension('Fleet')
                             names(dimnames(cov_fl)) <- c('Sim', 'Stock_i', 'Stock_j')
                             ArrayMultiply(cov_fl, w)
                           })
    )
    ST@Covariance[, , , replaceInd] <- ArrayDivide(cov_combined, totalCovWeight)
  }
  
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
