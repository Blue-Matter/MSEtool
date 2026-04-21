
# TODO
# - Combine Data
# - Combine Obs

#' `r lifecycle::badge("experimental")`
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
#' **WeightFleet** (age-specific F-weighted average)
#' \deqn{W_{combined}(a) = \frac{\sum_f F_{combined,f}(a)\cdot W_f(a)}{F_{combined}(a)}}
#' 
#' ## Limitations
#' Combination of `Data` and `Obs` slots is not yet implemented; these are
#' carried over unchanged from the original OM.
#'
#' @export
CombineFleets <- function(OM, FleetList, silent = FALSE) {
  
  CheckClass(OM)
  validate_fleet_list(OM, FleetList)
  
  OM <- Populate(OM, silent = TRUE)
  
  FleetIndList <- purrr::map(FleetList, \(Fleets)
                             resolve_fleet_indices(OM, Fleets)
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
      OM@Fleet[[st]][[replaceInd]] <- combine_fleets_stock(OM, st, Name, FleetInds)
      names(OM@Fleet[[st]])[replaceInd] <- Name
    }
  }
  
  # Combine Data
  OM <- combine_fleets_data(OM, FleetList, silent)
  
  # Combine Obs 
  OM <- combine_fleets_obs(OM, FleetList, silent)
  
  
  # Drop the source fleets (all but the first index per group)
  drop_names <- purrr::map(FleetList, \(f) f[-1]) |> unlist()
  for (st in seq_len(nStock(OM))) {
    OM@Fleet[[st]][drop_names] <- NULL
  }
  
  OM
}


comine_fleets_data_cpue <- function(OM, FleetList, silent=FALSE) {
  
  for (st in seq_along(OM@Data)) {
    data <- OM@Data[[st]]@CPUE
    
    if (is.null(data@Value)) next
    
    for (fl in seq_along(FleetList)) {
      combine_fleets <- FleetList[[fl]]
      
      ind <- match(combine_fleets, data@Name)
      if (!length(ind) || any(is.na(ind))) next
      
      data@Value[,ind[1]] <- weighted_mean_by_cv(values=data@Value[,ind, drop=FALSE], cvs=data@CV[,ind, drop=FALSE])
      
      if (!is.null(data@CV)) {
        # TODO data@CV[,ind[1]]
      }
            data@Value[,ind[-1]][] <- 1E-15
      colnames(data@Value)[ind[1]] <- names(FleetList)[fl]
      data@Name[ind[1]] <- names(FleetList)[fl] 
    }
    
    # drop fleet columns
    drop_ind <- which(colMeans(data@Value) <= 1E-15)
    if (length(drop_ind)) {
      data@Value <- data@Value[,-drop_ind, drop=FALSE]
      data@Name <- data@Name[-drop_ind]  
    }
    
    OM@Data[[st]]@CPUE <- data
  }
  
  OM 
}

weighted_mean_by_cv <- function(values, cvs) {
  weights <- 1/cvs
  out <- rowSums(values * weights, na.rm = TRUE) / rowSums(weights, na.rm = TRUE)
  out[!is.finite(out)] <- NA
  out
}


comine_fleets_data_catch <- function(OM,
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
      
      # CV weighted by catch 
      if (!is.null(data@CV)) {
        # TODO data@CV[,ind[1]]
      }
      data@Value[,ind[1]] <- rowSums(data@Value[,ind, drop=FALSE], na.rm=TRUE)
      data@Value[,ind[-1]][] <- 1E-15
      colnames(data@Value)[ind[1]] <- names(FleetList)[fl]
      data@Name[ind[1]] <- names(FleetList)[fl] 
    }
    # drop fleet columns
    drop_ind <- which(colMeans(data@Value) <= 1E-15)
    if (length(drop_ind)) {
      data@Value <- data@Value[,-drop_ind, drop=FALSE]
      data@Name <- data@Name[-drop_ind]  
    }
    
    
    slot(OM@Data[[st]], type) <- data
  }
  OM
}

combine_fleets_data <- function(OM, FleetList, silent=FALSE) {
  if (!length(OM@Data)) return(OM)
  
  # Effort TODO
  
  # Landings
  OM <- comine_fleets_data_catch(OM, FleetList, type = 'Landings', silent = silent)
  
  # Discards 
  OM <- comine_fleets_data_catch(OM, FleetList, type = 'Discards', silent = silent)

  # CPUE 
  OM <- comine_fleets_data_cpue(OM, FleetList, silent = silent)

  OM
}

combine_fleets_obs <- function(OM, FleetList, silent=FALSE) {
  
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
validate_fleet_list <- function(OM, FleetList) {
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
resolve_fleet_indices <- function(OM, Fleets) {
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
standardize_F <- function(Farray) {
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
combine_fleets_stock <- function(OM, st, Name, FleetInds) {
  
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
  
  Selectivity(NewFleet) <- Selectivity(MeanAtAge = standardize_F(FInteract))
  
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
  
  WF_list <- purrr::map2(FInteract_list, FleetList, \(Fint, fleet)
                         ArrayMultiply(Fint, AddDimension(WeightFleet(fleet),'Area'))
  )
  WeightFleet(NewFleet) <- ArrayDivide(Reduce(`+`, WF_list), FInteract) |>
    DropDimension('Area')
  
  NewFleet
}


