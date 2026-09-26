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
#' mortality, and weight-at-age.
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
#' **Years with zero F**
#'
#' In a year and simulation where every combined fleet has zero F, the
#' selectivity, retention, discard mortality, and weight-at-age curves above
#' use each fleet's catchability \eqn{q_f} in place of \eqn{F^{apical}_f}, or
#' equal weights if every \eqn{q_f} is also zero.
#'
#' ## Fleet settings
#'
#' Weights \eqn{w_f} are each fleet's share of the summed apical F in each
#' simulation and historical year (equal shares where the total is zero).
#'
#' - `Effort@Targeting` and `Effort@StockTargetingLambda` are the
#'   \eqn{w_f}-weighted means of the fleets' values.
#' - `Effort@Distribution` is the \eqn{w_f}-weighted mean of the fleets'
#'   fixed distributions, and is derived dynamically unless every fleet has
#'   one.
#' - `Effort@Units` are the first fleet's units, since effort is recovered
#'   from its catchability.
#' - `Closure`: an area is open to the combined fleet where any of its fleets
#'   is open.
#' - Projected catchability (from `qInc`/`qCV`) is
#'   \eqn{q_{combined}(y) = q_{combined}(y_c) \sum_f w_f(y_c)\, q_f(y) / q_f(y_c)},
#'   where \eqn{y_c} is the last historical year.
#' - Fleets must share `Effort@Mode`, and bag-limit settings
#'   (`TripsScalar`, `AnglerPerTrip`, `Theta`) cannot be combined; either is an
#'   error.
#'
#' The combined fleet uses the first fleet's [Obs()] and [Imp()] objects.
#'
#' ## Data
#'
#' For each stock's [Data()] object:
#'
#' - `Landings`, `Discards`, and `Effort` values are summed across the
#'   combined fleets in each year, and are `NA` in years with no data for any
#'   of them. `CV` is \eqn{\sqrt{\sum_f (CV_f \cdot x_f)^2} / \sum_f x_f}, and
#'   `Ref`/`RefCV` are combined in the same way. Catch units must match;
#'   effort recorded in different units is dropped for the combined fleet.
#' - `CPUE` indices named after combined fleets are moved to `Survey`, with
#'   their values, `CV`, `Units`, `Ref`, and `Timing`. Each moved index, and
#'   each `Survey` index named after a combined fleet, gets a survey [Obs()]
#'   entry that carries the fleet's index observation settings and, unless
#'   the index has its own selectivity, the fleet's selectivity-at-age from
#'   before combining. An index keeps its fleet's name, with a `" CPUE"` or
#'   `" Survey"` suffix where that name is taken by a fleet or another index.
#' - `LandingsAtAge`/`DiscardsAtAge` counts are summed across the combined
#'   fleets. `LandingsAtSize`/`DiscardsAtSize` counts are summed when every
#'   fleet shares the same size-class bins (within floating-point
#'   tolerance), and dropped otherwise. Counts are `NA` in years with no
#'   sample for any of the fleets.
#'
#' Catch, effort, and composition data held by only some fleets of a group
#' are dropped for the combined fleet, since the combined total is unknown.
#' Each drop is recorded as an assumption in the stock's `Data` object (see
#' [Log()]).
#'
#' ## Allocations
#'
#' [FleetAllocation()] and [CatchFrac()] shares are summed across the combined
#' fleets. For each complex with a [SeasonalAllocation()], the combined
#' fleet's seasonal shares are the `FleetAllocation`-weighted mean of its
#' fleets' shares. [HistoricalWeight()] is combined the same way for
#' complexes where `SeasonalAllocation` is derived. If `FleetAllocation` is
#' not set, the fleets' values must be identical. [EffortAllocation()] is
#' reset, and derived again in [Simulate()].
#'
#' ## Interim advice
#'
#' [InterimAdvice()] rows for the combined fleets are replaced by one row per
#' timestep for the new fleet. `Fleet = NA` rows are unchanged. Within each
#' calendar year x Complex x Type, every fleet in a group must have one row
#' for each of the same timesteps, or no rows so that a `Fleet = NA` total
#' covers the combined fleet.
#'
#' - `"TAC"` rows must share `TACType` and `TACUnit`. `Mean` is the sum of the
#'   fleets' means and `CV` is \eqn{\sqrt{\sum_f (CV_f \cdot Mean_f)^2} / \sum_f Mean_f},
#'   the CV of a sum of independent draws. `Max` is the sum of the fleets'
#'   bounds, where a fleet with `CV = NA`/`0` or `Mean = 0` is bounded by its
#'   `Mean`, and is `NA` if any other fleet has no bound.
#' - Relative `"Effort"` rows (`EffType = "Rel"`) must have the same `Mean`,
#'   `CV` and `Max` for every fleet, and are kept as a single row.
#' - Absolute `"Effort"` rows cannot be combined, since effort units depend on
#'   each fleet's catchability.
#'
#' Rows that cannot be combined are an error. In that case, set
#' `InterimAdvice(OM) <- NULL` before combining and supply rows for the new
#' fleet afterwards.
#'
#' @export
CombineFleets <- function(OM, FleetList = NULL, silent = FALSE) {

  .CheckClass(OM)
  if (is.null(FleetList))
    FleetList <- list(Combined = FleetNames(OM))
  .ValidateFleetList(OM, FleetList)
  OM <- .CombineFleetsInterimAdvice(OM, FleetList)

  OM <- Populate(OM, silent = TRUE)

  FleetIndList <- purrr::map(FleetList, \(Fleets)
                             .ResolveFleetIndices(OM, Fleets)
  )
  FleetMap <- .CombineFleetsMap(FleetNames(OM), FleetList)
  OM <- .CombineFleetsAllocation(OM, FleetMap)
  OM <- .CombineFleetsImp(OM, FleetMap)
  Indices <- .CombineFleetsIndices(OM, FleetList, FleetMap, silent)
  OM <- Indices$OM
  HistYears <- Years(OM, "Historical")
  ProjYears <- Years(OM, "Projection")

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
      OM@Fleet[[st]][[replaceInd]] <- .CombineFleetsSettings(
        Combined$Fleet, OM@Fleet[[st]][FleetInds], HistYears, ProjYears)
      names(OM@Fleet[[st]])[replaceInd] <- Name
      if (is.null(RefEffort)) RefEffort <- Combined$Effort
    }
  }

  # Combine stock targeting
  OM <- .CombineFleetsTargeting(OM, FleetList, FleetIndList, silent)
  
  # Combine Data
  OM <- .CombineFleetsData(OM, FleetList, silent)
  
  # Combine Obs
  OM <- .CombineFleetsObs(OM, FleetList, silent)
  OM@Obs <- purrr::map2(OM@Obs, Indices$Obs, c)

  OM@EffortAllocation <- list()
  OM@EFactor <- list()

  # Drop the source fleets (all but the first index per group)
  drop_names <- purrr::map(FleetList, \(f) f[-1]) |> unlist()
  for (st in seq_len(nStock(OM))) {
    OM@Fleet[[st]][drop_names] <- NULL
  }
  
  OM
}


# CPUE and Survey indices named after combined fleets become Survey indices that
# keep the selectivity of their fleet from before combining
.CombineFleetsIndices <- function(OM, FleetList, FleetMap, silent = FALSE) {
  GroupFleets <- unlist(FleetList, use.names = FALSE)
  nFleetOrig  <- length(FleetNames(OM))
  NewObs      <- purrr::map(OM@Obs, \(x) list())

  for (i in seq_along(OM@Data)) {
    data   <- OM@Data[[i]]
    cpue   <- data@CPUE
    survey <- data@Survey
    stocks <- OM@Complexes[[i]]
    ObsList <- OM@Obs[[i]]
    taken  <- c(names(FleetMap), names(ObsList)[-seq_len(nFleetOrig)],
                setdiff(survey@Name, GroupFleets))
    moved  <- character(0)

    for (j in which(survey@Name %in% GroupFleets)) {
      f <- survey@Name[j]
      NewName <- .UniqueIndexName(f, "Survey", taken)
      taken <- c(taken, NewName)
      IndexObs <- ObsList[[f]]@Survey
      IndexObs@Selectivity <- .IndexFleetSelectivity(IndexObs@Selectivity,
                                                     .IndexEntry(survey@Selectivity, j), OM, stocks, f)
      NewObs[[i]][[NewName]] <- .SurveyObs(NewName, IndexObs)
      survey@Name[j] <- NewName
      survey@Value <- .RenameColumn(survey@Value, j, NewName)
      if (.HasFleetColumns(survey@CV, length(survey@Name)))
        survey@CV <- .RenameColumn(survey@CV, j, NewName)
      moved <- c(moved, stats::setNames(NewName, paste("Survey", f)))
    }

    idx <- which(cpue@Name %in% GroupFleets)
    if (length(idx)) {
      nIndex   <- length(cpue@Name)
      nSurvey  <- length(survey@Name)
      NewNames <- character(0)
      for (j in idx) {
        f <- cpue@Name[j]
        NewName <- .UniqueIndexName(f, "CPUE", taken)
        taken <- c(taken, NewName)
        NewNames <- c(NewNames, NewName)
        IndexObs <- ObsList[[f]]@CPUE
        IndexObs@Selectivity <- .IndexFleetSelectivity(IndexObs@Selectivity,
                                                       .IndexEntry(cpue@Selectivity, j), OM, stocks, f)
        NewObs[[i]][[NewName]] <- .SurveyObs(NewName, IndexObs)
        moved <- c(moved, stats::setNames(NewName, paste("CPUE", f)))
      }

      Value <- cpue@Value[, idx, drop = FALSE]
      colnames(Value) <- NewNames
      CV <- if (.HasFleetColumns(cpue@CV, nIndex)) cpue@CV[, idx, drop = FALSE] else NULL
      if (!is.null(CV)) colnames(CV) <- NewNames
      if (!is.null(survey@CV) || !is.null(CV))
        survey@CV <- .AppendIndexColumns(survey@CV %||% .NALike(survey@Value), CV %||% .NALike(Value))
      survey@Value  <- .AppendIndexColumns(survey@Value, Value)
      survey@Units  <- .AppendIndexEntries(survey@Units, nSurvey, cpue@Units, idx, as.character)
      survey@Ref    <- .AppendIndexEntries(survey@Ref, nSurvey, cpue@Ref, idx, as.numeric)
      survey@Timing <- .AppendIndexEntries(survey@Timing, nSurvey, cpue@Timing, idx, as.numeric)
      survey@Name   <- c(survey@Name, NewNames)

      if (length(idx) == nIndex) {
        cpue <- new("indicesdata")
      } else {
        cpue@Value <- cpue@Value[, -idx, drop = FALSE]
        cpue@Name  <- cpue@Name[-idx]
        for (sl in c("CV", "Units", "Ref", "RefCV", "Timing", "Selectivity"))
          slot(cpue, sl) <- .DropIndexEntries(slot(cpue, sl), idx, nIndex)
      }
    }

    data@CPUE   <- cpue
    data@Survey <- survey
    for (k in seq_along(moved)) {
      msg <- cli::format_inline("{sub(' .*', '', names(moved)[k])} index {.val {sub('^[^ ]+ ', '', names(moved)[k])}} is Survey index {.val {moved[k]}}, using the selectivity of its fleet before combining.")
      if (!silent) cli::cli_alert_info(msg)
      data <- .CaptureLog(data, string = msg, name = "CombineFleets", type = "assumption")
    }
    OM@Data[[i]] <- data
  }
  list(OM = OM, Obs = NewObs)
}

.UniqueIndexName <- function(name, suffix, taken) {
  if (!name %in% taken) return(name)
  candidate <- paste(name, suffix)
  if (!candidate %in% taken) return(candidate)
  utils::tail(make.unique(c(taken, candidate)), 1)
}

# positional per-index entry, as read in index conditioning
.IndexEntry <- function(x, j) if (length(x) >= j) x[[j]] else NULL

# an index's own selectivity if set, otherwise its fleet's selectivity-at-age for each stock
.IndexFleetSelectivity <- function(ObsSel, DataSel, OM, stocks, fleet) {
  if (!is.null(ObsSel)) return(ObsSel)
  if (is.character(DataSel) && DataSel %in% c("Biomass", "SBiomass")) return(DataSel)
  sel <- purrr::map(stocks, \(st) OM@Fleet[[st]][[fleet]]@Selectivity@MeanAtAge)
  stats::setNames(sel, StockNames(OM)[stocks])
}

.SurveyObs <- function(Name, IndexObs) {
  obs <- Obs(Name = Name)
  obs@Survey <- IndexObs
  obs
}

.NALike <- function(x) {
  if (is.null(x)) return(NULL)
  x[] <- NA_real_
  x
}

# append Year x Index columns, aligning rows by year
.AppendIndexColumns <- function(x, y) {
  if (is.null(x)) return(y)
  if (is.null(rownames(x)) || is.null(rownames(y))) return(cbind(x, y))
  Years <- sort(unique(as.numeric(c(rownames(x), rownames(y)))))
  DimNames <- names(dimnames(x))
  if (is.null(DimNames)) DimNames <- c("Year", "Fleet")
  out <- matrix(NA_real_, length(Years), ncol(x) + ncol(y),
                dimnames = stats::setNames(list(Years, c(colnames(x), colnames(y))), DimNames))
  out[match(as.numeric(rownames(x)), Years), seq_len(ncol(x))] <- x
  out[match(as.numeric(rownames(y)), Years), ncol(x) + seq_len(ncol(y))] <- y
  out
}

# per-index slot of the first nSurvey indices, with the moved indices' entries appended
.AppendIndexEntries <- function(x, nSurvey, from, idx, as) {
  add <- vapply(idx, \(j) if (length(from) >= j) as(from[[j]]) else as(NA), as(NA))
  if (is.null(x) && all(is.na(add))) return(x)
  base <- if (is.null(x)) rep(as(NA), nSurvey) else as(x)[seq_len(nSurvey)]
  c(base, add)
}
.HasFleetColumns <- function(x, n) length(dim(x)) == 2 && ncol(x) == n

# drop the entries of dropped indices from a per-index slot, read by position (vector) or column (matrix)
.DropIndexEntries <- function(x, drop, n) {
  if (is.null(x)) return(x)
  d <- dim(x)
  if (length(d) == 2) return(if (d[2] == n) x[, -drop, drop = FALSE] else x)
  keep <- setdiff(seq_len(length(x)), drop)
  if (length(d) == 1) x[keep, drop = FALSE] else x[keep]
}

.SumOrNA <- function(values) {
  out <- rowSums(values, na.rm = TRUE)
  out[rowSums(!is.na(values)) == 0] <- NA
  out
}

# CV of a sum of independent observations (mean CV where the sum is zero); NA where an observed value has no CV
.SumCV <- function(values, cvs) {
  observed <- !is.na(values)
  total <- rowSums(values, na.rm = TRUE)
  out <- sqrt(rowSums((cvs * values)^2, na.rm = TRUE)) / total
  zero <- total == 0
  out[zero] <- rowMeans(ifelse(observed, cvs, NA), na.rm = TRUE)[zero]
  out[rowSums(observed & is.na(cvs)) > 0 | rowSums(observed) == 0 | !is.finite(out)] <- NA
  out
}

.RenameColumn <- function(x, col, name) {
  if (!is.null(colnames(x))) colnames(x)[col] <- name
  x
}

.LogCombineFleetsDrop <- function(OM, st, type, new_name, reason, silent) {
  msg <- cli::format_inline("{.val {type}} was dropped for the new fleet {.val {new_name}}: {reason}")
  if (!silent) cli::cli_alert_info(msg)
  OM@Data[[st]] <- .CaptureLog(OM@Data[[st]], string = msg, name = "CombineFleets", type = "assumption")
  OM
}

.MissingFleetsReason <- function(fleets) {
  cli::format_inline("fleet{?s} {.val {fleets}} {?has/have} no data, so the combined total is unknown.")
}

# Landings, Discards, and Effort: values summed; drops the group if a fleet has no data or effort units differ
.CombineFleetsDataSum <- function(OM, FleetList,
                                  type = c('Landings', 'Discards', 'Effort'),
                                  silent = FALSE) {

  type <- match.arg(type)

  for (st in seq_along(OM@Data)) {
    data <- slot(OM@Data[[st]], type)

    if (is.null(data@Value)) next

    nFleet   <- length(data@Name)
    hasRef   <- methods::.hasSlot(data, "Ref")
    drop_ind <- integer(0)

    for (fl in seq_along(FleetList)) {
      combine_fleets <- FleetList[[fl]]
      new_name <- names(FleetList)[fl]

      ind <- match(combine_fleets, data@Name)
      if (all(is.na(ind))) next

      if (anyNA(ind)) {
        OM <- .LogCombineFleetsDrop(OM, st, type, new_name,
                                    .MissingFleetsReason(combine_fleets[is.na(ind)]), silent)
        drop_ind <- c(drop_ind, ind[!is.na(ind)])
        next
      }

      units <- if (length(data@Units) == nFleet) data@Units[ind] else NULL
      if (length(unique(units)) > 1) {
        if (type != 'Effort')
          cli::cli_abort(c('x'='{.val {type}}: Units must be the same for all combined fleets',
                           'i'='Units for Fleets {.val {combine_fleets}}: {.val {units}}'))
        OM <- .LogCombineFleetsDrop(OM, st, type, new_name,
                                    cli::format_inline("fleets {.val {combine_fleets}} record effort in different units ({.val {units}})."),
                                    silent)
        drop_ind <- c(drop_ind, ind)
        next
      }

      values <- data@Value[, ind, drop = FALSE]
      if (.HasFleetColumns(data@CV, nFleet))
        data@CV[, ind[1]] <- .SumCV(values, data@CV[, ind, drop = FALSE])
      data@Value[, ind[1]] <- .SumOrNA(values)

      if (hasRef && .HasFleetColumns(data@Ref, nFleet)) {
        refs <- data@Ref[, ind, drop = FALSE]
        if (.HasFleetColumns(data@RefCV, nFleet))
          data@RefCV[, ind[1]] <- .SumCV(refs, data@RefCV[, ind, drop = FALSE])
        data@Ref[, ind[1]] <- .SumOrNA(refs)
      }

      for (sl in c("Value", "CV", if (hasRef) c("Ref", "RefCV")))
        if (.HasFleetColumns(slot(data, sl), nFleet))
          slot(data, sl) <- .RenameColumn(slot(data, sl), ind[1], new_name)
      data@Name[ind[1]] <- new_name
      drop_ind <- c(drop_ind, ind[-1])
    }

    if (length(drop_ind)) {
      drop_ind   <- sort(unique(drop_ind))
      data@Value <- data@Value[, -drop_ind, drop = FALSE]
      data@Name  <- data@Name[-drop_ind]
      for (sl in c("CV", "Units", if (hasRef) c("Ref", "RefCV")))
        slot(data, sl) <- .DropIndexEntries(slot(data, sl), drop_ind, nFleet)
    }

    slot(OM@Data[[st]], type) <- data
  }
  OM
}

# counts summed across fleets; NA where no fleet has a sample
.SumCompOrNA <- function(values) {
  out <- apply(values, c(1, 3), sum, na.rm = TRUE)
  out[apply(is.na(values), c(1, 3), all)] <- NA
  out
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
      new_name <- names(FleetList)[fl]

      ind <- match(combine_fleets, data@Name)
      if (all(is.na(ind))) next

      if (anyNA(ind)) {
        OM <- .LogCombineFleetsDrop(OM, st, type, new_name,
                                    .MissingFleetsReason(combine_fleets[is.na(ind)]), silent)
        drop_ind <- c(drop_ind, ind[!is.na(ind)])
        next
      }

      data@Value[,ind[1],] <- .SumCompOrNA(data@Value[,ind,, drop=FALSE])

      dimnames(data@Value)$Fleet[ind[1]] <- new_name
      data@Name[ind[1]] <- new_name
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
      if (all(is.na(ind))) next

      if (anyNA(ind)) {
        OM <- .LogCombineFleetsDrop(OM, st, type, new_name,
                                    .MissingFleetsReason(combine_fleets[is.na(ind)]), silent)
        drop_ind <- c(drop_ind, ind[!is.na(ind)])
        next
      }

      classes_ind <- purrr::map(ind, \(i) .CompdataClasses(data, i))
      same_bins   <- length(classes_ind) < 2 ||
        all(purrr::map_lgl(classes_ind[-1], \(cl) isTRUE(all.equal(cl, classes_ind[[1]]))))

      if (same_bins) {
        nC <- length(classes_ind[[1]])
        data@Value[,ind[1], seq_len(nC)] <- .SumCompOrNA(data@Value[,ind, seq_len(nC), drop=FALSE])
        dimnames(data@Value)$Fleet[ind[1]] <- new_name
        data@Name[ind[1]] <- new_name
        if (is.list(data@Classes))
          names(data@Classes)[ind[1]] <- new_name
        drop_ind <- c(drop_ind, ind[-1])
      } else {
        OM <- .LogCombineFleetsDrop(OM, st, type, new_name,
                                    cli::format_inline("fleets {.val {combine_fleets}} use different size-class bins."),
                                    silent)
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

  for (type in c('Landings', 'Discards', 'Effort'))
    OM <- .CombineFleetsDataSum(OM, FleetList, type = type, silent = silent)

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

# replace per-fleet weights with `fill_list` wherever they sum to zero; NULL fills equal weights
.FillZeroWeights <- function(weight_list, fill_list = NULL) {
  zero <- Reduce(ArraySum, weight_list) == 0
  if (!any(zero, na.rm = TRUE)) return(weight_list)
  if (is.null(fill_list)) fill_list <- rep(list(zero), length(weight_list))
  purrr::map2(weight_list, fill_list, \(w, fill)
    ArraySum(ArrayMultiply(w, !zero), ArrayMultiply(fill, zero))
  )
}


.CombineFleetsStock <- function(OM, st, Name, FleetInds, RefEffort = NULL) {

  FleetList <- OM@Fleet[[st]][FleetInds]
  NewFleet  <- Fleet(Name = Name)

  apicalF_list <- purrr::map(FleetList, \(fleet)
                             ArrayMultiply(fleet@Effort@Effort, fleet@Catchability@Efficiency)
  )

  Efficiency <- FleetList[[1]]@Catchability@Efficiency
  HistYears  <- as.numeric(dimnames(FleetList[[1]]@Effort@Effort)$Year)

  FAtAge <- \(weight_list) purrr::map2(weight_list, FleetList, \(w, fleet)
    w |>
      AddDimension("Age",  pos = 2) |>
      AddDimension("Area", pos = 4) |>
      ArrayMultiply(fleet@Selectivity@MeanAtAge)
  )

  trueApicalF <- Reduce(ArraySum, FAtAge(apicalF_list)) |>
    apply(c('Sim', 'Year'), max) |>
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

  # where every fleet has zero F, weight the curves by catchability, then equally
  FInteract_list <- apicalF_list |>
    .FillZeroWeights(purrr::map(FleetList, \(fleet) fleet@Catchability@Efficiency)) |>
    .FillZeroWeights() |>
    FAtAge()
  FInteract <- Reduce(ArraySum, FInteract_list)

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
