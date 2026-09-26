# original fleet indices (named by original fleet name) for each fleet after combining, in final order
.CombineFleetsMap <- function(fleetnames, FleetList) {
  FleetMap <- purrr::map(seq_along(fleetnames), \(i) stats::setNames(i, fleetnames[i]))
  names(FleetMap) <- fleetnames
  drop <- integer(0)
  for (i in seq_along(FleetList)) {
    inds <- match(FleetList[[i]], fleetnames)
    FleetMap[[inds[1]]] <- stats::setNames(inds, fleetnames[inds])
    names(FleetMap)[inds[1]] <- names(FleetList)[i]
    drop <- c(drop, inds[-1])
  }
  if (length(drop)) FleetMap <- FleetMap[-drop]
  FleetMap
}

# nSim x nFleet matrix of shares summing to 1 across fleets
.CombineFleetShares <- function(x, FleetMap) {
  if (is.null(x)) return(NULL)
  out <- vapply(FleetMap, \(ind) rowSums(x[, ind, drop = FALSE]), numeric(nrow(x)))
  out <- matrix(out, nrow(x))
  sims <- rownames(x)
  if (is.null(sims)) sims <- seq_len(nrow(x))
  dimnames(out) <- list(Sim = sims, Fleet = names(FleetMap))
  out
}

.CombineFleetsAllocation <- function(OM, FleetMap) {
  if (!length(OM@FleetAllocation) && length(OM@Allocation)) {
    OM@FleetAllocation <- OM@Allocation
    OM@Allocation <- list()
  }
  OM <- .CombineFleetsHistoricalWeight(OM, FleetMap)
  OM <- .CombineFleetsSeasonalAllocation(OM, FleetMap)
  OM@FleetAllocation <- purrr::map(OM@FleetAllocation, \(x) .CombineFleetShares(x, FleetMap))
  OM@CatchFrac <- purrr::map(OM@CatchFrac, \(x) .CombineFleetShares(x, FleetMap))
  OM
}

# seasonal shares of the combined fleet: FleetAllocation-weighted mean of its fleets' shares
.CombineFleetsSeasonalAllocation <- function(OM, FleetMap) {
  SA <- OM@SeasonalAllocation
  if (!length(SA)) return(OM)
  FA <- OM@FleetAllocation
  ComplexNames <- names(OM@Complexes)

  for (i in seq_along(SA)) {
    sa <- SA[[i]]
    if (is.null(sa)) next
    fa <- if (length(FA) >= i) FA[[i]] else NULL

    nS <- max(dim(sa)[1], if (is.null(fa)) 1 else nrow(fa))
    SimInd <- \(n) if (n == 1) rep(1L, nS) else seq_len(nS)
    saSim <- SimInd(dim(sa)[1])
    Seasons <- dimnames(sa)[[2]]
    if (is.null(Seasons)) Seasons <- seq_len(dim(sa)[2])

    out <- array(0, dim = c(nS, dim(sa)[2], length(FleetMap)),
                 dimnames = list(Sim = seq_len(nS), Season = Seasons, Fleet = names(FleetMap)))
    for (j in seq_along(FleetMap)) {
      ind <- FleetMap[[j]]
      s <- sa[saSim, , ind, drop = FALSE]
      if (length(ind) == 1) {
        out[, , j] <- s
        next
      }
      if (is.null(fa)) {
        same <- all(vapply(seq_along(ind)[-1], \(k) isTRUE(all.equal(s[, , k], s[, , 1])), logical(1)))
        if (!same)
          cli::cli_abort(c(
            "x" = "Cannot combine `SeasonalAllocation` for fleets {.val {names(ind)}} in Complex {.val {ComplexNames[i]}}: their seasonal allocations differ and `FleetAllocation` is not set to weight them.",
            "i" = "Set `FleetAllocation(OM)`, or set `SeasonalAllocation(OM) <- list()` before `CombineFleets()` and supply it for {.val {names(FleetMap)[j]}} afterwards."
          ))
        out[, , j] <- s[, , 1]
        next
      }
      w <- fa[SimInd(nrow(fa)), ind, drop = FALSE]
      w[rowSums(w) == 0, ] <- 1
      w <- w / rowSums(w)
      out[, , j] <- apply(sweep(s, c(1, 3), w, "*"), c(1, 2), sum)
    }
    SA[[i]] <- out
  }
  OM@SeasonalAllocation <- SA
  OM
}

# FleetAllocation-weighted mean for fleets with differing weights; only used where SeasonalAllocation is derived
.CombineFleetsHistoricalWeight <- function(OM, FleetMap) {
  HW <- OM@HistoricalWeight
  if (!length(HW)) return(OM)
  SA <- OM@SeasonalAllocation
  FA <- OM@FleetAllocation
  ComplexNames <- names(OM@Complexes)
  fleetnames   <- names(sort(unlist(unname(FleetMap))))

  for (i in seq_along(HW)) {
    if (is.null(HW[[i]]) || (length(SA) >= i && !is.null(SA[[i]]))) next
    hw <- .ResolveHistoricalWeight(HW[[i]], fleetnames)
    fa <- if (length(FA) >= i && !is.null(FA[[i]])) colMeans(FA[[i]]) else NULL
    HW[[i]] <- purrr::imap_dbl(FleetMap, \(ind, NewName) {
      h <- unname(hw[ind])
      if (isTRUE(all.equal(h, rep(h[1], length(h))))) return(h[1])
      if (is.null(fa))
        cli::cli_abort(c(
          "x" = "Cannot combine `HistoricalWeight` for fleets {.val {names(ind)}} in Complex {.val {ComplexNames[i]}}: their values differ ({.val {h}}) and `FleetAllocation` is not set to weight them.",
          "i" = "Set `FleetAllocation(OM)`, or give the fleets the same `HistoricalWeight` before `CombineFleets()`."
        ))
      wt <- fa[ind]
      if (sum(wt) == 0) wt[] <- 1
      sum(h * wt) / sum(wt)
    })
  }
  OM@HistoricalWeight <- HW
  OM
}

# the combined fleet uses the first fleet's implementation error
.CombineFleetsImp <- function(OM, FleetMap) {
  First <- purrr::map_int(FleetMap, \(ind) unname(ind[1]))
  OM@Imp <- purrr::map(OM@Imp, \(ImpList) stats::setNames(ImpList[First], names(FleetMap)))
  OM
}

.CombineFleetsInterimAdvice <- function(OM, FleetList) {
  IA <- OM@InterimAdvice
  if (is.null(IA) || !"Fleet" %in% names(IA)) return(OM)

  IA$Fleet <- as.character(IA$Fleet)
  IA$Fleet[!is.na(IA$Fleet) & !nzchar(IA$Fleet)] <- NA_character_
  cxCol   <- intersect(c("Complex", "Stock"), names(IA))
  Complex <- if (length(cxCol)) as.character(IA[[cxCol[1]]]) else rep("", nrow(IA))
  key     <- paste(Complex, IA$Type, floor(IA$Year), sep = "\r")

  keep   <- rep(TRUE, nrow(IA))
  merged <- list()
  for (i in seq_along(FleetList)) {
    NewName <- names(FleetList)[i]
    Fleets  <- FleetList[[i]]
    inGrp   <- !is.na(IA$Fleet) & IA$Fleet %in% Fleets
    for (k in unique(key[inGrp])) {
      idx  <- which(inGrp & key == k)
      rows <- IA[idx, , drop = FALSE]
      Where <- paste0(if (nzchar(Complex[idx[1]])) paste0("Complex ", Complex[idx[1]], ", "),
                      "Type ", rows$Type[1], ", Year ", floor(rows$Year[1]))
      Abort <- \(reason) cli::cli_abort(c(
        "x" = "Cannot combine `InterimAdvice` rows for fleets {.val {Fleets}} into {.val {NewName}}: {reason}",
        "i" = "{Where}.",
        "i" = "Set `InterimAdvice(OM) <- NULL` before `CombineFleets()`, then supply rows for {.val {NewName}} (or a `Fleet = NA` total) afterwards."
      ))

      Years <- purrr::map(Fleets, \(f) sort(rows$Year[rows$Fleet == f]))
      if (!all(purrr::map_lgl(Years[-1], \(y) isTRUE(all.equal(y, Years[[1]])))) || anyDuplicated(Years[[1]]))
        Abort("every fleet in the group must have one row for each of the same timesteps, or no rows so that a `Fleet = NA` total covers the combined fleet.")

      for (y in Years[[1]]) {
        r <- rows[abs(rows$Year - y) < 1e-8, , drop = FALSE]
        out <- r[1, , drop = FALSE]
        out$Fleet <- NewName
        if (r$Type[1] == "TAC") {
          TACType <- .InterimColumn(r, "TACType", "Removals")
          TACUnit <- .CatchUnitType(.InterimColumn(r, "TACUnit", "Biomass"), arg = "InterimAdvice$TACUnit")
          if (length(unique(TACType)) > 1 || length(unique(TACUnit)) > 1)
            Abort("the fleets' TAC rows differ in `TACType` or `TACUnit`.")
          out$Mean <- sum(r$Mean)
          CV <- if ("CV" %in% names(r)) r$CV else rep(NA_real_, nrow(r))
          if ("CV" %in% names(r))
            out$CV <- if (all(is.na(CV)) || out$Mean == 0) NA_real_ else
              sqrt(sum((ifelse(is.na(CV), 0, CV) * r$Mean)^2)) / out$Mean
          if ("Max" %in% names(r) && !all(is.na(r$Max))) {
            # a deterministic or zero row never exceeds its Mean
            Max <- r$Max
            fixed <- is.na(CV) | CV <= 0 | r$Mean == 0
            Max[fixed] <- r$Mean[fixed]
            out$Max <- if (anyNA(Max)) NA_real_ else sum(Max)
          }
        } else {
          if (any(.InterimColumn(r, "EffType", "Abs") == "Abs"))
            Abort("absolute Effort (`EffType = \"Abs\"`) is not comparable across fleets with different catchability.")
          cols <- intersect(c("Mean", "CV", "Max"), names(r))
          if (any(purrr::map_lgl(cols, \(cl) length(unique(r[[cl]])) > 1)))
            Abort("relative Effort rows must have the same `Mean`, `CV` and `Max` for every fleet.")
        }
        merged[[length(merged) + 1]] <- list(row = out, pos = min(idx))
      }
      keep[idx] <- FALSE
    }
  }
  if (!length(merged)) return(OM)

  IA  <- rbind(IA[keep, , drop = FALSE], do.call(rbind, purrr::map(merged, "row")))
  pos <- c(which(keep), purrr::map_int(merged, "pos"))
  IA  <- IA[order(pos), , drop = FALSE]
  rownames(IA) <- NULL
  OM@InterimAdvice <- IA
  OM
}

.InterimColumn <- function(df, col, default) {
  if (!col %in% names(df)) return(rep(default, nrow(df)))
  v <- as.character(df[[col]])
  v[is.na(v)] <- default
  v
}
