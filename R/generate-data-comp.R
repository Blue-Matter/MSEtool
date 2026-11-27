#' Generate Composition Data
#'
#' Internal functions that generate simulated age and size composition data
#' for fleets (`LandingsAtAge`, `DiscardsAtAge`, `LandingsAtSize`,
#' `DiscardsAtSize`) and surveys (`SurveyAtAge`, `SurveyAtSize`):
#' `.GenHistDataComp()` for the historical period of one simulation, and
#' `.GenProjDataComp()` for one projection year of every simulation.
#'
#' @param x Integer index of the simulation replicate.
#' @param Data A `data` object of the stock complex.
#' @param Hist,Proj A `hist` class object.
#' @param HistYears Numeric vector of historical years.
#' @param DataYear Numeric. The calendar year to generate data for.
#' @param YearsAll Numeric vector of historical and projection years.
#' @param i Integer index of the stock complex.
#' @param stocks Integer vector of stock indices in the complex.
#' @param nSim Integer. Number of simulation replicates.
#' @param type Character. The composition slot.
#'
#' @details
#' ## Elements
#'
#' A composition slot holds one [compdata-class] object or a list named by the
#' complex and/or its stocks (see [data-class]). Real historical data are
#' returned unchanged. Otherwise the elements follow the composition slots of
#' the `obs` objects: a single [compobs-class] object generates data for all
#' stocks of the complex combined, and a list named by stock generates data
#' for each stock. In projection years the elements of the historical data are
#' extended, and a single [compobs-class] object applies to every element.
#'
#' ## True composition
#'
#' The true composition of a fleet is the catch-at-age or catch-at-size of the
#' element's stocks, summed over stocks and areas. The true composition of a
#' survey is the survey's selected numbers-at-age (and, for size, multiplied by
#' the age-length key) of the element's stocks.
#'
#' ## Sampling
#'
#' For each year with a `SampleSize`, a Dirichlet-multinomial sample of size
#' `SampleSize` is drawn with concentration
#' \eqn{\alpha_b = \mathrm{ESS} \cdot \Theta \cdot q_b \cdot \exp(\mathrm{Shift}_b)},
#' where \eqn{q} is the true composition normalised to proportions. A fleet or
#' survey whose [compobs-class] objects are `Joint` in several stock elements
#' is drawn as one sample over the classes of all those elements, with the
#' `SampleSize`, `ESS`, and `Theta` of the first.
#'
#' In projection years, values of `OM@Data` for `DataYear` are used in place
#' of simulated values, and no composition is drawn when the gap between the
#' last historical observation and the last historical year exceeds
#' `MaxGap`.
#'
#' @return `.GenHistDataComp()` returns the composition slot of `Data`.
#'   `.GenProjDataComp()` returns a list of length `nSim` of composition slots
#'   with `DataYear` appended.
#'
#' @seealso [CompObs()], [CompData()], [rDirichletMultinomial()]
#' @keywords internal
#' @name GenDataComp
NULL

.CompIsAge    <- function(type) endsWith(type, "AtAge")
.CompIsSurvey <- function(type) startsWith(type, "Survey")

# fleet or survey names a composition type is generated for
.CompNames <- function(OM, i, type) {
  Fleets <- FleetNames(OM)
  if (!.CompIsSurvey(type)) return(Fleets)
  setdiff(names(OM@Obs[[i]]), Fleets)
}

.CompUnitsName <- function(type) if (.CompIsAge(type)) "years" else NULL

# Matrix [From x To] assigning the OM classes `From` to the composition bins
# `To` (lower bounds). The first bin also holds the classes below it and the
# last bin the classes above it. Ages go to the bin they fall in; a size class
# is split across bins in proportion to its overlap with each.
.CompBinMap <- function(From, To, isAge) {
  From <- as.numeric(From)
  To   <- as.numeric(To)
  nTo  <- length(To)
  Map  <- matrix(0, length(From), nTo)
  if (isAge) {
    Map[cbind(seq_along(From), pmax(findInterval(From + 1e-8, To), 1))] <- 1
    return(Map)
  }
  Width <- if (length(From) > 1) diff(From) else 1
  Upper <- From + c(Width, Width[length(Width)])
  Lo    <- c(-Inf, To[-1])
  Hi    <- c(To[-1], Inf)
  for (b in seq_len(nTo))
    Map[, b] <- pmax(0, pmin(Upper, Hi[b]) - pmax(From, Lo[b])) / (Upper - From)
  Map
}

# composition [Sim x Bin x Year] on the OM classes mapped to the bins `To`
.MapCompBins <- function(Comp, To, isAge) {
  From <- as.numeric(dimnames(Comp)[[2]])
  if (is.null(To) || (length(From) == length(To) && all(abs(From - as.numeric(To)) < 1e-8)))
    return(Comp)
  Map <- .CompBinMap(From, To, isAge)
  d   <- dim(Comp)
  dn  <- dimnames(Comp)
  Out <- matrix(aperm(Comp, c(1, 3, 2)), d[1] * d[3], d[2]) %*% Map
  Out <- aperm(array(Out, c(d[1], d[3], length(To))), c(1, 3, 2))
  dn[[2]] <- as.character(To)
  dimnames(Out) <- dn
  Out
}

# first and last reported bin of each year of observed counts [Year x Bin]; NA
# for years with none
.CompTailRange <- function(Counts) {
  t(apply(Counts, 1, \(x) {
    Reported <- which(!is.na(x))
    if (!length(Reported)) return(c(NA_integer_, NA_integer_))
    range(Reported)
  }))
}

# first and last reported bin of the last year with observations of column
# `Col`; all bins when there are none
.CompLastRange <- function(CompData, Col, nBin) {
  Value  <- CompData@Value[, Col, seq_len(nBin), drop = FALSE]
  Range  <- .CompTailRange(matrix(Value, dim(Value)[1], nBin))
  Last   <- which(!is.na(Range[, 1]))
  if (!length(Last)) return(c(1L, nBin))
  Range[max(Last), ]
}

# composition [Sim x Bin x Year] with the classes below (above) each year's
# reported range `Range` [Year x 2] added to its first (last) bin, and zero outside
.LumpCompTails <- function(Comp, Range) {
  n <- dim(Comp)[2]
  for (y in seq_len(dim(Comp)[3])) {
    r <- Range[y, ]
    if (anyNA(r)) next
    if (r[1] > 1) {
      Below <- seq_len(r[1] - 1)
      Comp[, r[1], y] <- Comp[, r[1], y] + rowSums(Comp[, Below, y, drop = FALSE])
      Comp[, Below, y] <- 0
    }
    if (r[2] < n) {
      Above <- (r[2] + 1):n
      Comp[, r[2], y] <- Comp[, r[2], y] + rowSums(Comp[, Above, y, drop = FALSE])
      Comp[, Above, y] <- 0
    }
  }
  Comp
}

# true composition of `Name` summed over `stocks` and areas: [Sim x Bin x Year],
# on the OM classes or, with `Classes`, on those composition bins
.TrueComp <- function(Hist, type, stocks, Name, Years, Sims, Classes = NULL) {
  Comp <- .TrueCompOM(Hist, type, stocks, Name, Years, Sims)
  if (is.null(Comp))
    return(NULL)
  .MapCompBins(Comp, Classes, .CompIsAge(type))
}

.TrueCompOM <- function(Hist, type, stocks, Name, Years, Sims) {
  if (.CompIsSurvey(type))
    return(.TrueSurveyComp(Hist, type, stocks, Name, Years, Sims))
  fl <- match(Name, FleetNames(Hist@OM))
  PerStock <- purrr::map(slot(Hist, type)[stocks], \(Stock) {
    arr <- if (.CompIsAge(type) || is.null(Stock) || fl > length(Stock)) Stock else Stock[[fl]]
    if (is.null(arr)) return(NULL)
    yr  <- match(round(as.numeric(Years), 6), round(as.numeric(dimnames(arr)$Year), 6))
    sim <- pmin(Sims, dim(arr)[1])
    out <- if (.CompIsAge(type)) {
      abind::adrop(arr[sim, , yr, fl, , drop = FALSE], 4)
    } else {
      arr[sim, , yr, , drop = FALSE]
    }
    SumOverArea(out)
  })
  if (!length(PerStock) || any(purrr::map_lgl(PerStock, is.null)))
    return(NULL)
  if (.CompIsAge(type) && length(PerStock) > 1)
    PerStock <- .AlignAgeDim(PerStock)
  PerStock |> List2Array('Stock') |> SumOverStock()
}

# true survey composition of `Name` summed over `stocks`: [Sim x Bin x Year].
# Selected numbers-at-age of the survey; at size, multiplied by the age-length key
.TrueSurveyComp <- function(Hist, type, stocks, Name, Years, Sims) {
  i        <- which(purrr::map_lgl(Hist@OM@Complexes, \(cx) stocks[1] %in% cx))[1]
  IndexObs <- Hist@OM@Obs[[i]][[Name]]@Survey
  if (is.list(IndexObs))
    IndexObs <- IndexObs[[c(intersect(StockNames(Hist@OM)[stocks], names(IndexObs)), names(IndexObs))[1]]]
  Areas  <- IndexObs@Areas %||NA% seq_len(nArea(Hist))
  Timing <- .SurveyTiming(Hist@OM, i, Name)
  isAge  <- .CompIsAge(type)

  BySim <- purrr::map(Sims, \(x) {
    Selected <- .CalcSurveySelectedN(Hist@Number[stocks], Hist, stocks, Name, IndexObs,
                                     Years, sim = x, timing = Timing)
    PerStock <- purrr::map2(Selected, stocks, \(num, st) {
      AtAge <- SumOverArea(num[, , Areas, drop = FALSE])
      if (isAge) return(AtAge)
      ALK <- Hist@OM@Stock[[st]]@Length@ALK
      ALK <- .ArraySubsetYear(ALK[min(x, dim(ALK)[1]), , , , drop = FALSE], Years) |> abind::adrop(1)
      Out <- vapply(seq_along(Years), \(y) as.numeric(crossprod(ALK[, , y], AtAge[, y])),
                    numeric(dim(ALK)[2]))
      array(Out, c(dim(ALK)[2], length(Years)),
            list(Class = dimnames(ALK)[[2]], Year = Years))
    })
    if (isAge && length(PerStock) > 1)
      PerStock <- .AlignAgeDim(PerStock)
    Reduce(`+`, PerStock)
  })
  Out <- abind::abind(BySim, along = 0)
  names(dimnames(Out)) <- c('Sim', if (isAge) 'Age' else 'Class', 'Year')
  dimnames(Out)$Sim <- Sims
  Out
}

# timing of survey `Name` in `OM@Data`; NA (start of the time step) if absent
.SurveyTiming <- function(OM, i, Name) {
  if (length(OM@Data) < i) return(NA_real_)
  for (Survey in .SlotElements(OM@Data[[i]]@Survey, "Complex")) {
    k <- match(Name, Survey@Name %||% colnames(Survey@Value))
    if (!is.na(k) && length(Survey@Timing) >= k) return(Survey@Timing[k])
  }
  NA_real_
}

# Dirichlet-multinomial draw of one composition; NULL when nothing is sampled
.SampleComp <- function(True, SampleSize, ESS, Theta, Shift, SeedKey = NULL) {
  if (is.na(SampleSize) || SampleSize == 0) return(NULL)
  Total <- sum(True, na.rm = TRUE)
  if (is.na(Total) || Total == 0) return(NULL)
  alpha <- ESS * Theta * True / Total * exp(Shift)
  if (any(is.na(alpha)) || sum(alpha) == 0) return(NULL)
  if (is.null(SeedKey))
    return(rDirichletMultinomial(n = round(SampleSize), alpha = alpha))
  .SeededDirichletMultinomial(SeedKey, n = round(SampleSize), alpha = alpha)
}

# sampling units of one fleet or survey: each element alone, or the Joint stock elements together
.CompSampleUnits <- function(Elements, IsJoint, ComplexName) {
  Joint <- IsJoint & Elements != ComplexName
  Units <- as.list(Elements[!Joint])
  if (sum(Joint) > 1) return(c(Units, list(Elements[Joint])))
  c(Units, as.list(Elements[Joint]))
}

# per-sim values of a [Sim x Year] obs slot in `Years`
.CompObsYears <- function(x, Years, Sims, Default) {
  if (is.null(x)) return(matrix(Default, length(Sims), length(Years)))
  x <- .SubsetYear(x, Years)
  x[pmin(Sims, nrow(x)), , drop = FALSE]
}

.CompClasses <- function(CompData, Name) {
  Classes <- CompData@Classes
  if (is.list(Classes)) Classes[[Name]] else Classes
}

.GenHistDataComp <- function(x, Data, Hist, HistYears, i, stocks, type) {
  Current <- slot(Data, type)
  if (!.EmptySlot(Current))
    return(Current)

  Complex  <- names(Hist@OM@Complexes)[i]
  StockNms <- StockNames(Hist@OM)
  Names    <- .CompNames(Hist@OM, i, type)
  ObsSlots <- purrr::map(Names, \(nm) .SlotElements(slot(Hist@OM@Obs[[i]][[nm]], type), Complex)) |>
    stats::setNames(Names)
  if (all(purrr::map_lgl(ObsSlots, \(o) all(purrr::map_lgl(o, isNewObject)))))
    return(Current)

  isAge    <- .CompIsAge(type)
  nTS      <- length(HistYears)
  Elements <- .SlotElementNames(Current, lapply(Names, \(nm) slot(Hist@OM@Obs[[i]][[nm]], type)), Complex)

  True <- list()
  for (el in Elements) {
    elStocks <- .ElementStocks(el, Complex, StockNms, stocks)
    elNames  <- Names[purrr::map_lgl(ObsSlots, \(o) el %in% names(o))]
    True[[el]] <- purrr::map(elNames, \(nm) {
      Tr <- .TrueComp(Hist, type, elStocks, nm, HistYears, x)
      if (!is.null(Tr)) abind::adrop(Tr, 1)
    }) |>
      stats::setNames(elNames) |>
      purrr::compact()
    if (!isAge && all(purrr::map_lgl(True[[el]], \(a) all(a == 0, na.rm = TRUE))))
      True[[el]] <- NULL
  }
  True <- purrr::compact(True)
  if (!length(True))
    return(Current)

  Value <- purrr::map(True, \(byName) {
    if (isAge) {
      Bins <- dimnames(byName[[1]])[[1]]
      return(array(0, c(nTS, length(byName), length(Bins)),
                   list(Year = HistYears, Fleet = names(byName), Age = Bins)))
    }
    Padded <- .PadClassesByFleet(byName, HistYears)
    for (nm in names(byName))
      Padded$Value[, nm, seq_along(Padded$Classes[[nm]])] <- 0
    Padded
  })
  Joint <- purrr::map(True, \(byName) logical(0))

  for (nm in Names) {
    ElObs <- ObsSlots[[nm]][intersect(names(ObsSlots[[nm]]), names(True))]
    ElObs <- ElObs[purrr::map_lgl(names(ElObs), \(el) nm %in% names(True[[el]]))]
    if (!length(ElObs)) next
    Units <- .CompSampleUnits(names(ElObs), purrr::map_lgl(ElObs, .IsJointObs), Complex)

    for (Unit in Units) {
      Obs1 <- ElObs[[Unit[1]]]
      if (EmptyObject(Obs1) || is.null(Obs1@SampleSize)) next
      SampleSize <- .CompObsYears(Obs1@SampleSize, HistYears, x, NA)[1, ]
      ESS   <- if (is.null(Obs1@ESS)) SampleSize else .CompObsYears(Obs1@ESS, HistYears, x, NA)[1, ]
      Theta <- .CompObsYears(Obs1@Theta, HistYears, x, 1)[1, ]
      nBin  <- purrr::map_int(Unit, \(el) nrow(True[[el]][[nm]])) |> stats::setNames(Unit)
      Shift <- purrr::map(Unit, \(el) {
        Sh <- ElObs[[el]]@Shift
        if (is.null(Sh)) return(matrix(0, nTS, nBin[[el]]))
        Sh <- .SubsetYear(Sh, HistYears)
        Sh[min(x, dim(Sh)[1]), , seq_len(nBin[[el]]), drop = FALSE] |> abind::adrop(1)
      }) |> stats::setNames(Unit)
      Split <- rep(Unit, nBin)

      for (yr in seq_len(nTS)) {
        TrueYr  <- unlist(purrr::map(Unit, \(el) True[[el]][[nm]][, yr]), use.names = FALSE)
        ShiftYr <- unlist(purrr::map(Unit, \(el) Shift[[el]][yr, ]), use.names = FALSE)
        Draw <- .SampleComp(TrueYr, SampleSize[yr], ESS[yr], Theta[yr], ShiftYr)
        if (is.null(Draw)) next
        for (el in Unit) {
          if (isAge) {
            Value[[el]][yr, nm, ] <- Draw[Split == el]
          } else {
            Value[[el]]$Value[yr, nm, seq_len(nBin[[el]])] <- Draw[Split == el]
          }
        }
      }
      if (length(Unit) > 1)
        for (el in Unit) Joint[[el]][nm] <- TRUE
    }
  }

  Out <- purrr::imap(Value, \(v, el) {
    elStocks <- .ElementStocks(el, Complex, StockNms, stocks)
    CompData <- new('compdata')
    CompData@Name    <- names(True[[el]])
    CompData@Value   <- if (isAge) v else v$Value
    CompData@Classes <- if (isAge) as.numeric(dimnames(v)$Age) else v$Classes
    CompData@Units   <- .CompUnitsName(type) %||% Hist@OM@Stock[[elStocks[1]]]@Length@Units
    CompData@Joint   <- Joint[[el]]
    CompData
  })
  .ElementsToSlot(Out, Complex)
}

.GenProjDataComp <- function(Proj, DataYear, YearsAll, i, stocks, nSim, type) {
  Complex <- names(Proj@OM@Complexes)[i]
  Slot1   <- .SlotElements(slot(Proj@Data[[1]][[i]], type), Complex)
  Active  <- names(Slot1)[!purrr::map_lgl(Slot1, \(cd)
    EmptyObject(cd) || DataYear %in% dimnames(cd@Value)[[1]])]
  if (!length(Active))
    return(purrr::map(Proj@Data, \(DataList) slot(DataList[[i]], type)))

  isAge        <- .CompIsAge(type)
  StockNms     <- StockNames(Proj@OM)
  LastHistYear <- max(Years(Proj@OM, 'Historical'))
  omSlot       <- if (length(Proj@OM@Data) >= i) .SlotElements(slot(Proj@OM@Data[[i]], type), Complex)
  KeyEl        <- !(length(Slot1) == 1 && identical(names(Slot1), Complex))

  ElNames <- purrr::map(Slot1[Active], .ResolveFleetNames)
  New <- purrr::map(Active, \(el) array(NA_real_, c(nSim, length(ElNames[[el]]), dim(Slot1[[el]]@Value)[3]))) |>
    stats::setNames(Active)

  for (nm in unique(unlist(ElNames))) {
    if (is.null(Proj@OM@Obs[[i]][[nm]])) next
    ObsSlot <- slot(Proj@OM@Obs[[i]][[nm]], type)
    ElsNm   <- Active[purrr::map_lgl(Active, \(el) nm %in% ElNames[[el]])]
    ElObs   <- purrr::map(ElsNm, \(el) .ElementObs(ObsSlot, el)) |> stats::setNames(ElsNm)
    ElObs   <- ElObs[!purrr::map_lgl(ElObs, is.null)]
    if (!length(ElObs)) next
    Units <- .CompSampleUnits(names(ElObs), purrr::map_lgl(ElObs, .IsJointObs), Complex)

    for (Unit in Units) {
      Obs1 <- ElObs[[Unit[1]]]
      if (EmptyObject(Obs1) || is.null(Obs1@SampleSize)) next
      Col  <- purrr::map_int(Unit, \(el) match(nm, ElNames[[el]])) |> stats::setNames(Unit)
      nBin <- purrr::map_int(Unit, \(el) length(.CompClasses(Slot1[[el]], nm))) |> stats::setNames(Unit)

      omRow <- purrr::map(Unit, \(el) {
        omEl <- omSlot[[el]]
        if (is.null(omEl) || EmptyObject(omEl)) return(NULL)
        Row    <- .ObjectYearRow(omEl, DataYear)
        omCol  <- match(nm, .ResolveFleetNames(omEl))
        if (is.na(Row) || is.na(omCol)) return(NULL)
        omEl@Value[Row, omCol, seq_len(nBin[[el]])]
      }) |> stats::setNames(Unit)
      if (!any(purrr::map_lgl(omRow, is.null))) {
        for (el in Unit)
          New[[el]][, Col[[el]], seq_len(nBin[[el]])] <- matrix(omRow[[el]], nSim, nBin[[el]], byrow = TRUE)
        next
      }

      SampleSize <- .CompObsYears(Obs1@SampleSize, DataYear, seq_len(nSim), NA)[, 1]
      ESS   <- if (is.null(Obs1@ESS)) SampleSize else .CompObsYears(Obs1@ESS, DataYear, seq_len(nSim), NA)[, 1]
      Theta <- .CompObsYears(Obs1@Theta, DataYear, seq_len(nSim), 1)[, 1]
      # reported bins of the last observed year; its first and last hold the tails
      Range <- purrr::map(Unit, \(el) .CompLastRange(Slot1[[el]], Col[[el]], nBin[[el]])) |>
        stats::setNames(Unit)
      InRange <- purrr::map(Range, \(r) seq(r[1], r[2]))
      True  <- purrr::map(Unit, \(el) {
        elStocks <- .ElementStocks(el, Complex, StockNms, stocks)
        Tr <- .TrueComp(Proj, type, elStocks, nm, DataYear, seq_len(nSim),
                        Classes = .CompClasses(Slot1[[el]], nm))
        if (is.null(Tr)) return(NULL)
        Tr <- .LumpCompTails(Tr[, seq_len(nBin[[el]]), , drop = FALSE], matrix(Range[[el]], 1))
        matrix(Tr[, InRange[[el]], 1], nSim)
      }) |> stats::setNames(Unit)
      if (any(purrr::map_lgl(True, is.null))) next
      Split <- rep(Unit, lengths(InRange))

      for (x in seq_len(nSim)) {
        if (is.na(SampleSize[x]) || SampleSize[x] == 0) next
        SimEl <- .SlotElements(slot(Proj@Data[[x]][[i]], type), Complex)[[Unit[1]]]
        if (.CompExceedsMaxGap(SimEl, Col[[Unit[1]]], Obs1, LastHistYear)) next
        TrueX  <- unlist(purrr::map(Unit, \(el) True[[el]][x, ]), use.names = FALSE)
        ShiftX <- unlist(purrr::map(Unit, \(el)
          .CompShiftAt(ElObs[[el]]@Shift, x, DataYear, nBin[[el]])[InRange[[el]]]), use.names = FALSE)
        Key <- paste(Proj@OM@Seed, DataYear, i, Col[[Unit[1]]], type, .GlobalSim(Proj@OM, x), sep = "_")
        if (KeyEl) Key <- paste(Key, paste(Unit, collapse = "+"), sep = "_")
        Draw <- .SampleComp(TrueX, SampleSize[x], ESS[x], Theta[x], ShiftX, Key)
        if (is.null(Draw)) next
        for (el in Unit)
          New[[el]][x, Col[[el]], InRange[[el]]] <- Draw[Split == el]
      }
    }
  }

  purrr::map(seq_len(nSim), \(x) {
    SlotX <- .SlotElements(slot(Proj@Data[[x]][[i]], type), Complex)
    for (el in Active) {
      CompData <- SlotX[[el]]
      BinName  <- names(dimnames(CompData@Value))[3] %||% if (isAge) "Age" else "Class"
      NewValue <- array(NA_real_, dim = c(1L, length(ElNames[[el]]), dim(New[[el]])[3]),
                        dimnames = stats::setNames(list(DataYear, ElNames[[el]], dimnames(CompData@Value)[[3]]),
                                                   c("Year", "Fleet", BinName)))
      NewValue[1, , ] <- New[[el]][x, , ]
      CompData@Value <- abind::abind(CompData@Value, NewValue, along = 1, use.dnns = TRUE)
      SlotX[[el]] <- CompData
    }
    .ElementsToSlot(SlotX, Complex)
  })
}

.AlignAgeDim <- function(arr_list, dim_name = "Age") {

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
