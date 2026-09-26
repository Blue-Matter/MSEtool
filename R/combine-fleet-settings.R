# carry the source fleets' effort, closure, and projected catchability settings onto the combined fleet
.CombineFleetsSettings <- function(NewFleet, Fleets, HistYears, ProjYears) {
  w <- .CombineFleetsWeights(Fleets, HistYears)
  NewFleet@Effort   <- .CombineFleetsEffortSettings(NewFleet@Effort, Fleets, w, HistYears, NewFleet@Name)
  NewFleet@Closure  <- .CombineFleetsClosure(Fleets)
  NewFleet@Catchability@Efficiency <- .CombineFleetsProjEfficiency(
    NewFleet@Catchability@Efficiency, Fleets, w, HistYears, ProjYears)
  NewFleet
}

# each fleet's share of the summed apical F (Sim x Year); equal shares where total F is zero
.CombineFleetsWeights <- function(Fleets, HistYears) {
  apicalF <- purrr::map(Fleets, \(fleet)
    ArrayMultiply(fleet@Effort@Effort, fleet@Catchability@Efficiency) |> .ArraySubsetYear(HistYears))
  total <- Reduce(ArraySum, apicalF)
  zero  <- as.vector(total == 0)
  purrr::map(apicalF, \(apF) {
    wt <- ArrayDivide(apF, total)
    wt[zero] <- 1 / length(apicalF)
    wt
  })
}

.FleetWeightedMean <- function(arrays, w) {
  purrr::map2(arrays, w, \(x, wt) {
    for (nm in setdiff(names(dimnames(x)), names(dimnames(wt))))
      wt <- AddDimension(wt, nm)
    ArrayMultiply(x, wt)
  }) |>
    Reduce(f = ArraySum) |>
    ReduceDims()
}

.CombineFleetsEffortSettings <- function(Effort, Fleets, w, HistYears, Name) {
  Abort <- \(msg) cli::cli_abort(c(
    "x" = "Cannot combine fleets {.val {names(Fleets)}} into {.val {Name}}: {msg}",
    "i" = "Make the setting the same for every fleet (or `NULL`) before `CombineFleets()`, and set it on {.val {Name}} afterwards if needed."
  ))
  Efforts <- purrr::map(Fleets, \(fleet) fleet@Effort)

  # combined effort is in the first fleet's units, since its catchability is used
  Effort@Units <- Efforts[[1]]@Units

  Mode <- unique(purrr::map_chr(Efforts, \(e) e@Mode %||NA% "Density"))
  if (length(Mode) > 1)
    Abort("their spatial utility `Mode` differs ({.val {Mode}}).")
  Effort@Mode <- Mode

  BagLimit <- c("TripsScalar", "AnglerPerTrip", "Theta")
  used <- BagLimit[purrr::map_lgl(BagLimit, \(sl) any(purrr::map_lgl(Efforts, \(e) !is.null(slot(e, sl)))))]
  if (length(used))
    Abort("bag-limit effort settings ({.field {used}}) are defined per fleet and cannot be combined.")

  Distribution <- purrr::map(Efforts, \(e) e@Distribution)
  if (!all(purrr::map_lgl(Distribution, \(d) is.null(d) || all(is.na(d)))))
    Effort@Distribution <- .FleetWeightedMean(Distribution, w)

  Effort@Targeting <- .FleetWeightedMean(
    purrr::map(Efforts, \(e) .AsSimYear(e@Targeting, HistYears)), w)

  Lambda <- purrr::map(Efforts, \(e) e@StockTargetingLambda)
  if (!all(purrr::map_lgl(Lambda, is.null))) {
    Lambda <- purrr::map(Lambda, \(l) .AsSimYear(l %||% 1, HistYears))
    Effort@StockTargetingLambda <- .FleetWeightedMean(Lambda, w)
  }
  Effort
}

# scalar or per-sim value as a Sim x Year array
.AsSimYear <- function(x, HistYears) {
  if (is.array(x) && length(dim(x)) == 2) return(x)
  x <- as.numeric(x)
  array(x, c(length(x), 1), dimnames = list(Sim = seq_along(x), Year = HistYears[1]))
}

# an area is open to the combined fleet if any of its fleets can fish it
.CombineFleetsClosure <- function(Fleets) {
  Closure <- purrr::map(Fleets, \(fleet) fleet@Closure)
  Closure <- Closure[!purrr::map_lgl(Closure, EmptyObject)]
  if (!length(Closure)) return(NULL)
  Reduce(\(a, b) {
    ab <- ArrayExtend(a, b)
    pmax(ab$array1, ab$array2)
  }, Closure) |>
    ReduceDims()
}

# projected catchability (qInc/qCV, applied by Populate) as the F-weighted mean of the fleets' relative change
.CombineFleetsProjEfficiency <- function(Efficiency, Fleets, w, HistYears, ProjYears) {
  hasProj <- purrr::map_lgl(Fleets, \(fleet)
    any(as.numeric(dimnames(fleet@Catchability@Efficiency)$Year) %in% ProjYears))
  if (!length(ProjYears) || !any(hasProj)) return(Efficiency)

  LastYear <- utils::tail(HistYears, 1)
  AllYears <- c(HistYears, ProjYears)
  Relative <- purrr::map2(Fleets, w, \(fleet, wt) {
    q   <- ExtendYears(fleet@Catchability@Efficiency, Years = AllYears)
    rel <- .ArrayOperation(q, .ArraySubsetYear(q, LastYear), `/`)
    rel[!is.finite(rel)] <- 1
    ArrayMultiply(rel, .ArraySubsetYear(wt, LastYear))
  }) |>
    Reduce(f = ArraySum) |>
    .ArraySubsetYear(c(LastYear, ProjYears)) |>
    ExtendYears(Years = AllYears, backfill = TRUE)

  ArrayMultiply(ExtendYears(Efficiency, Years = AllYears), Relative)
}
