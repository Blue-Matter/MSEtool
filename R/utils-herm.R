# Herm / stock transition helpers

#' Convert a Herm `Frac` Curve to a Per-Age Hazard Rate
#'
#' @param Frac `array` or `NULL`. `[sim, age]` or `[sim, age, year]`, values
#'   in `[0, 1]`. `NULL` returns `NULL`.
#' @return An array the same shape as `Frac`: `hazard[,1,] = 0` (no younger
#'   age to compare against); for age `a >= 2`,
#'   `hazard[,a,] = clamp(1 - Frac[,a,]/Frac[,a-1,], 0, 1)`, with the ratio
#'   forced to `1` (full hazard) wherever `Frac[,a-1,] <= 0` (nobody left in
#'   `From` at the previous age, avoiding `0/0`/`x/0`).
#' @keywords internal
.HermHazardRate <- function(Frac) {
  if (is.null(Frac)) return(NULL)

  d <- dim(Frac)
  if (is.null(d) || length(d) < 2L || length(d) > 3L)
    cli::cli_abort("`Frac` must be a `[sim, age]` or `[sim, age, year]` array")

  nAge  <- d[2L]
  nYear <- if (length(d) == 3L) d[3L] else 1L

  Hazard <- array(0, dim = d, dimnames = dimnames(Frac))
  if (nAge < 2L) return(Hazard)

  for (y in seq_len(nYear)) {
    prev <- if (length(d) == 3L) Frac[, 1L, y] else Frac[, 1L]
    for (a in 2:nAge) {
      cur <- if (length(d) == 3L) Frac[, a, y] else Frac[, a]
      h <- 1 - cur / prev
      h[!is.finite(h)] <- 1  # Frac[a-1] <= 0: nobody left in `From`, full hazard
      h <- pmin(pmax(h, 0), 1)
      if (length(d) == 3L) Hazard[, a, y] <- h else Hazard[, a] <- h
      prev <- cur
    }
  }
  Hazard
}

#' Convert a Herm `FracAtLength` Curve to `Frac` at Age
#'
#' Passes `FracAtLength` through the age-length key of the `From` stock,
#' using the same at-length to at-age conversion as the other at-length
#' schedules (e.g. `Maturity@MeanAtLength`).
#'
#' @param Herm A [stocktransition-class] object with `FracAtLength` set.
#' @param Stock The populated `From` [stock-class] object.
#' @param Years Numeric vector of model years.
#' @return A `[Sim, Age, Year]` array of the fraction remaining in `From` by
#'   age, values in `[0, 1]`.
#' @keywords internal
.HermFracAtLength2AtAge <- function(Herm, Stock, Years) {
  Length <- Stock@Length
  if (EmptyObject(Length) || is.null(Length@ALK))
    cli::cli_abort(c(
      "x" = "{.field FracAtLength} requires a populated age-length key for stock {.val {Stock@Name}}.",
      "i" = "Specify {.field Length} for stock {.val {Stock@Name}} and populate the OM with {.fn Populate}."
    ), call = NULL)

  # carry `FracAtLength` in an at-size object to reuse the at-length conversion
  Carrier <- methods::new('maturity', MeanAtLength = Herm@FracAtLength,
                          Classes = Herm@Classes)
  Carrier <- .NameAtSize(Carrier, 'MeanAtLength', Length, Years)
  Frac    <- .AtSize2AtAge(Carrier, Length, allow_shortcut = FALSE)@MeanAtAge
  Frac[]  <- pmin(pmax(Frac, 0), 1)
  Frac
}

# warn when a `Frac` converted from `FracAtLength` increases with age; no
# individuals move back to `From`, so these ages get a hazard of 0
.CheckHermFracDecreasing <- function(Frac, From, To, tol = 1e-3) {
  Increase <- apply(Frac, setdiff(seq_along(dim(Frac)), 2), \(x) max(diff(x)))
  if (!any(Increase > tol, na.rm = TRUE)) return(invisible())
  cli::cli_warn(c(
    "!" = "{.field Frac} converted from {.field FracAtLength} for transition {.val {From}} -> {.val {To}} increases with age.",
    "i" = "Individuals do not move back to {.val {From}}, so these ages have no transition, and the fraction remaining in {.val {From}} will differ from {.field Frac}.",
    "i" = "This can happen when the CV of length-at-age increases steeply with age. See {.help MSEtool::Herm}."
  ), call = NULL)
  invisible()
}

#' Populate `Frac` for Length-Based `om@Herm` Transitions
#'
#' Converts `FracAtLength` to `Frac` for each [stocktransition-class] object
#' in `OM@Herm` that has `FracAtLength` set, replacing any existing `Frac`.
#' Requires populated stocks.
#'
#' @param OM An [om-class] object with populated stocks.
#' @param silent Logical. If `TRUE`, suppress warnings about increasing `Frac`.
#' @return `OM` with `Frac` populated.
#' @keywords internal
.PopulateHerm <- function(OM, silent = FALSE) {
  HermList <- OM@Herm
  if (!length(HermList)) return(OM)
  IsSingle <- inherits(HermList, 'stocktransition')
  if (IsSingle) HermList <- list(HermList)

  StockNms <- StockNames(OM)
  Years    <- Years(OM)
  for (i in seq_along(HermList)) {
    h <- HermList[[i]]
    if (is.null(h@FracAtLength)) next
    From   <- .HermStockIndex(h@From, StockNms)
    h@Frac <- .HermFracAtLength2AtAge(h, OM@Stock[[From]], Years)
    if (!silent)
      .CheckHermFracDecreasing(h@Frac, StockNms[From],
                               StockNms[.HermStockIndex(h@To, StockNms)])
    HermList[[i]] <- h
  }
  OM@Herm <- if (IsSingle) HermList[[1]] else HermList
  OM
}

# 1-based stock index of a `Herm` `From`/`To` stock given by name or index
.HermStockIndex <- function(x, StockNames) {
  if (is.character(x)) {
    ind <- match(x, StockNames)
    if (is.na(ind))
      cli::cli_abort(c("x" = "`Herm` stock {.val {x}} not found.",
                       "i" = "Known stocks are {.val {StockNames}}"))
    return(ind)
  }
  as.integer(x)
}

#' Resolve `om@Herm` Pairs to Stock Indices and Broadcast Hazard Arrays
#'
#' @param OM An [om-class] object.
#' @param Years Numeric vector of years the hazard arrays must cover.
#' @return A list with `From`, `To` (integer vectors, one per pair, 1-based
#'   stock index) and `Hazard` (list of `[sim, age, year]` arrays, one per
#'   pair). All empty if `OM@Herm` is `NULL`/empty.
#' @keywords internal
.HermResolvePairs <- function(OM, Years) {
  HermList <- OM@Herm
  if (inherits(HermList, 'stocktransition')) HermList <- list(HermList)
  if (!length(HermList))
    return(list(From = integer(0), To = integer(0), Hazard = list()))

  stock_names <- StockNames(OM)

  From <- purrr::map_int(HermList, \(h) .HermStockIndex(h@From, stock_names))
  To   <- purrr::map_int(HermList, \(h) .HermStockIndex(h@To, stock_names))
  Hazard <- purrr::map2(HermList, From, \(h, st) {
    if (is.null(h@Frac) && !is.null(h@FracAtLength))
      h@Frac <- .HermFracAtLength2AtAge(h, OM@Stock[[st]], Years(OM))
    hazard <- .HermHazardRate(h@Frac)
    d   <- dim(hazard)
    hdn <- dimnames(hazard)

    sim_nm <- if (!is.null(hdn) && !is.null(hdn[[1]])) hdn[[1]] else seq_len(d[1])
    age_nm <- if (!is.null(hdn) && length(hdn) >= 2 && !is.null(hdn[[2]])) hdn[[2]] else seq_len(d[2])

    if (is.null(hdn) || !'Year' %in% names(hdn)) {
      hazard <- array(hazard, dim = c(d, 1),
                      dimnames = list(Sim = sim_nm, Age = age_nm, Year = Years[1]))
    } else {
      dimnames(hazard) <- list(Sim = sim_nm, Age = age_nm, Year = hdn[[3]])
    }
    hazard <- ExtendSims(hazard, nSim = OM@nSim)
    ExtendYears(hazard, Years = Years)
  })

  list(From = From, To = To, Hazard = Hazard)
}

# warn when the plus-group hazard of a `From` stock is strictly between 0 and 1
# and more than `MinFrac` of `From` remains at the plus group
.CheckHermPlusGroup <- function(OM, Resolved, tol = 1e-8, MinFrac = 1e-3) {
  StockNms <- StockNames(OM)
  HermList <- OM@Herm
  if (inherits(HermList, 'stocktransition')) HermList <- list(HermList)
  for (i in seq_along(Resolved$From)) {
    st <- Resolved$From[i]
    if (!isTRUE(OM@Stock[[st]]@Ages@PlusGroup)) next
    Hazard <- Resolved$Hazard[[i]]
    PlusGroup <- Hazard[, dim(Hazard)[2], ]
    # length-based: plus-group transitions are expected as fish keep growing
    if (!is.null(HermList[[i]]@FracAtLength)) next
    Frac <- HermList[[i]]@Frac
    if (is.null(Frac)) next
    FracPlus <- apply(Frac, setdiff(seq_along(dim(Frac)), 2), \(x) x[length(x)])
    if (!any(PlusGroup > tol & PlusGroup < 1 - tol, na.rm = TRUE) ||
        !any(FracPlus > MinFrac, na.rm = TRUE)) next
    cli::cli_warn(c(
      "!" = "Part of the plus group of stock {.val {StockNms[st]}} transitions to stock {.val {StockNms[Resolved$To[i]]}} each year.",
      "i" = "The split of the plus group between the two stocks then changes with fishing.",
      "i" = "To stop transitions within the plus group, set {.code Frac} at the plus group equal to {.code Frac} at the previous age, or to 0. See {.help MSEtool::Herm}."
    ), call = NULL)
  }
  invisible()
}

#' Connected Components of the Herm Pair Graph
#'
#' @param From,To Integer vectors of 1-based stock indices, one per pair.
#' @param nStock Total number of stocks.
#' @return A list of integer vectors partitioning `1:nStock`.
#' @keywords internal
.HermConnectedComponents <- function(From, To, nStock) {
  parent <- seq_len(nStock)
  find <- function(x) {
    while (parent[x] != x) x <- parent[x]
    x
  }
  for (i in seq_along(From)) {
    ra <- find(From[i]); rb <- find(To[i])
    if (ra != rb) parent[rb] <- ra
  }
  roots <- vapply(seq_len(nStock), find, integer(1))
  unname(split(seq_len(nStock), roots))
}
