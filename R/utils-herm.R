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
  ResolveHermStock <- function(x) {
    if (is.character(x)) {
      ind <- match(x, stock_names)
      if (is.na(ind))
        cli::cli_abort(c("x" = "`Herm` stock {.val {x}} not found.",
                         "i" = "Known stocks are {.val {stock_names}}"))
      return(ind)
    }
    as.integer(x)
  }

  From <- purrr::map_int(HermList, \(h) ResolveHermStock(h@From))
  To   <- purrr::map_int(HermList, \(h) ResolveHermStock(h@To))
  Hazard <- purrr::map(HermList, \(h) {
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
    ExtendYears(hazard, Years = Years)
  })

  list(From = From, To = To, Hazard = Hazard)
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
