#' Validate `Shift` for Composition Data
#'
#' Diagnostic plot comparing three catch-at-age or catch-at-size
#' proportions-at-bin curves: the true (OM-predicted) composition, the
#' composition expected once [CompObs()]'s `Shift` slot is applied to the
#' true composition, and the simulated observed composition actually drawn
#' by the Dirichlet-Multinomial sampler. Useful for confirming that a
#' configured `Shift` tilts the sampled composition in the expected
#' direction (e.g. towards larger sizes or older ages) before running a full
#' MSE.
#'
#' @param Hist A [hist-class] object, i.e. the output of [Simulate()].
#' @param sim Integer. Simulation replicate to plot.
#' @param i Integer or character. Stock complex index or name (as in
#'   `names(Hist@OM@Complexes)`). Default `1`.
#' @param type Character. One of `"LandingsAtSize"`, `"DiscardsAtSize"`,
#'   `"LandingsAtAge"`, or `"DiscardsAtAge"`.
#' @param fleets Character vector of fleet names to include, or `NULL`
#'   (default) to include all fleets with non-empty `Shift`.
#' @param years Numeric vector of calendar years to include, or `NULL`
#'   (default) to use the last `nYears` historical years. 
#' @param nYears Integer. Number of most recent historical years to plot
#'   when `years = NULL`. Default `4`. Ignored if `years` is supplied.
#'
#' @details
#' The true composition q is `Hist@LandingsAtSize`/`@LandingsAtAge` (or
#' the `Discards` equivalent), summed over stocks and areas within complex
#' `i`, and normalised to proportions within each fleet and year.
#'
#' The expected shifted composition is computed directly from `Shift`
#' (`Hist@OM@Obs[[i]][[fleet]]@<type>@Shift`) as
#' \eqn{q_b' = q_b \exp(\mathrm{Shift}_b) / \sum_b q_b \exp(\mathrm{Shift}_b)}.
#' This is the mean of the Dirichlet concentration vector used to generate
#' the observed draw (`ESS` and `Theta` scale the concentration but cancel
#' out of its mean, so they don't affect this curve).
#'
#' The observed composition is the corresponding slot of
#' `Hist@Data[[sim]][[i]]`, i.e. the actual noisy Dirichlet-Multinomial draw,
#' also normalised to proportions. All three series are drawn from the same
#' replicate `sim`.
#'
#' @return A `ggplot` object (invisibly plotted), faceted by `Fleet` and
#'   `Year`, with the true, expected-shifted, and observed proportions
#'   overlaid as lines.
#'
#' @export
PlotCompShift <- function(Hist,
                          sim    = 1,
                          i      = 1,
                          type   = c("LandingsAtSize", "DiscardsAtSize",
                                     "LandingsAtAge", "DiscardsAtAge"),
                          fleets = NULL,
                          years  = NULL,
                          nYears = 4) {

  .CheckClass(Hist, "hist", "Hist")
  type   <- match.arg(type)
  isSize <- grepl("Size", type)

  Complexes <- Hist@OM@Complexes
  if (is.character(i)) {
    if (!i %in% names(Complexes))
      cli::cli_abort("`i = {.val {i}}` not found in `names(Hist@OM@Complexes)`.")
    i <- match(i, names(Complexes))
  }
  stocks     <- Complexes[[i]]
  FleetNames <- FleetNames(Hist@OM)
  HistYears  <- Years(Hist, "H")

  if (is.null(years))
    years <- utils::tail(HistYears, nYears)

  DataObj <- Hist@Data[[min(sim, length(Hist@Data))]][[i]]
  ObsComp <- slot(DataObj, type)
  if (EmptyObject(ObsComp))
    cli::cli_abort(
      c("x" = "`Hist@Data[[{sim}]][[{i}]]@{type}` is empty.",
        "i" = "Composition data must be simulated (via a non-empty `CompObs()` with `SampleSize` set) before `Shift` can be validated.")
    )

  # fleets with a configured Shift, restricted to requested fleets
  ObsSlot   <- lapply(Hist@OM@Obs[[i]], slot, type)
  hasShift  <- purrr::map_lgl(ObsSlot, \(x) !EmptyObject(x) && !is.null(x@Shift))
  shiftFleets <- FleetNames[hasShift]
  if (!length(shiftFleets))
    cli::cli_abort("No fleet has a non-`NULL` `Shift` slot for `{type}`.")
  if (!is.null(fleets))
    shiftFleets <- intersect(shiftFleets, fleets)
  if (!length(shiftFleets))
    cli::cli_abort("None of `fleets` have a non-`NULL` `Shift` slot for `{type}`.")

  # --- true composition: sum over stocks/areas within the complex ---
  if (isSize) {
    TrueByFleet <- purrr::map(shiftFleets, \(fl) {
      fl_idx <- match(fl, FleetNames)
      purrr::map(slot(Hist, type)[stocks], \(stock_level) {
        catch_n <- stock_level[[fl_idx]]
        sim_x   <- min(sim, dim(catch_n)[1])
        catch_n[sim_x,,,,drop=FALSE] |>
          SumOverArea() |>
          DropDimension("Sim")
      }) |> List2Array("Stock") |>
        SumOverStock() |>
        .SubsetYear(HistYears)
    }) |> stats::setNames(shiftFleets)
  } else {
    CatchAtAge <- purrr::map(slot(Hist, type)[stocks], \(catch_n) {
      sim_x <- min(sim, dim(catch_n)[1])
      catch_n[sim_x,,,,,drop=FALSE] |>
        SumOverArea() |>
        DropDimension("Sim")
    }) |> List2Array("Stock") |>
      SumOverStock() |>
      .SubsetYear(HistYears)  # [Age x Year x Fleet]

    TrueByFleet <- purrr::map(shiftFleets, \(fl) {
      fl_idx <- match(fl, FleetNames)
      CatchAtAge[, , fl_idx]  # [Age x Year]
    }) |> stats::setNames(shiftFleets)
  }

  BinName <- if (isSize) "Class" else "Age"

  TrueDF <- purrr::imap(TrueByFleet, \(mat, fl) {
    prop <- apply(mat, 2, \(col) if (sum(col, na.rm = TRUE) > 0) col / sum(col, na.rm = TRUE) else col)
    df   <- Array2DF(prop)
    names(df)[names(df) == BinName] <- "Bin"
    df$Fleet  <- fl
    df$Source <- "True (OM)"
    df
  }) |> dplyr::bind_rows()

  # --- expected composition: true q tilted by exp(Shift), i.e. the mean of
  # the Dirichlet concentration vector actually used to draw the observed
  # data (see .GenHistDataSizeComp() / .GenHistDataAgeComp()) ---
  ExpectedDF <- purrr::imap(TrueByFleet, \(mat, fl) {
    fl_idx    <- match(fl, FleetNames)
    CompObsFl <- slot(Hist@OM@Obs[[i]][[fl_idx]], type)
    sim_sh    <- min(sim, dim(CompObsFl@Shift)[1])
    ShiftMat  <- .SubsetYear(CompObsFl@Shift, HistYears)[sim_sh,, ]  # [Year x Bin]
    nBin      <- nrow(mat)

    prop <- sapply(colnames(mat), \(yr) {
      q       <- mat[, yr]
      shift_b <- ShiftMat[yr, seq_len(nBin)]
      tilted  <- q * exp(shift_b)
      if (sum(tilted, na.rm = TRUE) > 0) tilted / sum(tilted, na.rm = TRUE) else tilted
    })
    dimnames(prop) <- dimnames(mat)

    df <- Array2DF(prop)
    names(df)[names(df) == BinName] <- "Bin"
    df$Fleet  <- fl
    df$Source <- "Expected (True x Shift)"
    df
  }) |> dplyr::bind_rows()

  # --- observed composition (Shift applied) ---
  ObsValue <- ObsComp@Value
  ObsDF <- purrr::map(shiftFleets, \(fl) {
    fl_idx <- match(fl, ObsComp@Name)
    if (is.na(fl_idx)) return(NULL)
    classes <- .CompdataClasses(ObsComp, fl_idx)
    mat <- ObsValue[, fl_idx, seq_along(classes), drop = FALSE][,1,]
    mat <- matrix(mat, nrow = dim(ObsValue)[1], ncol = length(classes),
                  dimnames = list(Year = dimnames(ObsValue)$Year, Bin = classes))
    prop <- t(apply(mat, 1, \(row) if (sum(row, na.rm = TRUE) > 0) row / sum(row, na.rm = TRUE) else row))
    df <- as.data.frame(as.table(prop), stringsAsFactors = FALSE)
    names(df) <- c("Year", "Bin", "Value")
    df$Year   <- as.numeric(as.character(df$Year))
    df$Bin    <- as.numeric(as.character(df$Bin))
    df$Fleet  <- fl
    df$Source <- "Observed (Shift applied)"
    df
  }) |> dplyr::bind_rows()

  PlotDF <- dplyr::bind_rows(TrueDF, ObsDF) |>
    dplyr::filter(!is.na(.data$Value), .data$Year %in% years)

  if (!nrow(PlotDF))
    cli::cli_abort("No overlapping years between true and observed composition data.")

  ggplot2::ggplot(PlotDF, ggplot2::aes(x = .data$Bin, y = .data$Value, colour = .data$Source)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::facet_grid(Year ~ Fleet, scales = "free_y") +
    ggplot2::labs(x = BinName, y = "Proportion",
                 title = paste("Shift validation:", type, "- Sim", sim),
                 colour = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top")
}
