#' Extract TAC Recommendations from an MSE Object
#'
#' Extracts the total allowable catch (TAC) recommendations issued by each
#' management procedure (MP) in each simulation, year, stock complex, and
#' fleet from the `PPD` slot of an [mse-class] object. Returns a tidy data
#' frame suitable for plotting and performance indicator calculations.
#'
#' @param MSE An [mse-class] object.
#'
#' @details
#' An MP's `Advice(TAC = ...)` only gets recorded in the year it actually
#' changes (see [Advice()]: a TAC applies from the year it's issued "until a
#' new Advice object is returned"), so the raw recommendation history is
#' sparse. `TACs()` forward-fills each gap (via [ExtendYears()]) so the
#' returned series has one row per year the TAC was actually in effect, not
#' just the years it changed. Years before an MP's first recommendation
#' (i.e. the historical period, where no MP has run yet) are absent rather
#' than filled with a placeholder.
#'
#' [TAC()] returns the same data frame when given an [mse-class] object.
#' [Advice()] returns all of the advice issued by each MP, including effort,
#' closures, bag limits, and size regulations.
#'
#' @return A data frame with columns:
#'   - `Sim`: simulation index.
#'   - `Stock`: stock complex name (`names(OM@Complexes)`). TAC advice is set
#'     per complex; for single-stock complexes this is the stock name.
#'   - `MP`: management procedure name.
#'   - `Year`: calendar year (or decimal date for sub-annual time steps).
#'   - `Period`: always `"Projection"` for the reasons above.
#'   - `Fleet`: fleet name.
#'   - `Value`: TAC recommendation in the units of the operating model.
#'   - `Variable`: always `"TAC"`.
#'
#' @seealso [TAC()], [mse-class], [Advice()], [PPD()]
#' @export
TACs <- function(MSE) {
  .ExtractAdviceDF(MSE, 'TAC')
}

.ExtractAdviceDF <- function(MSE, Variables) {
  .CheckClass(MSE, 'mse', 'MSE')

  # the projection time steps the object holds, so a Subset() MSE is filled only to its own years
  ProjYears <- as.numeric(dimnames(MSE@SBiomass)$Year)

  ppd     <- PPD(MSE)
  mpNames <- names(ppd)

  out <- purrr::map(mpNames, \(mp) {
    simNames <- names(ppd[[mp]])
    purrr::map(simNames, \(sim) {
      purrr::imap(ppd[[mp]][[sim]], \(dat, cx) {
        purrr::map(Variables, \(v) {
          arr <- slot(dat@Advice, v)
          if (is.null(arr) || !length(arr)) return(NULL)

          arr <- ExtendYears(arr, Years = ProjYears)

          Array2DF(arr) |>
            dplyr::mutate(
              Sim      = as.numeric(sim),
              Stock    = cx,
              MP       = mp,
              Period   = ifelse(.data$Year %in% ProjYears, 'Projection', 'Historical'),
              Variable = v,
              Fleet    = as.character(.data$Fleet)
            )
        }) |> dplyr::bind_rows()
      }) |> dplyr::bind_rows()
    }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows()

  if (!nrow(out))
    return(data.frame(Sim = numeric(0), Stock = character(0), MP = character(0),
                      Year = numeric(0), Period = character(0), Fleet = character(0),
                      Value = numeric(0), Variable = character(0)))

  out |>
    dplyr::relocate('Sim', 'Stock', 'MP', 'Year', 'Period') |>
    dplyr::mutate(Variable = factor(.data$Variable, levels = Variables)) |>
    dplyr::arrange(.data$Sim, .data$Stock, .data$MP, .data$Variable, .data$Year) |>
    dplyr::mutate(Variable = as.character(.data$Variable))
}

# all advice in MSE@Misc$Advice and the size-regulation curves applied in the projections
.ExtractAllAdvice <- function(MSE) {
  .CheckClass(MSE, 'mse', 'MSE')
  ProjYears  <- as.numeric(dimnames(MSE@SBiomass)$Year)
  FleetNms   <- FleetNames(MSE)
  AdviceByMP <- MSE@Misc$Advice

  Rows <- list()
  for (mp in names(AdviceByMP)) {
    Years <- names(AdviceByMP[[mp]])
    Years <- Years[order(as.numeric(Years))]
    LastClosure <- list()
    for (yr in Years) {
      BySim <- AdviceByMP[[mp]][[yr]]
      for (sim in names(BySim)) {
        for (cx in names(BySim[[sim]])) {
          A <- BySim[[sim]][[cx]]
          if (!inherits(A, 'advice')) next
          Key <- paste(sim, cx)
          if (!is.null(A@Closure)) LastClosure[[Key]] <- A@Closure
          Rows[[length(Rows) + 1]] <- .AdviceRows(A, LastClosure[[Key]], FleetNms) |>
            .AddAdviceKeys(Sim = sim, Stock = cx, MP = mp, Year = yr)
        }
      }
    }
  }

  for (sl in c('Selectivity', 'Retention', 'DiscardMortality')) {
    for (mp in names(MSE@Misc[[sl]])) {
      for (st in names(MSE@Misc[[sl]][[mp]])) {
        for (fl in names(MSE@Misc[[sl]][[mp]][[st]])) {
          Curves <- MSE@Misc[[sl]][[mp]][[st]][[fl]]
          for (at in names(Curves)) {
            Rows[[length(Rows) + 1]] <- .CurveRows(Curves[[at]], nSim(MSE), sl, at) |>
              dplyr::mutate(Stock = st, MP = mp, Fleet = fl)
          }
        }
      }
    }
  }

  Cols <- c('Sim', 'Stock', 'MP', 'Year', 'Period', 'Fleet', 'Area', 'Variable',
            'Type', 'Units', 'Class', 'Value')
  Out <- dplyr::bind_rows(Rows)
  if (!nrow(Out))
    return(data.frame(Sim = numeric(0), Stock = character(0), MP = character(0),
                      Year = numeric(0), Period = character(0), Fleet = character(0),
                      Area = character(0), Variable = character(0), Type = character(0),
                      Units = character(0), Class = numeric(0), Value = numeric(0)))
  for (cl in setdiff(Cols, names(Out))) Out[[cl]] <- NA
  Variables <- c('TAC', 'Effort', 'Closure', 'BagLimit', 'Selectivity', 'Retention', 'DiscardMortality')
  Out |>
    dplyr::mutate(Period = ifelse(.data$Year %in% ProjYears, 'Projection', 'Historical'),
                  Area = as.character(.data$Area)) |>
    dplyr::select(dplyr::all_of(Cols)) |>
    dplyr::arrange(.data$Sim, .data$Stock, .data$MP, match(.data$Variable, Variables),
                   .data$Fleet, .data$Year, .data$Type, .data$Class) |>
    as.data.frame()
}

# per-fleet value of a setting given as length 1 or one per fleet
.PerFleet <- function(x, FleetNms) {
  if (is.null(x) || !length(x)) return(rep(NA_character_, length(FleetNms)))
  if (length(x) == 1) return(rep(as.character(x), length(FleetNms)))
  if (!is.null(names(x)) && all(FleetNms %in% names(x))) return(as.character(x[FleetNms]))
  rep(as.character(x), length.out = length(FleetNms))
}

# a TAC/Effort/BagLimit/Closure record as rows by Fleet (and Area)
.AdviceArrayRows <- function(x, FleetNms, Variable, Type = NULL, Units = NULL) {
  if (is.null(x) || !length(x)) return(NULL)
  if (is.null(dim(x))) {
    x <- if (length(x) == 1) rep(x, length(FleetNms)) else x
    x <- array(x, dim = length(FleetNms), dimnames = list(Fleet = FleetNms))
  }
  dn <- dimnames(x)
  if (is.null(dn) || is.null(names(dn))) {
    dn <- list(Fleet = FleetNms, Area = as.character(seq_len(dim(x)[2])))[seq_along(dim(x))]
    dimnames(x) <- dn
  }
  df <- Array2DF(x) |> dplyr::mutate(Fleet = as.character(.data$Fleet), Variable = Variable)
  Lookup <- function(v) if (is.null(v)) NA_character_ else v[match(df$Fleet, FleetNms)]
  df$Type  <- Lookup(Type)
  df$Units <- Lookup(Units)
  df
}

.AdviceRows <- function(A, Closure, FleetNms) {
  TAC <- A@Misc$AnnualTAC %||% A@TAC
  dplyr::bind_rows(
    .AdviceArrayRows(TAC, FleetNms, 'TAC',
                     Type = .PerFleet(A@TACType, FleetNms), Units = .PerFleet(A@TACUnit, FleetNms)),
    .AdviceArrayRows(A@Effort, FleetNms, 'Effort', Type = .PerFleet(A@EffType, FleetNms)),
    .AdviceArrayRows(Closure, FleetNms, 'Closure'),
    .AdviceArrayRows(A@BagLimit, FleetNms, 'BagLimit',
                     Type = paste(.PerFleet(A@LimitType, FleetNms), .PerFleet(A@ClosureMode, FleetNms), sep = ', '),
                     Units = rep('Number', length(FleetNms)))
  )
}

.AddAdviceKeys <- function(df, Sim, Stock, MP, Year) {
  if (is.null(df) || !nrow(df)) return(NULL)
  df |> dplyr::mutate(Sim = as.numeric(Sim), Stock = Stock, MP = MP, Year = as.numeric(Year))
}

# a [Sim, Class, Year, Area] curve as rows; a shared curve (Sim length 1) is given to every sim
.CurveRows <- function(x, nSim, Variable, at) {
  dn <- names(dimnames(x))
  ClassDim <- setdiff(dn, c('Sim', 'Year', 'Area'))
  df <- Array2DF(x)
  names(df)[names(df) == ClassDim] <- 'Class'
  if (dim(x)[match('Sim', dn)] == 1 && nSim > 1)
    df <- dplyr::bind_rows(lapply(seq_len(nSim), \(s) dplyr::mutate(df, Sim = s)))
  df |> dplyr::mutate(Sim = as.numeric(.data$Sim), Year = as.numeric(.data$Year),
                      Class = as.numeric(as.character(.data$Class)),
                      Variable = Variable, Type = sub('MeanAt', '', at))
}
