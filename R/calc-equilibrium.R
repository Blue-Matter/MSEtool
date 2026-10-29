#' Calculate Absolute Equilibrium Quantities
#'
#' For a given vector of apical fishing mortality values, computes absolute
#' equilibrium abundance and yield by combining per-recruit calculations with
#' stock-recruitment relationship scaling. Results span all complexes defined
#' in the operating model.
#'
#' @param OM  An [om-class] or [hist-class] object. If a [hist-class]
#'   object is supplied, `Hist@OM` is used.
#'   
#' @param apicalF Numeric vector of apical fishing mortality values at which
#'   equilibrium quantities are evaluated. If `NULL` (default), a
#'   log-spaced sequence of 50 values is generated automatically. 
#'   
#' @param Years  Numeric years for which equilibrium quantities are
#'   evaluated. Biological and fishery parameters are subset to this year.
#'   Must match years in `Years(OM)`. If `NULL` (default), the final 
#'   historical year is used. 
#' @param Complex Character vector of complex names to evaluate. If `NULL`
#'   (default), all complexes are evaluated.
#'
#' @return An [equilibrium-class] object containing absolute equilibrium
#'   numbers, biomass, spawning biomass, spawning production, removals, and
#'   landings evaluated at each value of `apicalF`, with results reported per
#'   stock.
#'
#' @details
#' 
#' ## Default apicalF grid
#' When `apicalF = NULL`, a log-spaced sequence of 30 values is generated 
#' automatically, spanning from `0.01 × max(M)` to `2 × max(M)` with zero 
#' prepended, where  `max(M)` is the maximum natural mortality across all 
#' stocks. 
#' 
#' Log-spacing is used because population dynamics are approximately linear in 
#' log(F): yield and biomass change rapidly at low F relative to M but become 
#' increasingly insensitive at high F, so equal spacing on the log scale 
#' concentrates resolution where the equilibrium curve is most informative. 
#' 
#' Supply an explicit vector to override, for example
#' `apicalF = seq(0, 2, by = 0.01)`.
#'
#' ## Complexes
#' Per-recruit and equilibrium quantities are calculated separately for each
#' complex, with `apicalF` defined as the maximum fishing mortality across all
#' stocks within the complex. 
#'
#' ## Single year
#' Unlike [CalcPerRecruit()], this function accepts only a single `Year`
#' value. For multi-year equilibrium curves, call the function separately
#' for each year.
#'
#' @seealso [CalcPerRecruit()], [CalcMSY()], [equilibrium-class]
#' @export
CalcEquilibrium <- function(OM,
                            apicalF = NULL,
                            Years   = NULL,
                            Complex = NULL) {
  
  if (inherits(OM, 'hist')) OM <- OM@OM
  .CheckClass(OM, 'om', 'OM')
  
  OM <- Populate(OM, silent = TRUE)
  
  nSeason <- OM@Seasons
  RefSeason      <- OM@RefSeason
  RefEffortYears <- OM@RefEffortYears

  if (is.null(Years)) {
    Years <- utils::tail(Years(OM, 'Historical'), 1)
    if (nSeason > 1L) Years <- unique(floor(Years))
  }

  SPR0 <- CalcSPR0(OM, silent = TRUE) |> .SubsetYear(Years = Years)
  
  SPR0List <- Array2List(SPR0)
  Phi0Ref  <- .SRRPhi0Ref(OM)

  complexes <- Complexes(OM)
  if (!is.null(Complex))
    complexes <- complexes[Complex]
  
  StockNames <- StockNames(OM)
  
  if (is.null(apicalF)) {
    maxM <- purrr::map(complexes, \(stockInd) {
      purrr::map(OM@Stock[stockInd], \(stock)
                 stock@NaturalMortality@MeanAtAge |>
                   .ArraySubsetYear(Years) |>
                   max()
      ) |> unlist() |> max()
    }) |> unlist() |> max()
    
    apicalF <- c(0,
                 exp(seq(log(0.01  * maxM), log(0.25  * maxM), length.out = 20)),
                 exp(seq(log(0.25  * maxM), log(1.25  * maxM), length.out = 30)),
                 exp(seq(log(1.25  * maxM), log(2   * maxM), length.out = 20))) |>
      unique() |> sort()

  }
  
  EqByComplex <- purrr::map(complexes, \(stockInd) {
    
    inputs <- .PrepPerRecruitInputs(
      StockList   = OM@Stock[stockInd],
      FleetList   = OM@Fleet[stockInd],
      SPR0List    = SPR0List[stockInd],
      Years       = Years,
      EffortYears = RefEffortYears,
      RefSeason   = RefSeason,
      Herm        = .HermForStocks(OM, StockNames[stockInd]),
      Phi0Ref     = Phi0Ref[stockInd]
    )

    PerRecruit <- .CalcPerRecruitF(
      apicalF                   = apicalF,
      StockFleetAllocation      = inputs$StockFleetAllocation,
      NaturalMortalityList      = inputs$NaturalMortalityList,
      PlusGroupList             = inputs$PlusGroupList,
      MaturityList              = inputs$MaturityList,
      SemelparousList           = inputs$SemelparousList,
      WeightList                = inputs$WeightList,
      SpawnTimeFracList         = inputs$SpawnTimeFracList,
      SPFrom                    = inputs$SPFrom,
      SPR0List                  = inputs$SPR0List,
      FecundityList             = inputs$FecundityList,
      WeightFleetRetainedList   = inputs$WeightFleetRetainedList,
      WeightFleetSelectedList   = inputs$WeightFleetSelectedList,
      SelectivityFleetList      = inputs$SelectivityFleetList,
      RetentionFleetList        = inputs$RetentionFleetList,
      DiscardMortalityFleetList = inputs$DiscardMortalityFleetList,
      FleetNames                = inputs$FleetNames,
      Years                     = inputs$Years,
      nSeason                   = inputs$nSeason,
      SeasonalWeightsList       = inputs$SeasonalWeightsList,
      CalendarYears             = inputs$CalendarYears,
      RefSeason                 = inputs$RefSeason,
      NPR0List                  = inputs$NPR0List,
      NPR0_SPList               = inputs$NPR0_SPList,
      Herm                      = inputs$Herm
    )

    .CalcEquilibriumInternal(PerRecruit, inputs)
  })
  
  # assemble across complexes
  Eq <- new('equilibrium')
  Eq@apicalF     <- apicalF
  Eq@SPR0        <- purrr::map(EqByComplex, \(e) e@SPR0)        |> .JoinStockArrays(StockNames)
  Eq@SPR         <- purrr::map(EqByComplex, \(e) e@SPR)         |> .JoinStockArrays(StockNames)
  Eq@RelRecruits <- purrr::map(EqByComplex, \(e) e@RelRecruits) |> .JoinStockArrays(StockNames)
  Eq@Recruits    <- purrr::map(EqByComplex, \(e) e@Recruits)    |> .JoinStockArrays(StockNames)
  Eq@Number      <- purrr::map(EqByComplex, \(e) e@Number)      |> .JoinStockArrays(StockNames)
  Eq@Biomass     <- purrr::map(EqByComplex, \(e) e@Biomass)     |> .JoinStockArrays(StockNames)
  Eq@SBiomass    <- purrr::map(EqByComplex, \(e) e@SBiomass)    |> .JoinStockArrays(StockNames)
  Eq@SProduction <- purrr::map(EqByComplex, \(e) e@SProduction) |> .JoinStockArrays(StockNames)
  Eq@Removals    <- purrr::map(EqByComplex, \(e) e@Removals)    |> .JoinStockArrays(StockNames)
  Eq@Landings    <- purrr::map(EqByComplex, \(e) e@Landings)    |> .JoinStockArrays(StockNames)
  Eq
}


.CalcEquilibriumInternal <- function(PerRecruit, inputs) {

  SPRList <- .SRRSpawningPerRecruit(PerRecruit, inputs) |> Array2List('Stock')
  
  RelRecruits <- purrr::pmap(
    list(inputs$RecParsList, SPRList, inputs$RelRecFunList),
    \(RecPars, SPR, RelRecFun) {
      
      sims  <- seq_len(Dim(SPR)['Sim'])
      years <- seq_len(Dim(SPR)['Year'])
      fs    <- seq_len(Dim(SPR)['F'])
      
      grid <- expand.grid(sim = sims, year = years, f = fs,
                          stringsAsFactors = FALSE)
      
      rr_vec <- purrr::pmap_dbl(grid, \(sim, year, f) {
        spr_val  <- SPR[sim, year, f]
        pars_val <- purrr::map(RecPars, \(p) {
          dd <- dim(p)
          p[min(dd[1], sim), min(dd[2],year)]
        })
          
        rr <- RelRecFun(Pars = pars_val, SPR = spr_val)
        max(rr, 0)
      })
      array(rr_vec,
            dim      = c(length(sims), length(years), length(fs)),
            dimnames = list(Sim  = dimnames(SPR)[['Sim']],
                            Year = dimnames(SPR)[['Year']],
                            F    = dimnames(SPR)[['F']]))
    }) |>
    List2Array('Stock') |>
    .Aperm(c('Sim', 'Stock', 'Year', 'F'))

  StockNms  <- dimnames(RelRecruits)[['Stock']]
  SelfOnly  <- .SPFromSelfOnly(inputs$SPFrom, StockNms)
  HermCross <- PerRecruit@Misc$HermCross
  Simple    <- SelfOnly & !.HermInflow(HermCross, StockNms)
  if (!all(Simple))
    RelRecruits <- .SolveLinkedRelRecruits(RelRecruits, PerRecruit, inputs, Simple)

  R0_f     <- AddDimension(inputs$R0, 'F', val = PerRecruit@apicalF)
  Recruits <- ArrayMultiply(R0_f, RelRecruits)

  EqQuantity <- \(PR, Q) .AddHermCross(ArrayMultiply(PR, Recruits), HermCross, Q, Recruits)

  SProduction <- EqQuantity(PerRecruit@SProduction, 'SProduction')
  if (!all(SelfOnly))
    SProduction <- .BlendSPFrom(SProduction, inputs$SPFrom, SelfOnly)

  Eq             <- new('equilibrium')
  Eq@apicalF     <- PerRecruit@apicalF
  Eq@SPR0        <- PerRecruit@SPR0
  Eq@SPR         <- PerRecruit@SPR
  Eq@RelRecruits <- RelRecruits
  Eq@Recruits    <- Recruits
  Eq@Number      <- EqQuantity(PerRecruit@NPRF, 'NPRF')
  Eq@SProduction <- SProduction
  Eq@Removals    <- EqQuantity(PerRecruit@Removals, 'Removals')
  Eq@Landings    <- EqQuantity(PerRecruit@Landings, 'Landings')
  Eq@Biomass     <- EqQuantity(PerRecruit@Biomass,  'Biomass')
  Eq@SBiomass    <- EqQuantity(PerRecruit@SBiomass, 'SBiomass')
  Eq
}

# Spawning per recruit relative to the first-year unfished reference the
# stock-recruitment relationship is defined against (current-year SPR if the
# reference is not supplied)
.SRRSpawningPerRecruit <- function(PerRecruit, inputs) {
  if (is.null(inputs$Phi0SRR)) return(PerRecruit@SPR)
  SPR  <- .Aperm(PerRecruit@SPR, c('Sim', 'Stock', 'Year', 'F'))
  Ones <- SPR
  Ones[] <- 1
  Out <- .AlignToTemplate(PerRecruit@SPRF, Ones) / .AlignToTemplate(inputs$Phi0SRR, Ones)
  Out[!is.finite(Out)] <- 0
  Out
}

# Stocks that receive individuals recruited into another stock via Herm
.HermInflow <- function(HermCross, StockNames) {
  Inflow <- stats::setNames(rep(FALSE, length(StockNames)), StockNames)
  for (k in names(HermCross)) {
    Arr <- .Aperm(HermCross[[k]]$NPRF, c('Sim', 'Stock', 'Year', 'F'))
    for (s in setdiff(StockNames, k))
      Inflow[s] <- Inflow[s] || any(Arr[, s, , ] > 0)
  }
  Inflow
}

# Repeat a single-stock [Sim, 1, Year, F] array across all stocks of `Template`
.RepStock <- function(x, Template) {
  out <- x[, rep(1L, dim(Template)[2]), , , drop = FALSE]
  dimnames(out) <- dimnames(Template)
  out
}

# Equilibrium quantity plus contributions from other stocks' recruits via Herm
.AddHermCross <- function(Base, HermCross, Q, Recruits) {
  if (!length(HermCross)) return(Base)
  DimNames <- names(dimnames(Base))
  Out  <- .Aperm(Base, c('Sim', 'Stock', 'Year', 'F'))
  Ones <- Out
  Ones[] <- 1
  Rec <- .AlignToTemplate(Recruits, Ones)
  for (k in names(HermCross))
    Out <- Out + .AlignToTemplate(HermCross[[k]][[Q]], Ones) *
      .RepStock(Rec[, k, , , drop = FALSE], Ones)
  .Aperm(Out, DimNames)
}

.AlignToTemplate <- function(x, Ones) {
  out <- ArrayMultiply(.BroadcastTo(x, Ones), Ones) |> .Aperm(names(dimnames(Ones)))
  if (!.SameDim(out, Ones))
    cli::cli_abort("Internal error: could not align an array to the equilibrium dimensions.")
  out
}

# Relative recruitment of stocks whose spawning production comes from other stocks
# (SPFrom) or includes individuals recruited into other stocks (Herm):
# RR_i = SRR_i(x_i), x_i = sum_k w_ik SP_k / SP0_i, with SP_k = sum_j R0_j RR_j SPRF_{k<-j}
# and SP0_i the first-year unfished reference (sum_k w_ik SP0_k when not supplied).
# `SelfOnly` flags stocks solved in closed form by RelRecFun.
.SolveLinkedRelRecruits <- function(RelRecruits, PerRecruit, inputs, SelfOnly,
                                    tol = 1e-12, maxIter = 500) {
  StockNames <- dimnames(RelRecruits)[['Stock']]
  Years      <- as.numeric(dimnames(RelRecruits)[['Year']])
  Ones       <- ArrayMultiply(AddDimension(inputs$R0, 'F', val = PerRecruit@apicalF), RelRecruits) |>
    .Aperm(c('Sim', 'Stock', 'Year', 'F'))
  Ones[]     <- 1

  if (is.null(PerRecruit@Misc$SPR0Own))
    cli::cli_abort("Internal error: `PerRecruit@Misc$SPR0Own` is required for {.field SRR@SPFrom}-linked stocks.")

  SPRF    <- .AlignToTemplate(PerRecruit@SPRF, Ones)
  R0      <- .AlignToTemplate(inputs$R0, Ones)
  SPR0    <- .AlignToTemplate(.ArraySubsetYear(PerRecruit@SPR0, Years), Ones)
  SPR0Own <- .AlignToTemplate(.ArraySubsetYear(PerRecruit@Misc$SPR0Own, Years), Ones)
  SPR0[, !SelfOnly, , ] <- SPR0Own[, !SelfOnly, , ]

  HermCross <- PerRecruit@Misc$HermCross
  CrossSPRF <- purrr::map(HermCross, \(x) .AlignToTemplate(x$SPRF, Ones))
  CrossSPR0 <- purrr::map(HermCross, \(x) .AlignToTemplate(.ArraySubsetYear(x$SPR0, Years), Ones))

  .SP <- function(RR) {
    SP <- R0 * RR * SPRF
    for (j in names(HermCross))
      SP <- SP + CrossSPRF[[j]] * .RepStock(R0[, j, , , drop = FALSE] * RR[, j, , , drop = FALSE], Ones)
    SP
  }
  SP0 <- R0 * SPR0
  for (j in names(HermCross))
    SP0 <- SP0 + CrossSPR0[[j]] * .RepStock(R0[, j, , , drop = FALSE], Ones)
  Linked <- StockNames[!SelfOnly]

  .WeightedSum <- \(w, arr) Reduce(`+`, purrr::map2(w$from, w$weight, \(k, wt)
    wt * arr[, k, , , drop = FALSE]))

  # unfished reference: first-year SP0 per recruit x current R0, as in the dynamics
  SP0Ref <- if (!is.null(inputs$Phi0SRR)) .AlignToTemplate(inputs$Phi0SRR, Ones) * R0
  Den <- purrr::map(stats::setNames(Linked, Linked), \(st)
    if (is.null(SP0Ref)) .WeightedSum(inputs$SPFrom[[st]], SP0) else SP0Ref[, st, , , drop = FALSE])

  RR <- .AlignToTemplate(RelRecruits, Ones)
  for (iter in seq_len(maxIter)) {
    maxDiff <- 0
    for (st in Linked) {
      x <- .WeightedSum(inputs$SPFrom[[st]], .SP(RR)) / Den[[st]]
      x[!is.finite(x)] <- 0
      new <- .EvalRelativeSRR(inputs$SRRModelList[[st]], inputs$RecParsList[[st]], x, st)
      maxDiff <- max(maxDiff, abs(new - RR[, st, , , drop = FALSE]))
      RR[, st, , ] <- new
    }
    if (maxDiff < tol) break
  }
  if (maxDiff >= tol)
    cli::cli_warn("Equilibrium recruitment for {.field SRR@SPFrom}-linked stock{?s} {.val {Linked}} did not converge (max change {.val {signif(maxDiff, 3)}}).")
  RR
}

# Engine SRR evaluated at relative spawning production x (S0 = R0 = 1)
.EvalRelativeSRR <- function(Model, RecPars, x, StockName) {
  if (!is.function(Model))
    cli::cli_abort("Stock {.val {StockName}}: {.field SRR@Model} must be one of {.val {SRRModels(FALSE, FALSE)}} when {.field SRR@SPFrom} is another stock.")
  ParNames <- setdiff(names(formals(Model)), c('S', 'S0', 'R0'))
  d   <- dim(x)
  out <- x
  for (s in seq_len(d[1])) {
    for (y in seq_len(d[3])) {
      Pars <- purrr::map(RecPars[ParNames], \(p) {
        dd <- dim(p)
        p[min(dd[1], s), min(dd[2], y)]
      })
      out[s, 1, y, ] <- do.call(Model, c(list(S = x[s, 1, y, ], S0 = 1, R0 = 1), Pars))
    }
  }
  pmax(out, 0)
}

.BlendSPFrom <- function(SProduction, SPFrom, SelfOnly) {
  DimNames <- names(dimnames(SProduction))
  Raw <- .Aperm(SProduction, c('Sim', 'Stock', 'Year', 'F'))
  Out <- Raw
  for (st in names(SelfOnly)[!SelfOnly]) {
    w <- SPFrom[[st]]
    Out[, st, , ] <- Reduce(`+`, purrr::map2(w$from, w$weight, \(k, wt)
      wt * Raw[, k, , , drop = FALSE]))
  }
  .Aperm(Out, DimNames)
}
