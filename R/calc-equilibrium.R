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

  if (is.null(Years)) {
    Years <- utils::tail(Years(OM, 'Historical'), 1)
    if (nSeason > 1L) Years <- unique(floor(Years))
  }

  SPR0 <- CalcSPR0(OM, silent = TRUE) |> .SubsetYear(Years = Years)
  
  SPR0List <- Array2List(SPR0)
  
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
      StockList = OM@Stock[stockInd],
      FleetList = OM@Fleet[stockInd],
      SPR0List  = SPR0List[stockInd],
      Years     = Years
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
      CalendarYears             = inputs$CalendarYears
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
  
  SPRList <- PerRecruit@SPR |> Array2List('Stock')
  
  RelRecruits <- purrr::pmap(
    list(inputs$RecParsList, SPRList, inputs$RelRecFunList),
    \(RecPars, SPR, RelRecFun) {
      
      # RecPars  <<- RecPars
      # SPR       <<- SPR
      # RelRecFun <<- RelRecFun
      
      sims  <- seq_len(dim(SPR)['Sim'])
      years <- seq_len(dim(SPR)['Year'])
      fs    <- seq_len(dim(SPR)['F'])
      
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
  
  R0_f <- AddDimension(inputs$R0, 'F', val = PerRecruit@apicalF) 
  Recruits <- ArrayMultiply(R0_f, RelRecruits)
  
  Eq <- new('equilibrium')
  Eq@apicalF     <- PerRecruit@apicalF
  Eq@SPR0        <- PerRecruit@SPR0
  Eq@SPR         <- PerRecruit@SPR
  Eq@RelRecruits <- RelRecruits
  Eq@Recruits    <- Recruits
  Eq@Number      <- ArrayMultiply(PerRecruit@NPRF, Recruits)
  Eq@SProduction <- ArrayMultiply(PerRecruit@SProduction, Recruits)
  Eq@Removals    <- ArrayMultiply(PerRecruit@Removals, Recruits)
  Eq@Landings    <- ArrayMultiply(PerRecruit@Landings, Recruits)

  # For seasonal models, B^PR and SB^PR sum over S seasonal age classes per
  # calendar year, giving S × (mean seasonal snapshot biomass per recruit).
  # Dividing by nSeason converts to a single-snapshot-equivalent, directly
  # comparable to annual-model BMSY/SBMSY. SProduction (fecundity-weighted)
  # is a flow aggregated over the year and does not need this correction.
  nSeason <- inputs$nSeason %||% 1L
  B_PR  <- if (nSeason > 1L) PerRecruit@Biomass  / nSeason else PerRecruit@Biomass
  SB_PR <- if (nSeason > 1L) PerRecruit@SBiomass / nSeason else PerRecruit@SBiomass
  Eq@Biomass  <- ArrayMultiply(B_PR,  Recruits)
  Eq@SBiomass <- ArrayMultiply(SB_PR, Recruits)
  Eq
}
