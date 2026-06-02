#' Calculate Per-Recruit Quantities
#'
#' Extracts biological and fishery parameters from a [hist-class] or [om-class]
#' object and evaluates per-recruit quantities at a given apical fishing
#' mortality.
#'
#' @param OM A [om-class] or [hist-class] object. If an `om` is supplied
#'   it is first converted to a `hist` object.
#' @param apicalF Numeric vector of apical fishing mortality values at which
#'   per-recruit quantities are evaluated. Default `0.1`.
#' @param Years Integer vector of years for which per-recruit quantities are
#'   evaluated. Biological and fishery parameters are subset to these years
#'   If `NULL` (default), the final historical year is used.
#' @param Complex Character vector of complex names to evaluate. If `NULL`
#'   (default), all complexes are evaluated.
#'
#' @return A [perrecruit-class] object containing numbers-per-recruit,
#'   spawning-per-recruit, biomass-per-recruit, removals, and landings
#'   evaluated at each value of `apicalF`, with results reported per stock
#'   and apical F defined as the maximum F across all stocks within each
#'   complex.
#'
#' @details
#'
#' ## Complexes
#' Per-recruit quantities are calculated separately for each complex, with
#' `apicalF` defined as the maximum fishing mortality across all ages and
#' stocks within the complex. Results are reported per stock and reassembled
#' into a single [perrecruit-class] object spanning all stocks. The `Complex`
#' argument can be used to restrict calculations to a subset of complexes.
#'
#' ## Fleet allocation
#' The relative contribution of each fleet to total fishing mortality is
#' computed as the effort-weighted catchability
#' (i.e. `Effort × Efficiency`) normalised across fleets. These proportions
#' are used to distribute `apicalF` across fleets before selectivity is applied.
#'
#' ## Years
#' When `Years` contains multiple values, biological and fishery parameters
#' are subset to those years and the per-recruit calculations are evaluated
#' for each year independently. When `Years = NULL`, only the final historical
#' year is used, giving a single set of per-recruit quantities representative
#' of current conditions.
#'
#' @seealso [CalcSPR0()], [perrecruit-class]
#' @export
CalcPerRecruit <- function(OM, apicalF=0.1, Years=NULL, Complex=NULL) {
  CheckClass(OM, c('om', 'hist'))

  if (inherits(OM, 'om'))
    Hist <- OM2Hist(OM, silent=TRUE)

  if (inherits(OM, 'hist'))
    Hist <- OM

  CheckClass(Hist, 'hist', 'Hist')

  if (is.null(Years))
    Years <- utils::tail(Years(Hist@OM, 'Historical'), 1)

  StockNames <- StockNames(Hist)

  complexes <- Complexes(Hist)
  if (!is.null(Complex))
    complexes <- complexes[Complex]

  if (is.null(Hist@Reference@SPR0))
    Hist@Reference@SPR0 <- CalcSPR0(Hist, silent=TRUE)

  SPR0List <- Array2List(Hist@Reference@SPR0)

  PRByComplex <- purrr::map(complexes, \(stockInd) {
    CalcPerRecruit_StockList(
      StockList = Hist@OM@Stock[stockInd],
      FleetList = Hist@OM@Fleet[stockInd],
      apicalF   = apicalF,
      Years     = Years,
      SPR0List  = SPR0List[stockInd]
    )
  })
  
  PerRecruit <- new('perrecruit')
  PerRecruit@apicalF     <- apicalF
  PerRecruit@NPR0        <- purrr::map(PRByComplex, \(pr) pr@NPR0)        |> JoinStockArrays(StockNames)
  PerRecruit@NPR0_SP     <- purrr::map(PRByComplex, \(pr) pr@NPR0_SP)     |> JoinStockArrays(StockNames)
  PerRecruit@SPR0        <- purrr::map(PRByComplex, \(pr) pr@SPR0)        |> JoinStockArrays(StockNames)
  PerRecruit@NPRF        <- purrr::map(PRByComplex, \(pr) pr@NPRF)        |> JoinStockArrays(StockNames)
  PerRecruit@NPRF_SP     <- purrr::map(PRByComplex, \(pr) pr@NPRF_SP)     |> JoinStockArrays(StockNames)
  PerRecruit@SPRF        <- purrr::map(PRByComplex, \(pr) pr@SPRF)        |> JoinStockArrays(StockNames)
  PerRecruit@SPR         <- purrr::map(PRByComplex, \(pr) pr@SPR)         |> JoinStockArrays(StockNames)
  PerRecruit@Biomass     <- purrr::map(PRByComplex, \(pr) pr@Biomass)     |> JoinStockArrays(StockNames)
  PerRecruit@SBiomass    <- purrr::map(PRByComplex, \(pr) pr@SBiomass)    |> JoinStockArrays(StockNames)
  PerRecruit@SProduction <- purrr::map(PRByComplex, \(pr) pr@SProduction) |> JoinStockArrays(StockNames)
  PerRecruit@Removals    <- purrr::map(PRByComplex, \(pr) pr@Removals)    |> JoinStockArrays(StockNames)
  PerRecruit@Landings    <- purrr::map(PRByComplex, \(pr) pr@Landings)    |> JoinStockArrays(StockNames)
  PerRecruit
}

CalcPerRecruit_StockList <- function(StockList, FleetList, apicalF=0.1, Years, SPR0List) {

  inputs <- PrepPerRecruitInputs(StockList, FleetList, SPR0List, Years)
  
  PR <- CalcPerRecruit_F(
    apicalF              = apicalF,
    StockFleetAllocation = inputs$StockFleetAllocation,
    NaturalMortalityList = inputs$NaturalMortalityList,
    PlusGroupList        = inputs$PlusGroupList,
    MaturityList         = inputs$MaturityList,
    SemelparousList      = inputs$SemelparousList,
    WeightList           = inputs$WeightList,
    SpawnTimeFracList    = inputs$SpawnTimeFracList,
    SPFrom               = inputs$SPFrom,
    SPR0List             = inputs$SPR0List,
    FecundityList        = inputs$FecundityList,
    WeightFleetList      = inputs$WeightFleetList,
    Selectivity          = inputs$Selectivity,
    Retention            = inputs$Retention,
    DiscardMortality     = inputs$DiscardMortality,
    FleetNames           = inputs$FleetNames,
    Years                = Years
  )
  
  IsSpawnTimeFrac <- any(unlist(inputs$SpawnTimeFracList) != 0)
  
  NPR0List <- purrr::pmap(
    list(inputs$NaturalMortalityList, inputs$PlusGroupList, inputs$SemelparousList),
    \(NaturalMortality, PlusGroup, Semelparous)
    CalcSurvival(NaturalMortality, FishingMortality = NULL, PlusGroup,
                 SpawnTimeFrac = 0, Semelparous)
  )
  
  if (IsSpawnTimeFrac) {
    NPR0_SPList <- purrr::pmap(
      list(inputs$NaturalMortalityList, inputs$PlusGroupList,
           inputs$SemelparousList, inputs$SpawnTimeFracList),
      \(NaturalMortality, PlusGroup, Semelparous, SpawnTimeFrac)
      CalcSurvival(NaturalMortality, FishingMortality = NULL, PlusGroup,
                   SpawnTimeFrac, Semelparous)
    )
  } else {
    NPR0_SPList <- NPR0List
  }
  PR@NPR0    <- NPR0List |> List2Array("Stock", pos=2)
  PR@NPR0_SP <- if (IsSpawnTimeFrac) NPR0_SPList |> List2Array("Stock", pos=2) else NULL
  PR
}

JoinStockArrays <- function(arrayList, StockNames) {
  
  arrayList <- purrr::compact(arrayList)
  if (length(arrayList) == 0) return(NULL)
  
  StockInd <- match("Stock", names(dimnames(arrayList[[1]])))
  if (is.na(StockInd))
    cli::cli_abort("`Stock` dimension not found in arrays passed to `JoinStockArrays()`",
                   .internal = TRUE)
  
  # bind across complexes along Stock dimension
  # list names are complex names; stock names are in array dimnames
  combined <- abind::abind(arrayList, along = StockInd, use.dnns = TRUE)
  
  # reorder to StockNames order
  currentStocks <- dimnames(combined)$Stock
  if (!setequal(currentStocks, StockNames))
    cli::cli_abort(
      "Stock names in arrays {.val {currentStocks}} do not match expected {.val {StockNames}}",
      .internal = TRUE
    )
  
  ArraySubsetStock(combined, match(StockNames, currentStocks))
}

CalcFleetAllocationF <- function(FleetList, Years) {

  FDistribution <- purrr::map(FleetList, \(Fleet) {
    ArrayMultiply(Fleet@Effort@Effort |>  ArraySubsetYear(Years),
                  Fleet@Catchability@Efficiency |>  ArraySubsetYear(Years))
  }) |>
    List2Array('Fleet', pos=3)

  FDistributionTotal <- SumOverFleet(FDistribution)
  FDistributionTotal <- AddDimension(FDistributionTotal, 'Fleet') |> 
    ExtendFleets(Fleets = names(FleetList))
  dimnames(FDistributionTotal)[['Fleet']] <- names(FleetList)

  ArrayDivide(FDistribution, FDistributionTotal)
}

CalcPerRecruit_F <- function(apicalF = 0.1, ...) {
  names(apicalF) <- as.character(apicalF)
  PRList <- purrr::map(apicalF, \(F) CalcPerRecruit_F_scalar(F, ...))
  
  
  PerRecruit <- new('perrecruit')
  PerRecruit@apicalF     <- apicalF
  PerRecruit@SPR0        <- PRList[[1]]@SPR0  # F-invariant
  PerRecruit@NPRF        <- purrr::map(PRList, \(pr) pr@NPRF)      |> List2Array('F')
  if (!is.null(PRList[[1]]@NPRF_SP)) {
    PerRecruit@NPRF_SP   <- purrr::map(PRList, \(pr) pr@NPRF_SP)   |> List2Array('F')
  } else {
    PerRecruit@NPRF_SP   <- NULL  
  }
  PerRecruit@SPRF        <- purrr::map(PRList, \(pr) pr@SPRF)        |> List2Array('F')
  PerRecruit@SPR         <- purrr::map(PRList, \(pr) pr@SPR)         |> List2Array('F')
  PerRecruit@Biomass     <- purrr::map(PRList, \(pr) pr@Biomass)     |> List2Array('F')
  PerRecruit@SBiomass    <- purrr::map(PRList, \(pr) pr@SBiomass)    |> List2Array('F')
  PerRecruit@SProduction <- purrr::map(PRList, \(pr) pr@SProduction) |> List2Array('F')
  PerRecruit@Removals    <- purrr::map(PRList, \(pr) pr@Removals)    |> List2Array('F')
  PerRecruit@Landings    <- purrr::map(PRList, \(pr) pr@Landings)    |> List2Array('F')
  PerRecruit
}


CalcPerRecruit_F_scalar <- function(apicalF = 0.1,
                                    StockFleetAllocation,
                                    NaturalMortalityList,
                                    PlusGroupList,
                                    MaturityList,
                                    SemelparousList,
                                    WeightList,
                                    SpawnTimeFracList,
                                    SPFrom,
                                    SPR0List,
                                    FecundityList,
                                    WeightFleetList,
                                    Selectivity,
                                    Retention,
                                    DiscardMortality,
                                    FleetNames,
                                    Years) {

  apicalFAge <- apicalF * StockFleetAllocation  |>
    AddDimension("Age", pos=3)

  FInteract <- ArrayMultiply(apicalFAge, Selectivity)
  FRetain <- ArrayMultiply(FInteract, Retention)
  FDiscardTotal <- ArraySubtract(FInteract, FRetain)
  FDiscardDead <- ArrayMultiply(FDiscardTotal, DiscardMortality)
  FDead <- FRetain + FDiscardDead
  FDeadTotal <- SumOverFleet(FDead)
  ActualApicalF <- apply(FDeadTotal, setdnames('Year'), max) 

  if (apicalF>0 & any(abs(ActualApicalF/apicalF - 1) > 1E-2)) {
    # adjust for retention and discard mortality & different selectivity patterns by fleet
    apicalFSimTS <- array(apicalF, dim=dim(ActualApicalF), dimnames=dimnames(ActualApicalF))
    
    adjust <- ArrayDivide(apicalFSimTS, ActualApicalF)
    adjust <- adjust |> 
      AddDimension("Stock", pos=2) |>
      AddDimension("Age", pos=3) |>
      AddDimension("Fleet", pos=5) |>
      ExtendFleets(Fleets=FleetNames) |>
      ExtendStocks(Stocks=names(NaturalMortalityList))
    
    FInteract <- ArrayMultiply(adjust, FInteract)
    
    FRetain <- ArrayMultiply(FInteract, Retention)
    FDiscardTotal <- ArraySubtract(FInteract, FRetain)
    FDiscardDead <- ArrayMultiply(FDiscardTotal, DiscardMortality)
    FDead <- FRetain + FDiscardDead
    FDeadTotal <- SumOverFleet(FDead)
    ActualApicalF <- apply(FDeadTotal, setdnames('Year'), max)
  }

  stockInd <- which(names(dimnames(FDeadTotal)) == 'Stock')
  FDeadTotalList <- FDeadTotal |> Array2List(stockInd)
  ZDeadTotalList <- purrr::map2(FDeadTotalList, NaturalMortalityList, ArraySum)

  NPRFList <- purrr::pmap(list(NaturalMortalityList, FDeadTotalList, PlusGroupList, SemelparousList),
                          \(NaturalMortality, FishingMortalityAtAge, PlusGroup, Semelparous)
                          CalcSurvival(NaturalMortality,
                                       FishingMortalityAtAge,
                                       PlusGroup,
                                       SpawnTimeFrac=0,
                                       Semelparous)
  )

  IsSpawnTimeFrac <- any(unlist(SpawnTimeFracList)!=0)
  if (IsSpawnTimeFrac) {
    # per recruit spawning
    NPRF_SPList <- purrr::pmap(list(NaturalMortalityList, FDeadTotalList, PlusGroupList, SemelparousList, SpawnTimeFracList),
                               \(NaturalMortality, FishingMortalityAtAge, PlusGroup, Semelparous, SpawnTimeFrac)
                               CalcSurvival(NaturalMortality,
                                            FishingMortalityAtAge,
                                            PlusGroup,
                                            SpawnTimeFrac,
                                            Semelparous)
    )
  } else {
    NPRF_SPList <- NPRFList
  }

  # SPR
  SPRFList <- purrr::map2(NPRF_SPList, FecundityList, \(NPRF_SP, Fecundity) {
    SPRF <- ArrayMultiply(NPRF_SP, Fecundity) |> SumOverAge()
    if (!is.array(SPRF))
      SPRF <- array(SPRF, length(SPRF), dimnames = list(Year=Years))
    SPRF
  })

  SPRFList <- SPRFList[SPFrom]
  names(SPRFList) <- names(NPRFList)
  SPR <- purrr::map2(SPRFList, SPR0List, \(SPRF, SPR0) ArrayDivide(SPRF, SPR0)) |>
    List2Array('Stock') |>
    ArraySubsetYear(Years)


  # Removals and Landings
  stockInd <- which(names(dimnames(FDead)) == 'Stock')
  FDeadList <- FDead |> Array2List(stockInd)
  FishingDeadList <- purrr::map2(FDeadList, ZDeadTotalList, \(FDead, ZDeadTotal) {
    ZDeadTotalFleet <- AddDimension(ZDeadTotal, 'Fleet') |> ExtendFleets(Fleets=FleetNames)
    ArrayDivide(FDead, ZDeadTotalFleet)
  })
  names(FishingDeadList) <- names(NaturalMortalityList)

  NDeadList <- purrr::map2(NPRFList, ZDeadTotalList, \(NPRF, ZDeadTotal)
                           ArrayMultiply(NPRF, (1-exp(-ZDeadTotal))))


  Removals <- purrr::pmap(list(FishingDeadList, NDeadList, WeightFleetList), \(FishingDead, NDead, WeightFleet) {
    NDeadFleet <- AddDimension(NDead, 'Fleet') |> ExtendFleets(Fleets=FleetNames)
    removals <- ArrayMultiply(FishingDead, NDeadFleet) |> ArrayMultiply(WeightFleet) |>
      SumOverFleet() |> SumOverAge()
    removals
  }) |>
    List2Array('Stock', pos=2)

  stockInd <- which(names(dimnames(FRetain)) == 'Stock')
  FRetainList <- FRetain |> Array2List(stockInd)
  FishingRetainList <- purrr::map2(FRetainList, ZDeadTotalList, \(FRetain, ZDeadTotal) {
    ZDeadTotalFleet <- AddDimension(ZDeadTotal, 'Fleet') |> ExtendFleets(Fleets=FleetNames)
    ArrayDivide(FRetain, ZDeadTotalFleet)
  })
  names(FishingRetainList) <- names(NaturalMortalityList)

  Landings <- purrr::pmap(list(FishingRetainList, NDeadList, WeightFleetList), \(FishingRetain, NDead, WeightFleet) {
    NDeadFleet <- AddDimension(NDead, 'Fleet') |> ExtendFleets(Fleets=FleetNames)
    removals <- ArrayMultiply(FishingRetain, NDeadFleet) |> ArrayMultiply(WeightFleet) |>
      SumOverFleet() |> SumOverAge()
    removals
  }) |> List2Array('Stock', pos=2)

  Biomass <- purrr::map2(NPRFList, WeightList, \(NPRF, Weight) {
    ArrayMultiply(NPRF, Weight) |> SumOverAge()
  }) |> List2Array("Stock", pos=2)

  SBiomass <- purrr::pmap(list(NPRF_SPList, WeightList, MaturityList), \(NPRF_SP, Weight, Maturity) {
    ArrayMultiply(NPRF_SP, Weight) |> ArrayMultiply(Maturity) |> SumOverAge()
  }) |> List2Array("Stock", pos=2)

  SProduction <- purrr::map2(NPRF_SPList,FecundityList, \(NPRF_SP, Fecundity) {
    ArrayMultiply(NPRF_SP, Fecundity) |> SumOverAge()
  }) |> List2Array("Stock", pos=2)


  PerRecruit <- new('perrecruit')
  PerRecruit@SPR0 <- SPR0List |> List2Array("Stock", pos=2)
  PerRecruit@apicalF <- apicalF
  PerRecruit@NPRF <- NPRFList |> List2Array("Stock", pos=2)
  if (IsSpawnTimeFrac)
    PerRecruit@NPRF_SP <- NPRF_SPList |> List2Array("Stock", pos=2)
  PerRecruit@SPRF <- SPRFList |>  List2Array("Stock", pos=2)
  PerRecruit@SPR <- SPR
  PerRecruit@Biomass <- Biomass
  PerRecruit@SBiomass <- SBiomass
  PerRecruit@SProduction <- SProduction
  PerRecruit@Removals <- Removals
  PerRecruit@Landings <- Landings
  PerRecruit

}



PrepPerRecruitInputs <- function(StockList, FleetList, SPR0List, Years) {
  
  FleetNames <- names(FleetList[[1]])
  
  NaturalMortalityList <- purrr::map(StockList, \(Stock)
                                     Stock@NaturalMortality@MeanAtAge |> ArraySubsetYear(Years))
  
  PlusGroupList <- purrr::map(StockList, \(Stock) Stock@Ages@PlusGroup)
  
  MaturityList <- purrr::map(StockList, \(Stock)
                             Stock@Maturity@MeanAtAge |> ArraySubsetYear(Years))
  
  SemelparousList <- purrr::map(StockList, \(Stock)
                                Stock@Maturity@Semelparous |> ArraySubsetYear(Years))
  
  WeightList <- purrr::map(StockList, \(Stock)
                           Stock@Weight@MeanAtAge |> ArraySubsetYear(Years))
  
  SpawnTimeFracList <- purrr::map(StockList, \(Stock) Stock@SRR@SpawnTimeFrac)
  
  FecundityList <- purrr::map(StockList, \(Stock)
                              Stock@Fecundity@MeanAtAge |> ArraySubsetYear(Years))
  
  SPFrom <- purrr::imap(StockList, \(stock, i) {
    spfrom <- stock@SRR@SPFrom
    if (is.null(spfrom))      spfrom <- i
    if (is.character(spfrom)) spfrom <- match(spfrom, names(StockList))
    spfrom
  }) |> unlist()
  
  # F-invariant: fleet allocation depends only on effort and efficiency
  StockFleetAllocation <- purrr::map(FleetList, \(fl)
                                     CalcFleetAllocationF(fl, Years)
  ) |> List2Array('Stock', pos = 2)
  
  WeightFleetList <- purrr::map(FleetList, \(fl) {
    purrr::map(fl, \(Fleet) Fleet@WeightFleet |> ArraySubsetYear(Years)) |>
      List2Array(pos = 4)
  })
  
  Selectivity <- purrr::map(FleetList, \(fl) {
    purrr::map(fl, \(Fleet) Fleet@Selectivity@MeanAtAge |> ArraySubsetYear(Years)) |>
      List2Array(pos = 4)
  }) |> List2Array('Stock', pos = 2) |> CheckSpatial('Selectivity')
  
  Retention <- purrr::map(FleetList, \(fl) {
    purrr::map(fl, \(Fleet) Fleet@Retention@MeanAtAge |> ArraySubsetYear(Years)) |>
      List2Array(pos = 4)
  }) |> List2Array('Stock', pos = 2) |> CheckSpatial('Retention')
  
  DiscardMortality <- purrr::map(FleetList, \(fl) {
    purrr::map(fl, \(Fleet) Fleet@DiscardMortality@MeanAtAge |> ArraySubsetYear(Years)) |>
      List2Array(pos = 4)
  }) |> List2Array('Stock', pos = 2) |> CheckSpatial('DiscardMortality')
  
  # SRR quantities needed for MSY recruitment scaling
  RecParsList <- purrr::map2(StockList, SPR0List, \(Stock, SPR0) {
    Pars      <- purrr::map(Stock@SRR@Pars, \(pars) ArraySubsetYear(pars, Years))
    Pars$R0   <- ArraySubsetYear(Stock@SRR@R0, Years)
    Pars$SPR0 <- ArraySubsetYear(SPR0, Years)
    Pars
  })
  RecParsList <- RecParsList[SPFrom]
  names(RecParsList) <- names(SPFrom)
  
  R0 <- purrr::map(StockList, \(Stock)
                   Stock@SRR@R0 |> ArraySubsetYear(Years)
  ) |> List2Array('Stock') |> aperm(c('Sim', 'Stock', 'Year'))
  
  RelRecFunList <- purrr::map(StockList, \(Stock) {
    if (!is.null(Stock@SRR@Model) && inherits(Stock@SRR@Model, 'character')) {
      if (is.null(Stock@SRR@RelRecFun)) {
        mod <- get(paste0(Stock@SRR@Model, 'RelRec'))
        class(mod) <- 'function'
        Stock@SRR@RelRecFun <- mod
      }
    }
    if (inherits(Stock@SRR@RelRecFun, 'character'))
      Stock@SRR@RelRecFun <- get(Stock@SRR@RelRecFun)
    Stock@SRR@RelRecFun
  })
  
  list(
    NaturalMortalityList = NaturalMortalityList,
    PlusGroupList        = PlusGroupList,
    MaturityList         = MaturityList,
    SemelparousList      = SemelparousList,
    WeightList           = WeightList,
    SpawnTimeFracList    = SpawnTimeFracList,
    FecundityList        = FecundityList,
    SPFrom               = SPFrom,
    SPR0List             = SPR0List,
    StockFleetAllocation = StockFleetAllocation,
    WeightFleetList      = WeightFleetList,
    Selectivity          = Selectivity,
    Retention            = Retention,
    DiscardMortality     = DiscardMortality,
    RecParsList          = RecParsList,
    R0                   = R0,
    RelRecFunList        = RelRecFunList,
    FleetNames           = FleetNames,
    Years                = Years
  )
}

