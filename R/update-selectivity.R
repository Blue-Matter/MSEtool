#' Update selectivity or retention across all simulations
#'
#' Expands selectivity/retention arrays for future projection years then
#' delegates per-simulation updates to `.UpdateSelectivitySim()`.
#' 
#' @param Proj A `Proj` object.
#' @param Year Integer. Current projection year.
#' @param AdviceSimList Nested list of `advice` objects, indexed by sim then complex.
#' @param LastAdviceSimList Same structure as `AdviceSimList` for the previous year.
#' @param YearsHist Integer vector of historical years (unused here, kept for
#'   consistent `update_funs` signature).
#' @param YearsProj Integer vector of projection years.
#' @param Areas Integer vector of area indices.
#' @param FleetNames Character vector of fleet names.
#' @param StockNames Character vector of stock names.
#' @param type One of `"Selectivity"` or `"Retention"`.
#' @return Updated `Proj` object.
#' @keywords internal
.UpdateSelectivity <- function(Proj,
                               Year, 
                               AdviceSimList, 
                               LastAdviceSimList,
                               YearsHist,
                               YearsProj, 
                               Areas, 
                               FleetNames,
                               StockNames,
                               type=c('Selectivity', 'Retention')) {
  
  type        <- match.arg(type)
  nSim        <- Proj@OM@nSim
  nStock      <- nStock(Proj)
  nFleet      <- length(FleetNames)
  nArea       <- length(Areas)
  FutureYears <- YearsProj[YearsProj >= Year]
  
  if (.AllAdviceNull(AdviceSimList, type))
    return(Proj)
  
  # Expand Selectivity/Retention arrays to cover future years
  for (st in seq_len(nStock)) {
    for (fl in seq_len(nFleet)) {
      target <- slot(Proj@OM@Fleet[[st]][[fl]], type)
      target@MeanAtAge    <- Extend(target@MeanAtAge,    nSim = nSim, Years = FutureYears)
      target@MeanAtLength <- Extend(target@MeanAtLength, nSim = nSim, Years = FutureYears)
      target@MeanAtWeight <- Extend(target@MeanAtWeight, nSim = nSim, Years = FutureYears)
      slot(Proj@OM@Fleet[[st]][[fl]], type) <- target
    }
  }
  
  for (sim in seq_len(nSim)) {
    Proj <- .UpdateSelectivitySim(
      Proj           = Proj,
      sim            = sim,
      FutureYears    = FutureYears,
      AdviceList     = AdviceSimList[[sim]],
      LastAdviceList = LastAdviceSimList[[sim]],
      FleetNames     = FleetNames,
      nFleet         = nFleet,
      Complexes      = Proj@OM@Complexes,
      nArea          = nArea,
      nSim           = nSim,
      type           = type
    )
  }
  
  Proj
}


.CheckFleetWeightConsistency <- function(WSel, WRet, RetAge, st, FleetName) {
  if (is.null(RetAge) || is.null(WSel) || is.null(WRet)) return(invisible(NULL))
  
  # worst case over areas: retention closest to 1 gives the tightest bound
  dn <- names(dimnames(RetAge))
  if (!is.null(dn) && 'Area' %in% dn)
    RetAge <- apply(RetAge, setdiff(dn, 'Area'), max)
  
  if (!identical(dim(RetAge), dim(WSel)) || !identical(dim(WRet), dim(WSel)))
    return(invisible(NULL))
  
  bad <- WSel + 1e-8 < RetAge * WRet
  if (any(bad, na.rm = TRUE))
    cli::cli_abort(
      c("Fleet weight-at-age is inconsistent with retention-at-age for stock {st}, fleet {.val {FleetName}}.",
        "x" = "{sum(bad, na.rm = TRUE)} age/year combination{?s} would give negative discard biomass.",
        "i" = "Expected {.code WeightFleetSelected >= Retention * WeightFleetRetained}."),
      .internal = TRUE
    )
  
  invisible(NULL)
}

#' Update selectivity or retention for a single simulation
#'
#' Populates selectivity/retention arrays from advice for all future projection
#' years, updating both `Proj@OM@Fleet` and the relevant `Proj@Misc` lists.
#' Skips a complex when management is unchanged or advice is `NULL`.
#'
#' The fully-populated (fleet-wide-adoption) arrays are then blended against
#' the pre-update, status-quo arrays using `Imp@Size@Compliance` -- the
#' fraction of the fleet that adopts the new advice this year (default `1`,
#' i.e. full/immediate adoption, when `Imp`/`Compliance` is unset). See
#' [imp-class].
#'
#' @param Proj A `Proj` object.
#' @param sim Integer. Simulation index.
#' @param FutureYears Integer vector of years from current year to end of projection.
#' @param AdviceList List of `advice` objects for this simulation, one per complex.
#' @param LastAdviceList Same structure as `AdviceList` for the previous year.
#' @param FleetNames Character vector of fleet names.
#' @param nFleet Integer. Number of fleets.
#' @param Complexes List mapping complex indices to stock indices.
#' @param nArea Integer. Number of areas.
#' @param nSim Integer. Total number of simulations.
#' @param type One of `"Selectivity"` or `"Retention"`.
#' @return Updated `Proj` object.
#' @keywords internal
.UpdateSelectivitySim <- function(Proj,
                                   sim,
                                   FutureYears,
                                   AdviceList,
                                   LastAdviceList,
                                   FleetNames,
                                   nFleet,
                                   Complexes,
                                   nArea,
                                   nSim,
                                   type=c('Selectivity', 'Retention')) {
  
  type <- match.arg(type)
  
  populate  <- list(Selectivity=PopulateSelectivity,
                    Retention=PopulateRetention)[[type]]
  age_misc  <- list(Selectivity='SelAgeList',
                    Retention='RetAgeList')[[type]]
  size_misc <- list(Selectivity='SelSizeList',
                    Retention='RetSizeList')[[type]]
  
  for (i in seq_along(AdviceList)) {
    stocks          <- Complexes[[i]]
    ComplexName     <- names(Complexes)[i]
    Advice          <- AdviceList[[i]]
    AdvicePrevious  <- LastAdviceList[[i]]
    
    if (!inherits(Advice, 'advice')) next    
    if (is.null(slot(Advice,type))) next
    if (.UnchangedManagement(Advice, AdvicePrevious, slotName=type)) next
    
    SelectList <- slot(Advice, type) 
    
    if (length(SelectList) > 1 && length(SelectList) != nFleet)
      stop("Advice@", type, " must be a `", type, "()` object or a list of ",
           "`", type, "()` objects of length nFleet (", nFleet, ")")
    
    for (st in stocks) {
      Stock    <- Proj@OM@Stock[[st]]
      Ages     <- Stock@Ages
      Length   <- Subset(Stock@Length,   Sims=sim, Years=FutureYears)
      Weight   <- Subset(Stock@Weight,   Sims=sim, Years=FutureYears)
      Maturity <- Subset(Stock@Maturity, Sims=sim, Years=FutureYears)
      
      ReComputeALKList <- vector('list', length(SelectList))
      
      if (is.list(SelectList)) {
        ClassesList <- purrr::map(SelectList, slot, 'Classes') 
      } else {
        ClassesList <- list(SelectList@Classes)
      }
      
      for (fl in seq_along(ClassesList)) {
        if (is.null(ClassesList[[fl]])) 
          ClassesList[[fl]]  <- Length@Classes
         
        ReComputeALKList[[fl]] <- !setequal(Length@Classes, ClassesList[[fl]])
      }
      
      all_same <- all(sapply(ReComputeALKList[-1], identical, ReComputeALKList[[1]]))
      if (all_same) ReComputeALKList <- list(ReComputeALKList[[1]])
      
      all_same <- all(sapply(ClassesList[-1], identical, ClassesList[[1]]))
      if (all_same) ClassesList <- list(ClassesList[[1]])
    
      LinIntAge <- length(Ages@Classes) < 50
      
      for (fl in seq_along(FleetNames)) {
        select <- if (is.list(SelectList)) SelectList[[fl]] else SelectList
        
        # Compute ALK with new size classes and finer temporal resolution
        FleetLength  <- Length 
        Classes      <- ClassesList[[min(length(ClassesList), fl)]]
        ReComputeALK <- ReComputeALKList[[min(length(ReComputeALKList), fl)]]
        if (fl == 1) {
          # Get ALK using Length@Classes
          if (LinIntAge) {
            # Increases the temporal resolution of `ObjectMeanAtAge` and `ASK`
            # by linear interpolate Mean length-at-age and CV length-at-age
            ALK_1 <- CalcAgeSizeKey(MeanAtAge=.LinearInterpolateAge(Length@MeanAtAge),
                                  CVatAge=.LinearInterpolateAge(Length@CVatAge),
                                  Classes=Length@Classes,
                                  TruncSD=Length@TruncSD,
                                  Dist=Length@Dist,
                                  silent=TRUE)
          } else {
            ALK_1 <- Length@ALK
          }
        }
        
        if (ReComputeALK) {
          if (LinIntAge) {
            ALK <- CalcAgeSizeKey(MeanAtAge=.LinearInterpolateAge(Length@MeanAtAge),
                                    CVatAge=.LinearInterpolateAge(Length@CVatAge),
                                    Classes=Classes,
                                    TruncSD=Length@TruncSD,
                                    Dist=Length@Dist,
                                    silent=TRUE)
          } else {
            ALK <- CalcAgeSizeKey(MeanAtAge=Length@MeanAtAge,
                                  CVatAge=Length@CVatAge,
                                  Classes=Classes,
                                  TruncSD=Length@TruncSD,
                                  Dist=Length@Dist,
                                  silent=TRUE)
          }
          FleetLength@ALK <- CalcAgeSizeKey(MeanAtAge = Length@MeanAtAge,
                                       CVatAge   = Length@CVatAge,
                                       Classes   = Classes,
                                       TruncSD   = Length@TruncSD,
                                       Dist      = Length@Dist,
                                       silent    = TRUE)
          
          
        } else {
          ALK <- ALK_1
        }
        
        FleetLength@Classes <- Classes
        
        # Reshape mean-at-x slots to [nClass, nArea] then add Sim/Year dims
        select <- .ProcessSelectMeanAtAge(select,    Ages,   nArea, type, Year=FutureYears[1])
        select <- .ProcessSelectMeanAtLength(select, FleetLength, nArea, type, Year=FutureYears[1])
        select <- .ProcessSelectMeanAtWeight(select, Weight, nArea, type, Year=FutureYears[1])
        
        select <- populate(select,
                           Ages     = Ages,
                           Length   = FleetLength,
                           Weight   = Weight,
                           Maturity = Maturity,
                           nSim     = 1,
                           Years    = FutureYears,
                           nArea    = nArea,
                           CalcAtLength = TRUE,
                           silent   = TRUE,
                           replace  = TRUE,
                           ASKOverride = ALK)
        
        select@MeanAtAge <- .SetSimDimname(select@MeanAtAge, sim) |> 
          ExtendAreas(1:nArea) |> 
          ExtendYears(FutureYears)
        
        select@MeanAtLength <- .SetSimDimname(select@MeanAtLength, sim) |>
          ExtendAreas(1:nArea) |>
          ExtendYears(FutureYears)
        
        select@MeanAtWeight <- .SetSimDimname(select@MeanAtWeight, sim) |> 
          ExtendAreas(1:nArea) |> 
          ExtendYears(FutureYears)
        
        target <- slot(Proj@OM@Fleet[[st]][[fl]], type)

        ImpCx    <- Proj@OM@Imp[[ComplexName]]
        ImpObj   <- if (!is.null(ImpCx)) ImpCx[[FleetNames[fl]]] else NULL
        compFull <- if (!is.null(ImpObj)) ImpObj@Size@Compliance else NULL
        yr_chr   <- as.character(FutureYears[1])
        comp     <- if (!is.null(compFull) && length(compFull) &&
                        !is.null(dim(compFull)) && yr_chr %in% dimnames(compFull)$Year) {
          compFull[min(sim, nrow(compFull)), yr_chr]
        } else if (length(compFull)) {
          as.numeric(compFull)[1]
        } else {
          NA_real_
        }
        if (is.na(comp)) comp <- 1

        if (comp < 1) {
          baseline_age    <- Subset(target@MeanAtAge,    Sims = sim, Years = FutureYears)
          baseline_length <- Subset(target@MeanAtLength, Sims = sim, Years = FutureYears)
          baseline_weight <- Subset(target@MeanAtWeight, Sims = sim, Years = FutureYears)

          select@MeanAtAge    <- comp * select@MeanAtAge    + (1 - comp) * baseline_age
          select@MeanAtLength <- comp * select@MeanAtLength + (1 - comp) * baseline_length
          select@MeanAtWeight <- comp * select@MeanAtWeight + (1 - comp) * baseline_weight
        }

        ArrayFill(target@MeanAtAge)    <- select@MeanAtAge
        ArrayFill(target@MeanAtLength) <- select@MeanAtLength
        ArrayFill(target@MeanAtWeight) <- select@MeanAtWeight
        slot(Proj@OM@Fleet[[st]][[fl]], type) <- target
        
        ArrayFill(Proj@Misc[[age_misc]][[st]]) <- AddDimension(select@MeanAtAge,
                                                               'Fleet', 
                                                               val=FleetNames[fl],
                                                               pos=4)
        
        ArrayFill(Proj@Misc[[size_misc]][[st]][[fl]])   <- select@MeanAtLength

        FleetObj <- Proj@OM@Fleet[[st]][[fl]]
        SelObj   <- if (type == 'Selectivity') select else
          Subset(FleetObj@Selectivity, Sims = sim, Years = FutureYears)
        RetObj   <- if (type == 'Retention') select else
          Subset(FleetObj@Retention,   Sims = sim, Years = FutureYears)

        WSel <- .CalcFleetWeightAtAge(SelObj, Weight, FleetLength)
        WRet <- .CalcFleetWeightAtAge(SelObj, Weight, FleetLength,
                                      Retention = RetObj)

        .CheckFleetWeightConsistency(WSel, WRet, RetObj@MeanAtAge,
                                     st, FleetNames[fl])

        ArrayFill(FleetObj@WeightFleetSelected) <- WSel
        ArrayFill(FleetObj@WeightFleetRetained) <- WRet
        Proj@OM@Fleet[[st]][[fl]] <- FleetObj

        ArrayFill(Proj@Misc$WeightFleetSelectedList[[st]]) <-
          AddDimension(WSel, 'Fleet', val = FleetNames[fl], pos = 4)
        ArrayFill(Proj@Misc$WeightFleetRetainedList[[st]]) <-
          AddDimension(WRet, 'Fleet', val = FleetNames[fl], pos = 4)

      } # end fleet loop
    }  # end stock loop
  } # end complex loop
  Proj
} 

#' Update retention across all simulations
#'
#' Thin wrapper around `.UpdateSelectivity()` with `type = "Retention"`.
#'
#' @inheritParams .UpdateSelectivity
#' @return Updated `Proj` object.
#' @keywords internal
.UpdateRetention <- function(Proj,
                             Year, 
                             AdviceSimList,
                             LastAdviceSimList,
                             YearsHist,
                             YearsProj,
                             Areas, 
                             FleetNames,
                             StockNames) {
  
  type <- 'Retention'
  
  .UpdateSelectivity(Proj,
                     Year, 
                     AdviceSimList,
                     LastAdviceSimList, 
                     YearsHist,
                     YearsProj,
                     Areas, 
                     FleetNames,
                     StockNames,
                     type = type)
  
}

.SetSimDimname <- function(x, sim) {
  if (is.null(x)) return(x)
  if (!is.array(x)) return(x)
  
  dn <- dimnames(x)
  if (is.null(dn) || !"Sim" %in% names(dn)) return(x)
  
  dn[["Sim"]] <- sim
  dimnames(x) <- dn
  x
}
