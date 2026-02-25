#' Initialize dynamic population state in Hist
#'
#' Internal helper that constructs the initial age–area population structure
#' in a [Hist()] object by applying recruitment deviations, spatial
#' distribution, and optional initial depletion.
#'
#' @param Hist A [Hist()] object with initialized unfished equilibrium state.
#'
#' @return The modified [Hist()] object.
#'
#' @keywords internal
CalcDynamicInitial <- function(Hist) {
  nSim <- nSim(Hist)

  # ---- Loop over stocks -----
  for (st in 1:nStock(Hist)) {

    ## ---- Calculate dynamic age structure (multiply by rec devs) ----
    EquilNumber <- abind::adrop(Hist@Unfished@Equilibrium@Number[[st]][,,1, drop=FALSE], 3)

    RecDevInit <- Hist@OM@Stock[[st]]@SRR@RecDevInit |> ExtendSims(nSim)
    RecDevHist <- Hist@OM@Stock[[st]]@SRR@RecDevHist
    RecDevHist1 <- RecDevHist[,1, drop=FALSE] |> ExtendSims(nSim)
    names(dimnames(RecDevHist1))[2] <- 'Age'

    ages <- as.numeric(dimnames(RecDevInit)[['Age']])
    AgeClasses <- Hist@OM@Stock[[st]]@Ages@Classes
    if (min(ages) !=AgeClasses[2]) {
      cli::cli_abort(c("Error calculating initial age structure for Stock: {.val {names(Hist@OM@Stock)[st]}}",
                       "i"='The first age class in matrix `Stock |> SRR() |> RecDevInit()` must match the second age class',
                       '*'='Second age class: {.val {AgeClasses[2]}}',
                       '*'='First age class in `RecDevInit`: {.val {min(ages)}}'
      ), call=NULL)
    }

    InitAgeClassRecDevs <- cbind(RecDevHist1, RecDevInit)
    dimnames(InitAgeClassRecDevs) <- list(Sim=1:nSim,
                                          Age=c(AgeClasses[1], ages))


    ## ---- Distribute across areas ----
    UnfishedDist <- abind::adrop(Hist@OM@Stock[[st]]@Spatial@UnfishedDist[,,,1,drop=FALSE], 4) |>
      aperm(c('Sim', 'Age', 'Area'))

    nArea <- dim(UnfishedDist)[3]
    if (nArea>1 & Hist@OM@Seasons>1) {
      # cli::cli_abort(c("x"="Multi-area seasonal model spatial distribution are not currently supported."), .internal=TRUE)
      # This code maps out nAge seasons of nAge cohorts prior to season 1, year 1.
      # An initial vector of nAge season of R0 are multiplied by recdevs and distributed by movement in age class 1
      # Then these numbers are moved up cohorts according to movement by age
      InitMovMat = Hist@OM@Stock[[st]]@Spatial@Movement[,,,,1,drop=F] # Movement in first time step [sim, from, to ,age]
      Hist@Number[[st]][,,1,] <- calc_seas_spat(InitAgeClassRecDevs, UnfishedDist, InitMovMat, Hist@OM@Stock[[st]]@SRR@R0, Seasons(Hist))

    } else {
      # Multiply unfished by initial rec devs and add an Area dimension
      NatAge <- ArrayMultiply(InitAgeClassRecDevs, EquilNumber) |> AddDimension('Area')

      # Multiply by UnfishedDist to distribute across areas
      Hist@Number[[st]][,,1,] <- ArrayMultiply(NatAge, UnfishedDist)
    }

    ## ---- Fill recruitment for initial time steps if age rec > 0  ----
    RecruitTimeStep <- CalcRecruitment_AgeIndex(Hist, st)

    if (RecruitTimeStep>1) {
      # fill in recruits for initial time steps
      for (ts in 2:(RecruitTimeStep-1)) {
        UnfishedDist <- Hist@OM@Stock[[st]]@Spatial@UnfishedDist[,,1,ts,drop=FALSE] |>
          aperm(c('Sim', 'Age', 'Year', 'Area'))
        Recruit <- Hist@OM@Stock[[st]]@SRR@R0[,ts, drop=FALSE] |>
          AddDimension('Area') |>
          AddDimension('Age') |>
          aperm(c('Sim', 'Age', 'Year', 'Area'))
        Hist@Number[[st]][,1,ts,] <-  ArrayMultiply(Recruit, UnfishedDist)
      }
    }

    # ---- Initial Depletion ----
    InitialDepletion <- Hist@OM@Stock[[st]]@Depletion@Initial
    if (length(InitialDepletion) && all(InitialDepletion!=1))  {
      Hist <- DoOptInitialDepletion(Hist, st)
    }
  }

  Hist
}

# TODO - DoOptInitialDepletion should probably account for selectivity,
# but most of the time it's already done in Import(OM)

#' Apply initial depletion by scaling numbers-at-age
#'
#' Internal helper that rescales initial numbers-at-age to match a target
#' depletion level relative to unfished biomass or spawning biomass.
#'
#' @param Hist A [Hist()] object.
#' @param st Integer stock index.
#'
#' @return The modified [Hist()] object.
#'
#' @keywords internal
DoOptInitialDepletion <- function(Hist, st) {
  DepletionInitial <- Hist@OM@Stock[[st]]@Depletion@Initial
  DepletionReference <- Hist@OM@Stock[[st]]@Depletion@Reference

  if (is.null(DepletionInitial))
    return(Hist)

  if (!DepletionReference %in% c('B0', 'SB0'))
    cli::cli_abort("Currently only accepts `Depletion@Reference = 'B0' or 'SB0'")

  if (DepletionReference == 'B0') {
    # currently using Unfished Equilibrium Biomass from first time step
    RefVal <- abind::adrop(Hist@Unfished@Equilibrium@Biomass[,,1, drop=FALSE], 3) |> ExtendSims(Hist@OM@nSim) |>
      apply(c('Sim', 'Stock'), sum)
  } else {
    RefVal <- abind::adrop(Hist@Unfished@Equilibrium@SBiomass[,,1, drop=FALSE], 3) |> ExtendSims(Hist@OM@nSim) |>
      apply(c('Sim', 'Stock'), sum)
  }
  RefVal <- RefVal[,st, drop=FALSE] |> abind::adrop(2)

  NatAge <- Hist@Number[[st]][,,1,, drop=FALSE] |> abind::adrop(3)
  NumberAtAgeList <- Array2List(apply(NatAge, c('Sim', 'Age'), sum), 1)
  WeightAtAgeList <- Array2List(abind::adrop(Hist@OM@Stock[[st]]@Weight@MeanAtAge[,,1, drop=FALSE],3), 1)
  MaturityAtAgeList <- Array2List(abind::adrop(Hist@OM@Stock[[st]]@Maturity@MeanAtAge[,,1, drop=FALSE],3), 1)
  FecundityAtAgeList <- Array2List(abind::adrop(Hist@OM@Stock[[st]]@Fecundity@MeanAtAge[,,1, drop=FALSE],3), 1)

  interval=c(0.01, 10)

  dopt <- purrr::pmap(list(
    NumberAtAge=NumberAtAgeList,
    WeightAtAge=WeightAtAgeList,
    MaturityAtAge=MaturityAtAgeList,
    FecundityAtAge=FecundityAtAgeList,
    DepletionInitial=as.list(DepletionInitial),
    RefVal=as.list(RefVal)
  ), \(NumberAtAge, WeightAtAge, MaturityAtAge, FecundityAtAge,
       DepletionInitial, RefVal) {

    optimize(OptInitialDepletion,
             interval=c(0.01, 10),
             NumberAtAge=NumberAtAge,
             WeightAtAge=WeightAtAge,
             MaturityAtAge=MaturityAtAge,
             FecundityAtAge=FecundityAtAge,
             DepletionInitial=DepletionInitial,
             DepletionReference=DepletionReference,
             RefVal=RefVal)
  })

  nAge <- ncol(NatAge)
  nArea <- nArea(Hist@OM)

  adjust <- lapply(dopt, '[[', 'minimum') |>
    unlist() |>
    array(dim=length(dopt), dimnames=list(Sim=1:length(dopt))) |>
    AddDimension("Age") |> AddDimension("Area")

  Hist@Number[[st]][,,1,] <- ArrayMultiply(NatAge, adjust)
  Hist
}


#' Objective function for initial depletion optimization
#'
#' Internal objective function used to scale numbers-at-age such that biomass
#' or spawning biomass matches a target depletion level.
#'
#' @keywords internal
OptInitialDepletion <- function(par=1,
                                NumberAtAge,
                                WeightAtAge,
                                MaturityAtAge,
                                FecundityAtAge,
                                DepletionInitial,
                                DepletionReference,
                                RefVal) {
  NumberAtAge <- par * NumberAtAge

  if (DepletionReference == 'B0') {
    val <- sum(NumberAtAge * WeightAtAge)
    ssq <- ((val/RefVal - DepletionInitial)^2)
  }

  if (DepletionReference=='SB0') {
    val <-  sum(NumberAtAge * WeightAtAge * MaturityAtAge)
    ssq <- ((val/RefVal- DepletionInitial)^2)
  }
  ssq
}
