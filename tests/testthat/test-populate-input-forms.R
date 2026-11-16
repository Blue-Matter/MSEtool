skip_on_cran()

OM <- SingleStockOM
nSim(OM) <- 3
P0 <- suppressMessages(Populate(OM, silent = TRUE))
nS <- nSim(OM)
Ages <- P0@Stock[[1]]@Ages
nA <- length(Ages@Classes)
Yrs <- Years(P0)
HY <- Years(P0, 'H')
LenClasses <- P0@Stock[[1]]@Length@Classes
nL <- length(LenClasses)

# weight classes exist only when Weight@CVatAge is set
OMW <- OM
OMW@Stock[[1]]@Weight@CVatAge <- 0.1
PW <- suppressMessages(Populate(OMW, silent = TRUE))
WtClasses <- PW@Stock[[1]]@Weight@Classes
nW <- length(WtClasses)

Pop <- function(o) suppressMessages(suppressWarnings(Populate(o, silent = TRUE)))

# NULL when the OM populates, else the error message
TryPop <- function(o) tryCatch({ Pop(o); NULL }, error = function(e) conditionMessage(e))

SetComp <- function(o, comp, fn) {
  if (comp %in% c('Selectivity', 'Retention', 'DiscardMortality', 'Effort', 'Catchability')) {
    methods::slot(o@Fleet[[1]][[1]], comp) <- fn(methods::slot(o@Fleet[[1]][[1]], comp))
  } else {
    methods::slot(o@Stock[[1]], comp) <- fn(methods::slot(o@Stock[[1]], comp))
  }
  o
}

UsePars <- function(Pars, Model = NULL) {
  force(Pars); force(Model)
  function(x) {
  x@Pars <- Pars
  x@Model <- Model
  if ('isRel' %in% methods::slotNames(x)) x@isRel <- FALSE
  for (sl in intersect(c('MeanAtAge', 'MeanAtLength', 'MeanAtWeight'), methods::slotNames(x)))
    methods::slot(x, sl) <- NULL
  x
  }
}

UseArray <- function(slot, value, Classes = NULL) {
  force(slot); force(value); force(Classes)
  function(x) {
  if ('Pars' %in% methods::slotNames(x)) x@Pars <- list()
  if ('Model' %in% methods::slotNames(x)) x@Model <- NULL
  for (sl in intersect(c('MeanAtAge', 'MeanAtLength', 'MeanAtWeight'), methods::slotNames(x)))
    methods::slot(x, sl) <- NULL
  methods::slot(x, slot) <- value
  if (!is.null(Classes)) x@Classes <- Classes
  x
  }
}

ExpectPopulates <- function(o, label) {
  Err <- TryPop(o)
  expect(is.null(Err), paste0(label, ' failed to populate: ', Err))
}

ExpectAllPopulate <- function(Cases, Base = OM) {
  for (nm in names(Cases))
    ExpectPopulates(SetComp(Base, Cases[[nm]][[1]], Cases[[nm]][[2]]), nm)
}

test_that("every built-in model populates from Pars", {
  Models <- list(
    Length = list(vonBert = list(Linf = 120, K = 0.2, t0 = -0.5),
                  Gompertz = list(Linf = 120, g = 0.3, a = 2),
                  Richards = list(Linf = 120, K = 0.2, t0 = -0.5, d = 1.2),
                  Brody = list(L0 = 10, Linf = 120, K = 0.2)),
    Weight = list(WeightatMeanLength = list(alpha = 1.3e-5, beta = 3.1),
                  WeightatAge = list(a = 0.5, b = 2),
                  WeightatLength = list(Alpha = 1.3e-5, Beta = 3.1)),
    NaturalMortality = list(MortalityAtAge = list(M = 0.3),
                            LorenzenMortalityLength = list(Mref = 0.4, Lref = 80),
                            LorenzenMortalityWeight = list(Mref = 0.4, Wref = 10)),
    Maturity = list(MaturityAtAge = list(A50 = 3, A50_95 = 1),
                    MaturityAtLength = list(L50 = 85, L50_95 = 10),
                    MaturityAtWeight = list(W50 = 10, W50_95 = 3)),
    Fecundity = list(FecundityAtAge = list(A50 = 3, A50_95 = 1, MaxFec = 1),
                     FecundityAtLength = list(L50 = 85, L50_95 = 10, MaxFec = 1),
                     FecundityAtWeight = list(W50 = 10, W50_95 = 3, MaxFec = 1)),
    SRR = list(BevertonHolt = list(h = 0.7), Ricker = list(hR = 1.2), HockeyStick = list(Shinge = 0.3)),
    Selectivity = list(DoubleNormal = list(L5 = 60, LFS = 90, Vmaxlen = 0.8),
                       DoubleNormalWeight = list(W5 = 5, WFS = 15),
                       SelectivityAtAge = list(SA50 = 3, SA50_95 = 1),
                       SelectivityAtLength = list(SL50 = 80, SL50_95 = 10),
                       SelectivityAtWeight = list(SW50 = 10, SW50_95 = 3),
                       SelectivityKnifeEdgeAge = list(SA = 3),
                       SelectivityKnifeEdgeLength = list(SL = 80)),
    Retention = list(DoubleNormalRetention = list(LR5 = 60, LFR = 90),
                     DoubleNormalRetentionWeight = list(WR5 = 5, WFR = 15),
                     RetentionAtAge = list(RA50 = 3, RA50_95 = 1),
                     RetentionAtLength = list(RL50 = 80, RL50_95 = 10),
                     RetentionAtWeight = list(RW50 = 10, RW50_95 = 3),
                     RetentionKnifeEdgeAge = list(RA = 3),
                     RetentionKnifeEdgeLength = list(RL = 80))
  )
  for (comp in names(Models)) for (mod in names(Models[[comp]])) {
    Base <- if (grepl('Weight$', mod) && mod != 'WeightatMeanLength') OMW else OM
    P <- tryCatch(Pop(SetComp(Base, comp, UsePars(Models[[comp]][[mod]]))), error = function(e) conditionMessage(e))
    expect(methods::is(P, 'om'), paste(comp, mod, 'failed to populate:', if (is.character(P)) P))
    if (methods::is(P, 'om')) {
      Obj <- if (comp %in% c('Selectivity', 'Retention')) methods::slot(P@Fleet[[1]][[1]], comp) else methods::slot(P@Stock[[1]], comp)
      expect_identical(Obj@Model, mod, label = paste(comp, mod, 'Model'))
    }
  }
})

test_that("each Pars input form populates", {
  Arr2 <- array(c(0.3, 0.4), c(1, 2), list(Sim = 1, Year = HY[c(1, 10)]))
  ArrSims <- array(c(0.25, 0.3, 0.35), c(nS, 1), list(Sim = 1:nS, Year = HY[1]))
  Forms <- list(
    scalar = 0.3, bounds = c(0.25, 0.35), nSim_vector = c(0.25, 0.3, 0.35),
    named_year_array = Arr2, named_sim_array = ArrSims,
    unnamed_sim_array = array(c(0.25, 0.3, 0.35), c(nS, 1))
  )
  for (f in names(Forms)) {
    ExpectPopulates(SetComp(OM, 'NaturalMortality', UsePars(list(M = Forms[[f]]))), paste('Pars', f))
  }

  P <- Pop(SetComp(OM, 'NaturalMortality', UsePars(list(M = c(0.25, 0.35)))))
  M <- P@Stock[[1]]@NaturalMortality@Pars$M
  expect_true(all(M >= 0.25 & M <= 0.35))
  expect_gt(length(unique(as.numeric(M))), 1)

  P <- Pop(SetComp(OM, 'NaturalMortality', UsePars(list(M = c(0.25, 0.3, 0.35)))))
  expect_equal(as.numeric(P@Stock[[1]]@NaturalMortality@Pars$M[, 1]), c(0.25, 0.3, 0.35))

  P <- Pop(SetComp(OM, 'NaturalMortality', UsePars(list(M = Arr2))))
  MAA <- P@Stock[[1]]@NaturalMortality@MeanAtAge
  expect_equal(unique(as.numeric(MAA[1, , as.character(HY[1])])), 0.3)
  expect_equal(unique(as.numeric(MAA[1, , as.character(HY[10])])), 0.4)

  for (Pars in list(random_walk = list(M = 0.3, MSD = 0.1), bounds_random_walk = list(M = c(0.25, 0.35), MSD = 0.1))) {
    P <- Pop(SetComp(OM, 'NaturalMortality', UsePars(Pars)))
    MAA <- P@Stock[[1]]@NaturalMortality@MeanAtAge
    expect_gt(length(unique(as.numeric(MAA[1, 1, ]))), 1)
  }

  expect_s4_class(Pop(SetComp(OM, 'Length', UsePars(list(Linf = 120, K = 0.2, t0 = -0.5), 'vonBert'))), 'om')
  Custom <- function(Ages, M) seq(M, M / 2, length.out = length(Ages))
  P <- Pop(SetComp(OM, 'NaturalMortality', UsePars(list(M = c(0.3, 0.5)), Custom)))
  MAA <- P@Stock[[1]]@NaturalMortality@MeanAtAge
  expect_equal(as.numeric(MAA[1, nA, 1]), as.numeric(MAA[1, 1, 1]) / 2)
})

test_that("each MeanAtAge input form populates for every component", {
  Vals <- c(Length = 100, Weight = 10, NaturalMortality = 0.3, Maturity = 0.5, Fecundity = 1,
            Selectivity = 1, Retention = 1, DiscardMortality = 0.5)
  Forms <- function(v, fleet) c(list(
    vector = rep(v, nA),
    unnamed_1d = array(v, nA),
    unnamed_sim_age = array(v, c(1, nA)),
    unnamed_nsim_age = array(v, c(nS, nA)),
    unnamed_3d = array(v, c(1, nA, 1)),
    unnamed_3d_hist_years = array(v, c(1, nA, length(HY))),
    unnamed_3d_all_years = array(v, c(1, nA, length(Yrs))),
    named_change_years = array(v, c(1, nA, 2), list(Sim = 1, Age = Ages@Classes, Year = HY[c(1, 10)])),
    named_nsim = array(v, c(nS, nA, 1), list(Sim = 1:nS, Age = Ages@Classes, Year = HY[1]))),
    if (fleet) list(unnamed_4d = array(v, c(1, nA, 1, 1)),
                    named_4d = array(v, c(1, nA, 1, 2), list(Sim = 1, Age = Ages@Classes, Year = HY[1], Area = 1:2))))
  Cases <- list()
  for (comp in names(Vals)) {
    Fleet <- comp %in% c('Selectivity', 'Retention', 'DiscardMortality')
    F <- Forms(Vals[[comp]], Fleet)
    for (f in names(F))
      Cases[[paste(comp, f)]] <- list(comp, UseArray('MeanAtAge', F[[f]]))
  }
  ExpectAllPopulate(Cases)

  P <- Pop(SetComp(OM, 'NaturalMortality', UseArray('MeanAtAge', array(0.3, nA))))
  expect_equal(unique(as.numeric(P@Stock[[1]]@NaturalMortality@MeanAtAge)), 0.3)
})

test_that("each MeanAtLength input form populates and is converted to MeanAtAge", {
  Vals <- c(Weight = 10, NaturalMortality = 0.3, Maturity = 0.5, Fecundity = 1,
            Selectivity = 1, Retention = 1, DiscardMortality = 0.5)
  Forms <- function(v) list(
    named = array(v, c(1, nL, 1), list(Sim = 1, Class = LenClasses, Year = HY[1])),
    vector_with_classes = rep(v, nL),
    unnamed_3d_with_classes = array(v, c(1, nL, 1)),
    vector_without_classes = rep(v, nL))
  Cases <- list()
  for (comp in names(Vals)) {
    F <- Forms(Vals[[comp]])
    for (f in names(F))
      Cases[[paste(comp, f)]] <- list(comp, UseArray('MeanAtLength', F[[f]],
                                                     if (f != 'vector_without_classes') LenClasses))
  }
  ExpectAllPopulate(Cases)

  P <- Pop(SetComp(OM, 'Maturity', UseArray('MeanAtLength', rep(0.5, nL), LenClasses)))
  expect_false(is.null(P@Stock[[1]]@Maturity@MeanAtAge))
})

test_that("each MeanAtWeight input form populates", {
  Vals <- c(NaturalMortality = 0.3, Maturity = 0.5, Fecundity = 1, Selectivity = 1, Retention = 1)
  Cases <- list()
  for (comp in names(Vals)) {
    v <- Vals[[comp]]
    Cases[[paste(comp, 'named')]] <- list(comp, UseArray('MeanAtWeight',
      array(v, c(1, nW, 1), list(Sim = 1, Class = WtClasses, Year = HY[1])), WtClasses))
    Cases[[paste(comp, 'vector_with_classes')]] <- list(comp, UseArray('MeanAtWeight', rep(v, nW), WtClasses))
  }
  ExpectAllPopulate(Cases, OMW)
})

test_that("each CVatAge input form populates", {
  for (comp in c('Length', 'Weight')) {
    Forms <- list(scalar = 0.1, bounds = c(0.08, 0.12), vector = rep(0.1, nA),
                  named = array(0.1, c(1, nA, 1), list(Sim = 1, Age = Ages@Classes, Year = HY[1])))
    for (f in names(Forms))
      ExpectPopulates(SetComp(OM, comp, function(x) { x@CVatAge <- Forms[[f]]; x }), paste(comp, 'CVatAge', f))
  }
})

test_that("each form of the other primary slots populates", {
  Slot <- function(comp, sl, value) { force(value); list(comp, function(x) { methods::slot(x, sl) <- value; x }) }
  Cases <- list(
    `SRR R0 scalar` = Slot('SRR', 'R0', 1000),
    `SRR R0 bounds` = Slot('SRR', 'R0', c(800, 1200)),
    `SRR R0 nSim vector` = Slot('SRR', 'R0', c(800, 1000, 1200)),
    `SRR R0 year array` = Slot('SRR', 'R0', array(c(1000, 800), c(1, 2), list(Sim = 1, Year = HY[c(1, 10)]))),
    `SRR SD scalar` = Slot('SRR', 'SD', 0.5),
    `SRR SD bounds` = Slot('SRR', 'SD', c(0.3, 0.6)),
    `SRR AC bounds` = Slot('SRR', 'AC', c(0, 0.5)),
    `SRR SpawnTimeFrac` = Slot('SRR', 'SpawnTimeFrac', 0.5),
    `Depletion Final bounds` = Slot('Depletion', 'Final', c(0.2, 0.4)),
    `Depletion Final nSim vector` = Slot('Depletion', 'Final', c(0.2, 0.3, 0.4)),
    `Depletion Initial` = Slot('Depletion', 'Initial', 0.8),
    `Depletion Reference SB0` = Slot('Depletion', 'Reference', 'SB0'),
    `Spatial UnfishedDist scalar` = Slot('Spatial', 'UnfishedDist', 0.4),
    `Spatial UnfishedDist bounds` = Slot('Spatial', 'UnfishedDist', c(0.3, 0.5)),
    `Spatial ProbStaying bounds` = Slot('Spatial', 'ProbStaying', c(0.6, 0.9)),
    `Spatial RelativeSize EqualDensity` = Slot('Spatial', 'RelativeSize', 'EqualDensity'),
    `Spatial RelativeSize scalar` = Slot('Spatial', 'RelativeSize', 0.4),
    `Effort unnamed matrix` = Slot('Effort', 'Effort', matrix(1, 1, length(HY))),
    `Effort vector` = Slot('Effort', 'Effort', rep(1, length(HY))),
    `Effort data.frame` = Slot('Effort', 'Effort', data.frame(Year = c(0, 0.5, 1), Lower = c(0.2, 0.5, 1),
                                                             Upper = c(0.4, 0.8, 1.2), CV = 0.1)),
    `Effort data.frame no zero` = Slot('Effort', 'Effort', data.frame(Year = c(0.3, 1), Lower = c(0.5, 1), Upper = c(0.8, 1), CV = 0.1)),
    `Effort data.frame years` = Slot('Effort', 'Effort', data.frame(Year = range(HY), Lower = c(0.5, 1), Upper = c(0.8, 1), CV = 0.1)),
    `Effort Distribution 1-D` = Slot('Effort', 'Distribution', array(0.5, 2)),
    `Effort Targeting scalar` = Slot('Effort', 'Targeting', 1.5),
    `Effort Maximum scalar` = Slot('Effort', 'Maximum', 10),
    `Catchability Efficiency scalar` = Slot('Catchability', 'Efficiency', 0.1),
    `Catchability Efficiency array` = Slot('Catchability', 'Efficiency', array(0.1, c(1, 1), list(Sim = 1, Year = HY[1]))),
    `Catchability qCV` = Slot('Catchability', 'qCV', 0.1),
    `Catchability qInc` = Slot('Catchability', 'qInc', 1)
  )
  ExpectAllPopulate(Cases)

  Fleet <- function(sl, value) { force(value); function(o) { methods::slot(o@Fleet[[1]][[1]], sl) <- value; o } }
  ObsSet <- function(fn) function(o) { o@Obs[[1]][[1]] <- fn(o@Obs[[1]][[1]]); o }
  ImpSet <- function(fn) function(o) { o@Imp[[1]][[1]] <- fn(o@Imp[[1]][[1]]); o }
  OMCases <- list(
    `Closure 1-D` = Fleet('Closure', array(c(1, 0), 2)),
    `Closure 3-D named` = Fleet('Closure', array(1, c(1, 1, 2), list(Sim = 1, Year = HY[1], Area = 1:2))),
    `Obs Landings CV bounds` = ObsSet(function(x) { x@Landings@CV <- c(0.1, 0.3); x }),
    `Obs Landings Bias` = ObsSet(function(x) { x@Landings@Bias <- 0.2; x }),
    `Obs Landings Error 1-D` = ObsSet(function(x) { x@Landings@Error <- array(1, length(Yrs)); x }),
    `Obs Survey CV scalar` = ObsSet(function(x) { x@Survey@CV <- 0.2; x }),
    `Obs Survey AC bounds` = ObsSet(function(x) { x@Survey@AC <- c(0.1, 0.3); x }),
    `Obs Survey Selectivity Biomass` = ObsSet(function(x) { x@Survey@Selectivity <- 'Biomass'; x }),
    `Obs Survey Selectivity named vector` = ObsSet(function(x) { x@Survey@Selectivity <- c(`3` = 1, `4` = 1); x }),
    `Obs Survey Selectivity unnamed 1-D` = ObsSet(function(x) { x@Survey@Selectivity <- array(1, nA); x }),
    `Imp TAC Mean bounds` = ImpSet(function(x) { x@TAC@Mean <- c(0.9, 1.1); x }),
    `Imp TAC SD` = ImpSet(function(x) { x@TAC@SD <- 0.1; x }),
    `Imp TAC Compliance` = ImpSet(function(x) { x@TAC@Compliance <- 0.8; x })
  )
  for (nm in names(OMCases))
    ExpectPopulates(OMCases[[nm]](OM), nm)
})

test_that("an unnamed Year dimension must be 1, historical, or all time steps", {
  o <- SetComp(OM, 'NaturalMortality', UseArray('MeanAtAge', array(0.3, c(1, nA, 5))))
  expect_error(Pop(o), 'unnamed Year dimension; must have 1', fixed = TRUE, class = 'MSEtool_invalid_input')
  o <- SetComp(OM, 'NaturalMortality', UseArray('MeanAtLength', array(0.3, c(1, nL, 5)), LenClasses))
  expect_error(Pop(o), 'unnamed Year dimension; must have 1', fixed = TRUE, class = 'MSEtool_invalid_input')
  o <- SetComp(OM, 'Effort', function(x) { x@Effort <- matrix(1, 1, 5); x })
  expect_error(Pop(o), 'Year dimension', fixed = TRUE)
  P <- Pop(SetComp(OM, 'NaturalMortality', UseArray('MeanAtAge', array(0.3, c(1, nA, length(HY))))))
  expect_equal(dimnames(P@Stock[[1]]@NaturalMortality@MeanAtAge)$Year, as.character(HY))
})

test_that("bounds give their mean when nSim is 1", {
  o <- OM
  nSim(o) <- 1
  o <- SetComp(o, 'NaturalMortality', UsePars(list(M = c(0.2, 0.4))))
  o@Stock[[1]]@Depletion@Final <- c(0.2, 0.4)
  P <- Pop(o)
  expect_equal(as.numeric(P@Stock[[1]]@NaturalMortality@Pars$M), 0.3)
  expect_equal(as.numeric(P@Stock[[1]]@Depletion@Final), 0.3)
  L <- PopulateLength(Length(Pars = list(Linf = c(100, 120), K = 0.2, t0 = 0), CVatAge = 0.1),
                      Ages = Ages, Years = HY, nSim = 1)
  expect_equal(as.numeric(L@Pars$Linf), 110)
})

test_that("arrays leaving Populate have the expected dimension names and order", {
  P <- Pop(OM)
  Check <- function(obj, label) {
    Exp <- .ExpectedDims[[class(obj)[1]]]
    for (sl in names(Exp)) {
      Vals <- methods::slot(obj, sl)
      Items <- if (is.list(Vals)) Vals else list(Vals)
      for (x in Items) if (is.array(x))
        expect_identical(names(dimnames(x)), Exp[[sl]], label = paste(label, sl))
    }
  }
  St <- P@Stock[[1]]
  for (comp in c('Length', 'Weight', 'NaturalMortality', 'Maturity', 'Fecundity', 'SRR', 'Spatial', 'Depletion'))
    Check(methods::slot(St, comp), comp)
  Fl <- P@Fleet[[1]][[1]]
  Check(Fl, 'Fleet')
  for (comp in c('Effort', 'Catchability', 'Selectivity', 'Retention', 'DiscardMortality'))
    Check(methods::slot(Fl, comp), comp)

  Perm <- aperm(array(0.3, c(1, nA, 1), list(Sim = 1, Age = Ages@Classes, Year = HY[1])), c(2, 1, 3))
  P <- Pop(SetComp(OM, 'NaturalMortality', UseArray('MeanAtAge', Perm)))
  expect_identical(names(dimnames(P@Stock[[1]]@NaturalMortality@MeanAtAge)), c('Sim', 'Age', 'Year'))
  Bad <- array(0.3, c(1, nA), list(Sim = 1, Bogus = Ages@Classes))
  expect_error(Pop(SetComp(OM, 'NaturalMortality', UseArray('MeanAtAge', Bad))),
               'named dimensions must be from Sim x Age x Year', fixed = TRUE)
})

test_that("MeanAtWeight accepts the same forms as MeanAtLength", {
  Vals <- c(NaturalMortality = 0.3, Maturity = 0.5, Fecundity = 1, Selectivity = 1, Retention = 1)
  Forms <- function(v, fleet) c(list(
    named = array(v, c(1, nW, 1), list(Sim = 1, Class = WtClasses, Year = HY[1])),
    named_weight_dim = array(v, c(1, nW, 1), list(Sim = 1, Weight = WtClasses, Year = HY[1])),
    named_no_year = array(v, c(1, nW), list(Sim = 1, Class = WtClasses)),
    vector_with_classes = rep(v, nW),
    vector_without_classes = rep(v, nW),
    unnamed_1d = array(v, nW),
    unnamed_sim_class = array(v, c(1, nW)),
    unnamed_nsim_class = array(v, c(nS, nW)),
    unnamed_3d = array(v, c(1, nW, 1)),
    unnamed_3d_hist_years = array(v, c(1, nW, length(HY))),
    unnamed_3d_all_years = array(v, c(1, nW, length(Yrs)))),
    if (fleet) list(unnamed_4d = array(v, c(1, nW, 1, 1)),
                    named_4d = array(v, c(1, nW, 1, 2), list(Sim = 1, Class = WtClasses, Year = HY[1], Area = 1:2))))
  Cases <- list()
  for (comp in names(Vals)) {
    F <- Forms(Vals[[comp]], comp %in% c('Selectivity', 'Retention'))
    for (f in names(F))
      Cases[[paste(comp, 'MeanAtWeight', f)]] <- list(comp, UseArray('MeanAtWeight', F[[f]],
                                                                    if (f != 'vector_without_classes') WtClasses))
  }
  ExpectAllPopulate(Cases, OMW)
  P <- Pop(SetComp(OMW, 'Maturity', UseArray('MeanAtWeight', rep(0.5, nW))))
  expect_false(is.null(P@Stock[[1]]@Maturity@MeanAtAge))
  expect_equal(dimnames(P@Stock[[1]]@Maturity@MeanAtWeight)$Class, as.character(WtClasses))
})

test_that("MeanAtLength accepts unnamed lower-dimensional and multi-year arrays", {
  Vals <- c(Weight = 10, NaturalMortality = 0.3, Maturity = 0.5, Fecundity = 1,
            Selectivity = 1, Retention = 1, DiscardMortality = 0.5)
  Forms <- function(v, fleet) c(list(
    unnamed_1d = array(v, nL),
    unnamed_sim_class = array(v, c(1, nL)),
    unnamed_nsim_class = array(v, c(nS, nL)),
    unnamed_3d_hist_years = array(v, c(1, nL, length(HY))),
    unnamed_3d_all_years = array(v, c(1, nL, length(Yrs))),
    named_length_dim = array(v, c(1, nL, 1), list(Sim = 1, Length = LenClasses, Year = HY[1]))),
    if (fleet) list(unnamed_4d = array(v, c(1, nL, 1, 1)),
                    named_4d = array(v, c(1, nL, 1, 2), list(Sim = 1, Class = LenClasses, Year = HY[1], Area = 1:2))))
  Cases <- list()
  for (comp in names(Vals)) {
    F <- Forms(Vals[[comp]], comp %in% c('Selectivity', 'Retention', 'DiscardMortality'))
    for (f in names(F))
      Cases[[paste(comp, 'MeanAtLength', f)]] <- list(comp, UseArray('MeanAtLength', F[[f]], LenClasses))
  }
  ExpectAllPopulate(Cases)
})

test_that("fleet Pars accept spatial arrays and random walks", {
  Spatial <- array(c(70, 90), c(1, 1, 2), list(Sim = 1, Year = HY[1], Area = 1:2))
  for (comp in c('Selectivity', 'Retention')) {
    Pars <- if (comp == 'Selectivity') list(SL50 = Spatial, SL50_95 = 10) else list(RL50 = Spatial, RL50_95 = 10)
    P <- Pop(SetComp(OM, comp, UsePars(Pars)))
    MAA <- methods::slot(P@Fleet[[1]][[1]], comp)@MeanAtAge
    expect_false(isTRUE(all.equal(MAA[1, , 1, 1], MAA[1, , 1, 2])), label = paste(comp, 'spatial Pars'))

    Pars <- if (comp == 'Selectivity') list(SL50 = 80, SL50_95 = 10, SL50SD = 0.1) else list(RL50 = 80, RL50_95 = 10, RL50SD = 0.1)
    P <- Pop(SetComp(OM, comp, UsePars(Pars)))
    MAA <- methods::slot(P@Fleet[[1]][[1]], comp)@MeanAtAge
    expect_gt(length(unique(round(as.numeric(MAA[1, 5, , 1]), 8))), 1, label = paste(comp, 'random walk'))
  }
})

test_that("Pars accept unnamed multi-year arrays and named arrays in any order", {
  P <- Pop(SetComp(OM, 'NaturalMortality', UsePars(list(M = array(c(0.25, 0.3), c(1, length(HY)))))))
  expect_equal(dimnames(P@Stock[[1]]@NaturalMortality@Pars$M)$Year, as.character(HY))
  YearSim <- array(c(0.3, 0.4), c(2, 1), list(Year = HY[c(1, 10)], Sim = 1))
  P <- Pop(SetComp(OM, 'NaturalMortality', UsePars(list(M = YearSim))))
  expect_identical(names(dimnames(P@Stock[[1]]@NaturalMortality@Pars$M)), c('Sim', 'Year'))
  expect_error(Pop(SetComp(OM, 'NaturalMortality', UsePars(list(M = 0.3, KSD = 0.1)))),
               '`Pars$KSD` is a random-walk SD', fixed = TRUE, class = 'MSEtool_invalid_input')
})

test_that("remaining stock slot forms populate", {
  Slot <- function(comp, sl, value) { force(value); list(comp, function(x) { methods::slot(x, sl) <- value; x }) }
  nH <- length(HY)
  Cases <- list(
    `Length Dist lognormal` = Slot('Length', 'Dist', 'lognormal'),
    `Length TruncSD` = Slot('Length', 'TruncSD', 3),
    `Length Timing` = Slot('Length', 'Timing', 0.5),
    `Weight Dist normal` = list('Weight', function(x) { x@CVatAge <- 0.1; x@Dist <- 'normal'; x }),
    `Maturity Semelparous TRUE` = Slot('Maturity', 'Semelparous', TRUE),
    `Maturity Semelparous array` = Slot('Maturity', 'Semelparous',
                                        array(0.5, c(1, nA, 1), list(Sim = 1, Age = Ages@Classes, Year = HY[1]))),
    `SRR SD nSim vector` = Slot('SRR', 'SD', c(0.3, 0.4, 0.5)),
    `SRR AC scalar` = Slot('SRR', 'AC', 0.3),
    `SRR user RecDevs` = list('SRR', function(x) {
      x@RecDevInit <- matrix(1, nS, nA - 1)
      x@RecDevHist <- matrix(1, nS, nH)
      x@RecDevProj <- matrix(1, nS, length(Yrs) - nH)
      x
    }),
    `Depletion Initial bounds` = Slot('Depletion', 'Initial', c(0.6, 0.9)),
    `Spatial UnfishedDist named array` = Slot('Spatial', 'UnfishedDist',
      array(c(0.4, 0.6), c(1, 2, 1, 1), list(Sim = 1, Area = 1:2, Age = Ages@Classes[1], Year = HY[1]))),
    `Spatial UnfishedDist unnamed 1-D` = Slot('Spatial', 'UnfishedDist', array(c(0.4, 0.6), 2)),
    `Spatial UnfishedDist unnamed Sim x Area` = Slot('Spatial', 'UnfishedDist', array(c(0.4, 0.6), c(1, 2))),
    `Spatial UnfishedDist unnamed by age` = Slot('Spatial', 'UnfishedDist', array(rep(c(0.4, 0.6), nA), c(1, 2, nA))),
    `Spatial ProbStaying scalar` = Slot('Spatial', 'ProbStaying', 0.8),
    `Spatial ProbStaying named array` = Slot('Spatial', 'ProbStaying',
      array(0.8, c(1, 1, 1, 1), list(Sim = 1, Area = 1, Age = Ages@Classes[1], Year = HY[1]))),
    `Spatial RelativeSize named array` = Slot('Spatial', 'RelativeSize', array(c(0.4, 0.6), c(1, 2), list(Sim = 1, Area = 1:2))),
    `Spatial RelativeSize unnamed 1-D` = Slot('Spatial', 'RelativeSize', array(c(0.4, 0.6), 2)),
    `Ages no plus group` = list('Ages', function(x) { x@PlusGroup <- FALSE; x }),
    `Ages MinAge 1` = list('Ages', function(x) { x@MinAge <- 1; x })
  )
  ExpectAllPopulate(Cases)
  expect_error(Pop(SetComp(OM, 'Length', function(x) { x@CVatAge <- c(0.08, 0.1, 0.12); x })),
               '`CVatAge` has 3 values', fixed = TRUE, class = 'MSEtool_invalid_input')

  Mov <- array(c(0.9, 0.2, 0.1, 0.8), c(1, 2, 2), list(Sim = 1, FromArea = 1:2, ToArea = 1:2))
  ExpectPopulates(SetComp(OM, 'Spatial', function(x) Spatial(Movement = Mov, RelativeSize = 'EqualDensity')),
                  'Spatial Movement named')
  ExpectPopulates(SetComp(OM, 'Spatial', function(x) Spatial(Movement = array(c(0.9, 0.2, 0.1, 0.8), c(2, 2)),
                                                             RelativeSize = 'EqualDensity')),
                  'Spatial Movement unnamed')
})

test_that("multi-area FracOther populates when supplied directly", {
  o <- SeasonalSpatialOM
  nSim(o) <- 2
  FO <- o@Stock[[1]]@Spatial@FracOther
  ExpectPopulates(o, 'FracOther named')
  o@Stock[[1]]@Spatial@FracOther <- array(as.numeric(FO), c(3, 3))
  ExpectPopulates(o, 'FracOther unnamed FromArea x ToArea')
})

test_that("remaining fleet slot forms populate", {
  Slot <- function(comp, sl, value) { force(value); list(comp, function(x) { methods::slot(x, sl) <- value; x }) }
  Cases <- list(
    `Effort Theta` = Slot('Effort', 'Theta', 0.5),
    `Effort StockTargetingLambda` = Slot('Effort', 'StockTargetingLambda', 1),
    `Effort Distribution named` = Slot('Effort', 'Distribution',
                                       array(c(0.4, 0.6), c(1, 1, 2), list(Sim = 1, Year = HY[1], Area = 1:2))),
    `Effort Maximum NA` = Slot('Effort', 'Maximum', NA_real_),
    `Effort Mode Biomass` = Slot('Effort', 'Mode', 'Biomass'),
    `Catchability Efficiency unnamed 1-D` = Slot('Catchability', 'Efficiency', array(0.1, length(HY))),
    `Catchability Efficiency unnamed matrix` = Slot('Catchability', 'Efficiency', array(0.1, c(nS, 1)))
  )
  ExpectAllPopulate(Cases)

  Fleet <- function(sl, value) { force(value); function(o) { methods::slot(o@Fleet[[1]][[1]], sl) <- value; o } }
  ExpectPopulates(Fleet('Closure', array(c(1, 0), c(1, 2)))(OM), 'Closure unnamed Sim x Area')
  ExpectPopulates(Fleet('WeightFleetSelected', array(5, c(1, nA, 1)))(OM), 'WeightFleetSelected unnamed')
  ExpectPopulates(Fleet('WeightFleetRetained', array(5, nA))(OM), 'WeightFleetRetained unnamed 1-D')
  P <- Pop(Fleet('WeightFleetSelected', array(5, c(1, nA, 1)))(OM))
  expect_equal(unique(as.numeric(P@Fleet[[1]][[1]]@WeightFleetSelected)), 5)
})

test_that("remaining obs and imp forms populate", {
  ObsSet <- function(fn) function(o) { o@Obs[[1]][[1]] <- fn(o@Obs[[1]][[1]]); o }
  ImpSet <- function(fn) function(o) { o@Imp[[1]][[1]] <- fn(o@Imp[[1]][[1]]); o }
  StockName <- names(OM@Stock)[1]
  Cases <- list(
    `Landings Ref` = ObsSet(function(x) { x@Landings@Ref <- 0.2; x }),
    `Landings DataCV` = ObsSet(function(x) { x@Landings@DataCV <- 0.2; x }),
    `Discards CV` = ObsSet(function(x) { x@Discards@CV <- 0.3; x }),
    `Effort obs Error` = ObsSet(function(x) { x@Effort@CV <- NULL; x@Effort@Error <- array(1, c(1, length(Yrs))); x }),
    `Effort obs Ref` = ObsSet(function(x) { x@Effort@Ref <- 0.1; x }),
    `Survey Selectivity SBiomass` = ObsSet(function(x) { x@Survey@Selectivity <- 'SBiomass'; x }),
    `Survey Selectivity named Sim x Age` = ObsSet(function(x) {
      x@Survey@Selectivity <- array(1, c(1, nA), list(Sim = 1, Age = Ages@Classes)); x }),
    `Survey Selectivity named Age x Year` = ObsSet(function(x) {
      x@Survey@Selectivity <- array(1, c(nA, 1), list(Age = Ages@Classes, Year = HY[1])); x }),
    `Survey Selectivity named list by stock` = ObsSet(function(x) {
      x@Survey@Selectivity <- stats::setNames(list(rep(1, nA)), StockName); x }),
    `Survey Selectivity unnamed list` = ObsSet(function(x) { x@Survey@Selectivity <- list(rep(1, nA)); x }),
    `Survey Selectivity unnamed Sim x Age` = ObsSet(function(x) { x@Survey@Selectivity <- array(1, c(1, nA)); x }),
    `Survey Selectivity unnamed 3-D` = ObsSet(function(x) { x@Survey@Selectivity <- array(1, c(1, nA, 1)); x }),
    `Survey Beta scalar` = ObsSet(function(x) { x@Survey@Beta <- 0.8; x }),
    `Survey Beta bounds` = ObsSet(function(x) { x@Survey@Beta <- c(0.7, 1.2); x }),
    `Survey Efficiency` = ObsSet(function(x) { x@Survey@Efficiency <- 0.01; x }),
    `Survey TruncSD` = ObsSet(function(x) { x@Survey@TruncSD <- 3; x }),
    `Survey Units Number` = ObsSet(function(x) { x@Survey@Units <- 'Number'; x }),
    `Imp Effort Mean bounds` = ImpSet(function(x) { x@Effort@Mean <- c(0.9, 1.1); x }),
    `Imp Effort SD` = ImpSet(function(x) { x@Effort@SD <- 0.1; x }),
    `Imp Effort Compliance` = ImpSet(function(x) { x@Effort@Compliance <- 0.8; x }),
    `Imp Size Mean` = ImpSet(function(x) { x@Size@Mean <- 1; x }),
    `Imp Size Compliance bounds` = ImpSet(function(x) { x@Size@Compliance <- c(0.7, 0.9); x }),
    `Imp TAC Error unnamed 1-D` = ImpSet(function(x) { x@TAC@Error <- array(1, length(Yrs)); x })
  )
  for (nm in names(Cases))
    ExpectPopulates(Cases[[nm]](OM), nm)
})
