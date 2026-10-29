# Custom R functions as `Model` for every schedule object: each must reproduce
# the built-in model it copies, take its size basis from its arguments, and be
# checked against `Pars`.

skip_on_cran()

data(SingleStockOM, envir = environment())

nSim <- 3

.Stock <- function() {
  St <- SingleStockOM@Stock[[1]]
  St@Weight@CVatAge <- 0.1
  PopulateStock(St, nSim = nSim, nYear = SingleStockOM@nYear, pYear = SingleStockOM@pYear,
                CurrentYear = SingleStockOM@CurrentYear, silent = TRUE)
}
St <- .Stock()
Yrs <- as.numeric(dimnames(St@Length@MeanAtAge)$Year)

# repopulate `slot` of the populated stock with `Obj`
.Repop <- function(slot, Obj) {
  args <- list(Obj, Ages = St@Ages, Length = St@Length, Weight = St@Weight,
               Maturity = St@Maturity, Years = Yrs, nSim = nSim, silent = TRUE)
  switch(slot,
    Length = PopulateLength(Obj, Ages = St@Ages, Years = Yrs, nSim = nSim, silent = TRUE),
    Weight = PopulateWeight(Obj, Ages = St@Ages, Length = St@Length, Years = Yrs, nSim = nSim,
                            silent = TRUE),
    NaturalMortality = PopulateNaturalMortality(Obj, Ages = St@Ages, Length = St@Length,
                                                Years = Yrs, nSim = nSim, silent = TRUE),
    Maturity = PopulateMaturity(Obj, Ages = St@Ages, Length = St@Length, Weight = St@Weight,
                                Years = Yrs, nSim = nSim, silent = TRUE),
    Fecundity = do.call(PopulateFecundity, args),
    Selectivity = do.call(PopulateSelectivity, args[names(args) != ""]
                          |> c(list(Selectivity = Obj))),
    Retention = do.call(PopulateRetention, args[names(args) != ""]
                        |> c(list(Retention = Obj)))
  )
}

.New <- function(slot, ...) do.call(slot, list(...))

# values at age for one sim, first year (and area)
.SimAges <- function(x, sim) {
  idx <- rep(list(1), length(dim(x)))
  idx[[1]] <- sim
  idx[[2]] <- seq_len(dim(x)[2])
  do.call(`[`, c(list(x), idx))
}

# built-in model, its Pars, and a custom function copying it under renamed arguments
Cases <- list(
  list('Length', 'vonBert', list(Linf = c(100, 110, 120), K = c(0.2, 0.25, 0.3), t0 = -0.5),
       function(Ages, Li, k, a0) vonBert(Ages, Li, k, a0), list(Li = 'Linf', k = 'K', a0 = 't0')),
  list('Weight', 'WeightatLength', list(Alpha = 1e-5, Beta = c(3, 3.05, 3.1)),
       function(Length, a, b) WeightatLength(Length, a, b), list(a = 'Alpha', b = 'Beta')),
  list('Weight', 'WeightatAge', list(a = 2, b = c(1.1, 1.2, 1.3)),
       function(Ages, aa, bb) WeightatAge(Ages, aa, bb), list(aa = 'a', bb = 'b')),
  list('NaturalMortality', 'MortalityAtAge', list(M = c(0.2, 0.3, 0.4)),
       function(Ages, m) MortalityAtAge(Ages, m), list(m = 'M')),
  list('NaturalMortality', 'LorenzenMortalityLength', list(Mref = c(0.2, 0.3, 0.4), Lref = 60),
       function(Length, m, l) LorenzenMortalityLength(Length, m, l), list(m = 'Mref', l = 'Lref')),
  list('Maturity', 'MaturityAtAge', list(A50 = c(3, 4, 5), A50_95 = 1),
       function(Ages, a, d) MaturityAtAge(Ages, a, d), list(a = 'A50', d = 'A50_95')),
  list('Maturity', 'MaturityAtLength', list(L50 = c(40, 50, 60), L50_95 = 10),
       function(Length, l, d) MaturityAtLength(Length, l, d), list(l = 'L50', d = 'L50_95')),
  list('Maturity', 'MaturityAtWeight', list(W50 = c(3, 4, 5), W50_95 = 2),
       function(Weight, w, d) MaturityAtWeight(Weight, w, d), list(w = 'W50', d = 'W50_95')),
  list('Fecundity', 'FecundityAtAge', list(A50 = c(3, 4, 5), A50_95 = 1, MaxFec = 2),
       function(Ages, a, d, mx) FecundityAtAge(Ages, a, d, mx), list(a = 'A50', d = 'A50_95', mx = 'MaxFec')),
  list('Fecundity', 'FecundityAtLength', list(L50 = c(40, 50, 60), L50_95 = 10, MaxFec = 2),
       function(Length, l, d, mx) FecundityAtLength(Length, l, d, mx), list(l = 'L50', d = 'L50_95', mx = 'MaxFec')),
  list('Selectivity', 'SelectivityAtAge', list(SA50 = c(2, 3, 4), SA50_95 = 1),
       function(Ages, a, d) SelectivityAtAge(Ages, a, d), list(a = 'SA50', d = 'SA50_95')),
  list('Selectivity', 'DoubleNormal', list(L5 = c(30, 35, 40), LFS = 70, Vmaxlen = 0.5),
       function(Length, l5, lf, vm) DoubleNormal(Length, l5, lf, vm), list(l5 = 'L5', lf = 'LFS', vm = 'Vmaxlen')),
  list('Selectivity', 'SelectivityAtWeight', list(SW50 = c(3, 4, 5), SW50_95 = 2),
       function(Weight, w, d) SelectivityAtWeight(Weight, w, d), list(w = 'SW50', d = 'SW50_95')),
  list('Retention', 'RetentionAtLength', list(RL50 = c(60, 70, 80), RL50_95 = 10, MaxRet = 0.9),
       function(Length, l, d, mx) RetentionAtLength(Length, l, d, mx), list(l = 'RL50', d = 'RL50_95', mx = 'MaxRet')),
  list('Retention', 'RetentionAtWeight', list(RW50 = c(3, 4, 5), RW50_95 = 2),
       function(Weight, w, d) RetentionAtWeight(Weight, w, d), list(w = 'RW50', d = 'RW50_95'))
)

for (Case in Cases) {
  slot <- Case[[1]]; Builtin <- Case[[2]]; Pars <- Case[[3]]; Fun <- Case[[4]]; Map <- Case[[5]]
  test_that(paste(slot, "custom function reproduces", Builtin), {
    Ref <- .Repop(slot, .New(slot, Pars = Pars))
    expect_equal(.FindModel(Ref), Builtin)
    CustomPars <- lapply(Map, \(nm) Pars[[nm]])
    Custom <- .Repop(slot, .New(slot, Pars = CustomPars, Model = Fun))
    expect_identical(Custom@Model, Fun)
    expect_equal(unname(dim(Custom@MeanAtAge)[1]), nSim)
    expect_equal(unname(Custom@MeanAtAge), unname(Ref@MeanAtAge))
    if (methods::.hasSlot(Ref, 'MeanAtLength') && length(Ref@MeanAtLength))
      expect_equal(unname(Custom@MeanAtLength), unname(Ref@MeanAtLength))
    expect_false(isTRUE(all.equal(.SimAges(Custom@MeanAtAge, 1), .SimAges(Custom@MeanAtAge, nSim))))
  })
}

test_that("custom function Pars are checked against its arguments", {
  Fun <- function(Length, l, d) MaturityAtLength(Length, l, d)
  expect_error(.Repop('Maturity', Maturity(Pars = list(l = 40, d = 10, dd = 10), Model = Fun)),
               'not arguments of')
  expect_error(.Repop('Maturity', Maturity(Pars = list(l = 40), Model = Fun)), 'missing from')
  expect_no_error(.FindModel(Maturity(Pars = list(l = 40, d = 10, l_CV = 0.1, dSD = 0.2), Model = Fun)))

  Dots <- function(Length, l, ...) MaturityAtLength(Length, l, 10)
  expect_no_error(.Repop('Maturity', Maturity(Pars = list(l = 40, other = 1), Model = Dots)))
})

test_that("custom function size basis is checked", {
  NoSize <- function(l, d) MaturityAtLength(1:100, l, d)
  expect_error(.Repop('Maturity', Maturity(Pars = list(l = 40, d = 10), Model = NoSize)),
               'must have one of the arguments')
  LenOfLen <- function(Length, a) a * Length
  expect_error(.Repop('Length', Length(Pars = list(a = 2), Model = LenOfLen, CVatAge = 0.1)),
               '`Ages`')
  WtOfWt <- function(Weight, a) a * Weight
  expect_error(.Repop('Weight', Weight(Pars = list(a = 2), Model = WtOfWt)), '`Ages` or `Length`')
  MOfWt <- function(Weight, m) m * Weight
  expect_error(.Repop('NaturalMortality', NaturalMortality(Pars = list(m = 0.2), Model = MOfWt)),
               '`Ages` or `Length`')

  # NaturalMortality and Fecundity have no at-weight schedule
  expect_error(.Repop('NaturalMortality', NaturalMortality(Pars = list(Mref = 0.3, Wref = 5))),
               'does not support at-Weight')
  expect_error(.Repop('Fecundity', Fecundity(Pars = list(W50 = 4, W50_95 = 2, MaxFec = 1))),
               'does not support at-Weight')
})

test_that("omitted optional arguments of a custom function are filled from its defaults", {
  Fun <- function(Length, l, d = 10, mx = 1) MaturityAtLength(Length, l, d) * mx
  Custom <- .Repop('Maturity', Maturity(Pars = list(l = c(40, 50, 60)), Model = Fun))
  expect_true(all(Custom@Pars$d == 10))
  expect_true(all(Custom@Pars$mx == 1))
  Ref <- .Repop('Maturity', Maturity(Pars = list(L50 = c(40, 50, 60), L50_95 = 10)))
  expect_equal(unname(Custom@MeanAtAge), unname(Ref@MeanAtAge))
})

test_that("CV and SD entries of Pars do not reach a custom function", {
  Fun <- function(Length, l, d) MaturityAtLength(Length, l, d)
  Pars <- list(l = c(40, 50, 60), d = 10)
  Ref <- .Repop('Maturity', Maturity(Pars = Pars, Model = Fun))
  Obj <- Maturity(Pars = c(Pars, list(l_CV = 0.1)), Model = Fun)
  expect_equal(.Repop('Maturity', Obj)@MeanAtAge, Ref@MeanAtAge)
  Obj@Pars <- .StructurePars(c(Pars, list(lSD = 0.2)), nSim, Yrs)
  expect_no_error(GenMeanAtLength(Fun, Obj@Pars, 1:100))
})

# OM using custom functions for several schedules
.CustomOM <- function(OM) {
  Stock <- OM@Stock[[1]]
  Stock@Length <- Length(Pars = list(Li = c(100, 110, 120), k = 0.2, a0 = -0.5),
                         Model = function(Ages, Li, k, a0) vonBert(Ages, Li, k, a0),
                         CVatAge = Stock@Length@CVatAge)
  Stock@Maturity <- Maturity(Pars = list(l = c(40, 50, 60), d = 10),
                             Model = function(Length, l, d) MaturityAtLength(Length, l, d))
  Stock@NaturalMortality <- NaturalMortality(Pars = list(m = c(0.2, 0.3, 0.4)),
                                             Model = function(Ages, m) MortalityAtAge(Ages, m))
  OM@Stock[[1]] <- Stock
  Fleet <- OM@Fleet[[1]][[1]]
  Fleet@Selectivity <- Selectivity(Pars = list(l5 = 30, lf = 70),
                                   Model = function(Length, l5, lf, vm = 1) DoubleNormal(Length, l5, lf, vm))
  OM@Fleet[[1]][[1]] <- Fleet
  OM
}

.BuiltinOM <- function(OM) {
  Stock <- OM@Stock[[1]]
  Stock@Length <- Length(Pars = list(Linf = c(100, 110, 120), K = 0.2, t0 = -0.5),
                         CVatAge = Stock@Length@CVatAge)
  Stock@Maturity <- Maturity(Pars = list(L50 = c(40, 50, 60), L50_95 = 10))
  Stock@NaturalMortality <- NaturalMortality(Pars = list(M = c(0.2, 0.3, 0.4)))
  OM@Stock[[1]] <- Stock
  Fleet <- OM@Fleet[[1]][[1]]
  Fleet@Selectivity <- Selectivity(Pars = list(L5 = 30, LFS = 70))
  OM@Fleet[[1]][[1]] <- Fleet
  OM
}

test_that("an OM with custom model functions simulates and projects like the built-in OM", {
  OM <- SingleStockOM
  nSim(OM) <- nSim
  pYear(OM) <- 3
  HistC <- Simulate(.CustomOM(OM), silent = TRUE)
  HistB <- Simulate(.BuiltinOM(OM), silent = TRUE)
  expect_equal(unname(HistC@Biomass), unname(HistB@Biomass))
  expect_equal(unname(HistC@Landings), unname(HistB@Landings))

  MSEC <- Project(HistC, MPs = 'CurrentEffort', silent = TRUE)
  MSEB <- Project(HistB, MPs = 'CurrentEffort', silent = TRUE)
  expect_length(MSEC@Log$error, 0)
  expect_equal(unname(MSEC@SBiomass), unname(MSEB@SBiomass))
  expect_equal(unname(MSEC@Landings), unname(MSEB@Landings))

  # legacy Data conversion reads schedules, not custom-function Pars
  Legacy <- Convert(HistC@Data[[1]][[1]], silent = TRUE)
  expect_s4_class(Legacy, 'Data')
})

test_that("an OM with custom model functions projects in parallel like sequentially", {
  skip_on_ci()
  skip_if(parallel::detectCores() < 2)
  OM <- SingleStockOM
  nSim(OM) <- nSim
  pYear(OM) <- 3
  Hist <- Simulate(.CustomOM(OM), silent = TRUE)
  Seq <- Project(Hist, MPs = c('CurrentEffort', 'FMSYref'), silent = TRUE)
  SetupParallel(workers = 2, backend = 'multisession', silent = TRUE)
  on.exit(suppressMessages(DisableParallel()), add = TRUE)
  Par <- suppressMessages(Project(Hist, MPs = c('CurrentEffort', 'FMSYref'), parallel = TRUE, silent = TRUE))
  expect_null(Par@Log$error)
  expect_equal(Par@SBiomass, Seq@SBiomass)
  expect_equal(Par@Landings, Seq@Landings)
})
