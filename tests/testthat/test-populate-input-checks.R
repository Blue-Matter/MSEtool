OM <- SingleStockOM
nSim(OM) <- 3

expect_invalid <- function(object, pattern, ...) {
  err <- expect_error(Populate(object, silent = TRUE, ...), class = 'MSEtool_invalid_input')
  expect_match(conditionMessage(err), pattern, fixed = TRUE)
  invisible(err)
}

test_that("example objects pass the input checks", {
  for (nm in c('SingleStockOM', 'TwoFleetOM', 'MultiStockOM', 'HermOM', 'ComplexOM')) {
    o <- get(nm)
    nSim(o) <- 2
    expect_no_error(Populate(o, silent = TRUE))
  }
})

test_that("invalid stock inputs give informative errors", {
  o <- OM
  o@Stock[[1]]@SRR@Pars$h <- 1.2
  err <- expect_invalid(o, '`Pars$h` must be in [0.2, 1)')
  expect_match(conditionMessage(err), 'MSEtool::SRR', fixed = TRUE)
  expect_match(conditionMessage(err), 'Example Albacore Stock', fixed = TRUE)

  o <- OM

  o@Stock[[1]]@NaturalMortality@Pars$M <- -0.2
  expect_invalid(o, '`Pars$M` must be >= 0')

  o <- OM

  o@Stock[[1]]@Length@Pars$Linf <- 'a'
  expect_invalid(o, '`Pars$Linf` must be numeric')

  o <- OM

  o@Stock[[1]]@Maturity@Pars$L50 <- c(NA, 50)
  expect_invalid(o, '`Pars$L50` contains `NA`')

  o <- OM

  o@Stock[[1]]@Length@Dist <- 'Normal'
  expect_invalid(o, 'Did you mean "normal"?')

  o <- OM

  o@Stock[[1]]@Length@Model <- 'vonBertalanffy'
  expect_invalid(o, 'LengthModels()')

  o <- OM

  o@Stock[[1]]@SRR@AC <- 1
  expect_invalid(o, '`AC` must be in (-1, 1)')

  o <- OM

  o@Stock[[1]]@Spatial@UnfishedDist <- NA_real_
  expect_invalid(o, '`UnfishedDist` contains `NA`')
})

test_that("problems in one object are reported together", {
  o <- OM
  o@Stock[[1]]@SRR@Pars$h <- 0.1
  o@Stock[[1]]@SRR@SD <- -1
  err <- expect_invalid(o, 'invalid inputs')
  expect_length(err$problems, 2)
})

test_that("invalid fleet, obs, imp, and OM inputs give informative errors", {
  o <- OM
  o@Fleet[[1]][[1]]@Selectivity@Pars$Vmaxlen <- 1.5
  expect_invalid(o, '`Pars$Vmaxlen` must be in [0, 1]')

  o <- OM

  o@Fleet[[1]][[1]]@Effort@Mode <- 'density'
  expect_invalid(o, 'Did you mean "Density"?')

  o <- OM

  o@Imp[[1]][[1]]@TAC@Compliance <- 1.4
  expect_invalid(o, 'TAC: `Compliance` must be in [0, 1]')

  o <- OM

  o@Obs[[1]][[1]]@Survey@AC <- c(0.2, NA)
  expect_invalid(o, '`AC` contains `NA`')

  o <- OM

  o@Obs[[1]][[1]]@CPUE@Units <- 'biomass'
  expect_invalid(o, '`Units` is "biomass"')

  o <- OM

  o@nYear <- -5
  expect_invalid(o, '`nYear` must be >= 1')

  o <- OM

  o@pStar <- 1.5
  expect_invalid(o, '`pStar` must be in (0, 1)')
})

test_that("standalone Populate() checks component inputs", {
  L <- AlbacoreExStock@Length
  L@Pars$K <- -0.1
  expect_error(PopulateLength(L, Ages = AlbacoreExStock@Ages, Years = 2000:2010, nSim = 3),
               '`Pars$K` must be > 0', fixed = TRUE, class = 'MSEtool_invalid_input')
  expect_error(Populate(AlbacoreExStock, nYear = 0, pYear = 5, nSim = 3),
               '`nYear` must be >= 1', fixed = TRUE, class = 'MSEtool_invalid_input')
})

test_that("a valid Model name is used when Pars match it", {
  s <- Selectivity(Pars = list(SA50 = 3, SA50_95 = 1), Model = 'SelectivityAtAge')
  expect_identical(.FindModel(s), 'SelectivityAtAge')
})

test_that("Closure is checked against the number of areas", {
  o <- OM
  o@Fleet[[1]][[1]]@Closure <- array(0, c(1, 1, 3))
  expect_error(Populate(o, silent = TRUE), 'Area dimension; must have 1 or 2', fixed = TRUE)
  DN <- list(
    Sim  = 1,
    Year = 2007,
    Area = 1:3
  )
  o <- OM
  o@Fleet[[1]][[1]]@Closure <- array(0, c(1, 1, 3), dimnames = DN)
  expect_error(Populate(o, silent = TRUE), 'has 3 areas but the stock has 2 areas', fixed = TRUE)
  o <- OM
  o@Fleet[[1]][[1]]@Closure <- array(-1, c(1, 1, 2))
  expect_error(Populate(o, silent = TRUE), '`Closure` must be in [0, 1]', fixed = TRUE)
})

test_that("reversed uniform bounds for index AC are sorted", {
  o <- OM
  o@Obs[[1]][[1]]@Survey@AC <- c(0.3, 0.1)
  P <- Populate(o, silent = TRUE)
  AC <- P@Obs[[1]][[1]]@Survey@AC
  expect_true(all(AC >= 0.1 & AC <= 0.3))
})

test_that("populated objects are checked against the OM dimensions", {
  o <- OM
  o@Fleet[[1]][[1]]@Effort@Effort <- matrix(1, 5, nYear(o))
  expect_error(Populate(o, silent = TRUE), '`Effort` has 5 simulations', fixed = TRUE,
               class = 'MSEtool_invalid_input')

  o <- OM
  o@Fleet[[1]][[1]]@Effort@Distribution <- array(1/3, c(1, 1, 3))
  expect_error(Populate(o, silent = TRUE), 'Area dimension; must have 1 or 2', fixed = TRUE)

  nA <- length(CalcAgeClasses(OM@Stock[[1]]@Ages))
  o <- OM
  o@Stock[[1]]@NaturalMortality@Pars <- list()
  o@Stock[[1]]@NaturalMortality@Model <- NULL
  o@Stock[[1]]@NaturalMortality@MeanAtAge <- array(0.2, c(1, nA + 2, 1))
  expect_error(Populate(o, silent = TRUE), 'Age dimension; must have 1 or', fixed = TRUE)

  DN <- list(
    Sim  = 1,
    Age  = 0:(nA - 1),
    Year = 1900
  )
  o@Stock[[1]]@NaturalMortality@MeanAtAge <- array(0.2, c(1, nA, 1), dimnames = DN)
  expect_error(Populate(o, silent = TRUE), 'years outside the model years', fixed = TRUE)
})

test_that("unnamed Effort Distribution is given dimnames", {
  o <- OM
  o@Fleet[[1]][[1]]@Effort@Distribution <- array(0.5, c(1, 1, 2))
  P <- Populate(o, silent = TRUE)
  expect_named(dimnames(P@Fleet[[1]][[1]]@Effort@Distribution), c('Sim', 'Year', 'Area'))
})

test_that("PopulateDepletion respects force and checks Reference", {
  D <- PopulateDepletion(Depletion(Final = c(0.2, 0.4)), nSim = 3, seed = 1)
  expect_identical(PopulateDepletion(D, nSim = 3, seed = 1), D)
  D2 <- PopulateDepletion(D, nSim = 3, seed = 2, force = TRUE)
  expect_length(D2@Final, 3)
  D@Reference <- 'B'
  expect_error(PopulateDepletion(D, nSim = 3, seed = 1, force = TRUE),
               class = 'MSEtool_invalid_input')
})

test_that("unnamed arrays are read by position with one simulation and the first year", {
  nA <- length(CalcAgeClasses(OM@Stock[[1]]@Ages))
  nH <- nYear(OM)
  forms <- list(array(0.3, nA), array(0.3, c(1, nA)), array(0.3, c(nSim(OM), nA)),
                array(0.3, c(1, nA, 1)), array(0.3, c(1, nA, nH)))
  for (f in forms) {
    o <- OM
    o@Stock[[1]]@NaturalMortality@Pars <- list()
    o@Stock[[1]]@NaturalMortality@Model <- NULL
    o@Stock[[1]]@NaturalMortality@MeanAtAge <- f
    P <- Populate(o, silent = TRUE)
    M <- P@Stock[[1]]@NaturalMortality@MeanAtAge
    expect_named(dimnames(M), c('Sim', 'Age', 'Year'))
    expect_equal(dimnames(M)$Year[1], as.character(min(Years(P))))
  }

  o <- OM
  o@Fleet[[1]][[1]]@Selectivity@Pars <- list()
  o@Fleet[[1]][[1]]@Selectivity@Model <- NULL
  o@Fleet[[1]][[1]]@Selectivity@MeanAtLength <- NULL
  o@Fleet[[1]][[1]]@Selectivity@MeanAtAge <- array(1, nA)
  o@Obs[[1]][[1]]@Survey@Selectivity <- array(1, nA)
  o@Fleet[[1]][[1]]@Effort@Effort <- array(1, nH)
  o@Fleet[[1]][[1]]@Effort@Distribution <- array(c(1, 0), 2)
  o@Fleet[[1]][[1]]@Closure <- array(c(1, 0), 2)
  o@Obs[[1]][[1]]@Landings@Error <- array(1, nH + OM@pYear)
  expect_no_error(Simulate(o, silent = TRUE))
})

test_that("Closure must contain only 0 or 1", {
  o <- OM
  o@Fleet[[1]][[1]]@Closure <- array(0.5, c(1, 1, 2))
  expect_error(Populate(o, silent = TRUE), 'only 0 or 1', fixed = TRUE)
})

test_that("a scalar Efficiency works with a final depletion target", {
  o <- OM
  o@Fleet[[1]][[1]]@Catchability@Efficiency <- 0.1
  expect_no_error(Simulate(o, silent = TRUE))
})

test_that("years cannot be changed on a populated OM or Stock", {
  P <- Populate(OM, silent = TRUE)
  P@CurrentYear <- P@CurrentYear + 3
  expect_error(Populate(P, silent = TRUE), 'years of a populated OM cannot be changed', fixed = TRUE)
  St <- Populate(AlbacoreExStock, nYear = 20, pYear = 5, nSim = 3, silent = TRUE)
  expect_error(Populate(St, nYear = 25, pYear = 5, nSim = 3, silent = TRUE),
               'years of a populated Stock cannot be changed', fixed = TRUE)
})

test_that("nSim cannot be increased on a populated object unless it is constant across sims", {
  St <- Populate(AlbacoreExStock, nYear = 20, pYear = 5, nSim = 3, silent = TRUE)
  expect_error(PopulateLength(St@Length, Ages = St@Ages, Years = St@Years, nSim = 5),
               '`nSim` cannot be increased', fixed = TRUE, class = 'MSEtool_invalid_input')
  D <- PopulateDepletion(Depletion(Final = c(0.2, 0.4)), nSim = 3, seed = 1)
  expect_error(PopulateDepletion(D, nSim = 5), '`Final` has 3 simulations', fixed = TRUE)
  D1 <- PopulateDepletion(Depletion(Final = 0.3), nSim = 3, seed = 1)
  expect_no_error(PopulateDepletion(D1, nSim = 5))
})

test_that("force re-populates Selectivity", {
  P <- Populate(OM, silent = TRUE)
  Args <- list(
    Ages     = P@Stock[[1]]@Ages,
    Length   = P@Stock[[1]]@Length,
    Weight   = P@Stock[[1]]@Weight,
    Maturity = P@Stock[[1]]@Maturity,
    nSim     = 3,
    Years    = Years(P),
    nArea    = 2,
    seed     = 1
  )
  Sel <- do.call(PopulateSelectivity, c(list(OM@Fleet[[1]][[1]]@Selectivity), Args))
  expect_identical(do.call(PopulateSelectivity, c(list(Sel), Args)), Sel)
  # count calls with a mocked binding rather than trace(): trace()/untrace()
  # fail on covr's instrumented namespace
  Calls <- 0
  StructurePars <- .StructurePars
  local_mocked_bindings(.StructurePars = function(...) {
    Calls <<- Calls + 1
    StructurePars(...)
  })
  do.call(PopulateSelectivity, c(list(Sel), Args))
  expect_equal(Calls, 0)
  do.call(PopulateSelectivity, c(list(Sel), Args, force = TRUE))
  expect_gt(Calls, 0)
})

test_that("a missing SRR@R0 is logged once as an assumption; a missing SD is a warning", {
  OM <- SingleStockOM
  nSim(OM) <- 2
  OM@Stock[[1]]@SRR@R0 <- NULL
  OM@Stock[[1]]@SRR@SD <- NULL
  expect_no_warning(P <- Populate(OM, silent = TRUE))
  expect_equal(unique(c(P@Stock[[1]]@SRR@R0)), 1000)
  Msgs <- \(type, obj = P) vapply(obj@Log[[type]], \(e) e$message, character(1))
  expect_equal(sum(grepl("SRR@R0", Msgs('assumption'))), 1)
  expect_true(any(grepl("SD", Msgs('warning'))))
  P2 <- Populate(P, silent = TRUE, force = TRUE)
  expect_equal(sum(grepl("SRR@R0", Msgs('assumption', P2))), 1)
})
