# Populate*() precedence and re-population: a resolvable `Pars` model
# regenerates the at-age/at-size arrays, a supplied `MeanAtAge` is otherwise
# kept, and slots calculated by Populate() are recalculated when the inputs
# change (see R/utils-populate-derived.R)

.PopTestOM <- function() {
  OM <- SingleStockOM
  OM@nSim <- 2
  suppressMessages(Populate(OM, silent = TRUE))
}

.Repop <- function(OM) suppressMessages(Populate(OM, silent = TRUE))

test_that("MeanAtAge source follows the precedence rule for every schedule class", {
  OM <- SingleStockOM
  OM@nSim <- 2
  # absolute selectivity, so the maturity cases do not depend on maturity L50
  OM@Fleet[[1]][[1]]@Selectivity <- Selectivity(Pars = list(L5 = 50, LFS = 90, Vmaxlen = 1))
  P <- .Repop(OM)
  FleetCl <- c('Selectivity', 'Retention', 'DiscardMortality')
  Get <- function(O, cl) if (cl %in% FleetCl) methods::slot(O@Fleet[[1]][[1]], cl) else methods::slot(O@Stock[[1]], cl)
  Set <- function(O, cl, x) {
    if (cl %in% FleetCl) methods::slot(O@Fleet[[1]][[1]], cl) <- x else methods::slot(O@Stock[[1]], cl) <- x
    O
  }
  ParsSpec <- list(
    NaturalMortality = OM@Stock[[1]]@NaturalMortality,
    Weight           = OM@Stock[[1]]@Weight,
    Maturity         = OM@Stock[[1]]@Maturity,
    Fecundity        = Fecundity(Pars = list(L50 = 80, L50_95 = 10, MaxFec = 1000)),
    Length           = OM@Stock[[1]]@Length,
    Selectivity      = OM@Fleet[[1]][[1]]@Selectivity,
    Retention        = Retention(Pars = list(LR5 = 60, LFR = 100, Rmaxlen = 1)),
    DiscardMortality = NULL
  )
  Same <- function(a, b) isTRUE(all.equal(unname(c(a)), unname(c(b)), tolerance = 1e-6))

  for (cl in names(ParsSpec)) {
    Empty <- get(cl)() # constructor defaults, as a user would create it
    ref <- Get(P, cl)
    # supplied at-age values with a shape different from the model
    uAge <- ref@MeanAtAge
    uAge[] <- if (cl == 'Length') ref@MeanAtAge * 0.9 else
      seq(0.2, 1, length.out = dim(uAge)[2])[slice.index(uAge, 2)]
    uLen <- NULL
    if ('MeanAtLength' %in% methods::slotNames(Empty) && !is.null(ref@MeanAtLength)) {
      uLen <- ref@MeanAtLength
      uLen[] <- seq(1, 0.2, length.out = dim(uLen)[2])[slice.index(uLen, 2)]
    }
    Mk <- function(Pars = FALSE, Age = FALSE, Len = FALSE) {
      o <- if (Pars) ParsSpec[[cl]] else Empty
      if (Age) o@MeanAtAge <- uAge
      if (Len) {
        o@MeanAtLength <- uLen
        o@Classes <- as.numeric(dimnames(uLen)$Class)
      }
      o
    }
    Run <- function(...) Get(.Repop(Set(P, cl, Mk(...))), cl)

    # supplied MeanAtAge is used as is (selectivity is scaled to a maximum of 1)
    expect_true(Same(Run(Age = TRUE)@MeanAtAge, uAge), label = paste(cl, 'MeanAtAge only'))
    if (!is.null(uLen)) {
      FromLen <- Run(Len = TRUE)@MeanAtAge
      expect_false(Same(FromLen, uAge), label = paste(cl, 'MeanAtLength only'))
      expect_true(Same(Run(Age = TRUE, Len = TRUE)@MeanAtAge, uAge),
                  label = paste(cl, 'MeanAtAge + MeanAtLength'))
    }
    if (!is.null(ParsSpec[[cl]])) {
      FromPars <- Run(Pars = TRUE)@MeanAtAge
      expect_true(Same(Run(Pars = TRUE, Age = TRUE)@MeanAtAge, FromPars), label = paste(cl, 'Pars + MeanAtAge'))
      if (!is.null(uLen))
        expect_true(Same(Run(Pars = TRUE, Len = TRUE)@MeanAtAge, FromPars), label = paste(cl, 'Pars + MeanAtLength'))
    }
  }
})

test_that("relative selectivity is re-scaled when maturity changes", {
  OM <- .PopTestOM()
  expect_true(isTRUE(OM@Fleet[[1]][[1]]@Selectivity@Misc$Derived$isRel$Input))
  Mat <- OM@Stock[[1]]@Maturity
  Mat@Pars$L50 <- Mat@Pars$L50 * 0.5
  OM2 <- OM
  OM2@Stock[[1]]@Maturity <- Mat
  expect_false(isTRUE(all.equal(.Repop(OM2)@Fleet[[1]][[1]]@Selectivity@MeanAtAge,
                                OM@Fleet[[1]][[1]]@Selectivity@MeanAtAge)))
})

test_that("an empty retention placed in a populated OM is re-populated as empty", {
  OM <- .PopTestOM()
  OM@Fleet[[1]][[1]]@Retention <- Retention()
  A <- .Repop(OM)
  B <- suppressMessages(Populate(A, silent = TRUE, force = TRUE))
  expect_true(all(B@Fleet[[1]][[1]]@Retention@MeanAtAge == 1))
})

test_that("a Pars model overwrites an edited MeanAtAge for at-length models", {
  OM <- .PopTestOM()

  # maturity: at-length model (L50, L50_95)
  Mat <- OM@Stock[[1]]@Maturity
  Mat@MeanAtAge[] <- 0.123
  OM2 <- OM
  OM2@Stock[[1]]@Maturity <- Mat
  expect_false(all(.Repop(OM2)@Stock[[1]]@Maturity@MeanAtAge == 0.123))

  # selectivity: at-length double-normal model
  Sel <- OM@Fleet[[1]][[1]]@Selectivity
  Sel@MeanAtAge[] <- 0
  Sel@MeanAtAge[, as.character(15:20), , ] <- 1
  OM3 <- OM
  OM3@Fleet[[1]][[1]]@Selectivity <- Sel
  New <- .Repop(OM3)@Fleet[[1]][[1]]@Selectivity
  expect_equal(New@MeanAtAge, OM@Fleet[[1]][[1]]@Selectivity@MeanAtAge)
})

test_that("a supplied MeanAtAge is kept without Pars", {
  OM <- .PopTestOM()
  Mat <- OM@Stock[[1]]@Maturity
  Supplied <- Mat@MeanAtAge
  Supplied[] <- stats::plogis(as.numeric(dimnames(Supplied)$Age) - 6)[slice.index(Supplied, 2)]
  Mat@MeanAtAge <- Supplied
  Mat@Pars <- list()
  Mat@Model <- NULL
  Mat@MeanAtLength <- NULL
  OM@Stock[[1]]@Maturity <- Mat
  New <- .Repop(OM)
  expect_equal(New@Stock[[1]]@Maturity@MeanAtAge, Supplied)
  # relative selectivity: L50 from the supplied maturity-at-age via the ALK
  expect_false(isTRUE(all.equal(New@Fleet[[1]][[1]]@Selectivity@MeanAtAge,
                                OM@Fleet[[1]][[1]]@Selectivity@MeanAtAge)))
})

test_that("changing Pars updates the at-age and at-length arrays together", {
  OM <- .PopTestOM()
  Mat <- OM@Stock[[1]]@Maturity
  Mat@Pars$L50[] <- 40
  OM2 <- OM
  OM2@Stock[[1]]@Maturity <- Mat
  New <- .Repop(OM2)@Stock[[1]]@Maturity
  expect_gt(New@MeanAtAge[1, '5', 1], OM@Stock[[1]]@Maturity@MeanAtAge[1, '5', 1])
  expect_false(isTRUE(all.equal(New@MeanAtLength, OM@Stock[[1]]@Maturity@MeanAtLength)))
})

test_that("fecundity derived from weight x maturity follows a change in weight", {
  OM <- .PopTestOM()
  W <- OM@Stock[[1]]@Weight
  W@Pars$alpha <- W@Pars$alpha * 2
  OM2 <- OM
  OM2@Stock[[1]]@Weight <- W
  New <- .Repop(OM2)@Stock[[1]]@Fecundity@MeanAtAge
  expect_equal(New[, '10', 1] / OM@Stock[[1]]@Fecundity@MeanAtAge[, '10', 1], c(`1` = 2, `2` = 2))
})

test_that("length classes and ALK follow a change in growth", {
  OM <- .PopTestOM()
  L <- OM@Stock[[1]]@Length
  L@Pars$Linf <- L@Pars$Linf * 1.5
  OM2 <- OM
  OM2@Stock[[1]]@Length <- L
  New <- .Repop(OM2)@Stock[[1]]@Length
  expect_gt(max(New@Classes), max(OM@Stock[[1]]@Length@Classes))
  expect_false(identical(New@ALK, OM@Stock[[1]]@Length@ALK))
})

test_that("re-populating a populated OM reproduces its stocks", {
  Strip <- function(OM) purrr::map(OM@Stock, \(st) {
    for (sl in methods::slotNames(st)) {
      v <- methods::slot(st, sl)
      if (!isS4(v)) next
      attributes(v)$digest <- NULL
      if ('Misc' %in% methods::slotNames(v)) v@Misc$Derived <- NULL
      methods::slot(st, sl) <- v
    }
    attributes(st)$digest <- NULL
    st
  })
  for (nm in c('SingleStockOM', 'MultiStockOM', 'SeasonalSpatialOM', 'HermOM')) {
    A <- suppressMessages(Populate(get(nm), silent = TRUE))
    B <- suppressMessages(Populate(A, silent = TRUE, force = TRUE))
    expect_equal(Strip(B), Strip(A), label = nm)
    expect_equal(length(B@Log$assumption), length(A@Log$assumption), label = paste(nm, 'log'))
  }
})

test_that("SRR values shared from the SPFrom source are re-copied, not kept as supplied", {
  A <- suppressMessages(Populate(HermOM, silent = TRUE))
  Male <- A@Stock[[2]]@SRR
  expect_equal(Male@RecDevProj, A@Stock[[1]]@SRR@RecDevProj)
  expect_true(all(c('SD', 'RecDevProj') %in% names(Male@Misc$Derived)))

  # the dependent stock's own R0 keeps its shape on re-population
  B <- suppressMessages(Populate(A, silent = TRUE, force = TRUE))
  expect_identical(dim(B@Stock[[2]]@SRR@R0), dim(Male@R0))

  # no longer shared: the stock generates its own recruitment deviations
  C <- B
  C@Stock[[2]]@SRR@ShareSPFrom <- FALSE
  C@Stock[[2]]@SRR@Pars <- list(h = 0.6)
  C@Stock[[2]]@SRR@SD <- 0.5
  C <- suppressMessages(Populate(C, silent = TRUE))
  expect_false(isTRUE(all.equal(C@Stock[[2]]@SRR@RecDevProj, C@Stock[[1]]@SRR@RecDevProj)))
})

test_that("removing Pars keeps the arrays derived from them as supplied", {
  OM <- .PopTestOM()
  Mat <- OM@Stock[[1]]@Maturity
  Mat@Pars <- list()
  Mat@Model <- NULL
  OM2 <- OM
  OM2@Stock[[1]]@Maturity <- Mat
  New <- .Repop(OM2)@Stock[[1]]@Maturity
  expect_equal(New@MeanAtAge, OM@Stock[[1]]@Maturity@MeanAtAge)
  expect_equal(New@MeanAtLength, OM@Stock[[1]]@Maturity@MeanAtLength)
})

test_that("a derived slot edited after population is kept as supplied", {
  OM <- .PopTestOM()
  Mat <- OM@Stock[[1]]@Maturity
  Mat@Pars <- list()
  Mat@Model <- NULL
  # MeanAtLength now supplied, MeanAtAge derived from it
  Mat@MeanAtAge <- NULL
  OM@Stock[[1]]@Maturity <- Mat
  OM <- .Repop(OM)
  Mat <- OM@Stock[[1]]@Maturity
  expect_true('MeanAtAge' %in% names(Mat@Misc$Derived))

  # editing the derived MeanAtAge makes it supplied
  Mat@MeanAtAge[] <- 0.5
  OM@Stock[[1]]@Maturity <- Mat
  expect_true(all(.Repop(OM)@Stock[[1]]@Maturity@MeanAtAge == 0.5))
})
