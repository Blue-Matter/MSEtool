# Subset() of the Misc slots: MSEtool's own records follow every dimension; other content
# (here, Advice@Misc set by an MP) is left as supplied.

.UserMiscMP <- function(Fleets, nSim) {
  MP <- function(Data) {
    Adv <- CurrentCatch(Data)
    Yr  <- Data@Misc$AdviceYear
    Adv@Misc$Table    <- data.frame(Year = Yr + 0:1, Sim = 1:2, Value = c(1, 2))
    Adv@Misc$YearArr  <- array(1:3, 3, dimnames = list(Year = Yr + 0:2))
    Adv@Misc$AgeArr   <- array(1:4, 4, dimnames = list(Age = 0:3))
    Adv@Misc$Par      <- c(r = 0.3, K = 1000)
    Adv@Misc$ByFleet  <- stats::setNames(as.list(seq_along(Fleets)), Fleets)
    Adv@Misc$Names    <- c(Fleets, "CurrentCatch", "CurrentEffort", "other")
    Adv@Misc$PerSim   <- stats::setNames(seq_len(nSim) * 10, seq_len(nSim))
    Adv
  }
  class(MP) <- "mp"
  MP
}

.SubsetMiscMSE <- local({
  cache <- list()
  function(OMName = "TwoFleetOM") {
    if (!is.null(cache[[OMName]])) return(cache[[OMName]])
    om <- get(OMName)
    om@nSim <- 4
    pYear(om) <- 4
    set.seed(1)
    hist <- Simulate(om, silent = TRUE)
    Fleets <- unique(unlist(FleetNames(hist)))
    mse <- Project(hist, MPs = list(CurrentCatch = "CurrentCatch", CurrentEffort = "CurrentEffort",
                                    UserMisc = .UserMiscMP(Fleets, om@nSim)), silent = TRUE)

    # a retention change recorded for CurrentEffort only, laid out as .RecordSelRetDisc() does
    TS <- names(mse@Misc$Advice$CurrentCatch)
    Ages <- dimnames(mse@Number[[1]])$Age
    MAA <- array(stats::runif(om@nSim * length(Ages) * length(TS)), c(om@nSim, length(Ages), length(TS)),
                 dimnames = list(Sim = seq_len(om@nSim), Age = Ages, Year = TS))
    mse@Misc$Retention <- list(CurrentEffort = stats::setNames(lapply(StockNames(mse), \(s)
      stats::setNames(lapply(Fleets, \(f) list(MeanAtAge = MAA)), Fleets)), StockNames(mse)))
    mse@Misc$UserNote    <- c(names(mse@MPs), "keep me")
    mse@OM@Misc$UserNote <- c(names(mse@MPs), Fleets)
    cache[[OMName]] <<- list(hist = hist, mse = mse, Fleets = Fleets)
    cache[[OMName]]
  }
})

.UserAdviceMisc <- function(mse, yr = 1, sim = 1, st = 1)
  mse@Misc$Advice$UserMisc[[yr]][[sim]][[st]]@Misc

test_that("Subset(Sims = ) keeps each sim's Advice@Misc and PPD Data@Misc whole", {
  skip_on_cran()
  mse <- .SubsetMiscMSE()$mse
  Sims <- c(3, 1)
  m <- Subset(mse, Sims = Sims)
  for (i in seq_along(Sims)) {
    expect_identical(.UserAdviceMisc(m, sim = i), .UserAdviceMisc(mse, sim = Sims[i]))
    expect_identical(m@PPD$UserMisc[[i]][[1]]@Misc, mse@PPD$UserMisc[[Sims[i]]][[1]]@Misc)
  }
  TAC <- mse@Misc$TACAttainment$CurrentCatch$TAC[[1]]
  expect_equal(unname(m@Misc$TACAttainment$CurrentCatch$TAC[[1]]), unname(TAC[Sims, , , drop = FALSE]))
})

test_that("Subset(Years = ) trims the advice record and selectivity changes, not MP content", {
  skip_on_cran()
  mse <- .SubsetMiscMSE()$mse
  TS   <- names(mse@Misc$Advice$CurrentCatch)
  Keep <- c(Years(mse, "H"), as.numeric(TS[1:2]))
  m <- Subset(mse, Years = Keep)
  for (mp in names(m@MPs))
    expect_identical(names(m@Misc$Advice[[mp]]), TS[1:2])
  expect_identical(dimnames(m@Misc$Retention$CurrentEffort[[1]][[1]]$MeanAtAge)$Year, TS[1:2])
  expect_identical(dimnames(m@Misc$TACAttainment$CurrentCatch$TAC[[1]])$Year, TS[1:2])
  expect_identical(.UserAdviceMisc(m), .UserAdviceMisc(mse))
  expect_identical(m@Misc$UserNote, mse@Misc$UserNote)
  expect_setequal(TACs(m)$Year, as.numeric(TS[1:2]))
  expect_identical(TAC(m), TACs(m))
})

test_that("Subset(Years = ) keeps a seasonal advice record consistent", {
  skip_on_cran()
  mse <- .SubsetMiscMSE("SeasonalSpatialOM")$mse
  TS   <- names(mse@Misc$Advice$CurrentCatch)
  Keep <- c(Years(mse, "H"), as.numeric(TS[1:5]))
  m <- Subset(mse, Years = Keep)
  expect_identical(names(m@Misc$Advice$CurrentCatch), TS[1:5])
  expect_setequal(TACs(m)$Year, as.numeric(TS[1:5]))
})

test_that("Subset(Ages = ) subsets selectivity changes and leaves MP content and named vectors", {
  skip_on_cran()
  fx  <- .SubsetMiscMSE()
  mse <- fx$mse
  m <- Subset(mse, Ages = 0:5)
  expect_identical(dimnames(m@Misc$Retention$CurrentEffort[[1]][[1]]$MeanAtAge)$Age, as.character(0:5))
  expect_identical(.UserAdviceMisc(m), .UserAdviceMisc(mse))

  hist <- fx$hist
  h <- Subset(hist, Ages = 2:10)
  expect_identical(h@OM@Obs[[1]][[1]]@Survey@Efficiency, hist@OM@Obs[[1]][[1]]@Survey@Efficiency)
  expect_identical(h@OM@HistoricalWeight, hist@OM@HistoricalWeight)
  expect_identical(h@OM@Control$DataCV, hist@OM@Control$DataCV)
})

test_that("Subset(MPs = ) drops records of dropped MPs and leaves other Misc content", {
  skip_on_cran()
  mse <- .SubsetMiscMSE()$mse
  m <- Subset(mse, MPs = "CurrentCatch")
  expect_identical(m, Subset(mse, MPs = 1))
  expect_named(m@Misc$Advice, "CurrentCatch")
  expect_named(m@Misc$TACAttainment, "CurrentCatch")
  expect_length(m@Misc$Retention, 0)
  expect_identical(m@Misc$UserNote, mse@Misc$UserNote)
  expect_identical(m@OM@Misc$UserNote, mse@OM@Misc$UserNote)
  expect_error(Subset(mse, MPs = "Nope"), "not found")
})

test_that("Subset(MPs = ) keeps the object's MP order and the historical PPD", {
  skip_on_cran()
  fx  <- .SubsetMiscMSE()
  mse <- fx$mse
  Full <- PPD(mse)
  HistYears <- Years(mse, "H")
  LandYears <- \(d) as.numeric(rownames(d@Landings@Value))

  m <- Subset(mse, MPs = c("UserMisc", "CurrentCatch"))
  Order <- c("CurrentCatch", "UserMisc")
  expect_named(m@MPs, Order)
  expect_identical(dimnames(m@SBiomass)$MP, Order)
  expect_named(m@Misc$Advice, Order)
  expect_named(m@Misc$TACAttainment, Order)
  expect_named(m@PPD, Order)
  expect_identical(PPD(m), Full[Order])

  m <- Subset(mse, MPs = "CurrentEffort")
  expect_true(all(HistYears %in% LandYears(m@PPD[[1]][[1]][[1]])))
  expect_identical(m@PPD[[1]][[1]][[1]]@Misc, mse@PPD$CurrentEffort[[1]][[1]]@Misc)
  expect_identical(PPD(m), Full["CurrentEffort"])
  expect_identical(Hist(m)@Data, Hist(mse)@Data)
})

test_that("Subset(Fleets = ) subsets the TAC records and leaves MP content", {
  skip_on_cran()
  fx  <- .SubsetMiscMSE()
  mse <- fx$mse
  Fl  <- fx$Fleets[1]
  m <- Subset(mse, Fleets = Fl)
  expect_identical(dimnames(m@Misc$TACAttainment$CurrentCatch$TAC[[1]])$Fleet, Fl)
  expect_identical(dimnames(m@Misc$TACAttainment$CurrentCatch$AtCeiling)$Fleet, Fl)
  expect_named(m@Misc$Retention$CurrentEffort[[1]], Fl)
  expect_identical(dimnames(m@Misc$Advice$CurrentCatch[[1]][[1]][[1]]@TAC)$Fleet, Fl)
  expect_identical(.UserAdviceMisc(m), .UserAdviceMisc(mse))
  expect_identical(m@PPD$UserMisc[[1]][[1]]@Misc, mse@PPD$UserMisc[[1]][[1]]@Misc)
  expect_identical(m@OM@Misc$UserNote, mse@OM@Misc$UserNote)
})

test_that("Subset(Fleets = ) keeps an interim annual TAC aligned with the advice TAC", {
  skip_on_cran()
  data(TwoFleetOM, envir = environment())
  om <- TwoFleetOM
  om@nSim <- 3
  om@pYear <- 4
  om@Seasons <- 4
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  fl <- FleetNames(hist)
  hist@OM@MPStartYear <- 2029
  hist@OM@InterimAdvice <- data.frame(Year = rep(2027:2028, each = 2), Fleet = fl, Type = "TAC",
                                      Mean = c(300, 150, 600, 250))
  mse <- Project(hist, MPs = "CurrentCatch", parallel = FALSE, silent = TRUE)
  Adv <- unlist(mse@Misc$Advice$CurrentCatch, recursive = TRUE)
  expect_true(any(purrr::map_lgl(Adv, \(a) !is.null(a@Misc$AnnualTAC))))

  m <- Subset(mse, Fleets = fl[2])
  for (a in unlist(m@Misc$Advice$CurrentCatch, recursive = TRUE))
    if (!is.null(a@Misc$AnnualTAC))
      expect_identical(dimnames(a@Misc$AnnualTAC), dimnames(a@TAC))
})

test_that("Subset(Stocks = ) keys selectivity changes by the retained stock", {
  skip_on_cran()
  mse <- .SubsetMiscMSE("MultiStockOM")$mse
  St <- StockNames(mse)[2]
  m <- Subset(mse, Stocks = 2)
  expect_named(m@Misc$Retention$CurrentEffort, St)
  expect_identical(m@Misc$Advice$UserMisc[[1]][[1]][[1]]@Misc, .UserAdviceMisc(mse, st = 2))
})

test_that("an mse does not store the engine's working Misc", {
  skip_on_cran()
  expect_length(.SubsetMiscMSE()$mse@Hist@Misc, 0)
})
