# TACs() reads MSE@PPD, which is indexed per complex, not per stock.

.RunTACsMSE <- function(om) {
  om@nSim <- 3
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  Project(hist, MPs = "CurrentCatch", parallel = FALSE, silent = TRUE)
}

.ExpectTACs <- function(mse) {
  tac <- TACs(mse)
  expect_s3_class(tac, "data.frame")
  expect_gt(nrow(tac), 0)
  expect_identical(names(tac)[1:5], c("Sim", "Stock", "MP", "Year", "Period"))
  expect_setequal(unique(tac$Stock), names(mse@OM@Complexes))
  expect_setequal(unique(tac$Sim), seq_len(nSim(mse)))
  expect_setequal(unique(tac$MP), names(PPD(mse)))
  expect_true(all(tac$Variable == "TAC"))
  expect_true(all(is.finite(tac$Value)))

  aavy <- PM_AAVY(mse, Type = "TAC")
  expect_true(any(is.finite(aavy@Stat)))
  invisible(tac)
}

test_that("TACs() works for a single-stock OM", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  mse <- .RunTACsMSE(SingleStockOM)
  tac <- .ExpectTACs(mse)
  expect_identical(unique(tac$Stock), StockNames(mse))
})

test_that("TACs() labels rows by complex for a multi-stock complex OM", {
  skip_on_cran()
  data(ComplexOM, envir = environment())
  mse <- .RunTACsMSE(ComplexOM)
  tac <- .ExpectTACs(mse)
  expect_identical(unique(tac$Stock), "StockComplex")

  aavy <- PM_AAVY(mse, Type = "TAC", Stocks = StockNames(mse)[1])
  expect_true(any(is.finite(aavy@Stat)))
})

test_that("TACs() carries a TAC forward through years the MP leaves it unchanged", {
  skip_on_cran()
  data(TwoFleetOM, envir = environment())
  om <- TwoFleetOM
  om@nSim <- 2
  pYear(om) <- 5
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  Odd <- function(Data) if (Data@Misc$AdviceYear %% 2 == 1) CurrentCatch(Data) else Advice()
  class(Odd) <- "mp"
  mse <- Project(hist, MPs = list(Odd = Odd), parallel = FALSE, silent = TRUE)
  expect_lt(nrow(mse@PPD$Odd[[1]][[1]]@Advice@TAC), length(Years(mse, "P")))

  tac <- TACs(mse)
  expect_setequal(tac$Year, Years(mse, "P"))
  expect_true(all(tac$Period == "Projection"))
  Even <- tac[tac$Year %% 2 == 0, ]
  Prev <- tac[match(paste(Even$Sim, Even$Fleet, Even$Year - 1), paste(tac$Sim, tac$Fleet, tac$Year)), ]
  expect_equal(Even$Value, Prev$Value)
  expect_identical(TAC(mse), tac)
})

test_that("TACs() returns an empty data frame when no MP sets a TAC", {
  skip_on_cran()
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- 2
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  mse <- Project(hist, MPs = "CurrentEffort", parallel = FALSE, silent = TRUE)

  tac <- TACs(mse)
  expect_s3_class(tac, "data.frame")
  expect_equal(nrow(tac), 0)
  expect_identical(names(tac)[1:5], c("Sim", "Stock", "MP", "Year", "Period"))
  expect_no_error(PM_AAVY(mse, Type = "TAC"))
})

.AdviceRegMSE <- function(nSim = 2) {
  data(SingleStockOM, envir = environment())
  om <- SingleStockOM
  om@nSim <- nSim
  pYear(om) <- 4
  om@Fleet[[1]][[1]]@Effort@Theta <- 2
  om@Fleet[[1]][[1]]@Effort@TripsScalar <- 1
  set.seed(1)
  hist <- Simulate(om, silent = TRUE)
  nA <- nArea(om)
  Reg <- function(Data) {
    a <- CurrentCatch(Data)
    if (Data@Misc$AdviceYear == min(Years(hist, "P"))) a@Closure <- matrix(c(0, rep(1, nA - 1)), 1, nA)
    a@Retention <- Retention(Pars = list(RL50 = 60, RL50_95 = 10))
    a@BagLimit <- 5
    a@LimitType <- "boat"
    a
  }
  class(Reg) <- "mp"
  Project(hist, MPs = list(Reg = Reg, CurrentEffort = "CurrentEffort"), parallel = FALSE, silent = TRUE)
}

test_that("Advice() on an mse returns all of the advice returned by each MP", {
  skip_on_cran()
  mse <- .AdviceRegMSE()
  adv <- Advice(mse)
  expect_s3_class(adv, "data.frame")
  expect_identical(names(adv), c("Sim", "Stock", "MP", "Year", "Period", "Fleet", "Area",
                                 "Variable", "Type", "Units", "Class", "Value"))
  PYears <- Years(mse, "P")

  Reg <- adv[adv$MP == "Reg", ]
  expect_setequal(unique(Reg$Variable), c("TAC", "Closure", "BagLimit", "Retention"))
  expect_setequal(unique(adv$Variable[adv$MP == "CurrentEffort"]), "Effort")

  TAC <- Reg[Reg$Variable == "TAC", ]
  expect_equal(TAC$Value, TACs(mse)$Value[TACs(mse)$MP == "Reg"])
  expect_true(all(TAC$Type == "Removals" & TAC$Units == "Biomass"))

  # the first-year closure carries forward to later years
  Closure <- Reg[Reg$Variable == "Closure" & Reg$Sim == 1, ]
  expect_setequal(Closure$Year, PYears)
  expect_equal(Closure$Value[Closure$Area == "1"], rep(0, length(PYears)))

  Bag <- Reg[Reg$Variable == "BagLimit", ]
  expect_true(all(Bag$Value == 5 & Bag$Type == "boat, discard"))

  Ret <- Reg[Reg$Variable == "Retention" & Reg$Type == "Length", ]
  expect_setequal(Ret$Year, PYears)
  expect_setequal(unique(Ret$Sim), seq_len(nSim(mse)))
  Stored <- mse@Misc$Retention$Reg[[1]][[1]]$MeanAtLength
  expect_equal(sort(unique(Ret$Value)), sort(unique(as.numeric(Stored))))
})

test_that("Advice() on an mse survives Subset(), CombineMSE() and CombineMP()", {
  skip_on_cran()
  mse <- .AdviceRegMSE()
  adv <- Advice(mse)
  Key <- function(a) a[order(a$Sim, a$MP, a$Variable, a$Type, a$Fleet, a$Area, a$Year, a$Class), ] |>
    `rownames<-`(NULL)

  Sub <- Advice(Subset(mse, Sims = 2))
  expect_equal(Key(Sub)[, -1], Key(adv[adv$Sim == 2, ])[, -1])

  Comb <- Advice(CombineMSE(list(Subset(mse, Sims = 1), Subset(mse, Sims = 2)), silent = TRUE))
  expect_equal(Key(Comb), Key(adv))

  CE <- Subset(mse, MPs = "CurrentEffort")
  CE@Misc$Retention <- NULL
  Joined <- Advice(CombineMP(list(CE, Subset(mse, MPs = "Reg")), silent = TRUE))
  expect_equal(Key(Joined), Key(adv))
})
