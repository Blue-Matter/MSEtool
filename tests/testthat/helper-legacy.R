.LegacyCache <- new.env()

# snow warns on partial argument matching when legacy code first loads snowfall
suppressWarnings(requireNamespace("snowfall", quietly = TRUE))

# testOM with deterministic catchability so reference MPs give exact F
.LegacyOM <- function(nsim = 3, proyears = 12) {
  OM <- MSEtool::testOM
  OM@nsim <- nsim
  OM@proyears <- proyears
  OM@interval <- 1
  OM@qinc <- OM@qcv <- c(0, 0)
  OM
}

.LegacyHist <- function() {
  if (is.null(.LegacyCache$Hist))
    .LegacyCache$Hist <- Simulate(.LegacyOM(), silent = TRUE)
  .LegacyCache$Hist
}

.LegacyMSE <- function() {
  if (is.null(.LegacyCache$MSE))
    .LegacyCache$MSE <- Project(.LegacyHist(), MPs = c("FMSYref", "curEref", "NFref"), silent = TRUE)
  .LegacyCache$MSE
}

.LegacyMultiHist <- function() {
  if (is.null(.LegacyCache$multiHist)) {
    MOM <- MSEtool::Albacore_TwoFleet
    MOM@nsim <- 3
    MOM@proyears <- 5
    MOM@interval <- 1
    .LegacyCache$multiHist <- SimulateMOM(MOM, parallel = FALSE, silent = TRUE)
  }
  .LegacyCache$multiHist
}

# Legacy Project() looks MPs up by name in the global environment
.LocalLegacyMPs <- function(..., env = parent.frame()) {
  MPs <- list(...)
  for (nm in names(MPs)) {
    fn <- MPs[[nm]]
    class(fn) <- "MP"
    assign(nm, fn, envir = globalenv())
  }
  withr::defer(rm(list = names(MPs), envir = globalenv()), envir = env)
  invisible(names(MPs))
}

.LegacyRec <- function(...) {
  Rec <- new("Rec")
  args <- list(...)
  for (nm in names(args)) slot(Rec, nm) <- args[[nm]]
  Rec
}

.LegacyTwoStockMOM <- function() {
  new("MOM", Stocks = list(MSEtool::Albacore, MSEtool::Butterfish),
      Fleets = list(list(MSEtool::Generic_Fleet), list(MSEtool::Generic_Fleet)),
      Obs = list(list(MSEtool::Generic_Obs), list(MSEtool::Generic_Obs)),
      Imps = list(list(MSEtool::Perfect_Imp), list(MSEtool::Perfect_Imp)),
      CatchFrac = list(matrix(1, 3, 1), matrix(1, 3, 1)),
      nsim = 3, proyears = 5, interval = 1)
}

.LegacyTwoStockHist <- function() {
  if (is.null(.LegacyCache$TwoStockHist))
    .LegacyCache$TwoStockHist <- suppressMessages(SimulateMOM(.LegacyTwoStockMOM(), parallel = FALSE, silent = TRUE))
  .LegacyCache$TwoStockHist
}
