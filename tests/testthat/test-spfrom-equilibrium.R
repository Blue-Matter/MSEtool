skip_on_cran()

.spfrom_om <- function(hS, hD, SPFrom, Final = 0.4, Seasons = 1, nYear = 150) {
  data(MultiStockOM, envir = environment())
  om <- MultiStockOM
  om@nSim    <- 2
  om@nYear   <- nYear
  om@Seasons <- Seasons
  om@Stock[[1]]@SRR@Pars$h <- hS
  om@Stock[[2]]@SRR@Pars$h <- hD
  om@Stock[[2]]@SRR@SPFrom <- SPFrom
  om@Stock[[2]]@SRR@ShareSPFrom <- FALSE
  for (s in 1:2) {
    om@Stock[[s]]@SRR@SD <- 0
    om@Stock[[s]]@Spatial <- new("spatial")
    om@Stock[[s]]@Depletion@Final <- if (s == 1) Final else numeric()
    for (f in seq_along(om@Fleet[[s]])) {
      om@Fleet[[s]][[f]]@Effort@Effort <- data.frame(Year = c(0, 1), Lower = c(1, 1),
                                                     Upper = c(1, 1), CV = c(0, 0))
      if (!length(Final))
        om@Fleet[[s]][[f]]@Catchability@Efficiency <- 0.05
    }
  }
  om@Complexes <- list(Both = 1:2)
  om@Imp <- om@Imp[1]
  om@Obs <- om@Obs[1]
  om
}

# Equilibrium at the apical F that reproduces the source stock's terminal-year recruitment
.spfrom_compare <- function(hist) {
  yrs    <- as.numeric(dimnames(hist@SProduction)$Year)
  lastCY <- max(floor(yrs))
  idx    <- which(floor(yrs) == lastCY)
  R      <- sapply(hist@Number, \(N) apply(N[, 1, idx, , drop = FALSE], 1, sum))
  R0     <- sapply(hist@OM@Stock, \(s) {
    r0 <- s@SRR@R0
    sum(r0[1, floor(as.numeric(colnames(r0))) == lastCY])
  })
  RRobs <- sweep(R, 2, R0, "/")
  SPobs <- apply(hist@SProduction[, , idx, drop = FALSE], 1:2, mean)
  eq <- CalcEquilibrium(hist, apicalF = exp(seq(log(1e-3), log(2), length.out = 200)),
                        Years = lastCY)
  purrr::map(seq_len(nrow(RRobs)), \(s) {
    rr <- eq@RelRecruits[s, 1, 1, ]
    ok <- rr > 1e-6
    At <- \(q) purrr::map_dbl(seq_len(dim(q)[2]), \(st)
      stats::splinefun(rr[ok], q[s, st, 1, ok])(RRobs[s, 1]))
    list(RReq = At(eq@RelRecruits), RRobs = RRobs[s, ],
         SPeq = At(eq@SProduction), SPobs = SPobs[s, ])
  })
}

test_that("one-hop dependent with a different steepness matches long-run dynamics", {
  hist <- Simulate(.spfrom_om(0.85, 0.4, "Example Albacore Stock"), silent = TRUE)
  for (res in .spfrom_compare(hist)) {
    expect_equal(unname(res$RReq), unname(res$RRobs), tolerance = 1e-4)
    expect_gt(res$RReq[2], 0.1)
    expect_equal(unname(res$SPeq), unname(res$SPobs), tolerance = 1e-4)
  }
})

test_that("one-hop dependent with the source's SRR recruits like the source", {
  hist <- Simulate(.spfrom_om(0.7, 0.7, "Example Albacore Stock"), silent = TRUE)
  for (res in .spfrom_compare(hist)) {
    expect_equal(unname(res$RReq[2]), unname(res$RReq[1]), tolerance = 1e-10)
    expect_equal(unname(res$RReq), unname(res$RRobs), tolerance = 1e-4)
  }
})

test_that("self-blended SPFrom matches long-run dynamics", {
  SPFrom <- c("Example Albacore Stock" = 0.5, "Example Butterfish Stock" = 0.5)
  hist <- Simulate(.spfrom_om(0.85, 0.4, SPFrom, Final = numeric(), nYear = 200), silent = TRUE)
  for (res in .spfrom_compare(hist)) {
    expect_equal(unname(res$RReq), unname(res$RRobs), tolerance = 1e-4)
    expect_equal(unname(res$SPeq), unname(res$SPobs), tolerance = 1e-4)
  }
})

test_that("seasonal one-hop dependent with a different steepness matches long-run dynamics", {
  hist <- Simulate(.spfrom_om(0.85, 0.4, "Example Albacore Stock", Seasons = 2, nYear = 120),
                   silent = TRUE)
  for (res in .spfrom_compare(hist))
    expect_equal(unname(res$RReq), unname(res$RRobs), tolerance = 1e-4)
})

test_that("Depletion@Final aborts for a self-blended SPFrom group", {
  SPFrom <- c("Example Albacore Stock" = 0.5, "Example Butterfish Stock" = 0.5)
  expect_error(Simulate(.spfrom_om(0.85, 0.4, SPFrom), silent = TRUE),
               "blends the stock's own spawning production")
})

test_that("seasonal per-recruit SPR0 for a dependent is its blended SP0 per recruit", {
  hist <- Simulate(.spfrom_om(0.85, 0.4, "Example Albacore Stock", Seasons = 2, nYear = 30),
                   silent = TRUE, control = SimControl(MSYRefs = FALSE))
  lastCY <- max(floor(as.numeric(dimnames(hist@SProduction)$Year)))
  pr <- CalcPerRecruit(hist, apicalF = 0.1, Years = lastCY)
  R0 <- sapply(hist@OM@Stock, \(s) sum(s@SRR@R0[1, floor(as.numeric(colnames(s@SRR@R0))) == lastCY]))
  expect_equal(unname(pr@SPR0[1, 2, 1] * R0[2]), unname(pr@SPR0[1, 1, 1] * R0[1]), tolerance = 1e-8)
})
