test_that("legacy ProjectMOM reports effective effort when maxF binds", {
  skip_on_cran()
  recE <- function(x) { R <- new("Rec"); R@Effort <- 1; R }
  MMP_EE <- function(x, DataList, ...) lapply(seq_along(DataList), function(p) list(recE(x), recE(x)))
  class(MMP_EE) <- "MMP"
  assign("MMP_EE", MMP_EE, envir = globalenv())
  on.exit(rm("MMP_EE", envir = globalenv()))

  MOM <- Albacore_TwoFleet
  MOM@nsim <- 4; MOM@proyears <- 5; MOM@interval <- 1
  H <- SimulateMOM(MOM, parallel = FALSE, silent = TRUE)
  M <- ProjectMOM(H, "MMP_EE", silent = TRUE, parallel = FALSE)

  E1 <- M@Effort[, 1, 1, 1, ]
  E2 <- M@Effort[, 1, 2, 1, ]
  expect_true(all(abs(E1 - 1) < 1e-3))
  expect_true(all(E2 <= 1 + 1e-4))
  expect_true(all(E2 > 0.99))

  # sim 2 fleet 2 has effort-implied F above maxF; effective effort lies above maxF/target
  FP <- H[[1]][[2]]@SampPars$Fleet
  tgt <- FP$FinF[2] * FP$qs[2] * FP$qvar[2, 2:5] * (1 + FP$qinc[2]/100)^(2:5)
  expect_true(any(tgt > MOM@maxF))
  capped <- tgt > MOM@maxF
  expect_true(all(E2[2, 2:5][capped] > MOM@maxF/tgt[capped] + 1e-4))
})
