Mov2 <- function(p12, p21) matrix(c(1 - p12, p12, p21, 1 - p21), 2, byrow = TRUE)

test_that("CalcUnfishedDist uses the stationary distribution for annual age-invariant movement", {
  mov <- array(Mov2(0.2, 0.3), c(1, 2, 2, 1, 1),
               list(Sim = 1, FromArea = 1:2, ToArea = 1:2, Age = 0, Year = 2000))
  ud <- CalcUnfishedDist(Spatial(Movement = mov), Ages(MaxAge = 5), 2000:2010)@UnfishedDist
  expect_equal(as.vector(ud[1, , 1, 1]), CalcAsymDist(Mov2(0.2, 0.3)))
})

test_that("CalcUnfishedDist follows the seasonal cycle of movement matrices", {
  Steps <- 2000 + c(0, 0.25, 0.5, 0.75)
  M <- list(Mov2(0.9, 0.05), Mov2(0.1, 0.1), Mov2(0.05, 0.9), Mov2(0.1, 0.1))
  mov <- array(NA, c(1, 2, 2, 1, 4),
               list(Sim = 1, FromArea = 1:2, ToArea = 1:2, Age = 0, Year = Steps))
  for (k in 1:4) mov[1, , , 1, k] <- M[[k]]
  ud <- CalcUnfishedDist(Spatial(Movement = mov), Ages(MaxAge = 3), c(Steps, Steps + 1))@UnfishedDist

  p <- CalcAsymDist(Reduce(`%*%`, M))
  for (k in 1:4) {
    p <- as.vector(p %*% M[[k]])
    expect_equal(as.vector(ud[1, , 1, k]), p)
  }
})

test_that("CalcUnfishedDist handles age-varying Movement without UnfishedDist", {
  mov <- array(NA, c(1, 2, 2, 4, 1),
               list(Sim = 1, FromArea = 1:2, ToArea = 1:2, Age = 0:3, Year = 2000))
  for (a in 1:4) mov[1, , , a, 1] <- Mov2(0.1 * a, 0.3)
  ud <- CalcUnfishedDist(Spatial(Movement = mov), Ages(MaxAge = 3), 2000:2010)@UnfishedDist

  p <- CalcAsymDist(mov[1, , , 1, 1])
  expect_equal(as.vector(ud[1, , 1, 1]), p)
  for (a in 2:3) {
    p <- as.vector(p %*% mov[1, , , a, 1])
    expect_equal(as.vector(ud[1, , a, 1]), p)
  }
  expect_equal(as.vector(ud[1, , 4, 1]), CalcAsymDist(mov[1, , , 4, 1]))

  # plus group weighted by the numbers entering from the previous age
  S <- exp(-0.2)
  N <- array(S^(0:3) * c(1, 1, 1, 1 / (1 - S)), c(1, 4, 1),
             list(Sim = 1, Age = 0:3, Year = 2000))
  Surv <- array(S, c(1, 4, 1), list(Sim = 1, Age = 0:3, Year = 2000))
  ud <- CalcUnfishedDist(Spatial(Movement = mov), Ages(MaxAge = 3), 2000:2010,
                         Number = N, Survival = Surv)@UnfishedDist
  A <- mov[1, , , 4, 1]
  Plus <- as.vector((N[1, 3, 1] * S * ud[1, , 3, 1]) %*% A %*% solve(diag(2) - S * A))
  expect_equal(as.vector(ud[1, , 4, 1]), Plus / sum(Plus))
})

test_that("Unfished spatial distribution is stationary under seasonal and age-varying movement", {
  skip_on_cran()
  data(SeasonalSpatialOM, envir = environment())
  om <- SeasonalSpatialOM
  om@nSim <- 2
  om@Stock[[1]]@SRR@SD <- 0
  nAge  <- nAge(om@Stock[[1]])
  Steps <- Years(om, 'Hist')[1:om@Seasons]
  mov <- array(NA, c(1, 3, 3, nAge, om@Seasons),
               list(Sim = 1, FromArea = 1:3, ToArea = 1:3,
                    Age = om@Stock[[1]]@Ages@Classes, Year = Steps))
  for (a in seq_len(nAge)) for (k in seq_len(om@Seasons)) {
    stay <- c(0.6 + 0.35 * sin(2 * pi * k / om@Seasons)^2, 0.7, 0.5 + 0.4 * a / nAge)
    w <- if (k <= om@Seasons / 2) c(0.7, 0.3) else c(0.2, 0.8)
    m <- diag(stay)
    for (i in 1:3) m[i, setdiff(1:3, i)] <- (1 - stay[i]) * w
    mov[1, , , a, k] <- m
  }
  om@Stock[[1]]@Spatial <- Spatial(Movement = mov, RelativeSize = array(1/3, c(1, 3)))

  hist <- Simulate(om, silent = TRUE)
  Frac <- function(N) N / array(apply(N, 1:3, sum), dim(N))
  Dyn <- Frac(hist@Unfished@Dynamic@Number[[1]])
  Eq  <- Frac(hist@Unfished@Equilibrium@Number[[1]])
  nT <- dim(Dyn)[3]
  Last <- (nT - om@Seasons + 1):nT
  expect_lt(max(abs(Dyn[1, , Last, ] - Eq[1, , 1:om@Seasons, ]), na.rm = TRUE), 1e-6)
})
