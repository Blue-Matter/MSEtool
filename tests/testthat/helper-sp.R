SPReferenceModel <- function(Pars, Catch, Index, SD, Timing, Weight, EstSD,
                             nSub = 4, nItF = 5, Fmax = 3, FPenalty = 1e3,
                             FoxTol = 1e-3, MinSD = 0.05) {
  Catch <- unname(Catch)
  FMSY <- Pars[1]; MSY <- Pars[2]; Dep <- Pars[3]; n <- Pars[4]
  Fox   <- abs(n - 1) < FoxTol
  BMSYK <- if (Fox) exp(-1) else n^(1 / (1 - n))
  K     <- MSY / (FMSY * BMSYK)
  Rate  <- (if (Fox) exp(1) else n^(n / (n - 1)) / (n - 1)) * MSY / K
  Growth <- function(B) {
    x <- B / K
    if (Fox) c(-Rate * log(x), -Rate / B) else c(Rate * (1 - x^(n - 1)), -Rate * (n - 1) * x^(n - 1) / B)
  }
  dt <- 1 / nSub
  Step <- function(B0, F) {
    B <- B0; dB <- 0; C <- 0; dC <- 0; BSub <- ZSub <- numeric(nSub)
    for (s in seq_len(nSub)) {
      g <- Growth(B)
      z <- g[1] - F
      dz <- g[2] * dB - 1
      e <- exp(z * dt)
      if (abs(z * dt) < 1e-6) {
        h <- dt * (1 + z * dt / 2); dh <- dt^2 / 2 * (1 + 2 * z * dt / 3)
      } else {
        h <- (e - 1) / z; dh <- (dt * e * z - (e - 1)) / z^2
      }
      BSub[s] <- B; ZSub[s] <- z
      C  <- C + F * B * h
      dC <- dC + B * h + F * h * dB + F * B * dh * dz
      dB <- e * (dB + B * dt * dz)
      B  <- B * e
    }
    list(B = B, C = C, dC = dC, BSub = BSub, ZSub = ZSub)
  }
  nYear <- length(Catch)
  B <- numeric(nYear + 1); F <- numeric(nYear)
  B[1] <- Dep * K
  BIndex <- matrix(NA_real_, nYear, ncol(Index))
  Pen <- 0
  for (y in seq_len(nYear)) {
    Fy <- 0
    if (Catch[y] > 0) {
      Fy <- min(Catch[y] / B[y], Fmax)
      for (it in seq_len(nItF)) {
        st <- Step(B[y], Fy)
        if (!(st$dC > 0)) break
        Fy <- min(max(Fy - (st$C - Catch[y]) / st$dC, 0), Fmax)
      }
    }
    F[y] <- Fy
    st <- Step(B[y], Fy)
    B[y + 1] <- st$B
    for (i in seq_len(ncol(Index))) {
      s <- min(max(floor(Timing[i] * nSub), 0), nSub - 1)
      BIndex[y, i] <- st$BSub[s + 1] * exp(st$ZSub[s + 1] * (Timing[i] - s * dt))
    }
    if (Catch[y] > 0) Pen <- Pen + FPenalty * (log(Catch[y]) - log(max(st$C, 1e-300)))^2
  }
  NLL <- Pen
  for (i in seq_len(ncol(Index))) {
    ok <- is.finite(Index[, i]) & Index[, i] > 0
    if (!any(ok)) next
    d <- log(Index[ok, i]) - log(BIndex[ok, i])
    if (EstSD[i]) {
      r <- d - mean(d)
      sig <- max(sqrt(mean(r^2)), MinSD)
      nll <- sum(ok) * log(sig) + sum(r^2) / (2 * sig^2)
    } else {
      s <- SD[ok, i]
      w <- 1 / s^2
      r <- d - sum(w * d) / sum(w)
      nll <- sum(log(s) + r^2 / (2 * s^2))
    }
    NLL <- NLL + Weight[i] * nll
  }
  list(NLL = NLL, B = B, F = F, BIndex = BIndex, K = K)
}

SimSPData <- function(nYear = 40, nIndex = 2, Seed = 1, CV = c(0.2, 0.3),
                      IndexStart = c(1, 15), FMSY = 0.2, MSY = 100, Shape = 2,
                      Dep = 0.9, Timing = c(0.5, 0), IndexGap = NULL,
                      FPattern = c('oneway', 'contrast')) {
  FPattern <- match.arg(FPattern)
  set.seed(Seed)
  nRamp <- round(nYear * 0.6)
  FSeries <- if (FPattern == 'oneway') {
    c(seq(0.1, 1.8, length.out = nRamp), rep(0.9, nYear - nRamp)) * FMSY
  } else {
    c(seq(0.1, 2, length.out = nRamp), seq(2, 0.5, length.out = nYear - nRamp)) * FMSY
  }
  Pars <- c(FMSY, MSY, Dep, Shape)
  K <- MSY / (FMSY * Shape^(1 / (1 - Shape)))
  Truth <- SPProject_cpp(Pars, Dep * K, FSeries, rep(1L, nYear), 4L, 5L, 3, 1e-3)
  Catch <- Truth$Catch
  Years <- 1980 + seq_len(nYear) - 1
  Index <- CVMat <- matrix(NA_real_, nYear, nIndex)
  CV <- rep_len(CV, nIndex); IndexStart <- rep_len(IndexStart, nIndex); Timing <- rep_len(Timing, nIndex)
  Ref <- SPReferenceModel(Pars, Catch, matrix(1, nYear, nIndex), matrix(NA, nYear, nIndex),
                          Timing, rep(1, nIndex), rep(TRUE, nIndex))
  for (i in seq_len(nIndex)) {
    Rows <- IndexStart[i]:nYear
    if (!is.null(IndexGap)) Rows <- setdiff(Rows, IndexGap)
    sd <- sqrt(log(1 + CV[i]^2))
    Index[Rows, i] <- 0.001 * i * Ref$BIndex[Rows, i] * exp(rnorm(length(Rows), -sd^2 / 2, sd))
    CVMat[Rows, i] <- CV[i]
  }
  dimnames(Index) <- dimnames(CVMat) <- list(Year = Years, Index = paste0('Index', seq_len(nIndex)))
  Landings <- matrix(Catch, ncol = 1, dimnames = list(Year = Years, Fleet = 'Fleet1'))
  D <- Data(Years = Years, YearLH = max(Years),
            Landings = CatchData(Name = 'Fleet1', Value = Landings, Units = 'Biomass'),
            Survey = IndicesData(Name = colnames(Index), Value = Index, CV = CVMat,
                                 Units = rep('Biomass', nIndex), Timing = Timing))
  BMSY <- MSY / FMSY
  list(Data = D, Truth = list(B = Truth$B, F = FSeries, BMSY = BMSY, FMSY = FMSY, MSY = MSY,
                              K = K, B_BMSY = utils::tail(Truth$B, 1) / BMSY,
                              F_FMSY = utils::tail(FSeries, 1) / FMSY),
       Pars = Pars)
}
