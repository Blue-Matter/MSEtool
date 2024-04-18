
convert_units <- function(rdat) {
  if(rdat$info$units.weight =='lb') {
    rdat$a.series$weight <- rdat$parms$wgt.a*
      rdat$a.series$length^rdat$parms$wgt.b
  }
  rdat
}

#' Import a multi-stock, multi-fleet OM from a BAM object
#'
#' @param rdat A list object from the `BAMextras` package. Use `bamExtras::standardize_rdat(rdat)`
#' @param nsim the number of simulations
#' @param proyears the number of projection years
#' @param interval the management interval
#' @param stock_name Name of the stock(s)
#' @param fleet_name Name of the fleet(s)
#' @param LowerTri Integer. The number of recent years for which model estimates of recruitment are ignored (not reliably estimated by the assessment)
#' @param report  Logical, if TRUE, a diagnostic will be reported showing the matching of the OM reconstructed numbers at age vs the assessment.
#' @param ... Additional arguments passed to `MSEtool::Assess2MOM`
#'
#' @return An object of class `MOM`
#' @export
BAM2MOM <- function(rdat, nsim = 48, proyears = 50, interval = 1,
                    stock_name=NULL, fleet_name=NULL, LowerTri=0, report = FALSE, ...) {
  
  parms <- rdat[["parms"]]
  
  # Convert all weights to metric (kg)
  rdat <- convert_units(rdat)
  
  # Grab name of stock
  np <- 1
  snameBAM <- paste(rdat$info$title, rdat$info$species)
  if(!is.null(stock_name)) {
    if(length(stock_name) != np) {
      stop("stock_name is not a length ", np, " character vector")
    }
    snameBAM <- stock_name
  }
  message("BAM model: ", snameBAM)
  
  # Fleet names
  fnameBAM <- names(parms)[grepl("F.prop", names(parms))] |>
    vapply(function(x) strsplit(x, "F.prop.")[[1]][2], character(1))
  
  nf <- length(fnameBAM)
  if(!is.null(fleet_name)) {
    if(length(fleet_name) != nf) {
      stop("fleet_name is not a length ", nf, " character vector")
    }
    fnameBAM <- fleet_name
  }
  message(nf, "-fleet BAM model found:\n", paste0(fnameBAM, collapse = ", "))
  
  
  # Time-series
  a.series <- rdat[["a.series"]] # age data frame
  maxage <- max(a.series$age)
  n_age <- maxage + 1
  sage <- min(a.series$age) # First age class in BAM
  BAM_age <- a.series$age
  
  t.series <- rdat[["t.series"]] # time data frame
  nyears <- nrow(rdat[["F.age"]])
  CurrentYr <- t.series$year[nyears]
  
  ## M-at-Age
  maa <- local({
    M <- array(.Machine$double.eps, c(nsim, n_age, nyears, np))
    M[, BAM_age + 1, , ] <- array(a.series$M, c(length(BAM_age), nsim, nyears, np)) |>
      aperm(c(2, 1, 3, 4))
    M
  })
  
  ## F-at-Age
  faa_agg <- local({
    FM_agg <- array(0, c(nsim, n_age, nyears))
    FM_agg[, BAM_age + 1, ] <- replicate(nsim, rdat[["F.age"]]) %>% aperm(3:1)
    FM_agg
  })
  
  faa <- local({
    FM <- array(0, c(nsim, n_age, nyears, np, nf))
    
    FF <- sapply(fnameBAM, function(fname) {
      apicalF <- t.series[[paste0("F.", fname)]][1:nyears]
      
      sel <- rdat[["sel.age"]][[paste0("sel.m.", fname)]]
      if(is.null(sel)) sel <- rdat[["sel.age"]][["sel.m.D"]]
      
      F_age <- apicalF * sel
      return(F_age)
    }, simplify = "array")
    
    FM[, BAM_age + 1, , , ] <- array(FF, dim = c(nyears, length(BAM_age), nf, np, nsim)) %>%
      aperm(c(5, 2, 1, 4, 3))
    FM
  })
  
  if(any(is.na(faa))) {
    stop("F-at-age for some fleets could not be found")
  }
  
  # Check if faa matches faa_agg
  faa_check <- apply(faa[1, , , 1, ], 1:2, sum)
  message("Difference in aggregate F.age and sum of fishery F-at-age: ", range(faa_agg[1, , ] - faa_check) |>
            round(2) |> paste0(collapse = " to "))
  
  ## Weight-at-Age
  waa <- local({
    Wt <- array(0, c(nsim, n_age, nyears, np))
    Wt[, BAM_age + 1, , ] <- array(a.series$weight, c(length(BAM_age), nsim, nyears, np)) %>%
      aperm(c(2, 1, 3, 4))
    Wt
  })
  
  ## Maturity-at-Age
  mataa <- local({
    if (is.null(a.series$mat.male)) {
      mat_vec <- a.series$mat.female
      # mat_vec <- a.series$mat.female * a.series$prop.female
    } else {
      mat_vec <- a.series$mat.female * a.series$prop.female + a.series$mat.male * (1 - a.series$prop.female)
    }
    mat <- array(0, c(nsim, n_age, nyears, np))
    mat[, BAM_age + 1, , ] <- array(mat_vec, c(length(BAM_age), nsim, nyears, np)) %>%
      aperm(c(2, 1, 3, 4))
    mat
  })
  
  ## Length-at-Age
  laa <- local({
    len <- array(0, c(nsim, n_age, nyears, np))
    len[, BAM_age + 1, , ] <- array(a.series$length, c(length(BAM_age), nsim, nyears, np)) %>%
      aperm(c(2, 1, 3, 4))
    len
  })
  
  ## N-at-Age
  naa <- local({
    add_year <- dim(rdat[["N.age"]])[1] - nyears
    N <- array(NA_real_, c(nsim, n_age, nyears + add_year, np))
    N[, BAM_age + 1, , ] <- rdat[["N.age"]] %>% array(c(nyears + add_year, length(BAM_age), nsim, np)) %>% aperm(c(3:1, 4))
    
    if(sage > 0) {
      aind_missing <- sage:1 # Missing cohorts to be filled in
      for(i in 1:length(aind_missing)) {
        N[, aind_missing[i], 1:nyears, ] <- N[, aind_missing[i] + 1, 1 + 1:nyears, ] *
          exp(maa[, aind_missing[i], 1:nyears, ])
      }
    }
    N[, , 1:nyears, , drop = FALSE]
  })
  
  
  
  
  if (is.null(a.series$prop.male)) {
    ##### For red snapper & black sea bass
    # a.series$reprod is the product of batch fecundity, n batches per year, proportion female
    fecaa <- local({
      bfec <- array(0, c(nsim, n_age, nyears, np))
      
      bfec[, BAM_age + 1, , ] <- array(a.series$reprod, c(length(BAM_age), nsim, nyears, np)) %>%
        aperm(c(2, 1, 3, 4))
      bfec # * mataa
    })
  } else {
    ##### For gag
    # a.series$reprod = weight * (mat.female * prop.female + mat.male * (1 - prop.female))
    fecaa <- local({
      fec <- array(0, c(nsim, n_age, nyears, np))
      
      fec[, BAM_age + 1, , ] <- array(a.series$reprod, c(length(BAM_age), nsim, nyears, np)) %>%
        aperm(c(2, 1, 3, 4))
      fec
    })
  }
  
  # Compare SSB
  # OM_SSB <- apply(naa[,,,1] * fecaa[,,,1] * exp(-parms[["spawn.time"]] * (faa_agg + maa[,,,1])), c(1, 3), sum)
  # plot(OM_SSB[1, ], typ = 'o', ylim=c(0, max(c(OM_SSB[1,], t.series$SSB), na.rm=TRUE)))
  # lines(t.series$SSB, pch = 16, col='red')
  
  R0 <- parms$R.virgin.bc #  [["BH.R0"]]
  
  h <- parms[["BH.steep"]]
  if(is.null(h)) h <- 0.999
  
  phi0 <- parms[["BH.Phi0"]]
  if(is.null(phi0)) phi0 <-  parms[["Phi0"]]
  
  Perr <- parms[["R.sigma.logdevs"]]
  AC <- acf(t.series$logR.dev, lag.max = 1, plot = FALSE, na.action =na.pass)$acf[2]
  
  message("Running Assess2MOM()...")
  
  MOM <- Assess2MOM(Name = paste0(snameBAM, " OM created by BAM2MOM"),
                    proyears = proyears,
                    interval = interval,
                    CurrentYr = CurrentYr,
                    h = h,
                    naa = naa,
                    faa = faa,
                    waa = waa,
                    Mataa = mataa,
                    Maa = maa,
                    laa = laa,
                    nyr_par_mu = 1,
                    report = report,
                    silent = TRUE,
                    R0 = R0,
                    phi0 = phi0,
                    Perr = Perr,
                    AC = AC,
                    LowerTri=LowerTri,
                    fecaa = fecaa
  )
  
  # Add names
  names(MOM@Stocks) <- snameBAM
  names(MOM@Fleets) <- snameBAM
  names(MOM@Fleets[[1]]) <- fnameBAM
  
  # custom parameters 
  LatASD <- local({
    lsd <- array(0, c(nsim, n_age, nyears + proyears))
    lsd[, BAM_age + 1, ] <- array(a.series$length.sd, c(length(BAM_age), nsim, nyears + proyears)) %>%
      aperm(c(2, 1, 3))
    lsd
  })
  
  for(p in 1:np) {
    MOM@Stocks[[p]]@a <- parms$wgt.a
    MOM@Stocks[[p]]@b <- parms$wgt.b
    MOM@Stocks[[p]]@Name <- snameBAM[p]
    for(f in 1:nf) {
      MOM@Fleets[[p]][[f]]@Name <- fnameBAM[f]
      MOM@cpars[[p]][[f]][["LatASD"]] <- LatASD
      MOM@cpars[[p]][[f]][["Linf"]] <- rep(parms$Linf, nsim)
      MOM@cpars[[p]][[f]][["K"]] <- rep(parms$K, nsim)
      MOM@cpars[[p]][[f]][["t0"]] <- rep(parms$t0, nsim)
    }
    MOM@cpars[[p]][[1]]$spawn_time_frac <- rep(parms$spawn.time, nsim)
  }
  MOM
  
}


#' @describeIn BAM2MOM Create a single stock/fleet OM from a BAM object
BAM2OM <- function(rdat, nsim = 48, proyears = 50, interval = 2, report = FALSE, ...) {
  
  a.series <- rdat[["a.series"]] # age data frame
  maxage <- max(a.series$age)
  n_age <- maxage + 1
  sage <- min(a.series$age) # First age class in BAM
  BAM_age <- a.series$age
  
  t.series <- rdat[["t.series"]] # time data frame
  
  parms <- rdat[["parms"]]
  
  nyears <- length(parms$styr:parms$endyr)
  CurrentYr <- parms$endyr
  
  maa <- local({
    M <- array(MSEtool:::tiny + .Machine$double.eps, c(nsim, n_age, nyears))
    M[, BAM_age + 1, ] <- array(a.series$M, c(length(BAM_age), nsim, nyears)) |>
      aperm(c(2, 1, 3))
    M
  })
  
  faa <- local({
    FM <- array(0, c(nsim, n_age, nyears))
    FM[, BAM_age + 1, ] <- replicate(nsim, rdat[["F.age"]]) |> aperm(3:1)
    FM
  })
  
  waa <- local({
    Wt <- array(0, c(nsim, n_age, nyears))
    Wt[, BAM_age + 1, ] <- array(a.series$weight, c(length(BAM_age), nsim, nyears)) |>
      aperm(c(2, 1, 3))
    Wt
  })
  
  mataa <- local({
    if (is.null(a.series$mat.male)) {
      mat_vec <- a.series$mat.female
      # mat_vec <- a.series$mat.female * a.series$prop.female
    } else {
      mat_vec <- a.series$mat.female * a.series$prop.female + a.series$mat.male * (1 - a.series$prop.female)
    }
    mat <- array(0, c(nsim, n_age, nyears))
    mat[, BAM_age + 1, ] <- array(mat_vec, c(length(BAM_age), nsim, nyears)) |>
      aperm(c(2, 1, 3))
    mat
  })
  
  laa <- local({
    len <- array(0, c(nsim, n_age, nyears))
    len[, BAM_age + 1, ] <- array(a.series$length, c(length(BAM_age), nsim, nyears)) |>
      aperm(c(2, 1, 3))
    len
  })
  
  naa <- local({
    N <- array(NA_real_, c(nsim, n_age, nyears + 1))
    N[, BAM_age + 1, ] <- replicate(nsim, rdat[["N.age"]]) |> aperm(3:1)
    if(sage > 0) {
      aind_missing <- sage:1 # Missing cohorts to be filled in
      for(i in 1:length(aind_missing)) {
        N[, aind_missing[i], 1:nyears] <- N[, aind_missing[i] + 1, 1 + 1:nyears] *
          exp(maa[, aind_missing[i], 1:nyears])
      }
    }
    N[, , 1:nyears]
  })
  
  if (is.null(a.series$prop.male)) {
    ##### For red snapper
    # a.series$reprod is the product of batch fecundity, n batches per year, proportion female
    fecaa <- local({
      bfec <- array(0, c(nsim, n_age, nyears))
      
      bfec[, BAM_age + 1, ] <- array(a.series$reprod, c(length(BAM_age), nsim, nyears)) |>
        aperm(c(2, 1, 3))
      bfec * mataa
    })
  } else {
    ##### For gag
    # a.series$reprod = weight * (mat.female * prop.female + mat.male * (1 - prop.female))
    fecaa <- local({
      fec <- array(0, c(nsim, n_age, nyears))
      
      fec[, BAM_age + 1, ] <- array(a.series$reprod, c(length(BAM_age), nsim, nyears)) |>
        aperm(c(2, 1, 3))
      fec
    })
  }
  
  # Compare SSB
  #OM_SSB <- apply(naa * fecaa * exp(-parms[["spawn.time"]] * (faa + maa)), c(1, 3), sum)
  #plot(OM_SSB[1, ], typ = 'o')
  #lines(t.series$SSB, pch = 16)
  
  R0 <- parms[["BH.R0"]]
  
  h <- parms[["BH.steep"]]
  if(is.null(h)) h <- 0.999
  
  phi0 <- parms[["BH.Phi0"]]
  if(is.null(phi0)) phi0 <-  parms[["Phi0"]]
  
  Perr <- parms[["R.sigma.logdevs"]]
  AC <- acf(t.series$logR.dev, lag.max = 1, plot = FALSE)$acf[2]
  
  OM <- Assess2OM(Name = paste(rdat$info$title, rdat$info$species),
                  proyears = proyears,
                  interval = interval,
                  CurrentYr = CurrentYr,
                  h = h,
                  naa = naa,
                  faa = faa,
                  waa = waa,
                  Mataa = mataa,
                  Maa = maa,
                  laa = laa,
                  nyr_par_mu = 1,
                  report = report,
                  silent = TRUE,
                  R0 = R0,
                  phi0 = phi0,
                  Perr = Perr,
                  AC = AC,
                  fecaa = fecaa
  )
  
  OM@cpars$LatASD <- local({
    lsd <- array(0, c(nsim, n_age, nyears + proyears))
    lsd[, BAM_age + 1, ] <- array(a.series$length.sd, c(length(BAM_age), nsim, nyears + proyears)) |>
      aperm(c(2, 1, 3))
    lsd
  })
  OM@cpars$Linf <- rep(parms$Linf, nsim)
  OM@cpars$K <- rep(parms$K, nsim)
  OM@cpars$t0 <- rep(parms$t0, nsim)
  
  OM@a <- parms$wgt.a
  OM@b <- parms$wgt.b
  
  return(OM)
}





