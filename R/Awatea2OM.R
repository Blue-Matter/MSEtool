#' Reads MCMC estimates from Awatea (Paul Starr) processed r file structure into an operating model
#'
#' @description A function that generates an operating model from the MCMC samples of an Awatea model.
#' Code optimized for the BC Pacific ocean perch assessment (Haigh et al. 2018).
#' @param AwateaDir A folder with Awatea files
#' @param nsim The number of simulations
#' @param proyears The number of projection years for the MSE
#' @param Name The name of the operating model
#' @param Source Reference to assessment documentation e.g. a url
#' @param Author Who did the assessment
#' @param verbose Return detailed messages?
#' @details
#' This function averages biological parameters across sex and then sends arrays to \link{VPA2OM}, assumes
#' unfished status (B/B0 = 1) in the first year, and assumes a single fishing fleet.
#' @references
#' Haigh, R., et al. 2018. Stock assessment for Pacific Ocean Perch (\emph{Sebastes alutus}) in Queen Charlotte Sound, British Columbia in 2017.
#' Canadian Science Advisory Secretariat (CSAS) Research Document 2018/038. 232 pp. 
#' \url{https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2018/2018_038-eng.html}
#' 
#' @author Q. Huynh and T. Carruthers
#' @export
Awatea2OM <- function(AwateaDir, nsim = 48, proyears = 50, Name = "OM made by Awatea2OM",
                      Source = "No source provided", Author = "No author provided", verbose = TRUE) {

  Aenv <- new.env()
  
  #bmcmc_file <- list.files(AwateaDir)[grep("Bmcmc", list.files(AwateaDir))]
  #load(file.path(AwateaDir, bmcmc_file), envir = Aenv)
  #bmcmc <- Aenv$Bmcmc[[1]][[1]] # Second tier list is the outputs
  
  Awateatxt <- file.path(AwateaDir, list.files(AwateaDir)[grep(".txt",list.files(AwateaDir))])
  Apar <- AwateaRead(Awateatxt)
  
  if(verbose) message("Searching for currentMCMC.rda...")
  load(file.path(AwateaDir, "currentMCMC.rda"), envir = Aenv)
  if(verbose) message("Searching for currentRes.rda...")
  load(file.path(AwateaDir, "currentRes.rda"), envir = Aenv)
  
  if(verbose) message("Awatea .txt input file (growth parameters) and process R output files read successfully.")
  
  # Select posterior samples
  nmcmc <- nrow(Aenv$currentMCMC$P)
  samp <- sample(1:nmcmc, nsim, replace = nsim > nmcmc)
  if(verbose && nsim > nmcmc) {
    message("The number of simulations (", nsim, ") is greater than the number of MCMC samples (",
            nmcmc,"). Posterior will be sampled with replacement.")
  }
  
  years <- Aenv$currentRes$extra$general$StartYear:Aenv$currentRes$extra$general$EndYear
  nyears <- length(years)
  CurrentYr <-  Aenv$currentRes$extra$general$EndYear
  
  #### Assume constant biological parameters
  age <- min(Aenv$currentRes$Sel$Age):max(Aenv$currentRes$Sel$Age)
  maxage <- max(age)
  n_age <- maxage + 1
  
  ## Female maturity only
  Mataa <- local({
    out <- array(0, c(n_age, nyears, nsim))
    out[age + 1, , ] <- Aenv$currentRes$Sel$P[Aenv$currentRes$Sel$Series == "Maturity" & Aenv$currentRes$Sel$Sex == "Female"]
    aperm(out, c(3, 1, 2))
  })
  
  ## Take the average between male and female. Constant with time and age
  M <- Aenv$currentMCMC$P[grepl("M", names(Aenv$currentMCMC$P))][samp, ] %>% apply(1, mean) %>%
    array(c(nsim, n_age, nyears))
  
  # Awatea inputs to growth parameters -----------------------------------
  laa <- local({
    out <- matrix(0:maxage, 2, n_age, byrow = TRUE)
    apply(Apar$Linf * (1 - exp(-Apar$K * (out - Apar$t0))), 2, mean) %>% array(c(n_age, nsim, nyears)) %>%
      aperm(c(2, 1, 3))
  })
  
  wataa <- local({
    out <- matrix(0:maxage, 2, n_age, byrow = TRUE)
    laa <- Apar$Linf * (1 - exp(-Apar$K * (out - Apar$t0)))
    apply(Apar$a * laa ^ Apar$b, 2, mean) %>% array(c(n_age, nsim, nyears)) %>%
      aperm(c(2, 1, 3))
  })
  
  # Fleets. Using harvest rate, nfleet should be 1
  nfleet <- substr(colnames(Aenv$currentMCMC$U), 6, 6) %>% unique() %>% length()
  if(!verbose && nfleet > 1) warning("Number of fishing fleets > 1")
  
  FAA <- local({
    mu <- Aenv$currentMCMC$P[grepl("mu", names(Aenv$currentMCMC$P))][samp, 1]
    Delta <- Aenv$currentMCMC$P[grepl("Delta", names(Aenv$currentMCMC$P))][samp, 1]
    log_nu <- Aenv$currentMCMC$P[grepl("log v", names(Aenv$currentMCMC$P))][samp, 1]
    
    # sim x year
    U <- Aenv$currentMCMC$U[samp, ]
    U_F <- lapply(1:length(samp), function(x) {
      sapply(1:nyears, function(y) U[x, y] * AwateaSel(0:maxage, mu[x], nu = exp(log_nu[x])))
    })
    U_M <- lapply(1:length(samp), function(x) {
      sapply(1:nyears, function(y) U[x, y] * AwateaSel(0:maxage, mu[x] + Delta[x], nu = exp(log_nu[x])))
    })
    
    U_mean <- lapply(1:length(samp), function(x) c(U_F[x], U_M[x]) %>% simplify2array() %>% apply(1:2, mean))
    F_mean <- -log(1 - simplify2array(U_mean))
    aperm(F_mean, c(3, 1, 2))
  })
  
  # Numbers at age
  naa <- local({
    out <- array(NA_real_, c(nsim, n_age, nyears))
    out[, age[1] + 1, ] <- Aenv$currentMCMC$R[samp, ] %>% as.matrix() # Year-1 recruitment does not match R0
    out[, age[1] + 1, 1] <- Aenv$currentMCMC$P$R_0[samp] 
    
    # Calc abundance for ages younger than the age of recruitment
    if(age[1] > 0) {
      aind_missing <- age[1]:1
      for(i in 1:length(aind_missing)) {
        out[, aind_missing[i], 1:(nyears-i)] <- out[, aind_missing[i]+1, 2:(nyears+1-i)] * 
          exp(M[, aind_missing[i], 1:(nyears-i)])
      }
    }
    
    # Assume unfished in year one
    for(a in age[2]:maxage + 1) out[, a, 1] <- out[, a-1, 1] * exp(-M[, a-1, 1])
    out[, n_age, 1] <- out[, n_age, 1]/(1 - exp(-M[, n_age, 1]))
    
    for(y in 2:nyears) {
      for(a in age[2]:maxage + 1) out[, a, y] <- out[, a-1, y-1] * exp(-FAA[, a-1, y-1] - M[, a-1, y-1])
      out[, n_age, y] <- out[, n_age, y] + out[, n_age, y-1] * exp(-FAA[, n_age, y-1] - M[, n_age, y-1])
    }
    out
  })
  
  # Steepness R0 based on phi0 from age-one only
  # Adjust with new phi0 starting from age-zero so that alpha and beta are same
  # Beverton-Holt
  R0 <- Aenv$currentMCMC$P$R_0[samp]
  h <- Aenv$currentMCMC$P$h[samp]
  B0 <- rowSums(naa[, -c(1:age[1]), 1] * Mataa[, -c(1:age[1]), 1] * wataa[, -c(1:age[1]), 1]) # Different than in Awatea B0 (female only)
  #SSB <- apply(naa * Mataa * wataa, c(1, 3), sum)
  #B0 <- Aenv$Bmcmc[[1]][[1]]$B0.MCMC[samp]
  phi0 <- B0/R0 # Should be through variability in M
  alpha <- (4*h)/(1-h)/phi0
  beta <- (alpha - 1/phi0)/R0
  phi0_age0 <- local({
    NPR <- matrix(1, nsim, n_age)
    for(a in 2:n_age) NPR[, a] <- NPR[, a-1] * exp(-M[, a-1, 1])
    NPR[, n_age] <- NPR[, n_age]/(1 - exp(-M[, n_age, 1]))
    rowSums(NPR * Mataa[, , 1] * wataa[, , 1])
  })
  mu <- exp(apply(M[, , 1][, 1:age[1], drop = FALSE], 1, cumsum))
  h_new <- h
  R0_new <- R0 * mu
  
  # Identical to lines above, this is the general solution
  #h_new <- mu * alpha * phi0_age0/(4 + mu * alpha * phi0_age0)
  #R0_new <- (mu * alpha - 1/phi0_age0)/beta
  
  ## Process error
  SSB <- apply(naa * Mataa * wataa, c(1, 3), sum)
  Rpred <- 4 * h_new * R0_new * SSB/((1 - h_new) * SSB[, 1] + (5 * h_new - 1) * SSB)
  Dev <- naa[, 1, ]/Rpred
  
  Perr <- apply(log(Dev), 1, sd, na.rm = TRUE)
  AC <- apply(log(Dev), 1, function(x) acf(x[!is.na(x)], plot = FALSE, lag.max = 1)$acf[2])
  
  if(verbose) message("Sending arrays to VPA2OM...")
  
  OM <- VPA2OM(Name = Name, proyears = proyears, CurrentYr = CurrentYr, h = h_new, naa = naa, faa = FAA, waa = wataa, 
               Mataa = Mataa, M = M, laa = laa, LowerTri = age[1], Perr = Perr, AC = AC, R0 = R0_new, phi0 = phi0_age0,
               silent = !verbose)
  
  OM@cpars$Linf <- rep(mean(Apar$Linf), nsim)
  OM@cpars$K <- rep(mean(Apar$K), nsim)
  OM@cpars$t0 <- rep(mean(Apar$t0), nsim)
  
  OM@LenCV <- rep(mean(Apar$Lsd/Apar$Linf), 2)
  
  # Add catch
  OM@cpars$Data <- new("Data")
  OM@cpars$Data@Cat <- matrix(Apar$Catch, 1, nyears)
  OM@cpars$Data@CV_Cat <- matrix(0.01, 1, nyears)
  
  OM@cpars$Data@CAA <- local({
    Age <- unique(Aenv$currentRes$CAc$Age)
    Year <- unique(Aenv$currentRes$CAc$Year)
    CAA <- Aenv$currentRes$CAc %>% group_by(Year, Age) %>% summarise(Nobs = sum(SS * Obs)) %>% 
      reshape2::acast(Year ~ Age, value.var = "Nobs")
    CAA_out <- array(0, c(1, nyears, n_age))
    CAA_out[1, match(Year, years), match(Age, 0:maxage)] <- CAA
    CAA_out
  })
  
  # surveys
  if(nrow(Aenv$currentRes$Survey) > 0) {
    
    survey <- split(Aenv$currentRes$Survey, Aenv$currentRes$Survey$Series)
    nsurvey <- length(survey)
    OM@cpars$Data@AddInd <- sapply(survey, function(x) x$Obs[match(years, x$Year)]) %>% 
      array(c(1, nsurvey, nyears))
    OM@cpars$Data@CV_AddInd <- sapply(survey, function(x) x$CV[match(years, x$Year)]) %>% 
      array(c(1, nsurvey, nyears))
    
    OM@cpars$Data@AddIunits <- ifelse(Apar$Iunits == 1, 1, 0) # 1 = Biomass, not 1 = abundance
    OM@cpars$Data@AddIndType <- rep(1, nsurvey)
    
    OM@cpars$Data@AddIndV <- local({ # -1 assumes that there is a single fleet
      mu <- Aenv$currentMCMC$P[grepl("mu", names(Aenv$currentMCMC$P))][samp, -1, drop = FALSE]
      Delta <- Aenv$currentMCMC$P[grepl("Delta", names(Aenv$currentMCMC$P))][samp, -1, drop = FALSE]
      log_nu <- Aenv$currentMCMC$P[grepl("log v", names(Aenv$currentMCMC$P))][samp, -1, drop = FALSE]
      
      sel_mean_over_sex <- lapply(1:nsurvey, function(s) {
        sapply(1:length(samp), function(x) {
          sel_F <- AwateaSel(0:maxage, mu[x, s], exp(log_nu[x, s]))
          sel_M <- AwateaSel(0:maxage, mu[x, s] + Delta[x, s], exp(log_nu[x, s]))
          rbind(sel_F, sel_M) %>% apply(2, mean)
        })
      })
      
      simplify2array(sel_mean_over_sex) %>% aperm(c(2, 3, 1))
    })
    OM@cpars$AddIbeta <- matrix(1, nsim, nsurvey)
  }
  
  return(OM)
}


AwateaRead <- function(Awateatxt, quiet = TRUE) {
  uniquetext <- c("Bi-scalar", "bii exponent", "L-infinity", "k of the von", "t0 of the von", "S.d. of length at age of oldest",
                  "Survey abundance units", "Catch")
  param <- c("a", "b", "Linf", "K", "t0", "Lsd", "Iunits", "Catch")
  alltext <- readLines(Awateatxt)
  out <- lapply(uniquetext, function(x) scan(Awateatxt, skip = grep(x, alltext), nlines = 1, quiet = quiet))
  structure(out, names = param)
}

AwateaSel <- function(age, mu, nu) ifelse(age > mu, 1, exp(-(age - mu)^2/nu))
