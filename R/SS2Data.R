
#' Reads data Stock Synthesis file structure into a Data object using package r4ss
#'
#' @description A function that uses the file location of a fitted SS3 model including input files to population
#' the various slots of an Data object.
#' @param SSdir A folder with Stock Synthesis input and output files in it
#' @param Name The name for the Data object
#' @param Common_Name Character string for the common name of the stock.
#' @param Species Scientific name of the species
#' @param Region Geographic region of the stock or fishery.
#' @param min_age_M Currently, the Data object supports a single value of M for all ages. The argument selects the
#' minimum age for calculating the mean of age-dependent M from the SS assessment.
#' @param gender An integer index for the sex for importing biological parameters (1 = female, 2 = male).
#' @param comp_fleet A vector of indices corresponding to fleets in the assessment over which to aggregate the composition
#' (catch-at-length and catch-at-age) data. By default, characer string \code{"all"} will aggregate across all fleets.
#' @param comp_season Integer, for seasonal models, the season for which the value of the index will be used. By default, \code{"mean"}
#' will take the average across seasons.
#' @param comp_partition Integer vector for selecting length/age observations that are retained (2), discarded (1), or both (0). By default, \code{"all"}
#' sums over all available partitions.
#' @param comp_gender Integer vector for selecting length/age observations that are female (1), male (2), or both (0), or both scaled to sum to one (3).
#' By default, \code{"all"} sums over all gender codes.
#' @param index_fleet Obsolete as of DLMtool version 5.4 (all indices will now be included in the AddInd slot).
#' @param index_season Integer, for seasonal models, the season for which the value of the index will be used. By default, \code{"mean"}
#' will take the average across seasons.
#' @param ... Arguments to pass to \link[r4ss]{SS_output}
#' @return An object of class Data.
#' @note Currently supports the  version of r4ss on CRAN (v.1.24) and Github (v.1.34-38). Function may be incompatible with other versions of r4ss.
#' @author T. Carruthers and Q. Huynh
#' @export
#' @seealso \link{SS2OM}
SS2Data <- function(SSdir, Name = "Imported by SS2Data", Common_Name = "", Species = "", Region = "",
                    min_age_M = 1, gender = 1,
                    comp_fleet = "all", comp_season = "sum", comp_partition = "all", comp_gender = "all",
                    index_fleet = "SSB", index_season = "mean", ...) {

  replist <- SS_import(SSdir, ...)

  season_as_years <- FALSE
  if(replist$nseasons == 1 && replist$seasduration < 1) {
    message(paste("Season-as-years detected in SS model. There is one season in the year with duration of", replist$seasduration, "year."))
    season_as_years <- TRUE
    nseas <- 1/replist$seasduration
    message("DLMtool operates on annual basis. Since the SS model is seasonal, we need to aggregate over seasons.\n")
  } else {
    nseas <- replist$nseasons
    if(nseas > 1) {
      message("DLMtool operating model is an annual model. Since the SS model is seasonal, we need to aggregate over seasons.\n")
    }
  }

  # Create Data object
  Data <- new("Data")
  Data@Common_Name <- Common_Name
  Data@Species <- Species
  Data@Region <- Region

  Data@MPs <- NA
  Data@TAC <- array(NA, dim = c(1, 1, 1))
  if(is.null(Name)) {
    Data@Name <- SSdir
  } else Data@Name <- as.character(Name)
  Data@MPeff <- 1

  mainyrs <- replist$startyr:replist$endyr
  if(season_as_years) {
    nyears <- ceiling(length(mainyrs)/nseas)
    Data@Year <- 1:nyears

    seas1_yind_full <- expand.grid(nseas = 1:nseas, true_year = 1:nyears) # Group assessment years to true years
    seas1_yind_full$assess_year <- mainyrs
    seas1_yind <- which(seas1_yind_full$nseas == 1)

  } else {
    nyears <- length(mainyrs)
    Data@Year <- mainyrs
  }
  Data@LHYear <- Data@Year[length(Data@Year)]
  message(paste("Detected", nyears, "years in the assessment model."))
  message(paste0("First year: ", Data@Year[1], ", Last year: ", Data@Year[length(Data@Year)], "\n"))

  ##### Life history
  #### Growth --------------------------------------
  if(replist$nsexes == 1) gender <- 1
  growdat <- getGpars(replist)[[gender]]      # Age-specific parameters in endyr

  if("int_Age" %in% names(growdat)) {
    ages <- unique(growdat$int_Age)
  } else {
    ages <- unique(growdat$Age)
  }

  # Max age
  Data@MaxAge <- maxage <- floor(max(ages)/ifelse(season_as_years, nseas, 1))

  seas1_aind_full <- expand.grid(nseas = 1:nseas, true_age = 0:maxage)[1:length(ages), ] # Group assessment ages to true ages
  seas1_aind_full$assess_age <- ages
  seas1_aind <- which(seas1_aind_full$nseas == 1) # Age indices that correspond to season 1

  #seas1_aind_full <- expand.grid(nseas = 1:nseas, age = 1:maxage)
  #seas1_aind <- which(seas1_aind_full$nseas == 1)

  GP <- replist$Growth_Parameters   # Some growth parameters (presumably in endyr)
  if(nrow(GP)>1) {
    message(nrow(GP)," rows of growth parameters were reported by r4ss:")
    print(GP)
    message("Row ", gender, " will be used (see gender argument in SS2Data).\n")
    GP <- GP[gender, ]
  }

  #### Length at age --------------------------------------
  Len_age <- growdat$Len_Mid

  Data@vbLinf <- GP$Linf[1]
  t0 <- GP$A_a_L0[1]
  #t0[t0 > 1] <- 0
  Data@vbt0 <- t0
  muK <- GP$K[1]
  if(muK <= 0) { #Estimate K from Len_age if K < 0 (e.g., age-varying K with negative deviations in K).
    message("Negative K value was detected. Attempting to re-estimate K based on mean length-at-age...")
    get_K <- function(K, Lens, Linf, t0, ages) sum((Lens - (Linf * (1 - exp(-K * (ages - t0)))))^2)
    muK <- optimize(get_K, c(0, 2), Lens = Len_age, Linf = GP$Linf[1], t0 = t0, ages = 1:maxage)$minimum
  }
  Data@vbK <- muK

  message("Von Bertalanffy parameters: Linf = ", Data@vbLinf, ", K = ", muK, ", t0 = ", t0)

  LenCV <- GP$CVmax[1]
  if(LenCV > 1) LenCV <- LenCV/Data@vbLinf
  Data@LenCV <- LenCV
  message(paste0("Data@LenCV = ", Data@LenCV, "\n"))

  #### Weight
  Data@wla <- GP$WtLen1[1]
  Data@wlb <- GP$WtLen2[1]

  message(paste0("Length-weight parameters: a = ", Data@wla, ", b = ", Data@wlb))

  #### Maturity --------------------------------------
  if(min(growdat$Len_Mat < 1)) {                    # Condition to check for length-based maturity
    Mat <- growdat$Len_Mat/max(growdat$Len_Mat)
  } else {                                          # Use age-based maturity
    Mat <- growdat$Age_Mat/max(growdat$Age_Mat)
  }
  #if(season_as_years) Mat <- Mat[seas1_aind]

  # Currently using linear interpolation of mat vs len, is robust and very close to true logistic model predictions
  Data@L50 <- LinInterp(Mat, Len_age, 0.5+1e-6)
  Data@L95 <- LinInterp(Mat, Len_age, 0.95)

  message(paste0("Lengths at 50% and 95% maturity: ", paste(round(Data@L50, 2), round(Data@L95, 2), collapse = " "), "\n"))

  #### M --------------------------------------
  M <- growdat$M

  if(length(unique(M)) > 1) {
    if(season_as_years) {
      seasonal_min_age <- min_age_M * nseas
      Data@Mort <- mean(M[growdat$Age >= seasonal_min_age])
    } else {
      Data@Mort <- mean(M[growdat$Age >= min_age_M])
    }
    message("Age-dependent natural mortality detected, but only a single value of M is currently supported for the Data object.")
    message(paste0("Using mean from age ", min_age_M, " and up. Data@Mort = ", Data@Mort, ".\n"))
  } else {
    Data@Mort <- unique(M)
    message(paste0("Natural mortality Data@Mort = ", Data@Mort, "\n"))
  }

  #### Composition data -------------------------

  #### CAA
  if(nrow(replist$agedbase) > 0) {
    CAA <- SS2Data_get_comps(replist, mainyrs, maxage, season_as_years, nseas, comp_gender, comp_fleet, comp_partition, comp_season,
                             type = "age") %>% as.matrix()
    if(!is.null(CAA)) {
      Data@CAA <- array(CAA, c(1, nyears, ncol(CAA)))
      message("Collected age comps.")
    } else {
      message("Could not find age comps that matched these filtering criteria.")
    }
  } else {
    message("No age comps found in SS assessment.")
    Data@CAA <- array(NA, c(1, 1, 1))
  }

  #### CAL
  message("\n")
  if(nrow(replist$lendbase) > 0) {
    CAL <- SS2Data_get_comps(replist, mainyrs, maxage, season_as_years, nseas, comp_gender, comp_fleet, comp_partition, comp_season,
                             type = "length") %>% as.matrix()
    if(!is.null(CAL)) {
      Data@CAL <- array(CAL, c(1, nyears, ncol(CAL)))
      message("Collected length comps.")

      CAL_bins <- replist$lbins # add one more length bin
      width_bin <- CAL_bins[length(CAL_bins)] - CAL_bins[length(CAL_bins)-1]
      plus_one <- CAL_bins[length(CAL_bins)] + width_bin

      Data@CAL_bins <- c(CAL_bins, plus_one)
      Data@CAL_mids <- Data@CAL_bins[1:(length(Data@CAL_bins)-1)] + Data@CAL_bins[2:length(Data@CAL_bins)]

      ML <- rowSums(CAL * rep(Data@CAL_mids, each = nyears), na.rm = TRUE)/rowSums(CAL, na.rm = TRUE)
      ML[ML <= 0 | ML=="NaN"] <- NA
      Data@ML <- matrix(ML, nrow = 1)
      message("Calculated mean lengths.")

      lcpos <- apply(CAL, 1, function(x) if(all(is.na(x))) return(NA) else which.max(x))
      Data@Lc <- matrix(CAL_bins[lcpos], nrow = 1)
      Data@Lc[Data@Lc <= 0] <- NA
      Data@Lbar <- matrix(NA, nrow = 1, ncol = ncol(Data@Lc))
      for(i in 1:ncol(Data@Lbar)) {
        if(!is.na(lcpos[i])) {
          Data@Lbar[1, i] <- weighted.mean(x = Data@CAL_mids[lcpos[i]:length(replist$lbins)],
                                           w = Data@CAL[1, i, lcpos[i]:length(replist$lbins)],
                                           na.rm = TRUE)
        }
      }

    } else {
      message("Could not find length comps that matched these filtering criteria.")
    }
  } else {
    message("No length comps found in SS assessment.")
    Data@CAL <- array(NA, c(1, 1, 1))
  }

  #### Catch -------------------------
  message("\nAdding total catch in weight across all fleets...")
  cat_yr_ind <- !is.na(match(replist$timeseries$Yr, mainyrs))
  ts <- replist$timeseries[cat_yr_ind, ]

  cat_col <- grepl("obs_cat", colnames(ts))
  cat <- ts[, cat_col, drop = FALSE]

  is_weight <- replist$catch_units[replist$IsFishFleet] == 1

  cat_weight <- cat[, is_weight]
  cat_numbers <- cat[, !is_weight]
  if(ncol(cat_numbers) > 0) {
    fleet_in_numbers <- which(replist$catch_units == 2)
    message(paste0("Catch in numbers was detected for Fleets: \n", paste(paste(fleet_in_numbers, replist$FleetNames[fleet_in_numbers]), collapse = "\n")))
    message("Will use estimated assessment output for catch in weight (dead biomass) for these Fleets.")

    cat_col2 <- grepl("dead\\(B", colnames(ts))
    cat2 <- ts[, cat_col2]
    cat_numbers <- cat2[, !is_weight]
  }
  total_catch <- aggregate(rowSums(cbind(cat_weight, cat_numbers)), list(Yr = ts$Yr), sum, na.rm = TRUE)

  tc_ind <- match(total_catch$Yr, mainyrs)
  total_catch_vec <- total_catch$x[tc_ind]
  if(season_as_years) {
    total_catch2 <- aggregate(total_catch_vec, list(Yr = seas1_yind_full$true_year), sum, na.rm = TRUE)
    total_catch_vec <- total_catch2$x
    message("Summing catch across seasons.")
  }
  Data@Cat <- matrix(total_catch_vec, nrow = 1)
  message(paste0(sum(!is.na(Data@Cat[1, ])), " years of catch in Data@Cat."))

  CSD <- replist$catch_error
  if(is.null(CSD) && packageVersion("r4ss") > 1.34) CSD <- replist$catch_se
  if(!all(is.na(CSD))) {
    CSD <- CSD[!is.na(CSD)]
    CSD[CSD <= 0] <- NA
    if(packageVersion("DLMtool") < 5.4) {
      Csd_weighted <- weighted.mean(CSD, colSums(cbind(cat_weight, cat_numbers)), na.rm = TRUE)
      Data@CV_Cat <- sqrt(exp(Csd_weighted^2) - 1)
      message(paste0("CV of Catch (weighted by catch of individual fleets), Data@CV_Cat = ", round(Data@CV_Cat, 2)))
    } else {
      Csd_weighted <- colSums(t(cbind(cat_weight, cat_numbers)) * CSD, na.rm = TRUE)/rowSums(cbind(cat_weight, cat_numbers))
      Data@CV_Cat <- matrix(sqrt(exp(Csd_weighted^2) - 1), 1)
      message("Annual CV of Catch (Data@CV_Cat) is weighted by catch of individual fleets. Range: ",
              paste(signif(range(Data@CV_Cat, na.rm = TRUE), 3), collapse = " - "))
    }
  }
  Data@AvC <- mean(total_catch_vec)
  message("Mean catch, Data@AvC = ", round(Data@AvC, 2), "\n")

  #### Index -------------------------
  Ind <- SS2Data_get_index(replist, mainyrs, season_as_years, nseas, index_season)

  if(is.null(Ind)) {
    message("No indices found.")
    if(packageVersion("DLMtool") >= 5.4) {
      Data@AddInd <- Data@CV_AddInd <- Data@AddIndV <- array(NA, c(1, 1, 1))
    }
  } else {
    message(length(Ind$Iname), " indices of abundance found:")
    message(paste(Ind$Iname, collapse = "\n"))

    if(packageVersion("DLMtool") >= "5.4.4") {

      Data@AddInd <- Ind$AddInd
      Data@CV_AddInd <- sqrt(exp(Ind$SE_AddInd^2) - 1)
      Data@AddIunits <- Ind$AddIunits
      Data@AddIndType <- Ind$AddIndType

      if(season_as_years) {
        AddIndV <- apply(Ind$AddIndV, 1, function(x) {
          xx <- data.frame(assess_age = as.numeric(names(x)), sel = x) %>% left_join(seas1_aind_full[, -1], by = "assess_age")
          xx_agg <- aggregate(xx$sel, by = list(age = xx$true_age), mean, na.rm = TRUE)
          xx_agg$x[xx_agg$age >= 1]
        }) %>% t()
      } else {
        AddIndV <- Ind$AddIndV[ , -1]
      }
      Data@AddIndV <- array(AddIndV, c(1, dim(AddIndV)))

      message("Updated Data@AddInd, Data@CV_AddInd, Data@AddIndV.")
    } else {
      message("\n\n *** Update DLMtool to latest version (5.4.4+) in order to add indices to Data object. *** \n\n")
    }

  }

  #### Recruitment
  message("\n")
  if(packageVersion("r4ss") == 1.24) {
    rec_ind <- match(mainyrs, replist$recruit$year)
  } else rec_ind <- match(mainyrs, replist$recruit$Yr)
  rec <- replist$recruit$pred_recr[rec_ind]

  if(season_as_years) {
    rec2 <- aggregate(rec, by = list(Yr = seas1_yind_full$true_year), mean, na.rm = TRUE)$x
    rec <- rec2
    message("Summing recruitment across seasons.")
  }

  Data@Rec <- matrix(rec/mean(rec), nrow = 1)
  message("Relative recruitment strength to Data@Rec obtained from assessment.")

  #### Depletion
  if(packageVersion("r4ss") == 1.24) {
    SSB <- replist$recruit$spawn_bio[rec_ind]
  } else SSB <- replist$recruit$SpawnBio[rec_ind]

  Data@Dt <- SSB[length(SSB)]/SSB[1]
  message("Depletion since year ", mainyrs[1], " (Data@Dt) = ", round(Data@Dt, 2))

  Data@Dep <- replist$current_depletion
  message("Depletion from unfished conditions (Data@Dep) = ", round(Data@Dep, 2))

  Data@t <- length(Data@Year)

  #### Reference points ----------------------
  message("\n")
  if(packageVersion("r4ss") == 1.24) {
    Data@Cref <- replist$derived_quants$Value[replist$derived_quants$LABEL == "TotYield_MSY"] * ifelse(season_as_years, nseas, 1)
    FMSY <- replist$derived_quants$Value[replist$derived_quants$LABEL == "Fstd_MSY"] * ifelse(season_as_years, nseas, 1)
    Data@Bref <- replist$derived_quants$Value[replist$derived_quants$LABEL == "SSB_MSY"]
    SSB0 <- replist$derived_quants$Value[replist$derived_quants$LABEL == "SSB_Unfished"]
  } else {
    Cref <- replist$derived_quants$Value[replist$derived_quants$Label == "TotYield_MSY"] * ifelse(season_as_years, nseas, 1)
    if(length(Cref) == 0) Cref <- replist$derived_quants$Value[replist$derived_quants$Label == "Dead_Catch_MSY"] * ifelse(season_as_years, nseas, 1)
    Data@Cref <- Cref

    FMSY <- replist$derived_quants$Value[replist$derived_quants$Label == "Fstd_MSY"] * ifelse(season_as_years, nseas, 1)
    Data@Bref <- replist$derived_quants$Value[replist$derived_quants$Label == "SSB_MSY"]
    SSB0 <- replist$derived_quants$Value[replist$derived_quants$Label == "SSB_Unfished"]
    if(length(SSB0) == 0) SSB0 <- replist$derived_quants$Value[replist$derived_quants$Label == "SSB_unfished"]
  }
  message("Reference catch set to MSY, Data@Cref = ", Data@Cref)
  message("Reference biomass set to spawning biomass at MSY, Data@Bref = ", Data@Bref)

  Data@FMSY_M <- FMSY/Data@Mort
  message("FMSY = ", FMSY, ", Data@FMSY_M = ", round(Data@FMSY_M, 2))

  Data@BMSY_B0 <- Data@Bref/SSB0
  message("Data@BMSY_B0 = ", Data@BMSY_B0)

  Data@Units <- "metric tonnes"
  if(packageVersion("r4ss") == 1.24) {
    OFLs <- replist$derived_quants[grepl("OFLCatch", replist$derived_quants$LABEL), ]
  } else OFLs <- replist$derived_quants[grepl("OFLCatch", replist$derived_quants$Label), ]
  if(season_as_years) {
    OFL_terminal <- sum(OFLs$Value[1:nseas])
  } else OFL_terminal <- OFLs$Value[1]

  if(is.na(OFL_terminal)) {
    Data@Ref <- Data@Cat[1, ncol(Data@Cat)]
    Data@Ref_type <- paste("Catch in Year", Data@Year[length(Data@Year)])
    message("No OFL detected from SS Assessment.")
  } else {
    Data@Ref <- OFL_terminal
    if(packageVersion("r4ss") == 1.24) {

      if(season_as_years) {
        Yr_OFL <- vapply(OFLs$LABEL[1:nseas], function(x) strsplit(x, "_")[[1]][2], character(1))
      } else Yr_OFL <- strsplit(OFLs$LABEL[1], "_")[[1]][2]

    } else {
      if(season_as_years) {
        Yr_OFL <- vapply(OFLs$Label[1:nseas], function(x) strsplit(x, "_")[[1]][2], character(1))
      } else Yr_OFL <- strsplit(OFLs$Label[1], "_")[[1]][2]
    }

    if(length(Yr_OFL) == 1){
      Data@Ref_type <- paste("OFL in Year", Yr_OFL, "from SS Assessment")
    } else {
      Data@Ref_type <- paste("OFL in Years", Yr_OFL[1], "-", Yr_OFL[length(Yr_OFL)] , "from SS Assessment")
    }
  }

  message("Data@Ref = ", Data@Ref, " (", Data@Ref_type, ")")

  #### Steepness --------------------------------------
  message("\n")
  if(replist$SRRtype == 3 || replist$SRRtype == 6) { # Beverton-Holt SR
    steep <- replist$parameters[grepl("steep", rownames(replist$parameters)), ]
    Data@steep <- steep$Value

    message("Beverton-Holt steepness = ", Data@steep)
  } else if(replist$SRRtype == 2) { # Ricker
    steep <- replist$parameters[grepl("SR_Ricker", rownames(replist$parameters)), ]
    Data@steep <- steep$Value

    message("Ricker steepness = ", Data@steep)
  } else if(replist$SRRtype == 7) {

    if(packageVersion("r4ss") == 1.24) {
      SSB0 <- replist$derived_quants[replist$derived_quants$LABEL == "SPB_Virgin", 2]
      R0 <- replist$derived_quants[replist$derived_quants$LABEL == "Recr_Virgin", 2]

    } else {
      SSB0 <- replist$derived_quants[replist$derived_quants$Label == "SSB_Virgin", 2]
      R0 <- replist$derived_quants[replist$derived_quants$Label == "Recr_Virgin", 2]
    }
    SpR0 <- SSB0/(R0 * ifelse(season_as_years, nseas, 1))

    s_frac <- replist$parameters$Value[replist$parameters$Label == "SR_surv_Sfrac"]
    Beta <- replist$parameters$Value[replist$parameters$Label == "SR_surv_Beta"]

    s0 <- 1/SpR0
    z0 <- -log(s0)
    z_min <- z0 * (1 - s_frac)

    hs <- 0.2 * exp(z0 * s_frac * (1 - 0.2 ^ Beta))
    Data@steep <- hs
    message("Survival-based stock-recruit relationship with implied steepness = ", round(hs, 2))

  } else message("No steepness value found.")

  if(packageVersion("r4ss") == 1.24) {
    SpAbun_ind <- match(replist$endyr+1, replist$recruit$year)
    Data@SpAbun <- replist$recruit$spawn_bio[SpAbun_ind]
  } else {
    SpAbun_ind <- match(replist$endyr+1, replist$recruit$Yr)
    Data@SpAbun <- replist$recruit$SpawnBio[SpAbun_ind]
  }
  message("Data@SpAbun = ", Data@SpAbun)


  #### Selectivity
  # Get F-at-age in terminal year, then obtain LFC and LFS
  cols <- match(ages, names(replist$Z_at_age))
  rows <- match(mainyrs, replist$Z_at_age$Year)
  if(all(is.na(rows)) && packageVersion("r4ss") >= 1.35) rows <- match(mainyrs, replist$Z_at_age$Yr)

  Z_at_age <- replist$Z_at_age[rows, ]
  M_at_age <- replist$M_at_age[rows, ]

  rows2 <- Z_at_age$Gender == 1 & Z_at_age$Bio_Pattern == 1
  if((all(!rows2, na.rm = TRUE) | all(is.na(rows2))) && packageVersion("r4ss") >= 1.35) rows2 <- Z_at_age$Sex == 1 & Z_at_age$Bio_Pattern == 1

  F_at_age <- t(Z_at_age[rows2, cols] - M_at_age[rows2, cols])
  F_at_age[nrow(F_at_age), ] <- F_at_age[nrow(F_at_age) - 1, ] # assume F at maxage = F at maxage-1

  if(ncol(F_at_age) == nyears - 1) { # Typically because forecast is off
    F_at_age_terminal <- F_at_age[, ncol(F_at_age)]
    F_at_age <- cbind(F_at_age, F_at_age_terminal)
  }
  if(ncol(F_at_age) == nyears && all(is.na(F_at_age[, ncol(F_at_age)]))) {
    F_at_age[, ncol(F_at_age)] <- F_at_age[, ncol(F_at_age)-1]
  }

  F_at_age[F_at_age < 1e-8] <- 1e-8

  V_terminal <- F_at_age[, ncol(F_at_age)]/max(F_at_age[, ncol(F_at_age)])
  Data@LFC <- LinInterp(V_terminal, Len_age, 0.05, ascending = TRUE, zeroint = TRUE) %>% round(2)
  Data@LFS <- Len_age[which.min((exp(V_terminal)-exp(1.05))^2 * 1:length(V_terminal))] %>% round(2)
  if(Data@LFS > Data@vbLinf) warning("Data@LFS > Data@vbLinf")
  Data@Vmaxlen <- V_terminal[length(V_terminal)] %>% round(2)
  message(paste0("\nData@LFC = ", Data@LFC, ", Data@LFS = ", Data@LFS, ", Data@Vmaxlen = ", Data@Vmaxlen))
  if("sigma_R_in" %in% names(replist)) {
    Data@sigmaR <- replist$sigma_R_in
    message("Data@sigmaR = ", Data@sigmaR)
  }
  Data@Log <- Data@Misc <- list(note = paste("SS2Data function from MSEtool", packageVersion("MSEtool")))

  message("\nImport was successful.")

  Data

}



## Internal functions
# Get age or length comps
SS2Data_get_comps <- function(replist, mainyrs, maxage, season_as_years = FALSE, nseas = 1,
                      comp_gender = "all", comp_fleet = "all", comp_partition = "all", comp_season = "sum",
                      type = c("length", "age")) {
  type <- match.arg(type)
  if(type == "length") dbase <- replist$lendbase else dbase <- replist$agedbase

  if(is.character(comp_fleet) && comp_fleet == "all") comp_fleet <- unique(dbase$Fleet)

  if(comp_partition != "all") dbase <- dbase[dbase$Part %in% comp_partition, ]
  message("For ", type, " comps, using partition codes: ", paste(unique(dbase$Part), collapse = " "))

  if(comp_gender != "all") {
    if(!is.null(dbase$Gender)) {
      dbase <- dbase[dbase$Gender %in% comp_gender, ]
    } else {
      dbase <- dbase[dbase$Sex %in% comp_gender, ]
    }
  }
  if(!is.null(dbase$Gender)) {
    message("For ", type, " comps, using gender codes: ", paste(unique(dbase$Gender), collapse = " "))
  } else {
    message("For ", type, " comps, using gender codes: ", paste(unique(dbase$Sex), collapse = " "))
  }

  if(!season_as_years && nseas > 1 && is.numeric(comp_season)) { # Only if !season_as_years & nseas > 1
    dbase <- dbase[dbase$Seas == comp_season, ]
    message("For ", type, " comps, using season ", comp_season)
  }

  dbase_ind <- match(dbase$Yr, mainyrs) # Match years
  dbase <- dbase[!is.na(dbase_ind), ]
  dbase$Obs2 <- dbase$Obs * dbase$N # Expand comp proportions to numbers

  comp_mat <- split(dbase, dbase$Fleet) %>% lapply(acast, formula = list("Yr", "Bin"), fun.aggregate = sum, value.var = "Obs2", fill = 0) # Convert to matrix
  comp_mat <- comp_mat[match(comp_fleet, as.numeric(names(comp_mat)))] # Subset fleets

  expand_matrix <- function(x) {
    yr_match <- match(as.numeric(rownames(x)), mainyrs)
    if(type == "length") {
      res <- matrix(NA, length(mainyrs), length(replist$lbins))
      bin_match <- match(as.numeric(colnames(x)), replist$lbins)
    }
    if(type == "age") {
      res <- matrix(NA, length(mainyrs), maxage)
      bin_match <- 1:(ncol(x) - 1)
      x <- x[, -1]
    }
    res[yr_match, bin_match] <- x
    return(res)
  }

  comp_mat2 <- lapply(comp_mat, expand_matrix) # Expand matrices to full years (by mainyrs)
  comp_all <- do.call(rbind, comp_mat2)

  comp_res <- aggregate(comp_all, list(Yr = rep(1:length(mainyrs), length(comp_fleet))), sum, na.rm = TRUE) # Sum across fleets

  if(season_as_years) {
    seas1_yind_full <- get("seas1_yind_full", envir = parent.frame(), inherits = FALSE)
    if(is.numeric(comp_season) && comp_season <= nseas) {
      comp_res <- comp_res[seas1_yind_full$nseas == comp_season, ]
      message("For ", type, " comps, using season ", comp_season)
    } else if(is.character(comp_season) && comp_season == "sum") {
      comp_res_list <- split(comp_res, seas1_yind_full$true_year)
      comp_res_list <- lapply(comp_res_list, colSums, na.rm = TRUE)
      comp_res <- do.call(rbind, comp_res_list)
      message(type, "comps summed across seasons.")
    }
  }
  comp_res[comp_res == 0] <- NA
  return(comp_res[, -1])
}

SS2Data_get_index <- function(replist, mainyrs, season_as_years = FALSE, nseas = 1, index_season = "mean") {

  cpue_split <- split(replist$cpue, replist$cpue$Fleet)
  cpue_name <- vapply(cpue_split, function(x) {
    out <- unique(x$Fleet_name)
    ifelse(length(out) == 1, out, NA_character_)
  }, character(1))

  if(nrow(replist$cpue) == 0) return(NULL)

  if(season_as_years) {

    seas1_yind_full <- get("seas1_yind_full", envir = parent.frame(), inherits = FALSE)

    cpue_split <- lapply(cpue_split, function(x, y) left_join(x, y, by = c("Yr" = "assess_year")), y = seas1_yind_full[, -1])
    cpue_split <- lapply(cpue_split, function(x) cbind(x$Obs, x$SE) %>% aggregate(by = list(Yr = x$true_year), mean, na.rm = TRUE) %>%
                           structure(names = c("Yr", "Obs", "SE")))

    message("For indices of abundance, taking mean across seasons.")
    mainyrs <- unique(seas1_yind_full$true_year)

  } else if(nseas > 1) {
    if(is.numeric(index_season) && index_season <= nseas) {
      cpue_split <- lapply(cpue_split, function(x) x[x$Seas == index_season, ])
      message("For indices of abundance, using season ", index_season)
    } else if(is.character(index_season) && index_season == "mean") {
      cpue_split <- lapply(cpue_split, function(x) {
        cbind(x$Obs, x$SE) %>% aggregate(by = list(Yr = x$Yr), mean, na.rm = TRUE) %>%
          structure(names = c("Yr", "Obs", "SE"))
      })
      message("For indices of abundance, taking mean across seasons.")
    }
  }

  AddInd <- do.call(rbind, lapply(cpue_split, function(x) x$Obs[match(mainyrs, x$Yr)])) %>%
    array(c(1, length(cpue_split), length(mainyrs)))

  SE_AddInd <- do.call(rbind, lapply(cpue_split, function(x) x$SE[match(mainyrs, x$Yr)])) %>%
    array(c(1, length(cpue_split), length(mainyrs)))

  AddIunits <- replist$survey_units[names(cpue_split) %>% as.numeric()]

  # Selectivity
  agesel_split <- split(replist$ageselex, replist$ageselex$Fleet)
  agesel_ind <- match(names(cpue_split) %>% as.numeric(), names(agesel_split) %>% as.numeric())
  agesel <- agesel_split[agesel_ind]

  lensel_split <- split(replist$sizeselex, replist$sizeselex$Fleet)
  lensel_ind <- match(names(cpue_split) %>% as.numeric(), names(lensel_split) %>% as.numeric())
  lensel <- lensel_split[lensel_ind]

  get_AddIndV <- function(agesel, lensel) {
    agesel <- agesel[agesel$Factor == "Asel" | agesel$Factor == "Asel2", ]
    lensel <- lensel[lensel$Factor == "Lsel", ]
    sel_warn <- any(is.na(match(agesel$Yr, range(mainyrs)))) # Potential change in selectivity

    if(all(agesel$Factor != "Asel2")) {
      ALK <- replist$ALK[, , 1]
      ALK <- ALK[order(as.numeric(rownames(ALK))), ]

      Lsel <- suppressWarnings(lensel[nrow(lensel), !is.na(as.numeric(colnames(lensel)))]) %>% as.numeric()
      Asel2 <- (Lsel %*% ALK)[1, ]
    } else {
      agesel2 <- agesel[agesel$Factor == "Asel2", ]
      Asel2 <- suppressWarnings(agesel2[nrow(agesel2), !is.na(as.numeric(colnames(agesel2)))]) %>% as.numeric()
    }
    Asel <- suppressWarnings(agesel[nrow(agesel), !is.na(as.numeric(colnames(agesel)))]) %>% as.numeric()
    return(list(Asel = Asel * Asel2, sel_warn = sel_warn))
  }
  AddIndV <- Map(get_AddIndV, agesel = agesel, lensel = lensel) %>% lapply(getElement, "Asel")

  return(list(AddInd = AddInd, SE_AddInd = SE_AddInd, AddIunits = AddIunits,
              AddIndV = do.call(rbind, AddIndV), AddIndType = rep(1, length(cpue_split)),
              Iname = cpue_name))
}

