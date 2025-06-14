The current version of the `MSEtool` package is available for download from [CRAN](https://CRAN.R-project.org/package=MSEtool).

# MSEtool 3.7.9999

## Fixes
- `calc_weightedmean_c`, used in `SSMOM2OM`, can return `NaN` in `cpars$Wt_age_C = 0`. Overwrite `NaN` with `0`. Occurs with un-exploited age classes, e.g., age zero.

# MSEtool 3.7.4

## Fixes
- Fixed issue where TAC was being applied to landings rather than removals 
- `updateData` and `updateData_MS` use the Baranov catch equation to simulate `Data@CAA` in the projection. Previously, the catch at age was simulated with only the retention curve.
- `multiMSE` solves for F from multi-fleet TAC simultaneously with function `CalcMPDynamics_MF`
- fix check for simulated `Data@VInd` in `multiMSE`
- turn off check for valid `Data@AddInd` statistics (AC, SD) when `cpars$AddIerr` is provided

## Updates
- When indices are provided to the operating model, the default value of beta = 1 (hyperstability parameter) if not specified in `cpars`.
- Add grid to base graphics
- MSY calculations in `Simulate` use fishery weight at age `FleetPars$Wt_age_C`
- `multiMSE(extended = TRUE)` returns overall F by stock (and area) and `MMSE@PPD` includes StockPars, FleetPars, ReferencePoints in `Data@Misc`
- `runMSE(extended = TRUE)` leaves StockPars, FleetPars, ReferencePoints in `Data@Misc` of `MSE@PPD` slot
- add `addMMPs()` function
- Internal function `Export_customMPs` allows for MPs defined in more than one namespace, e.g., functions initially defined in a package and modified in the global environment.
# MSEtool 3.7.3

## Fixes
- fix to `addMPs`
- fix print for `PM` objects when `nsim<15`
- fix to `LinInterp_cpp` when searching for values on bounds

## New additions
- add `Splot` and empirical MP `Emp`

# MSEtool 3.7.2 

## New additions
- added `nsim` argument to `Simulate` as an override to `OM@nsim`. Useful for generating `Hist` object with 2 sims when all historical values are identical across simulations
- added `BAM2OM` to create OM from Beaufort Assessment Model (BAM) output
- add MICE rel for recruitment deviates in `multiMSE` which can be a function of the state dynamics in either the current year or previous year
- MICE rel for natural mortality can be age-specific in `multiMSE`
- custom stock-recruit functions available for `multiMSE`
- allow plusgroup to be turned off in `multiMSE`
- add `spawn_time_frac` argument to `iSCAM2OM`
- hermaphroditism in `multiMSE` can be time-varying if entries in the `MOM@SexPars$Herm` list is an array with dimensions `nsim, maxage+1, nyears+proyears`
- MICE relationship can be specified by reading in the abundance at age and year from the operating model
- MICE functions can operate as a multiplier on the base value instead of over-writing the base value
- `MOM@cpars$control <- list(HermEq = FALSE)` sidesteps the first-year equilibrium age and sex distribution calculation from the hermaphroditic age schedule
- document the various `cpars$control` options in `help("validcpars")`

## Internal
- projections use `dynGet` then `get` to find the MPs in the R session (see function `getMP`). `dynGet` finds functions that are defined inside the function call stack but not available in the global environment.

## Fixes
- fix `plot_SS2MOM` for latest version of `r4ss`
- fix to reported `MMSE@SB_SBMSY` for `MMSE` objects
- fix to `OMdoc`
- fixes to simulation of length comp
- add `cpars$qs` to `OM` produce by `SS2OM`
- Note: `SS2OM` does not return `cpars$Data`

# MSEtool 3.7.1 

## Changes
- modifications to calculation of selectivity and retention curves when there is high discarding

## Fixes 
- fix calculation of `retL` when `retA` passed in `cpars`
- fix calculation of F when `spawn_time_frac` used in `multiMSE`


# MSEtool 3.7.0

## Fixes 
- fix to `Calc_Residuals` when not estimating beta parameter
- improve mapping of data across stocks in multiMSE
- improve calculation of weighted mean of empirical weight-at-age for SS2OM
- change to calculation of F in `popdyn.R` to include discard mortality
- fix SPR reporting for multiMSE
- fix `SubCpars` for `MOM` objects
- fix year indexing for stock allocation for species complexes in multiMSE

## New additions
- allow steepness (`OM@h`) to exceed 1 for Ricker stock-recruit relationship (the simulated steepness value in the Data object will be `Data@steep = rlnorm(OM@nsim, log(StockPars$hs - 0.2), sdconv(1, Obs@hbiascv)) + 0.2 - 0.5 * sdconv(1, Obs@hbiascv)^2`.
- `Assess2OM` utilizes `spawn_time_frac`

# MSEtool 3.6.2

## Fixes
- important fix to a bug for projected indices when real indices are provided with NA values
- minor fix to `SS2OM` when empirical weight-at-age is not available
- minor fix to `L95` calculation when all age classes are mature (defaults to 1.5)
- minor fix to `OM.Rmd`

## New additions
- add ability to control timing of spawning with `cpars$spawn_time_frac` (default is beginning of year `spawn_time_frac=0`). Need to check timing on order of operations if this is used together with MICE features!
- implemented `Data@Vuln_CAL` in generating CAL samples. `Data@Vuln_CAA` is not currently implemented
- add ability to map `CAL_nsamp` and other sample size data across stocks in `MOM`

## Minor changes 
- `MSE@Hist` now includes slots `AtAge` and `TSdata` when `extended=FALSE`
- changed `MSE@N` slot to include age and area dimensions.
- changed `MMSE@N` slot to include age and area dimensions.
- added `extended` argument to `multiMSE` to report N-at-age in `MMSE@Misc$extended$N`
- added optional `MOM@SexPars$share_par = FALSE` argument that turns off parameter mirroring of stock-recruit, depletion parameters; fleet parameters related to effort trends; and all observation/implementation parameters. This can be used to generalize a 2-sex MOM to a multi-stock model with shared spawning output (`SexPars$SSBfrom`) and/or movement-at-age between stocks (`SexPars$Herm`).

## Fixes
- fix issue with calculation of indices in projections when there are NAs in the real data
- minor fix to `SS2OM` when empirical weight-at-age is not available
- minor fix to `L95` calculation when all age classes are mature (defaults to 1.5)
- minor fix to `OM.Rmd`

# MSEtool 3.6.1

## New additions
- added additional indices to `plot.Data`
- added option for custom stock-recruitment function (in development/testing)

## Minor changes 
- Add `Misc` information to `PPD` for `MMSE`
- remove `devtools` from Suggests and replace with `remotes`
- updates to `SS2MOM_plots` plots

## Fixes
- fix minor bugs in `SS2Data`
- fix bug in calculating selectivity/retention in `SSMOM2OM`
- fix bug in calculating fecundity-at-age for `SS2OM` in `SSinternal`
- fix minor bug in `SS2MOM_plots`
- fix calculation of `L95array` when `Mat_age` passed in through `cpars`
- fix minor bug in `SS2OM` when `report=TRUE`

# MSEtool 3.6.0

## Major changes
- By default, MPs are now no longer run in parallel mode when `parallel=TRUE` in `runMSE` and `multiMSE`. To run MPs in parallel, specify a named list with the name of the MP(s) assigned as TRUE. For example,`parallel=list(AvC=TRUE`).

## New additions
- added `summary.MMSE` generic function
- added `Lag_Data` function to be used internally in custom MPs to lag all time-series data by specified number of time-steps (or optionally only some data slots). See `?Lag_Data` for more information.
- added stock and fleet names (if provided in `MOM`) to `(multiHist)`
- added `Real.Data.Map` to `cpars`. This allows mapping of simulated data across stocks in `MOMs` where the real data
  is not stock-specific; e.g., cases where `MOM` includes male and female stocks but the real data in `cpars$Data` is not sex-specific 
- `runMSE` can use the split-apply-combine technique to run projections in parallel with argument `parallel = "sac"`. Implementation details and notable issues are documented in the help doc `?runMSE`. This is the fastest method for running simulations but may not be suitable for all operating models and it may be more difficult to troubleshoot errors.


## Minor changes 
- minor patch to `ASAP2Data` 
- minor patch in `Assess2OM`
- removed duplicated information in `multiHist` object (now only returns biological information for fleet=1)
- patch to `gettaxa` function for species that are found in FishBase database but not in FishLife database
- update `joinData`, `joinHist`, `joinMSE`, and `addMPs` functions
- `runMSE` calculates vulnerable biomass from fishery weight at age `cpars$Wt_age_C` if provided.
- the realized catch from TAC is no longer constrained `0.5 * VBiomass` (a legacy feature which is now removed). The constraint is updated to `0.999 * Biomass`.
- increase iteration limit to 300 when solving for Baranov F during the MP projections.

# MSEtool 3.5.0

## New additions
- added check for `OM` completeness and default values for some parameters
- historical MPAs now work in `multiMSE`
- argument `parallel` in `runMSE` can takes a named list of MPs
- two new functions `WHAM2OM` and `ASAP2OM` to import assessments into operating models
- `Assess2MOM` to generate a multi-fleet operating model, either for a 2-sex population where recruitment is predicted from female spawning output, or a single-sex population.
- `Assess2OM` supports Ricker stock-recruit relationship.

## Minor changes
- fix to fleet-specific vulnerable biomass provided to `MMPs`
- updated code for conditioning observation parameters on observed indices
- add message showing range of estimated von Bert. parameters when `OM@cpars$Len_age` is provided.
- fix bug in SSBpR for 2-sex `MOM` (introduced in 3.4.0)
- pass projected OM abundance and SSB arrays to `Data@Misc$StockPars` (slot `N_P`, `SSB_P`, etc.) during closed-loop
- now use `usethis` for console messages

# MSEtool 3.4.0 

## New additions
- new functions `makeRel` and `makeMOM` with methods for developing MICE relationships, e.g., density-dependent M.
- depletion optimization in `multiMSE` uses `Wt_age_C` for catch fraction calculations.
- argument `parallel` in `runMSE` can now be MP-specific by specifying a logical vector of length(MPs). This is useful to apply slow-running MPs in parallel, while avoiding the parallel overhead time for fast-running MPs.

## Fixes
- fix bug in re-calculation of MSY refs points when selectivity parameters change (introduced in last version)
- fix indexing in `multiMSE` when growth and natural mortality are time-varying. Internal code in `HistMICE`, `qestMICE`, `popdynMICE` and `popdynOneMICE` was updated for better readability, i.e., removing obsolete/redundant code and better references for array dimensions.

## Minor changes
- add progress bar when knitting OM.rmd in Shiny
- edits to make compatible with latest version of `rfishbase`

# MSEtool 3.3.0 

## New additions
- added `Awatea2OM` to import MCMC output from an Awatea assessment model to openMSE
- reference points are now calculated assuming constant stock-recruit alpha and beta parameters. Previously, constant R0 and steepness was assumed. See [documentation](https://openmse.com/tutorial-reference-points/) for more details.

## Fixes
- fix minor bug in calculating SSB in first year when using OMs imported from SS3
- fix minor bug when passing movement matrix in with cpars
- fix minor bug so that the mean of sampled recruitment deviations in `SS2MOM` and `SS2OM` is one. The issue was noticeable when the autocorrelation is high.
- add check for missing OM slots such as interval, pstar, nyears, etc

## Minor changes
- update `plot_mov` for plotting movement matrices.
- `ProjectMOM` and `multiMSE` now drop the `multiHist` object from the `MMSE` object unless argument `dropHist=FALSE`

# MSEtool 3.2.0

## Fixes 
- fix issue in `SSMOM2OM` where selectivity was NA when fishing mortality was 0
- remove debugging message that was stupidly left in last release

## Minor changes
- add `silent` option to `SS2Data`

## New additions
- add option to include fecundity-at-age used to calculate spawning biomass (`cpars$Fec_age`)

# MSEtool 3.1.1 

## Fixes
- patch bug in generation of size composition data (introduced in v3.1.0)
- fix minor issue with `SSMOM2OM` where it was generating redundant rec devs

## New additions
- added `plot_SS2MOM` to compare MOM dynamics with SS3 predictions

# MSEtool 3.1.0

## New additions
- Dynamic and equilibrium spawning potential ratio (SPR) are now calculated for the historical and projection years.
- add option to include empirical weight-at-age for the catches with `cpars$Wt_age_C`
- F_crash, F_med, and other reference points have been added to `Hist@Ref$ByYear`
- can now optimize for depletion in terms of SB/SBMSY with `control$D <- 'SBMSY'`
- option to simulate CAL data including removals (retained + discards) with `OM@cpars$control$CAL="removals"`. 
Default is to simulate only retained catch-at-length.
- option to apply TAC to all removals with `OM@cpars$control$TAC="removals"`. 
Default is to for TAC to refer to retained catch (i.e., removals will be greater than TAC if there are discards)

## Minor changes
- add more informative messages when conditioning on real data
- add conditioning on real data for `multiMSE`
- MSY calculations now in  C++ for significant speed increase

## Fixes
- fix the plus-group calculations in `popdynOneTScpp`
- fix issue with simulated index in `AddInd` in the projection years


# MSEtool 3.0.2

## Updates
- Help description has been updated for `Stock`, `Fleet`, and `Obs` objects (many thanks to Sarah Valencia).

## Fixes
- fix minor bugs in `XL2Data` for importing data from CSV

# MSEtool 3.0.1 

## Minor Changes
- Add `I_beta`, `SpI_beta`, and `VI_beta` for the individual indices. Defaults to use `OM@beta` for all, unless a real
index is supplied, e.g., `OM@cpars$Data@Ind`, `OM@cpars$Data@SpInd`, or `OM@cpars$Data@VInd`, or if supplied in cpars, 
e.g., `OM@cpars$I_beta`

## Fixes
- fix bug where variability in von Bert *K* with `OM@Ksd` was not implemented correctly (5e6e8c6).
- fix error with incorrect beta values when conditioning with real index (ie cpars$Data@Ind)
- fix randomly occurring bug in C++ code that was causing crashes
- fix issue with importing composition data with `new('Data',..)` (issue #33)
- fix `cpars$beta` and `cpars$Esd` issue (issue #34)
- fix issue with importing Data files from csv

# MSEtool 3.0.0
This is a new major release of the `MSEtool` package. It is not backwards compatible with previous versions of `MSEtool` or `DLMtool`.

## Major Changes
- The most significant change in this version is that the `MSEtool` package now contains all code related to generating operating models, simulating fisheries dynamics, conducting management strategy evaluation, and examining the results (previously in the `DLMtool` package). This change was primarily done to better align the actual contents of the packages with the respective package names.
- `MSEtool` now only has a set of reference management procedures (e.g., `FMSYref`)
- The [Data-Limited Methods Toolkit](https://github.com/Blue-Matter/DLMtool) package now contains the collection of data-limited methods, and uses `MSEtool` V3+ as a dependency; i.e., installing and loading `DLMtool` will also install and load `MSEtool` and make all functions for generating OMs, conducting MSE, etc available. 
- The operating model has been updated and an age-0 age-class has been added to the population dynamics model; i.e., recruitment is now to age-0 (previously the OM in `DLMtool` had recruitment to age-1).
- The catch-at-age (CAA) in the `Data` object now includes age-0 (i.e., all age data must be length `maxage+1`)
