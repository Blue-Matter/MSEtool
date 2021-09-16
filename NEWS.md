The current version of the `MSEtool` package is available for download from [CRAN](https://CRAN.R-project.org/package=MSEtool).

## MSEtool 3.2.9999 - Development Version 

### Fixes
- fix minor bug in calculating SSB in first year when using OMs imported from SS3
- fix minor bug when passing movement matrix in with cpars
- fix minor bug so that the mean of sampled recruitment deviations in `SS2MOM` and `SS2OM` is one. The issue was noticeable when the autocorrelation is high.

### Minor changes
- update `plot_mov` for plotting movement matrices.

### New additions
- added `Awatea2OM` to import MCMC output from an Awatea assessment model to openMSE
- reference points are now calculated assuming constant stock-recruit alpha and beta parameters. Previously, constant R0 and steepness was assumed. See (https://openmse.com/tutorial-reference-points/) for more details.

## MSEtool 3.2.0

### Fixes 
- fix issue in `SSMOM2OM` where selectivity was NA when fishing mortality was 0
- remove debugging message that was stupidly left in last release

### Minor changes
- add `silent` option to `SS2Data`

### New additions
- add option to include fecundity-at-age used to calculate spawning biomass (`cpars$Fec_age`)

## MSEtool 3.1.1 

### Fixes
- patch bug in generation of size composition data (introduced in v3.1.0)
- fix minor issue with `SSMOM2OM` where it was generating redundant rec devs

### New additions
- added `plot_SS2MOM` to compare MOM dynamics with SS3 predictions

## MSEtool 3.1.0

### New additions
- Dynamic and equilibrium spawning potential ratio (SPR) are now calculated for the historical and projection years.
- add option to include empirical weight-at-age for the catches with `cpars$Wt_age_C`
- F_crash, F_med, and other reference points have been added to `Hist@Ref$ByYear`
- can now optimize for depletion in terms of SB/SBMSY with `control$D <- 'SBMSY'`
- option to simulate CAL data including removals (retained + discards) with `OM@cpars$control$CAL="removals"`. 
Default is to simulate only retained catch-at-length.
- option to apply TAC to all removals with `OM@cpars$control$TAC="removals"`. 
Default is to for TAC to refer to retained catch (i.e., removals will be greater than TAC if there are discards)

### Minor changes
- add more informative messages when conditioning on real data
- add conditioning on real data for `multiMSE`
- MSY calculations now in  C++ for significant speed increase

### Fixes
- fix the plus-group calculations in `popdynOneTScpp`
- fix issue with simulated index in `AddInd` in the projection years


## MSEtool 3.0.2

### Updates
- Help description has been updated for `Stock`, `Fleet`, and `Obs` objects (many thanks to Sarah Valencia).

### Fixes
- fix minor bugs in `XL2Data` for importing data from CSV

## MSEtool 3.0.1 

### Minor Changes
- Add `I_beta`, `SpI_beta`, and `VI_beta` for the individual indices. Defaults to use `OM@beta` for all, unless a real
index is supplied, e.g., `OM@cpars$Data@Ind`, `OM@cpars$Data@SpInd`, or `OM@cpars$Data@VInd`, or if supplied in cpars, 
e.g., `OM@cpars$I_beta`

### Fixes
- fix bug where variability in von Bert *K* with `OM@Ksd` was not implemented correctly (5e6e8c6).
- fix error with incorrect beta values when conditioning with real index (ie cpars$Data@Ind)
- fix randomly occurring bug in C++ code that was causing crashes
- fix issue with importing composition data with `new('Data',..)` (issue #33)
- fix `cpars$beta` and `cpars$Esd` issue (issue #34)
- fix issue with importing Data files from csv

## MSEtool 3.0.0
This is a new major release of the `MSEtool` package. It is not backwards compatible with previous versions of `MSEtool` or `DLMtool`.

### Major Changes
- The most significant change in this version is that the `MSEtool` package now contains all code related to generating operating models, simulating fisheries dynamics, conducting management strategy evaluation, and examining the results (previously in the `DLMtool` package). This change was primarily done to better align the actual contents of the packages with the respective package names.
- `MSEtool` now only has a set of reference management procedures (e.g., `FMSYref`)
- The [Data-Limited Methods Toolkit](https://github.com/Blue-Matter/DLMtool) package now contains the collection of data-limited methods, and uses `MSEtool` V3+ as a dependency; i.e., installing and loading `DLMtool` will also install and load `MSEtool` and make all functions for generating OMs, conducting MSE, etc available. 
- The operating model has been updated and an age-0 age-class has been added to the population dynamics model; i.e., recruitment is now to age-0 (previously the OM in `DLMtool` had recruitment to age-1).
- The catch-at-age (CAA) in the `Data` object now includes age-0 (i.e., all age data must be length `maxage+1`)
