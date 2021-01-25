The current version of the `MSEtool` package is available for download from [CRAN](https://CRAN.R-project.org/package=MSEtool).

## MSEtool 3.0.9000 - Development Version

### Fixes
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
