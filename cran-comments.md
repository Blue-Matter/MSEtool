## Note to CRAN Maintainers

Major release (4.0.0). Fixes the issues on the CRAN Package Check page for
3.7.5 (missing Rd link and missing object `r4ss::SS_output`).

## Test environments
* local - Windows, R 4.6.0
* Github actions - windows-latest (release)
* Github actions - windows-latest (devel)
* Github actions - macOS-latest (release)
* Github actions - ubuntu-latest (release)
* Github actions - ubuntu-latest (devel)
* Github actions - ubuntu-latest (oldrel-1)

## R CMD check results
0 ERRORs | 0 WARNINGs | 0 NOTEs

## Reverse Dependencies
We checked the 4 reverse dependencies (DLMtool, SAMtool, openMSE, Slick) with
R CMD check against this version. No new problems were found.

openMSE will be updated to version 2.0 once this version of MSEtool is on CRAN.
