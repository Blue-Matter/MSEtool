## Note to CRAN Maintainers

This release fixes the issue flagged on the CRAN check page: "Missing or unexported object: r4ss::SS_output" and the missing Rd cross-references to `r4ss::SS_output`. The CRAN version of r4ss (1.44.1) does not export `SS_output`. MSEtool now looks up `SS_output` at run time and, when it is not available, stops with an error explaining how to install r4ss from r-universe or GitHub. The Rd cross-references are now plain code text.

The release also fixes several bugs in the legacy code and moves snowfall from Depends to Suggests.

This is the final release of the 3.x series. A major version (MSEtool 4.0.0) will be submitted in the coming weeks.

## Test environments
* local - Windows, R 4.6.1
* Github actions - windows-latest (release)
* Github actions - windows-latest (devel)
* Github actions - macOS-latest (release)
* Github actions - ubuntu-latest (release)
* Github actions - ubuntu-latest (devel)
* Github actions - ubuntu-latest (oldrel-1)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Reverse Dependencies
DLMtool, SAMtool, openMSE, and Slick were checked with this release and pass R CMD check.
