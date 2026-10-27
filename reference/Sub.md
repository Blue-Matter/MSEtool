# Subset MSE object by management procedure (MP) or simulation.

Subset the MSE object by particular MPs (either MP number or name), or
particular simulations, or a subset of the projection years (e.g., 1: \<
projection years).

## Usage

``` r
Sub(MSEobj, MPs = NULL, sims = NULL, years = NULL)
```

## Arguments

- MSEobj:

  A MSE object.

- MPs:

  A vector MPs names or MP numbers to subset the MSE object. Defaults to
  all MPs.

- sims:

  A vector of simulation numbers to subset the MSE object. Can also be a
  logical vector. Defaults to all simulations.

- years:

  A numeric vector of projection years. Should start at 1 and increase
  by one to some value equal or less than the total number of projection
  years.

## See also

[SubOM](https://msetool.openmse.com/reference/SubOM.md) for OM
components and
[SubCpars](https://msetool.openmse.com/reference/SubCpars.md) for
subsetting by simulation and projection years.

## Author

A. Hordyk

## Examples

``` r
if (FALSE) { # \dontrun{
MSE <- runMSE()
MSE_1 <- Sub(MSE, MPs=1:2)
MSE_1@MPs
MSE_2 <- Sub(MSE, sims=1:10)
MSE_2@nsim
} # }
```
