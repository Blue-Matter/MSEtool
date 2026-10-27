# Determine optimal number of cpus

Determine optimal number of cpus

## Usage

``` r
optCPU(nsim = 96, thresh = 5, plot = TRUE, msg = TRUE, maxn = NULL)
```

## Arguments

- nsim:

  Numeric. Number of simulations.

- thresh:

  Recommended n cpus is what percent of the fastest time?

- plot:

  Logical. Show the plot?

- msg:

  Logical. Should messages be printed to console?

- maxn:

  Optional. Maximum number of cpus. Used for demo purposes

## See also

[setup](https://msetool.openmse.com/reference/setup.md)

## Author

A. Hordyk

## Examples

``` r
if (FALSE) { # \dontrun{
optCPU()
} # }
```
