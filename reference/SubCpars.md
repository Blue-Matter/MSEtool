# Subset the cpars slot in an operating model

Subset the custom parameters of an operating model by simulation and
projection years

## Usage

``` r
SubCpars(x, ...)

# S4 method for class 'OM'
SubCpars(x, sims = 1:x@nsim, proyears = x@proyears, silent = FALSE)

# S4 method for class 'MOM'
SubCpars(x, sims = 1:x@nsim, proyears = x@proyears, silent = FALSE)
```

## Arguments

- x:

  An object of class
  [OM](https://msetool.openmse.com/reference/OM-class.md) or
  [MOM](https://msetool.openmse.com/reference/MOM-class.md)

- ...:

  Arguments for method.

- sims:

  A logical vector of length `x@nsim` to either retain (TRUE) or remove
  (FALSE). Alternatively, a numeric vector indicating which simulations
  (from 1 to nsim) to keep.

- proyears:

  If provided, a numeric to reduce the number of projection years (must
  be less than `x@proyears`).

- silent:

  Logical to indicate if messages will be reported to console.

## Value

An object of class
[OM](https://msetool.openmse.com/reference/OM-class.md) or
[MOM](https://msetool.openmse.com/reference/MOM-class.md) (same class as
`x`).

## Details

Useful function for running
[multiMSE](https://msetool.openmse.com/reference/multiMSE.md) in batches
if running into memory constraints.

## See also

[Sub](https://msetool.openmse.com/reference/Sub.md) for MSE objects,
[SubOM](https://msetool.openmse.com/reference/SubOM.md) for OM
components.

## Author

T. Carruthers, Q. Huynh
