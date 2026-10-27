# A basic comparison of runMSE output (MSE) and multiMSE (MMSE)

A basic comparison of runMSE output (MSE) and multiMSE (MMSE)

## Usage

``` r
multidebug(MSEsingle, MSEmulti, p = 1, f = 1, MPno = 1, maxsims = 4)
```

## Arguments

- MSEsingle:

  An object of class MSE arising from a run of runMSE(OM, ...)

- MSEmulti:

  An object of class MMSE arising from a run of multiMSE(MOM, ...)

- p:

  Integer. The stock number from the MSEmulti object (to be plotted)

- f:

  Integer. The fleet number from the MSEmulti object (to be plotted)

- MPno:

  Integer. The MP number from the MSEmulti and MSEsingle object (to be
  plotted)

- maxsims:

  Integer. The maximum number of simulations to plot.

## Author

T.Carruthers
