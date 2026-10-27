# Department of Fisheries and Oceans default plot 2

A preliminary plot for returning trade-offs plots and performance table
for probability of obtaining half reference (FMSY) yield and probability
of biomass dropping below 50 per cent BMSY

## Usage

``` r
DFO_plot2(MSEobj, nam = NA, panel = T, Bcut = 50, Ycut = 50)
```

## Arguments

- MSEobj:

  An object of class MSE

- nam:

  Title of plot

- panel:

  Should the plots be organized in many panels in a single figure

- Bcut:

  The cutoff biomass for satisficing (relative to BMSY)

- Ycut:

  the cutoff yield for satisficing (relative to reference yield)

## Value

A table of performance metrics.

## Author

T. Carruthers
