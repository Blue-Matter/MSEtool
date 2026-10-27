# Department of Fisheries and Oceans historical plot

A plot of current and historical stock status by simulation according to
the stock status zones and reference points of DFO.
http://www.dfo-mpo.gc.ca/reports-rapports/regs/sff-cpd/precaution-eng.htm

## Usage

``` r
DFO_hist(OM, panel = T, nsim = 48)
```

## Arguments

- OM:

  An operating model object of class OM

- panel:

  should the plots be separate or in two panels?

- nsim:

  how many simulations should be plotted (over-ridden by OM@nsim where
  cpars is specified)

## Author

T. Carruthers
