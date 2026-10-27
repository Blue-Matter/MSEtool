# COSEWIC MSE run using the correct MPs and projected time horizon

Dedicated functions for MSE run and reporting for COSEWIC (Committee on
the Status of Endangered Wildlife in Canada). MSE projects for 6x
maximum age using NFref, FMSYref and curE management procedures.

## Usage

``` r
runCOSEWIC(OM, ...)

COSEWIC_Pplot(
  MSEobj,
  syear = 2017,
  qcol = "#FFCB62",
  quants = c(0.05, 0.25, 0.5, 0.75, 0.95)
)

COSEWIC_Dplot(
  MSEobj,
  syear = 2017,
  qcol = "#79F48D",
  quants = c(0.05, 0.25, 0.5, 0.75, 0.95),
  nGT = 3
)

COSEWIC_Blow(
  MSEobj,
  syear = 2017,
  qcol = rgb(0.4, 0.8, 0.95),
  quants = c(0.05, 0.25, 0.5, 0.75, 0.95),
  nGT = 3
)

COSEWIC_Hplot(
  MSEobj,
  syear = 2017,
  qcol = rgb(0.4, 0.8, 0.95),
  quants = c(0.05, 0.25, 0.5, 0.75, 0.95)
)

COSEWIC_report(
  MSEobj,
  output_file = NA,
  author = "Author not specified",
  title = NA
)

COSEWIC_tab(MSEobj, rnd = 0, GTs = c(3, 6), syear = 2017, nGT = 3)

COSEWIC_tab_formatted(
  Ptab1,
  thresh = c(20, 40, 40, 20, 40, 40, 40, 30, 5),
  ret_thresh = F
)
```

## Arguments

- OM:

  An operating model object of class OM

- ...:

  Other named arguments to pass to `runMSE`

- MSEobj:

  An object of class MSE with `MPs = c("NFref", "FMSYref", "curE")`

- syear:

  Current year, starting year for projections (e.g. 2017)

- qcol:

  Color of shaded regions (bars, quantiles)

- quants:

  Quantiles of the shaded regions (vector 5 long e.g. 0.1, 0.2, 0.5,
  0.8, 0.9)

- nGT:

  Number of generation times. For `COSEWIC_tab`, for moving window of
  SSB chance (metrics A1 and A2). For `COSEWIC_Blow` and
  `COSEWIC_Dplot`, used for projections (the number of projection years
  should be greater than `MaxAge * nGT`).

- output_file:

  The directory and filename you wish to use for the report e.g.
  "C:/temp/myMSEreport.html"

- author:

  The person who made this report

- title:

  The title of the report

- rnd:

  The number of significant figures for rounding.

- GTs:

  A vector of mean generation times to evaluate performance metrics over

- Ptab1:

  A COSEWIC performance table made by `COSEWIC_tab`

- thresh:

  A vector of thresholds for each column Health, Yield and Reb are
  'greater than threshold' conditions

- ret_thresh:

  Logical: if true just the threshold levels are returned

## Functions

- `runCOSEWIC()`: Calls `runMSE` with number of projection years for 6x
  maximum age and uses `NFref`, `FMSYref`, and `curE` MPs.

- `COSEWIC_Pplot()`: Projection plots of spawning stock biomass under
  three scenarios: no catch, FMSY fishing and status quo fishing effort.

- `COSEWIC_Dplot()`: Depletion plots evaluate whether significant
  declines have occurred over three generation times in both historical
  and projection years.

- `COSEWIC_Blow()`: Plots that evaluate the likelihood of declining
  below `Blow`, by default, biomass that takes 3 generation times to
  reach half BMSY with zero fishing

- `COSEWIC_Hplot()`: Plots of historical spawning stock relative to
  unfished and MSY levels.

- `COSEWIC_report()`: Create a standard DFO COSEWIC report (provides
  performance plots to inform COSEWIC processes in Canadian fish
  stocks).

- `COSEWIC_tab()`: Creates a standard COSEWIC performance table:

  - `P_Cr` is the probability of being in the critical zone (less than
    20% depletion)

  - `P_Ct` is the probability of being in the cautious zone (between 20%
    and 40% depletion)

  - `P_H` is the probability of being in the healthy zone (above 40%
    depletion)

  - `P_Cr_MSY` is the probability of being in the critical zone (less
    than 40% BMSY)

  - `P_Ct_MSY` is the probability of being in the cautious zone (between
    40% and 80% BMSY)

  - `P_H_MSY` is the probability of being in the healthy zone (above 80%
    BMSY)

  - `Caut` is the probability of being in the cautious zone in the last
    10 projected years

  - `P_A1` is the probability of being designated threatened according
    to COSEWIC Indicator A1 (Spawning biomass less than 70% that three
    generation times previously)

  - `P_A2` is the probability of being designated threatened according
    to COSEWIC Indicator A2 (Spawning biomass less than 50% that three
    generation times previously)

  - `Blow` is the probability that the stock is below the biomass for
    which it takes 3 generation times to reach 50% BMSY with zero
    fishing

- `COSEWIC_tab_formatted()`: A formatted version of the standard COSEWIC
  performance plot, color coded by thresholds.

## References

<https://cosewic.ca/index.php/en/>

## Author

T. Carruthers
