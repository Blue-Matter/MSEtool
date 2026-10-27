# A formatted version of the standard DFO performance plot, color coded by thresholds

Crit_S is the probability of being in the critical zone in the first 10
projected years Caut_S is the probability of being in the cautious zone
in the first 10 projected years Health_S is the probability of being in
the healthy zone in the first 10 projected years OvFish_S is the
probability of overfishing in the first 10 projected years Yield_S is
the mean yield relative to FMSY management over the first 10 projected
years Crit is the probability of being in the critical zone in the last
10 projected years Caut is the probability of being in the cautious zone
in the last 10 projected years Health is the probability of being in the
healthy zone in the last 10 projected years OvFish is the probability of
overfishing in the last 10 projected years Yield is the mean yield
relative to FMSY management over the last 10 projected years AAVY is the
average annual variability in yield over the whole projection phrased as
a CV percentage Reb is the probability the stock has rebuilt to over
BMSY in 2 mean generation times

## Usage

``` r
DFO_tab_formatted(
  Ptab1,
  thresh = c(30, 50, 40, 60, 50, 20, 40, 50, 60, 50, 30, 50),
  ret_thresh = F
)
```

## Arguments

- Ptab1:

  A DFO performance table made by DFO_tab()

- thresh:

  A vector of thresholds for each column Health, Yield and Reb are
  'greater than threshold' conditions

- ret_thresh:

  Logical: if true just the threshold levels are returned

## Author

T. Carruthers
