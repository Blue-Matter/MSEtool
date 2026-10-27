# Combine data among fleets

Catches, CAA, CAL are summed. LFC and LFS are weighted averages. ML, Lc
and Lbar are recalculated from summed CAL. All other observations are
for fleet 1 (indicative)

## Usage

``` r
multiData(MSElist, StockPars, p, mm, nf)
```

## Arguments

- MSElist:

  A hierarchical list of data objects stock then fleet then MP

- StockPars:

  A list of stock parameters

- p:

  Integer the Stock number

- mm:

  Integer the MP number

- nf:

  The number of fleets

## Author

T. Carruthers
