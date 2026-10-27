# Combine data among stocks

Catches, CAA, CAL are summed. Indices, LFC and LFS are weighted
averages. ML, Lc and Lbar are recalculated from summed CAL. All other
observations are for fleet 1 and weighted average across stocks

## Usage

``` r
multiDataS(MSElist, Real.Data.Map, np, mm, nf, realVB)
```

## Arguments

- MSElist:

  A hierarchical list of data objects stock then fleet then MP

- Real.Data.Map:

  Matrix describing which data are mapped across stocks

- np:

  The number of stocks

- mm:

  Integer the MP number

- nf:

  The number of fleets

- realVB:

  A matrix of real vulnerable biomass `[nsim,np, year]`

## Author

T. Carruthers
