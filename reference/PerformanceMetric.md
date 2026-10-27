# Performance Metrics Methods

Performance metric (PMs) methods for your management strategy
evaluation.

## Usage

``` r
P10(MSEobj = NULL, Ref = 0.1, Yrs = NULL)

P50(MSEobj = NULL, Ref = 0.5, Yrs = NULL)

P100(MSEobj = NULL, Ref = 1, Yrs = NULL)

PNOF(MSEobj = NULL, Ref = 1, Yrs = NULL)

LTY(MSEobj = NULL, Ref = 0.5, Yrs = -10)

STY(MSEobj = NULL, Ref = 0.5, Yrs = 10)

Yield(MSEobj = NULL, Ref = 1, Yrs = NULL)

PGK(MSEobj = NULL, Ref = 1, Yrs = NULL)

AAVY(MSEobj = NULL, Ref = 0.2, Yrs = NULL)

AAVE(MSEobj = NULL, Ref = 0.2, Yrs = NULL)
```

## Arguments

- MSEobj:

  An object of class MSE

- Ref:

  Reference point for calculating the performance metric. See details.

- Yrs:

  Numeric vector of length 2 with year indices to summarize performance.
  If NULL, the performance is summarized over all projection years.

## Value

An object of class `PMobj`

## Details

Performance Metric definitions:

|         |                                                                |
|---------|----------------------------------------------------------------|
| `P10`   | Probability B \> 0.1 BMSY                                      |
| `P50`   | Probability B \> 0.5 BMSY                                      |
| `P100`  | Probability B \> BMSY                                          |
| `PNOF`  | Probability F \< FMSY                                          |
| `LTY`   | Probability Long-Term Yield \> 0.5 Relative Yield              |
| `STY`   | Probability Short-Term Yield \> 0.5 Relative Yield             |
| `AAVY`  | Probability AAVY \< 0.2 (Average Annual Variability in Yield)  |
| `AAVE`  | Probability AAVE \< 0.2 (Average Annual Variability in Effort) |
| `Yield` | Average Yield (relative to Reference Yield)                    |

Argument `Ref` provides the ratio relative to the reference point for
calculating the performance metric. For biomass-based PMs (`P10`, `P50`,
`P100`), this is the fraction of BMSY. For `PNOF`, the fraction of FMSY.
For `Yield` (and `LTY`/`STY`), the fraction of the Reference Yield. For
`AAVY` is it the maximum acceptable variability in yield (i.e, default
for `AAVY` is `Ref=0.2`)

The `Yrs` argument defines the number of years to calculate the
performance statistic over. A value of `NULL`, the default for `AAVY`,
`AAVE`, `P10`, `P50`, `P100`, and `PNOF`, means that the performance
metric is calculated over all projection years. A numeric vector of
length two is used to specify the first and last year, e.g, if
`Yrs=c(1,10)` the performance statistic is calculated over the first 10
projection years. A numeric vector of length one with positive or
negative value respectively can be used to specify the first *x* or last
*x* years, e.g, `Yrs=10` is first 10 years, and `Yrs=-10` is the last 10
years. See [`ChkYrs`](https://msetool.openmse.com/reference/ChkYrs.md)
for more details.

By default Long-Term Yield (`LTY`) is the Yield in the last ten years of
the projection period in the MSE, and Short-Term Yield (`STY`) is that
in the first 10 years of the projection period.

## Examples

``` r
if (FALSE) { # \dontrun{
myMSE <- runMSE()
P10(myMSE)
P50(myMSE)
P100(myMSE)
PNOF(myMSE)
LTY(myMSE)
STY(myMSE)
AAVY(myMSE)
AAVE(myMSE)
Yield(myMSE)
} # }
```
