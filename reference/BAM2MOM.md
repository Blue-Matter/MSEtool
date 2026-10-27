# Import a multi-stock, multi-fleet OM from a BAM object

Import a multi-stock, multi-fleet OM from a BAM object

## Usage

``` r
BAM2MOM(
  rdat,
  nsim = 48,
  proyears = 50,
  interval = 1,
  stock_name = NULL,
  fleet_name = NULL,
  LowerTri = 0,
  report = FALSE,
  ...
)

BAM2OM(rdat, nsim = 48, proyears = 50, interval = 2, report = FALSE, ...)
```

## Arguments

- rdat:

  A list object from the `BAMextras` package. Use
  `bamExtras::standardize_rdat(rdat)`

- nsim:

  the number of simulations

- proyears:

  the number of projection years

- interval:

  the management interval

- stock_name:

  Name of the stock(s)

- fleet_name:

  Name of the fleet(s)

- LowerTri:

  Integer. The number of recent years for which model estimates of
  recruitment are ignored (not reliably estimated by the assessment)

- report:

  Logical, if TRUE, a diagnostic will be reported showing the matching
  of the OM reconstructed numbers at age vs the assessment.

- ...:

  Additional arguments passed to
  [`MSEtool::Assess2MOM`](https://msetool.openmse.com/reference/Assess2MOM.md)

## Value

An object of class `MOM`

## Functions

- `BAM2OM()`: Create a single stock/fleet OM from a BAM object
