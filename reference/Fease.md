# MP feasibility diagnostic

What MPs may be run (best case scenario) for various data-availability
scenarios and management constraints?

## Usage

``` r
Fease(
  Data = NULL,
  TAC = TRUE,
  TAE = TRUE,
  SL = TRUE,
  Spatial = TRUE,
  names.only = TRUE,
  msg = TRUE,
  include.ref = FALSE
)
```

## Arguments

- Data:

  An object of class 'Data'. Optional. If Data object is included, the
  returned MPs are both feasible (in terms of management) and possible
  (sufficient data to run MP)

- TAC:

  Logical. Are catch limits feasible for this fishery?

- TAE:

  Logical. Are effort controls feasible for this fishery?

- SL:

  Logical. Are size-selectivity regulations (either gear changes or
  size-retention regulations) feasible for this fishery?

- Spatial:

  Logical. Are spatial closures feasible for this fishery?

- names.only:

  Logical. Should only the names of the feasible MPs be returned
  (default)? If FALSE, a data frame with MP name, and two columns of
  logical values: Can (possible given data) and Fease (feasible given
  management constraints) is returned

- msg:

  Logical. Should messages be printed to the console?

- include.ref:

  Logical. Should reference MPs (e.g. FMSYref) be included as feasible
  methods? Default is FALSE

## Value

Either a vector of MP names that are feasible for the fishery (default)
or a 3 column data frame (`names.only=FALSE`).

## Author

T. Carruthers & A. Hordyk

## Examples

``` r
if (FALSE) { # \dontrun{
Fease(TAC=FALSE)
Fease(SL=FALSE, Spatial=FALSE)
Fease(Atlantic_mackerel, TAE=FALSE, names.only=FALSE)
} # }
```
