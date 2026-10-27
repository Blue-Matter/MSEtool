# Identify management procedures (MPs) based on data availability

Diagnostic tools that look up the slot requirements of each MP and
compares to the data available in the Data object.

## Usage

``` r
Can(Data, timelimit = 1, MPs = NA, dev = FALSE, silent = FALSE)

Cant(Data, timelimit = 1, silent = FALSE)

DLMdiag(
  Data,
  command = c("available", "not available", "needed"),
  reps = 5,
  timelimit = 1,
  funcs1 = NA,
  dev = FALSE,
  silent = FALSE
)

Needed(Data, timelimit = 1, silent = FALSE)
```

## Arguments

- Data:

  A data-limited methods data object (class Data)

- timelimit:

  The maximum time (seconds) taken for an MP to undertake 5 reps (this
  filters out methods that are too slow)

- MPs:

  Optional list of MP names

- dev:

  Logical. Run in development mode?

- silent:

  Logical Display messages?

- command:

  What to calculate? Character. Options = c("available", "not
  available", "needed")

- reps:

  The number of replicates for the MP

- funcs1:

  A character vector of the MP names (optional)

## Functions

- `Can()`: Identifies MPs that have the correct data, do not produce
  errors, and run within the time limit.

- `Cant()`: Identifies MPs that don't have sufficient data, lead to
  errors, or don't run in time along with a list of their data
  requirements.

- `DLMdiag()`: Internal function called by `Can` and `Cant`

- `Needed()`: Identifies what data are needed to run the MPs that are
  currently not able to run given a Data object

## See also

[avail](https://msetool.openmse.com/reference/avail.md)
[Data](https://msetool.openmse.com/reference/Data-class.md)

## Examples

``` r
CanMPs <- Can(MSEtool::Cobia)
#> ✔ Checking  Emp  ( 1 / 6 )
#> ✔ Checking  FMSYref  ( 2 / 6 )
#> ✔ Checking  FMSYref50  ( 3 / 6 )
#> ✔ Checking  FMSYref75  ( 4 / 6 )
#> ✔ Checking  NFref  ( 5 / 6 )
#> ✔ Checking  curEref  ( 6 / 6 )
CantMPs <- Cant(MSEtool::Cobia)
#> ✔ Checking  Emp  ( 1 / 6 )
#> ✔ Checking  FMSYref  ( 2 / 6 )
#> ✔ Checking  FMSYref50  ( 3 / 6 )
#> ✔ Checking  FMSYref75  ( 4 / 6 )
#> ✔ Checking  NFref  ( 5 / 6 )
#> ✔ Checking  curEref  ( 6 / 6 )
Needs <- Needed(MSEtool::Cobia)
```
