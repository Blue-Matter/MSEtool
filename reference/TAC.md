# Calculate TAC recommendations for more than one MP

A function that returns the stochastic TAC recommendations from a vector
of output control MPs given a data object Data

## Usage

``` r
TAC(Data, MPs = NA, reps = 100, timelimit = 1, checkMP = TRUE, silent = FALSE)
```

## Arguments

- Data:

  A data-limited methods data object

- MPs:

  optional vector of MP names

- reps:

  Number of repetitions

- timelimit:

  The maximum time (seconds) taken to complete 10 reps

- checkMP:

  Logical. Check if the MP can be run first?

- silent:

  Logical. Suppress messages?

## Author

T. Carruthers

## Examples

``` r
if (FALSE) { # \dontrun{
library(MSEtool)
Data <- TAC(MSEtool::Cobia)
plot(Data)
} # }
```
