# Function to run a set of input control methods

Runs a set of input control methods are returns the output in a single
table

## Usage

``` r
Input(Data, MPs = NA, reps = 100, timelimit = 10, CheckMPs = TRUE, msg = TRUE)
```

## Arguments

- Data:

  A Data object

- MPs:

  A list of input MPs, if NA all available input MPs are run

- reps:

  Number of repetitions (for those methods that use them)

- timelimit:

  Maximum timelimit to run MP (in seconds)

- CheckMPs:

  Logical, the Can function is run if this is TRUE

- msg:

  Logical. Should messages be printed?

## Author

A. Hordyk

## Examples

``` r
if (FALSE) { # \dontrun{
library(MSEtool)
Input(MSEtool::Cobia)
} # }
```
