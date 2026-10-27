# Setup parallel processing

Sets up parallel processing using the snowfall package

## Usage

``` r
setup(cpus = NULL, logical = FALSE, ...)
```

## Arguments

- cpus:

  the number of CPUs to use for parallel processing. If left empty all
  physical cores will be used, unless `logical=TRUE`, in which case both
  physical and logical (virtual) cores will be used.

- logical:

  Use the logical cores as well? Using the virtual cores may not lead to
  any significant decrease in run time. You can test the optimal number
  of cores using
  [`optCPU()`](https://msetool.openmse.com/reference/optCPU.md)

- ...:

  other arguments passed to 'snowfall::sfInit'

## Examples

``` r
if (FALSE) { # \dontrun{
setup() # set-up the physical processors
setup(6) # set-up 6 processors
setup(logical=TRUE) # set-up physical and logical cores
} # }
```
