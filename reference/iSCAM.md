# Reads iSCAM files into a hierarchical R list object

Internal functions for reading iSCAM input and output files into R

## Usage

``` r
load.iscam.files(model.dir, burnin = 1000, thin = 1, verbose = FALSE)

fetch.file.names(path, filename)

read.report.file(fn)

read.data.file(file = NULL, verbose = FALSE)

read.control.file(
  file = NULL,
  num.gears = NULL,
  num.age.gears = NULL,
  verbose = FALSE
)

read.projection.file(file = NULL, verbose = FALSE)

read.par.file(file = NULL, verbose = FALSE)

read.mcmc(model.dir = NULL, verbose = TRUE)
```

## Arguments

- model.dir:

  Folder name

- burnin:

  The initial mcmc samples to be discarded

- thin:

  The degree of chain thinning 1 in every thin iterations is kept

- verbose:

  should detailed results be printed to console

- path:

  File path

- filename:

  The filename

- fn:

  File location

- file:

  File location

- num.gears:

  The number of gears

- num.age.gears:

  The number age-gears

## Functions

- `load.iscam.files()`: Wrapper function to generate R list

- `fetch.file.names()`: A function for returning the three types of
  iSCAM input and output files

- `read.report.file()`: A function for returning the results of the .rep
  iscam file

- `read.data.file()`: A function for returning the results of the .dat
  iscam file

- `read.control.file()`: A function for returning the results of the
  iscam control file

- `read.projection.file()`: A function for returning the results of the
  iscam projection file

- `read.par.file()`: A function for returning the results of the iscam
  .par file

- `read.mcmc()`: A function for returning the results of the iscam mcmc
  files

## See also

[iSCAM2OM](https://msetool.openmse.com/reference/iSCAM2OM.md)

## Author

Chris Grandin (DFO PBS)
