# Initialize Operating Model

Generates an Excel spreadsheet and a source.rmd file in the current
working directory for specifying and documenting a MSEtool Operating
Model.

## Usage

``` r
OMinit(
  name = NULL,
  ...,
  files = c("xlsx", "rmd"),
  dir = NULL,
  overwrite = FALSE
)
```

## Arguments

- name:

  The name of the Excel and source.rmd file to be created in the working
  directory (character). Use 'example' for a populated example OM XL and
  documentation file.

- ...:

  Optional MSEtool objects to use as templates: OM, Stock, Fleet, Obs,
  or Imp objects

- files:

  What files should be created: 'xlsx', 'rmd', or c('xlsx', 'rmd')
  (default: both) to use as templates for the Operating Model.

- dir:

  Optional file path to create the xlsx and rmd files. Default is
  [`getwd()`](https://rdrr.io/r/base/getwd.html)

- overwrite:

  Logical. Should files be overwritten if they already exist?

## Value

name.xlsx and name.rmd files are created in the working directory.

## Author

A. Hordyk

## Examples

``` r
if (FALSE) { # \dontrun{
# Create an Excel OM template and rmd file called 'myOM.xlsx' and 'myOM.rmd':
OMinit('myOM')

# Create an Excel OM template and text file called 'myOM.rmd' and 'myOM.rmd', using
# another OM as a template:
OMinit('myOM', myOM)

# Create an Excel OM template and text file called 'myOM.rmd' and 'myOM.rmd', using
# the Stock object 'Herring' as a template:
OMinit('myOM', Herring)

# Create an Excel OM template and text file called 'myOM.rmd' and 'myOM.rmd', using
# the Stock object 'Herring', and Obs object 'Generic_obs' as templates:
OMinit('myOM', Herring, Generic_obs)
} # }
```
