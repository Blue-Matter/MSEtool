# Generate OM Documentation Report

Generate OM Documentation Report

## Usage

``` r
OMdoc(
  OM = NULL,
  rmd.source = NULL,
  overwrite = FALSE,
  out.file = NULL,
  inc.plot = TRUE,
  render = TRUE,
  output = "html_document",
  openFile = TRUE,
  quiet = FALSE,
  dir = NULL,
  ...
)
```

## Arguments

- OM:

  An object of class 'OM' or the name of an OM xlsx file

- rmd.source:

  Optional. Name of the source.rmd file corresponding to the 'OM'.
  Default assumption is that the file is 'OM@Name.Rmd'

- overwrite:

  Logical. Should existing files be overwritten?

- out.file:

  Optional. Character. Name of the output file. Default is the same as
  the text file.

- inc.plot:

  Logical. Should the plots be included?

- render:

  Logical. Should the document be compiled? May be useful to turn off if
  there are problems with compiling the Rmd file.

- output:

  Character. Output file type. Default is 'html_document'.
  'pdf_document' is available but may require additional software and
  have some formatting issues.

- openFile:

  Logical. Should the compiled file be opened in web browser?

- quiet:

  TRUE to suppress printing of the pandoc command line.

- dir:

  Optional file path to read the xlsx and rmd files. Default is
  [`getwd()`](https://rdrr.io/r/base/getwd.html)

- ...:

  Optional additional named arguments provided to `runMSE`

## Value

Creates a Rmarkdown file and compiles a HTML report file in the working
directory.

## Author

A. Hordyk

## Examples

``` r
if (FALSE) { # \dontrun{
OMinit('myOM', Stock='Herring', Fleet='Generic_Fleet', Obs='Generic_Obs',
Imp='Perfect_Imp', overwrite=TRUE)
myOM <- XL2OM('myOM.xlsx')
OMdoc(myOM)
} # }
```
