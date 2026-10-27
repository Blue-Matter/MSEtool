# Create a table of Performance Limits and Performance Objectives

Create a table of Performance Limits and Performance Objectives

## Usage

``` r
PMLimit(
  MSE,
  ...,
  Prob = NULL,
  Labels = NULL,
  FeaseMPs = NULL,
  out.file = NULL,
  output_format = "html_document",
  openFile = TRUE,
  quiet = TRUE,
  dir = NULL,
  RMDfile = NULL,
  font_size = 14,
  auto_width = FALSE,
  enableSearch = TRUE,
  PMlist = NULL,
  build = TRUE
)

PMObj(
  MSE,
  ...,
  Labels = NULL,
  out.file = NULL,
  output_format = "html_document",
  openFile = TRUE,
  quiet = TRUE,
  dir = NULL,
  RMDfile = NULL,
  font_size = 14,
  use.colors = TRUE,
  cols = NULL,
  show.legend = TRUE,
  auto_width = FALSE,
  enableSearch = TRUE,
  PMlist = NULL,
  build = TRUE,
  cex.tex = 0.75,
  inc.title = TRUE,
  title = "Legend"
)
```

## Arguments

- MSE:

  An object of class 'MSE'

- ...:

  PM objects to be used as performance limits. Characters (i.e names of
  PM objects)

- Prob:

  Minimum probability threshold

- Labels:

  Optional named list specifying new labels for MPs. For example:
  `Labels = list(AvC="Average Catch", CC1="Constant Catch")`

- FeaseMPs:

  Optional. Character vector of MP names that are considered feasible.
  e.g. the output from
  [`Fease()`](https://msetool.openmse.com/reference/Fease.md)

- out.file:

  Name of the output file. If none provided, output file will be named
  'PerfLimTable'

- output_format:

  Output file format. Currently only 'html_document' is supported

- openFile:

  Logical. Should the file be opened in browser?

- quiet:

  Logical. An option to suppress printing of the pandoc command line.

- dir:

  Optional. Directory for output file. Default is working directory.

- RMDfile:

  Optional. RMD template file

- font_size:

  Numeric. Font size for text in the table

- auto_width:

  Logical. Should table be width be automatic?

- enableSearch:

  Currently disabled. Logical. Should search be enabled in the html
  table?

- PMlist:

  Optional. List of PM names.

- build:

  Logical. Build the html table?

- use.colors:

  Logical. Color scale the probability text?

- cols:

  Optional character vector of colors for probability text

- show.legend:

  Logical. Show the legend??

- cex.tex:

  Size of legend text

- inc.title:

  Logical. Include title for legend?

- title:

  Title for the legend

## Value

`PMLimit` invisibly returns names of MPs that pass all performance
limits

## Functions

- `PMLimit()`: Create a table of Performance Limits

- `PMObj()`: Create a table of Performance Objectives.

## Author

A. Hordyk

## Examples

``` r
if (FALSE) { # \dontrun{
MSE <- runMSE()
PMLimit(MSE, "P50", "PNOF", Prob=0.9)
PMObj(MSE, "P100", "LTY")
} # }
```
