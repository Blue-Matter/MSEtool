# Initialize Data Input Files

Creates template for the Data input file (Excel or CSV) and Data
documentation file (Markdown) in the working directory or the directory
specified by the `dir` argument

## Usage

``` r
DataInit(name = "Data", ext = c("xlsx", "csv"), overwrite = FALSE, dir = NULL)
```

## Arguments

- name:

  Name of the data input files. Default is 'Data'. Use 'Example' to
  create populated example Data Input and Data Documentation files.

- ext:

  Optional file extension for input file. 'xlsx' (default) or 'csv'

- overwrite:

  Logical. Overwrite existing files?

- dir:

  Optional directory path to create the Data files. Default is
  \`getwd()“

## Value

Nothing. Creates template data files in the working directory.

## Author

A. Hordyk

## Examples

``` r
if (FALSE) { # \dontrun{
DataInit("Example") # populated example
DataInit("myData") # empty template
} # }
```
