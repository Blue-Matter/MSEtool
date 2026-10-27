# Generate a Data Report

A HTML Data Report is generated and opened in a web browser

## Usage

``` r
Report(
  Data = NULL,
  md = NULL,
  name = "Data-Report",
  title = "Data Documentation",
  author = "Author Name",
  date = Sys.Date(),
  output_format = c("html_document", "pdf_document"),
  open = TRUE,
  quiet = TRUE,
  dir = NULL,
  overwrite = FALSE
)
```

## Arguments

- Data:

  Either an object of class `Data` or the file path to a valid file to
  be imported with `XL2Data`

- md:

  Full file path to a valid text file documenting the Data

- name:

  Optional. Name of the output file

- title:

  Title for the Report. Title in the markdown file will override this
  value

- author:

  Author of the Report. Author in the markdown file will override this
  value

- date:

  Date of the Report. Date in the markdown file will override this value

- output_format:

  Output file format: `html_document` or `pdf_document`

- open:

  Logical. Open the compiled report?

- quiet:

  Logical.An option to suppress printing of the pandoc command line.

- dir:

  Optional. Directory to save the file. Defaults to
  [`getwd()`](https://rdrr.io/r/base/getwd.html)

- overwrite:

  Logical. Overwrite an existing file with the same name?

## Value

Nothing. A Data Report is generated and saved in `dir`

## Author

A. Hordyk

## Examples

``` r
if (FALSE) { # \dontrun{
DataInit('Example') # generate example Data Input and Documentation files
Report('Example', 'Example.md')
} # }
```
