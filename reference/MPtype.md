# Management Procedure Type

Management Procedure Type

## Usage

``` r
MPtype(MPs = NA)
```

## Arguments

- MPs:

  A vector of MP names. If none are provided function is run on all
  available MPs

## Value

A data.frame with MP names, management type (e.g "Input", "Output") and
management recommendations returned by the MP (e.g, TAC (total allowable
catch), TAE (total allowable effort), SL (size-selectivity), and/or or
Spatial)

## See also

[Required](https://msetool.openmse.com/reference/Required.md)

## Examples

``` r
MPtype(c("AvC", "curE", "matlenlim", "MRreal", "FMSYref"))
#> Warning: Some MPs are not found in environment: AvC, Some MPs are not found in environment: curE, Some MPs are not found in environment: matlenlim, Some MPs are not found in environment: MRreal
#>          MP      Type    Recs
#> 1   FMSYref     Input     TAE
#> 2   FMSYref Reference     TAE
#> 3       AvC   unknown unknown
#> 4      curE   unknown unknown
#> 5 matlenlim   unknown unknown
#> 6    MRreal   unknown unknown
#> 7   FMSYref      <NA>     TAE
#> 8   FMSYref      <NA>     TAE
#> 9   FMSYref      <NA>     TAE
```
