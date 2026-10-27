# What management procedures need what data

A function that finds all the MPs and searches the function text for
slots in the Data object

## Usage

``` r
Required(funcs = NA, noCV = FALSE)
```

## Arguments

- funcs:

  A character vector of management procedures

- noCV:

  Logical. Should the CV slots be left out?

## Value

A matrix of MPs and their required data in terms of `slotnames('Data')`,
and broad Data classes for each MP

## See also

[Can](https://msetool.openmse.com/reference/Can.md)
[Cant](https://msetool.openmse.com/reference/Can.md)
[Needed](https://msetool.openmse.com/reference/Can.md)
[MPtype](https://msetool.openmse.com/reference/MPtype.md)
[Data](https://msetool.openmse.com/reference/Data-class.md)

## Author

T. Carruthers
