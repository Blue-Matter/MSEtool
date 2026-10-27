# Remove observation, implementation, and process error

Takes an existing OM object and converts it to one without any
observation error, implementation error, very little process error,
and/or gradients in life history parameters and catchability.

## Usage

``` r
tinyErr(x, ...)

# S4 method for class 'OM'
tinyErr(x, obs = TRUE, imp = TRUE, proc = TRUE, grad = TRUE, silent = FALSE)
```

## Arguments

- x:

  An object of class `OM`

- ...:

  Arguments to generic function

- obs:

  Logical. Remove observation error? `Obs` is replaced with
  `Perfect_Info`

- imp:

  Logical. Remove implementation error? `Imp` is replaced with
  `Perfect_Imp`

- proc:

  Logical. Remove process error? All `sd` and `cv` slots in `Stock` and
  `Fleet` object are set to 0.

- grad:

  Logical. Remove gradients? All `grad` slots in `Stock` and `qinc` in
  `Fleet` are set to 0.

- silent:

  Logical. Display messages?

## Value

An updated object of class `OM`

## Details

Useful for debugging and testing that MPs perform as expected under
perfect conditions.

## Examples

``` r
OM_noErr <- tinyErr(MSEtool::testOM)
#> ✔ Removing all Observation Error
#> ✔ Removing all Implementation Error
#> ✔ Removing all Process Error
#> ✔ Removing all Gradients
```
