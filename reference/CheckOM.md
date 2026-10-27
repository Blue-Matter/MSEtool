# Check OM object is complete

Check OM object is complete

## Usage

``` r
CheckOM(OM, msg = TRUE, stop_if_missing = TRUE)
```

## Arguments

- OM:

  An object of class `OM`

- msg:

  Logical. Display messages?

- stop_if_missing:

  Logical. Stop with error is values are missing and there is no
  default?

## Value

The OM object with default values (if needed)

## Examples

``` r
testOM <- CheckOM(testOM)
```
