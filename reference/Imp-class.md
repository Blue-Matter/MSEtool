# Class `'Imp'`

An operating model component that specifies the degree of adherence to
management recommendations (Implementation error)

## Slots

- `Name`:

  The name of the Implementation error object. Single value. Character
  string.

- `Name`:

  The name of the Implementation error object. Single value. Character
  string.

- `TACFrac`:

  Mean fraction of recommended TAC that is actually taken. For each
  historical simulation a single value is drawn from a uniform
  distribution specified by the upper and lower bounds provided. This
  value is the mean TAC fraction obtained across all years of that
  simulation, and a yearly TAC frac is drawn from a log-normal
  distribution with the simulation mean and a coefficient of variation
  specified by the value of `TACSD` drawn for that simulation. If the
  value drawn is greater than 1 the amount of catch taken is greater
  than that recommended by the TAC, and if it is less than 1 the amount
  of catch taken is less than that recommended by the TAC. Positive real
  numbers.

- `TACSD`:

  Log-normal coefficient of variation in the fraction of recommended TAC
  that is actually taken. For each historical simulation a single value
  is drawn from a uniform distribution specified by the upper and lower
  bounds provided. This value is used, along with the `TACFrac` drawn
  for that simulation, to create a log-normal distribution that yearly
  values specifying the actual amount of catch taken are drawn from.
  Positive real numbers.

- `TAEFrac`:

  Mean fraction of recommended TAE that is actually taken. For each
  historical simulation a single value is drawn from a uniform
  distribution specified by the upper and lower bounds provided. This
  value is the mean TAE fraction obtained across all years of that
  simulation, and a yearly TAE frac is drawn from a log-normal
  distribution with the simulation mean and a coefficient of variation
  specified by the value of `TAESD` drawn for that simulation. If the
  value drawn is greater than 1 the amount of effort employed is greater
  than that recommended by the TAE, and if it is less than 1 the amount
  of effort employed is less than that recommended by the TAE. Positive
  real numbers.

- `TAESD`:

  Log-normal coefficient of variation in the fraction of recommended TAE
  that is actually taken. For each historical simulation a single value
  is drawn from a uniform distribution specified by the upper and lower
  bounds provided. This value is used, along with the `TAEFrac` drawn
  for that simulation, to create a log-normal distribution that yearly
  values speciying the actual amount of efort employed are drawn from.
  Positive real numbers.

- `SizeLimFrac`:

  Mean fraction of recommended size limit that is actually retained. For
  each historical simulation a single value is drawn from a uniform
  distribution specified by the upper and lower bounds provided. This
  value is the mean size limit fraction obtained across all years of
  that simulation, and a yearly size limit fraction is drawn from a
  log-normal distribution with the simulation mean and a coefficient of
  variation specified by the value of `SizeLimSD` drawn for that
  simulation. If the value drawn is greater than 1 the size of fish
  retained is greater than that recommended by the size limit, and if it
  is less than 1 the amount of size of fish retained is less than that
  recommended by the size limit. Positive real numbers.

- `SizeLimSD`:

  Log-normal coefficient of variation in the fraction of recommended
  size limit that is actually retained. For each historical simulation a
  single value is drawn from a uniform distribution specified by the
  upper and lower bounds provided. This value is used, along with the
  `SizeLimFrac` drawn for that simulation, to create a log-normal
  distribution that yearly values speciying the actual fraction of the
  size limit retained are drawn from. Positive real numbers.

## Objects from the Class

Objects can be created by calls of the form `new('Imp')`\#'

## Author

T. Carruthers and A. Hordyk

## Examples

``` r

showClass('Imp')
#> Class "Imp" [package "MSEtool"]
#> 
#> Slots:
#>                                                                               
#> Name:         Name     TACFrac       TACSD     TAEFrac       TAESD SizeLimFrac
#> Class:   character     numeric     numeric     numeric     numeric     numeric
#>                   
#> Name:    SizeLimSD
#> Class:     numeric
#> 
#> Known Subclasses: "OM"
```
