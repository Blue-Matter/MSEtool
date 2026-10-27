# Stock recruit parameterization

Convert stock recruit parameters from steepness parameterization to
alpha/beta (and vice versa)

## Usage

``` r
hconv(alpha, phi0, SR = 1, type = 1)

R0conv(alpha, beta, phi0, SR = 1, type = 1)

SRalphaconv(h, phi0, SR = 1, type = 1)

SRbetaconv(h, R0, phi0, SR = 1, type = 1)
```

## Arguments

- alpha:

  Alpha parameter

- phi0:

  Unfished spawners per recruit

- SR:

  Stock-recruit function: (1) Beverton-Holt, or (2) Ricker

- type:

  The parameterization of the Beverton-Holt function with respect to
  `alpha` and `beta`. See details.

- beta:

  Beta parameter

- h:

  Steepness parameter

- R0:

  Unfished recruitment parameter

## Value

A numeric.

## Details

The Type 1 Beverton-Holt equation is \$\$R = \alpha S/(1 + \beta S)\$\$

The Type 2 Beverton-Holt equation is \$\$R = S/(\alpha + \beta S)\$\$

The Ricker equation is \$\$R = \alpha S \exp(-\beta S)\$\$

## Functions

- `hconv()`: Returns steepness (h) from `alpha` and `phi0`

- `R0conv()`: Returns unfished recruitment (R0) from `alpha`, `beta`,
  and `phi0`

- `SRalphaconv()`: Returns `alpha` from `h` and `phi0`

- `SRbetaconv()`: Returns `beta` from `h`, `R0`, and `phi0`

## Author

Q. Huynh
