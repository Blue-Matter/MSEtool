# Optimization function for CSRA

What depletion level and corresponding equilibrium F arise from data
regarding mean length of current catches, natural mortality rate,
steepness of the stock recruitment curve, maximum length, maximum growth
rate, age at maturity, age based vulnerability, maturity at age, maximum
age and number of historical years of fishing.

## Usage

``` r
CSRAfunc(
  lnF,
  Mc,
  hc,
  maxage,
  nyears,
  AFSc,
  AFCc,
  Linfc,
  Kc,
  t0c,
  AMc,
  ac,
  bc,
  vulnc,
  matc,
  MLc,
  CAL,
  CAA,
  opt = T,
  meth = "ML"
)
```

## Arguments

- lnF:

  A proposed value of current instantaneous fishing mortality rate

- Mc:

  Natural mortality rate estimates

- hc:

  Steepness (Beverton-Holt stock recruitment)

- maxage:

  Maximum age

- nyears:

  Number of historical years of fishing

- AFSc:

  Age at full selection

- AFCc:

  Age at first capture

- Linfc:

  Maximum length (von Bertalanffy growth)

- Kc:

  Maximum growth rate (von Bertalanffy growth)

- t0c:

  Theoretical age at length zero (von Bertalanffy growth)

- AMc:

  Age at maturity

- ac:

  Length-weight conversion parameter a (W=aL^b)

- bc:

  Length-weight conversion parameter b (W=aL^b)

- vulnc:

  A vector (nage long) of the vulnerability at age (max 1) to fishing.

- matc:

  A vector (nage long) of the maturity at age (max 1)

- MLc:

  A current mean length estimates

- CAL:

  A catch-at-length matrix nyears x (1 Linf unit) length bins

- CAA:

  A catch-at-age matrix nyears x maximum age

- opt:

  Should the measure of fit be returned?

- meth:

  Are we fitting to mean length or catch composition?

## Author

T. Carruthers
