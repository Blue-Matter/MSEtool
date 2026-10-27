# Returns Results of a set of MICE relationships

Predicts stock-specific parameters from another stocks biomass, spawning
biomass or numbers

## Usage

``` r
ResFromRel(Rel, Bcur, SSBcur, Ncur, SSB0, B0, seed, x, y)
```

## Arguments

- Rel:

  A list of inter-stock relationships see slot Rel of MOM object class

- Bcur:

  An array of current stock biomass `[stock, age, area]`

- SSBcur:

  An array of current spawning stock biomass `[stock, age, area]`

- Ncur:

  An array of current stock numbers `[stock, age, area]`

- SSB0:

  A vector of unfished spawning biomass `[stock]`

- B0:

  A vector of unfished biomass `[stock]`

- seed:

  Seed for sampling.

- x:

  The simulation number.

- y:

  The current year of the simulation.

## Author

T. Carruthers, Q. Huynh
