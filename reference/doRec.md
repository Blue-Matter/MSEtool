# Calculate a management recommendation given constraints

Creates a TAC management recommendation given constraints on how much
that can change from previous TAC and contraints on minimum and maximum
TAC

## Usage

``` r
doRec(MPrec, mod, delta_down, delta_up, TACrng)
```

## Arguments

- MPrec:

  Positive real number, the previous management recommendation (e.g. 100
  tonnes).

- mod:

  Imperfect fraction, the proposed modification (change to MPrec) (e.g.
  1.2 is a 20% increase)

- delta_down:

  A vector 2 positions long, the minimum and maximum levels of downward
  change (e.g. when mod \< 1) in the recommendation.

- delta_up:

  A vector 2 positions long, the minimum and maximum levels of upward
  change (e.g. when mod \> 1) in the recommendation.

- TACrng:

  A vector 2 positions long, the minimum and maximum TAC (same units as
  MPrec).

## Value

n object of class
[Rec](https://msetool.openmse.com/reference/Rec-class.md).

## Author

T. Carruthers
