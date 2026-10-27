# Class `'Rec'`

An object for storing the MP recommendations

## Slots

- `TAC`:

  A numeric value with the TAC recommendation

- `Effort`:

  A numeric value with the effort recommendation as a fraction of
  current (nyear) fishing effort

- `Spatial`:

  A boolean vector of length 'nareas' specifying if area is open (1) or
  closed (0) to fishing

- `Allocate`:

  A boolean value describing if effort should be re-allocated from close
  to open areas

- `LR5`:

  smallest length at 5 per cent retention - in absolute units - i.e same
  units as Linf and L50

- `LFR`:

  smallest length at full retention - in absolute units - i.e same units
  as Linf and L50

- `HS`:

  upper harvest slot (no retention above this) - in absolute units - i.e
  same units as Linf and L50

- `Rmaxlen`:

  retention of the largest size class - fraction between 0 and 1

- `L5`:

  smallest length at 5 per cent selection - in absolute units - i.e same
  units as Linf and L50

- `LFS`:

  smallest length at full selection - in absolute units - i.e same units
  as Linf and L50

- `Vmaxlen`:

  selection of the largest size class - fraction between 0 and 1

- `Fdisc`:

  fraction of discarded fish that die - fraction between 0 and 1

- `DR`:

  Discard rate - the fraction of caught fish that are discarded

- `Misc`:

  An empty list that can be used to store information and pass on to MPs
  in future. Selectivity-at-age and retention-at-age (vectors of length
  `maxage+1`) for the projection can be set directly with `Misc$V_age`
  and `Misc$R_age`, and discard mortality-at-age with `Misc$Fdisc`.
  These replace the selectivity and retention parameters in the other
  slots. The discard ratio is applied to `Misc$R_age`, and the discard
  mortality of the operating model is used if `Misc$Fdisc` is not
  provided. The at-length curves used to generate length composition
  data are not updated.

## Objects from the Class

Objects can be created by calls of the form `new('Rec')`

## Author

A. Hordyk
