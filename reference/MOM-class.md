# Class `'MOM'`

An object containing all the parameters needed to control a multi-stock,
multi-fleet MSE which can be build from component Stock, Fleet, Obs, and
Imp objects.

## Details

Almost all of these inputs are a vector of length 2 which describes the
upper and lower bounds of a uniform distribution from which to sample
the parameter.

## Slots

- `Name`:

  Name of the operating model

- `Agency`:

  Name of the agency responsible for the management of the fishery.
  Character string

- `Region`:

  Name of the general geographic region of the fishery. Character string

- `Sponsor`:

  Name of the organization who sponsored the OM. Character string

- `Latitude`:

  Latitude (decimal degrees). Negative values represent the South of the
  Equator. Numeric. Single value

- `Longitude`:

  Longitude (decimal degrees). Negative values represent the West of the
  Prime Meridian. Numeric. Single value

- `nsim`:

  The number of simulations

- `proyears`:

  The number of projected years

- `interval`:

  The assessment interval - how often would you like to update the
  management system?

- `pstar`:

  The percentile of the sample of the management recommendation for each
  method

- `maxF`:

  Maximum instantaneous fishing mortality rate that may be simulated for
  any given age class

- `reps`:

  Number of samples of the management recommendation for each method.
  Note that when this is set to 1, the mean value of the data inputs is
  used.

- `cpars`:

  A hierarchical list nstock then nfleet long of custom parameters. Time
  series are a matrix nsim rows by nyears columns. Single parameters are
  a vector nsim long. See
  [`validcpars()`](https://msetool.openmse.com/reference/validcpars.md)

- `seed`:

  A random seed to ensure users can reproduce results exactly

- `Source`:

  A reference to a website or article from which parameters were taken
  to define the operating model

- `Stocks`:

  List of stock objects

- `Fleets`:

  List of Fleet objects

- `Obs`:

  Hierarchical List of Observation model objects Level 1 is stock, level
  2 is fleet

- `Imps`:

  Hierarchical List of Implementation model objects Level 1 is stock,
  level 2 is fleet

- `CatchFrac`:

  A list nstock long, of matrices nsim x nfleet representing the
  fraction of current catches of the various fleets to each stock (each
  matrix is nsim by nfleet long and rows sum to 1 for each stock)

- `Allocation`:

  A list nstock long, of matrices nsim x nfleet representing the
  fraction of future TACs of the various fleets to each stock (each
  matrix is nsim by nfleet long and rows sum to 1 for each stock).

- `Efactor`:

  A list nstock long, of current effort factors by fleet (default is 1 -
  same as current effort)

- `Complexes`:

  A list of stock complexes. Each position is a vector of stock numbers
  (as they appear in StockPars) for which data should be aggregated and
  TAC recommendations split among stocks according to vulnerable biomass

- `SexPars`:

  A list of slots that control sex-specific dynamics, i.e., sex-specific
  spawning and hermaphroditism. More generally, controls spawning and
  moving abundance between stocks. See details.

- `Rel`:

  A list of biological / ecological relationships among stocks
  over-ridden if an MP of class 'MP_F" is supplied that is a multi-fleet
  MP.

## Objects from the Class

Objects can be created by calls of the form
`new('MOM', Stock_list, Fleet_list, Obs_list, Imp_list)`.

## SexPars

- `SSBfrom` A nstock x nstock matrix that specifies the proportion of
  the spawning output of the row p stock for the column p' stock. A
  diagonal matrix means each stock is responsible for its own
  recruitment.

- `Herm` A list with each entry containing a matrix (nsim x maxage + 1)
  that specifies the proportion at age that moves from stock p to p'
  (sequential hermaphroditism). The names of the list should be of the
  form "H_p'\_p" where p and p' are integers that identify the stock.
  Arrays can also be used (nsim x maxage + 1 x nyears + proyears) for
  time-varying values.

- `share_par` Optional. Logical to indicate whether stock-recruit,
  depletion, and observation/implementation parameters are mirrored
  between stocks. By default, TRUE.

## See also

Article on `MOM` and `multiMSE`:
<https://openmse.com/features-multimse/>

## Author

T. Carruthers and A. Hordyk
