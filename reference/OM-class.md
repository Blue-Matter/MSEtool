# Class `'OM'`

An object containing all the parameters needed to control the MSE which
can be build from component Stock, Fleet, Obs, and Imp objects.

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

  A list of custom parameters. Time series are a matrix nsim rows by
  nyears columns. Single parameters are a vector nsim long. See
  [`validcpars()`](https://msetool.openmse.com/reference/validcpars.md)

- `seed`:

  A random seed to ensure users can reproduce results exactly

- `Source`:

  A reference to a website or article from which parameters were taken
  to define the operating model

## Objects from the Class

Objects can be created by calls of the form
`new('OM', Stock, Fleet, Obs, Imp)`.

## Author

T. Carruthers and A. Hordyk
