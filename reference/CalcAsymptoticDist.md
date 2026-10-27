# Calculates the asymptotic distribution from an initial distribution vector (V) and a markov movement matrix (M) (rows sum to 1)

Calculates the asymptotic distribution from an initial distribution
vector (V) and a markov movement matrix (M) (rows sum to 1).

## Usage

``` r
CalcAsymptoticDist(M, V = NULL, nits = 50, plot = F)
```

## Arguments

- M:

  A square markov movement matrix M of nareas rows and nareas columns
  (rows sum to 1)

- V:

  An optional vector nareas long of initial fractions by area (if
  unspecified the calculation will start from a uniform distribution)

- nits:

  An integer number of iterations for multiplying V by M to get the
  asymptotic distribution (~50 is usually enough)

- plot:

  Should the convergence to a stable distribution be plotted?

## See also

[simmov2](https://msetool.openmse.com/reference/simmov2.md)

## Author

T. Carruthers
