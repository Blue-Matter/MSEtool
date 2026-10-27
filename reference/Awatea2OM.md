# Reads MCMC estimates from Awatea (Paul Starr) processed r file structure into an operating model

A function that generates an operating model from the MCMC samples of an
Awatea model. Code optimized for the BC Pacific ocean perch assessment
(Haigh et al. 2018).

## Usage

``` r
Awatea2OM(
  AwateaDir,
  nsim = 48,
  proyears = 50,
  Name = "OM made by Awatea2OM",
  Source = "No source provided",
  Author = "No author provided",
  verbose = TRUE
)
```

## Arguments

- AwateaDir:

  A folder with Awatea files

- nsim:

  The number of simulations

- proyears:

  The number of projection years for the MSE

- Name:

  The name of the operating model

- Source:

  Reference to assessment documentation e.g. a url

- Author:

  Who did the assessment

- verbose:

  Return detailed messages?

## Details

This function averages biological parameters across sex and then sends
arrays to [VPA2OM](https://msetool.openmse.com/reference/Assess2OM.md),
assumes unfished status (B/B0 = 1) in the first year, and assumes a
single fishing fleet.

## References

Haigh, R., et al. 2018. Stock assessment for Pacific Ocean Perch
(*Sebastes alutus*) in Queen Charlotte Sound, British Columbia in 2017.
Canadian Science Advisory Secretariat (CSAS) Research Document 2018/038.
232 pp.
<https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2018/2018_038-eng.html>

## Author

Q. Huynh and T. Carruthers
