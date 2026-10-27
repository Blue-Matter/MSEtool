# Optimization function to find a movement model that matches user specified movement characteristics modified for Rcpp.

The user specifies the probability of staying in the same area and
spatial heterogeneity (both in the unfished state).

## Usage

``` r
getmov2(x, Prob_staying, Frac_area_1)
```

## Arguments

- x:

  A position in vectors Prob_staying and Frac_area_1

- Prob_staying:

  User specified probability that individuals in area 1 remain in that
  area (unfished conditions)

- Frac_area_1:

  User specified fraction of individuals found in area 1 (unfished
  conditions)

## Value

A markov movement matrix

## Details

This is paired with movfit to find the correct movement model.

## Author

T. Carruthers

## Examples

``` r

Prob_staying<-0.8 # probability  that individuals remain in area 1 between time-steps
Frac_area_1<-0.35 # the fraction of the stock found in area 1 under equilibrium conditions
markovmat<-getmov2(1,Prob_staying, Frac_area_1)
vec<-c(0.5,0.5) # initial guess at equilibrium distribution (2 areas)
for(i in 1:300)vec<-apply(vec*markovmat,2,sum) # numerical approximation to stable distribution
c(markovmat[1,1],vec[1]) # pretty close right?
#> [1] 0.7999999 0.3500000

```
