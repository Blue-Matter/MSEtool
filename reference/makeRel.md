# MICE relationships for multi-OM

Generate a MICE `Rel` object, with `predict` and `simulate` methods, for
[multiMSE](https://msetool.openmse.com/reference/multiMSE.md). Currently
implements intra-stock dynamics via density-dependent processes.

## Usage

``` r
makeRel(type = "DDM", stock = 1, CV = 0, ...)

# S3 method for class 'Rel'
print(x, ...)

# S3 method for class 'Rel'
predict(object, newdata, ...)

# S3 method for class 'Rel'
simulate(object, nsim = 1, seed = 1, ...)
```

## Arguments

- type:

  String to indicate the type of stock interaction. `"DDM"` for
  density-dependent natural mortality.

- stock:

  The index position of the stock in the MOM.

- CV:

  Coefficient of variation of the predicted value for `simulate`. Used
  to pass values to the operating model.

- ...:

  Additional arguments depending on `type`. See details below.

- x:

  For `print.Rel`, a `Rel` class object from `make_Rel`.

- object:

  A `Rel` class object from `make_Rel`.

- newdata:

  A data frame to provide values of predictor variables with which to
  calculate the response variable.

- nsim:

  The number of simulations.

- seed:

  Integer to specify the seed for the random number generator.

## Value

A class "Rel" object to pass to `MOM@Rel`.

## Density-dependent M ("DDM")

Natural mortality (M) is a linear function of stock depletion in terms
to total biomass (B) in year y (Forrest et al. 2018): \$\$M_y = M_0 +
(M_1 + M_0) (1 - B_y/B_0)\$\$ with a constraint that \\M_y = M_0\\ if
\\B_y \> B_0\\

Provide the following arguments:

- `M0`: Natural mortality as B approaches B0. Vector `[nsim]`

- `M1`: Natural mortality as B approaches zero. Vector `[nsim]`

- Optional `B0`: Unfished biomass. Calculated from stock-recruit `alpha`
  and `beta` and unfished biomass per recruit at M = M0. Vector `[nsim]`

## References

Forrest, R., Holt, K., and Kronlund, A. 2018. Performance of alternative
harvest control rules for two Pacific groundfish stocks with uncertain
natural mortality: Bias, robustness and trade-offs. Fisheries Research
206: 259–286.
[doi:10.1016/j.fishres.2018.04.007](https://doi.org/10.1016/j.fishres.2018.04.007)

## Author

Q. Huynh

## Examples

``` r
# Depensatory natural mortality
Rel <- makeRel(type = "DDM", M0 = 0.8, M1 = 0.2, CV = 0.1)

# Predict M when B/B0 = 0.1
pred <- predict(Rel, newdata = data.frame(B_1 = 0.1, B0_1 = 1))

# Simulate values of M with CV = 0.1
Rel$fitted.values <- pred
simulate(Rel, nsim = 10, seed = 1)
#>       sim_1     sim_2     sim_3    sim_4     sim_5     sim_6     sim_7
#> 1 0.2430378 0.2634926 0.2380192 0.303335 0.2673545 0.2383794 0.2715994
#>       sim_8     sim_9    sim_10
#> 1 0.2784825 0.2740037 0.2509475

# Add Rel to MOM
MOM <- makeMOM(testOM)
MOM@Rel <- list(Rel)
```
