# Lag the time-series slots in a `Data` object by a specified number of time-steps

Lag the time-series slots in a `Data` object by a specified number of
time-steps

## Usage

``` r
Lag_Data(Data, Data_Lag = 0, msg = FALSE)
```

## Arguments

- Data:

  An object of class `Data`

- Data_Lag:

  Either a numeric vector of length 1 with a positive number specifying
  the number of time-steps to lag all time-series data, or a named list
  with numeric values (length 1). See details for more information.

- msg:

  Logical. Display the messages?

## Value

An object of class `Data` with time-series slots lagged.

## Details

By default, all simulated data in the forward projections are provided
up to the previous time-step. That is, in projection year `t`, the
simulated data are up to and including `t-1`. This function will lag the
time-series values by the specified value. For example, `Data_Lag=5`
will mean in projection time-step `t` the data will be up to and
including `t-6`.

*Note*: The `Data@Year` slot is *not* lagged by this function. Many
built-in MPs use the length of `Data@Year` to determine the number of
years of data for smoothing over recent years etc. This may not be
appropriate so check the MP is behaving as you expect if you use
`Lag_Data`.

## Examples

``` r
# Lag all time-series slots by 2 time-steps (usually years)
Data <- Example_datafile
Lagged_1 <- Lag_Data(Data, 2)
length(Data@Year)
#> [1] 13
length(Lagged_1@Year)
#> [1] 13
length(Data@Cat[1,])
#> [1] 13
length(Lagged_1@Cat[1,])
#> [1] 11
length(Data@Ind[1,])
#> [1] 13
length(Lagged_1@Ind[1,])
#> [1] 11

# Lag CAA by 5 and Ind by 3 time-steps
Lagged_2 <- Lag_Data(Data, Data_Lag=list(CAA=5, Ind=3))
length(Lagged_2@Year)
#> [1] 13
length(Lagged_2@Cat[1,])
#> [1] 13
dim(Data@CAA[1,,])
#> [1] 13 16
dim(Lagged_2@CAA[1,,])
#> [1]  8 16

length(Data@Ind[1,])
#> [1] 13
length(Lagged_2@Ind[1,])
#> [1] 10
```
