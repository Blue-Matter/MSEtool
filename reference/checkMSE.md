# Utility functions for MSE objects

Utility functions for MSE objects

## Usage

``` r
checkMSE(MSEobj)

addMPs(MSEobjs)

joinMSE(MSEobjs = NULL)

joinHist(Hist_List)

updateMSE(MSEobj, save.name = NULL)
```

## Arguments

- MSEobj:

  A `MSE` object

- MSEobjs:

  A list of MSE objects

- Hist_List:

  A list of objects of class `Hist`

- save.name:

  Character string. Optional file name to save the updated MSE object to
  disk.

## Value

An object of class `MSE`

A new object of class `Hist`

## Functions

- `checkMSE()`: Check that an MSE object includes all slots in the
  latest version of DLMtool

- `addMPs()`: Adds additional MPs to an MSE object by combining multiple
  MSE objects that have identical historical OM values but different
  MPs.

- `joinMSE()`: Joins two or more MSE objects together across
  simulations. MSE objects must have identical number of historical
  years, and projection years.

- `joinHist()`: Join objects of class `Hist`. Does not join slot `OM`

- `updateMSE()`: Updates an existing MSE object (class MSE) from a
  previous version of the MSEtool to include slots new to the latest
  version. Also works with Stock, Fleet, Obs, Imp, and Data objects. The
  new slots will be empty, but avoids the 'slot doesn't exist' error
  that sometimes occurs. Returns an object of class matching
  class(MSEobj)

## See also

[`addMMPs()`](https://msetool.openmse.com/reference/addMMPs.md)

[joinData](https://msetool.openmse.com/reference/joinData.md)

## Author

A. Hordyk
