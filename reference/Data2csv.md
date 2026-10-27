# Converts a Data object into a .csv data file

A function that writes a correctly formatted .csv file from a MSEtool
Data object

## Usage

``` r
Data2csv(Data, file = NULL, simno = 1, overwrite = F, keepNAs = T)
```

## Arguments

- Data:

  An object of class 'Data'.

- file:

  Character string. The name of the location and file you wish to create
  (e.g. "C:/temp/mydata.csv")

- simno:

  Integer. An optional argument to specify the simulation number if
  writing simulated data

- overwrite:

  Boolean. Should existing data files be automatically overwritten.

- keepNAs:

  Boolean. Should slots with NAs still be written to the data file.

## Author

T. Carruthers
