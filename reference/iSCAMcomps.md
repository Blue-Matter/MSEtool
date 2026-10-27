# Combines all iSCAM age composition data across fleets

iSCAM assessments are often fitted to numerous fleets that have
differing age selectivities. iSCAMcomps is a simple way of providing the
aggregate catch at age data. It should be noted that this process is
important and in a real application would require due diligence (ie peer
reviewed data workshop).

## Usage

``` r
iSCAMcomps(replist, Year)
```

## Arguments

- replist:

  S3 class object: the output from a read from an iSCAM data folder

- Year:

  Integer vector: the years of the data object ie Data@Year

## Author

T. Carruthers
