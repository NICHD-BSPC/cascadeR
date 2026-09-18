# Bin gene expression in columns of a data frame

The expression of a gene present in columns of a data frame are binned
based either on the range of observed expression ('range') or quantiles
of cells where the gene is expressed.

## Usage

``` r
get_binned_exp(df, bins = 100, mode = "range")
```

## Arguments

- df:

  data.frame with expression data

- bins:

  number of bins

- mode:

  how to calculate bins? options are 'range' or 'quantile'

## Value

data.frame
