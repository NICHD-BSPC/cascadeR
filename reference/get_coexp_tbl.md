# Get counts table based on coexpression

Get data frame with counts of cells binned into four categories based on
expression of two genes

## Usage

``` r
get_coexp_tbl(
  df,
  genes,
  n = 100,
  threshold1 = 0.5,
  threshold2 = 0.5,
  bin_mode = "range"
)
```

## Arguments

- df:

  data.frame with gene expression data

- genes:

  genes to calculate coexpression for

- n:

  number of bins

- threshold1:

  percentile to define expression for gene 1

- threshold2:

  percentile to define expression for gene 2

- bin_mode:

  how to bin? can be 'range' (gene expression binned into n bins) or
  'quantile' (cells binned into n bins by expression quantiles)

## Value

data.frame with co-expression counts

## Details

- both genes low

- gene A low, gene B high

- gene A high, gene B low

- gene A high, gene B high

## Examples

``` r
df <- data.frame(
  GeneA = c(0, 10, 0, 10),
  GeneB = c(0, 0, 10, 10)
)

get_coexp_tbl(df, genes = c("GeneA", "GeneB"), n = 10)
#>                              labels # cells  %
#> GeneA < 50%; GeneB < 50%    neither       1 25
#> GeneA > 50%; GeneB < 50% GeneA only       1 25
#> GeneA < 50%; GeneB > 50% GeneB only       1 25
#> GeneA > 50%; GeneB > 50%       both       1 25
```
