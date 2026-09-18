# Generalized version of Seurat:::BlendExpression

Seurat:::BlendExpression for arbitrary number of bins n = 1, means 10
bins n = 2, means 100 bins ...

## Usage

``` r
BlendExpression2(data, n = 1)
```

## Arguments

- data:

  2-column matrix with gene expression to be blended

- n:

  integer, defines number of bins = 10^n (default=1)

## Value

data.frame with blended expression

## Details

NOTE: not used
