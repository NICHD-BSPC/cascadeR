# Interactive violin plot

Violin plot implemented in plotly. This works fine, but is a bit
slower/takes the same time as the ggplot version. But the main issue is
that combining multiple plots using subplot() results in very skinny
violins.

- This might be a good option for single gene plots.

## Usage

``` r
violin_ly(df, xcol, ycol, color, colors, showlegend = FALSE)
```

## Arguments

- df:

  data.frame with expression data

- xcol:

  x-axis grouping column

- ycol:

  values to show on y-axis

- color:

  column to split violins by

- colors:

  vector of colors to use (not used)

- showlegend:

  boolean, should legend be shown? Default: FALSE

## Value

plotly handle

## Details

NOTE: not used
