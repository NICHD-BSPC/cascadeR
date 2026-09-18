# Function to generate coexpression plot legend

This interactive legend is shown in the sidebar to help visually select
coexpression blend threshold.

## Usage

``` r
get_coexp_legend(
  colors,
  dimnames,
  xline,
  yline,
  n = 100,
  neutral_color = "lightgray",
  margin = 0.03
)
```

## Arguments

- colors:

  3-color vector (geneA, geneB, both)

- dimnames:

  names to show on x & y axis

- xline:

  x-axis threshold

- yline:

  y-axis threshold

- n:

  number of bins

- neutral_color:

  color to use for negative cells

- margin:

  plot margin

## Value

plotly handle

## Examples

``` r
if (FALSE) { # interactive()
get_coexp_legend(
  colors = c("#ff0000", "#0000ff", "#ff00ff"),
  dimnames = c("GeneA", "GeneB"),
  xline = 0.25,
  yline = 0.75,
  n = 20,
  neutral_color = "#eeeeee"
)
}
```
