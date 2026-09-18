# Binned coexpression plot for 2 features

This is a wrapper around umap_ly that uses four colors for four
categories instead of a blend matrix.

## Usage

``` r
feature_blend(
  df,
  xcol,
  ycol,
  blend_cols,
  colors,
  split = NULL,
  col_threshold_1 = 0.5,
  col_threshold_2 = 0.5,
  n = 100,
  bin_mode = "range",
  neutral_color = "lightgray",
  showlegend = TRUE,
  showticklabels = TRUE,
  type = "scattergl",
  marker_size = 2,
  margin = 0.03,
  alpha = 0.3,
  free_axes = FALSE,
  width = NULL,
  height = NULL,
  source = "A"
)
```

## Arguments

- df:

  data.frame with plotting data

- xcol:

  x-axis coordinates

- ycol:

  y-axis coordinates

- blend_cols:

  columns to blend

- colors:

  3 color vector (geneA, geneB, merge)

- split:

  metadata column to split plot by

- col_threshold_1:

  blending threshold for gene 1

- col_threshold_2:

  blending threshold for gene 2

- n:

  number of bins for color range

- bin_mode:

  how to bin? can be 'range' (gene expression binned into n bins) or
  'quantile' (cells binned into n bins by exp)

- neutral_color:

  double negative color

- showlegend:

  should legend be shown?

- showticklabels:

  should tick labels be drawn?

- type:

  type of plotly trace (default: 'scattergl')

- marker_size:

  size of markers

- margin:

  margin of plot (should be between 0 & 1)

- alpha:

  marker opacity

- free_axes:

  should subplots have free axes?

- width:

  width of plot in pixels. If NULL (default), plot is auto-sized.

- height:

  height of plot in pixels. if NULL (default), plot is auto-sized.

- source:

  name of source to return data from

## Value

list with plot data and plotly handle

## Examples

``` r
if (FALSE) { # interactive()
df <- data.frame(
  UMAP_1 = c(-1, 0, 1, 2),
  UMAP_2 = c(0, 1, 0, -1),
  GeneA = c(0, 5, 0, 10),
  GeneB = c(0, 0, 8, 10)
)

blended <- feature_blend(
  df,
  xcol = "UMAP_1",
  ycol = "UMAP_2",
  blend_cols = c("GeneA", "GeneB"),
  colors = c("#ff0000", "#0000ff", "#ff00ff"),
  n = 10
)
names(blended)
blended$data
}
```
