# Interactive umap plot

Interactive umap plot

## Usage

``` r
umap_ly(
  df,
  xcol,
  ycol,
  color,
  colors,
  split = NULL,
  label_cols = NULL,
  showlegend = TRUE,
  showticklabels = TRUE,
  type = "scatter",
  highlight_click = TRUE,
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

- color:

  column to color markers by

- colors:

  colors to use for markers

- split:

  metadata column to split by

- label_cols:

  Additional metadata columns to use for labeling pts

- showlegend:

  should legend be shown?

- showticklabels:

  should tick labels be drawn?

- type:

  type of plotly trace (default: 'scatter')

- highlight_click:

  should points be highlighted on clicking

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

plotly handle

## Examples

``` r
if (FALSE) { # interactive()
df <- data.frame(
  UMAP_1 = c(-1, 0, 1, 2),
  UMAP_2 = c(0, 1, 0, -1),
  cluster = factor(c("A", "A", "B", "B"))
)
colors <- c(A = "#4477aa", B = "#cc6677")

umap_ly(
  df,
  xcol = "UMAP_1",
  ycol = "UMAP_2",
  color = "cluster",
  colors = colors
)
}
```
