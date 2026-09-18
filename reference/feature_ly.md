# Interactive feature plot

Interactive feature plot

## Usage

``` r
feature_ly(
  df,
  xcol,
  ycol,
  color,
  colors,
  crange = NULL,
  split = NULL,
  label_cols = NULL,
  row_view = "auto",
  showscale = TRUE,
  reversescale = FALSE,
  showticklabels = TRUE,
  marker_size = 2,
  margin = 0.03,
  alpha = 0.3,
  free_axes = FALSE,
  reorder = TRUE,
  width = NULL,
  height = NULL,
  title_mode = "color",
  source = "A"
)
```

## Arguments

- df:

  data.frame with expresion data & plot coordinates

- xcol:

  x-axis coordinates

- ycol:

  y-axis coordinates

- color:

  color column. should be numeric

- colors:

  plotly color scale. should be continuous

- crange:

  range of gene expression to show on plot

- split:

  column to split plots by

- label_cols:

  Additional metadata columns to use for labeling pts

- row_view:

  how should plots be laid out? can be 'single' or 'auto'

- showscale:

  show the color scale?

- reversescale:

  should we reverse the color scale?

- showticklabels:

  should tick labels be drawn?

- marker_size:

  marker size

- margin:

  plot margin

- alpha:

  marker opacity

- free_axes:

  should subplots have free axes?

- reorder:

  should be sort cells in ascending order of expression?

- width:

  width of plot in pixels

- height:

  height of plot in pixels

- title_mode:

  string specifying how axes should be titled. If 'color' (default), the
  y-axis is titled with the coloring variable; if 'xy', xcol and ycol
  are used as labels.

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
  GeneA = c(0, 2, 4, 8)
)

feature_ly(
  df,
  xcol = "UMAP_1",
  ycol = "UMAP_2",
  color = "GeneA",
  colors = "Viridis",
  crange = c(0, 8)
)
}
```
