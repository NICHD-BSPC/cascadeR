# Line plot module

Line plot module

## Usage

``` r
linePlotUI(id, panel)

linePlotServer(
  id,
  app_object,
  filtered,
  genes_to_plot,
  args,
  gene_choices,
  reload_global,
  refresh,
  config
)
```

## Arguments

- id:

  Input id

- panel:

  string, can be 'sidebar' or 'main'

- app_object:

  Cascade app object

- filtered:

  barcodes to filter object

- genes_to_plot:

  reactive list with genes in scratchpad

- args:

  reactive list with elements: 'assay' for selected assay, 'dimred' for
  which dimension reduction to use and 'grp_by' for grouping variable

- gene_choices:

  reactive list with all genes present in object

- reload_global:

  reactive to trigger reload

- refresh:

  reactive to trigger plot refresh from sidebar button

- config:

  reactive list with config settings

## Value

Shiny UI & server elements for the line plot module

Shiny module server return value; called for the side effect of
rendering a line plot.
