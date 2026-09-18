# Dotplot module

Dotplot module

## Usage

``` r
dotPlotUI(id, panel)

dotPlotServer(
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

UI returns sidebar/main panel UI elements for dotplot module Server
called for the side effect of rendering a dotplot.

## Examples

``` r
if (FALSE) { # interactive()
# example obj
obj <- make_example_seurat_object()

# prep metadata
metadata <- obj[[]]
metadata_levels <- lapply(
  metadata[c("cluster", "condition", "orig.ident", "seurat_clusters")],
  levels
)

# get grouping vars and colors
grouping_vars <- names(metadata_levels)
names(grouping_vars) <- paste0(
  grouping_vars,
  " (n = ",
  lengths(metadata_levels),
  ")"
)
cluster_colors <- lapply(metadata_levels, function(lvls) {
  stats::setNames(rep_len(c("#4477aa", "#cc6677"), length(lvls)), lvls)
})

app_object <- list(
  rds = obj,
  obj_type = "seurat",
  metadata = metadata,
  metadata_levels = metadata_levels,
  cluster_colors = cluster_colors,
  grouping_vars = grouping_vars,
  spatial_coords = NULL,
  imagerow_max = NULL,
  imagerow_min = NULL
)

global_args <- list(
  assay = "RNA",
  slot = "data",
  grp_by = "cluster",
  dimred = "umap"
)

config <- get_config()

ui <- shiny::fluidPage(
  shiny::sidebarPanel(dotPlotUI("dotplot", "sidebar")),
  shiny::mainPanel(shiny::tabsetPanel(dotPlotUI("dotplot", "main")))
)

server <- function(input, output, session) {
  dotPlotServer(
    "dotplot",
    app_object = shiny::reactive({ app_object }),
    filtered = shiny::reactive({ colnames(obj) }),
    genes_to_plot = shiny::reactive({ c("GeneA", "GeneB") }),
    args = shiny::reactive({ global_args }),
    gene_choices = shiny::reactive({ rownames(obj) }),
    reload_global = shiny::reactiveVal(0),
    refresh = shiny::reactiveVal(0),
    config = shiny::reactive({ config })
  )
}

shiny::shinyApp(ui, server)
}
```
