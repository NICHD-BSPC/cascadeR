# Marker plots module

Marker plots module

## Usage

``` r
markerPlotUI(id, panel)

markerPlotServer(
  id,
  obj,
  filtered,
  genes_to_plot,
  args,
  gene_choices,
  all_selected,
  show_selection,
  reset_selection,
  reload_global,
  config
)
```

## Arguments

- id:

  Input id

- panel:

  string, can be 'sidebar' or 'main'

- obj:

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

- all_selected:

  reactive containing list of selected points

- show_selection:

  reactive to show selection

- reset_selection:

  reactive to reset selection

- reload_global:

  reactive to trigger reload

- config:

  reactive list with config settings

## Value

UI returns sidebar/main panel UI elements for marker plots Server
returns reactive expression containing selected points from marker plot
modules

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
  metadata_levels = list(
    all = metadata_levels,
    filtered = metadata_levels
  ),
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
  shinyjs::useShinyjs(),
  shiny::sidebarLayout(
    shiny::sidebarPanel(markerPlotUI("markers", "sidebar")),
    shiny::mainPanel(
      markerPlotUI("markers", "main"),
      shiny::verbatimTextOutput("selected")
    )
  )
)

server <- function(input, output, session) {
  selected <- markerPlotServer(
    "markers",
    obj = app_object,
    filtered = shiny::reactive({ colnames(obj) }),
    genes_to_plot = shiny::reactive({ c("GeneA", "GeneB") }),
    args = shiny::reactive({ global_args }),
    gene_choices = shiny::reactive({ rownames(obj) }),
    all_selected = shiny::reactive({ list() }),
    show_selection = shiny::reactive({ NULL }),
    reset_selection = shiny::reactive({ NULL }),
    reload_global = shiny::reactiveVal(0),
    config = shiny::reactive({ config })
  )

  output$selected <- shiny::renderPrint({
    selected()
  })
}

shiny::shinyApp(ui, server)
}
```
